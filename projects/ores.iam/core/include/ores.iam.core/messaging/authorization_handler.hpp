/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_IAM_MESSAGING_AUTHORIZATION_HANDLER_HPP
#define ORES_IAM_MESSAGING_AUTHORIZATION_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.iam.api/domain/permission_codes.hpp"
#include "ores.iam.api/messaging/authorization_protocol.hpp"
#include "ores.iam.core/messaging/principal.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.iam.core/repository/tenant_lookups.hpp"
#include "ores.iam.core/service/authorization_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <optional>
#include <string>

namespace ores::iam::messaging {

namespace {

inline auto& authorization_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.iam.messaging.authorization_handler");
    return instance;
}

/*
 * The service answers a composed read; the response is the wire shape of that
 * answer. The mapping is field for field so the two shapes stay one change
 * apart rather than two declarations that drift.
 */
inline get_account_roles_response to_response(service::account_access answer) {
    get_account_roles_response response;
    response.result = std::move(answer.result);
    response.roles.reserve(answer.roles.size());
    for (auto& entry : answer.roles) {
        response.roles.push_back(
            account_role_access{.role = std::move(entry.role),
                                .permission_codes = std::move(entry.permission_codes),
                                .assigned_by = std::move(entry.assigned_by),
                                .assigned_at = entry.assigned_at,
                                .change_reason_code = std::move(entry.change_reason_code),
                                .change_commentary = std::move(entry.change_commentary)});
    }
    return response;
}

/*
 * The sentence every operation answers when the authenticated actor names no
 * account in the tenant. One string, so the sites cannot drift apart.
 */
constexpr std::string_view no_account_answer =
    "The authenticated actor names no account in this tenant";

/*
 * A response for a failure that stopped the work rather than a refusal the
 * operation decided. The outcome is set because the default is ok, and an
 * exception reported as ok would read as an account that holds nothing.
 */
inline ores::utility::domain::result failed_result(const std::string& message) {
    ores::utility::domain::result result;
    result.outcome = ores::utility::domain::outcome::failed;
    result.message = message;
    return result;
}

} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::log_handler_entry;
using namespace ores::logging;
using ores::service::messaging::error_reply;

/**
 * @brief Why a revoke is refused, or nothing when it may go ahead.
 *
 * Nobody takes a role away from their own account: an administrator who did
 * could remove the access they need to put it back, and a change to one's own
 * access is a decision another person makes.
 */
inline std::optional<std::string> authorization_revoke_refusal(const boost::uuids::uuid& caller,
                                                               const boost::uuids::uuid& account) {
    if (caller == account)
        return "You cannot take a role away from yourself.";
    return std::nullopt;
}

class authorization_handler {
public:
    authorization_handler(ores::nats::service::client& nats,
                          ores::database::context ctx,
                          ores::security::jwt::jwt_authenticator signer)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , signer_(std::move(signer)) {}

    void assign(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(authorization_handler_lg(), msg);
        auto req = decode<assign_role_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(authorization_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            auto ctx_expected = ores::service::service::make_request_context(
                ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>{signer_});
            if (!ctx_expected) {
                error_reply(nats_, msg, ctx_expected.error());
                return;
            }
            const auto& ctx = *ctx_expected;
            boost::uuids::string_generator sg;
            service::authorization_service svc(ctx);
            const auto caller_id = svc.caller_account();
            if (!caller_id) {
                reply(nats_,
                      msg,
                      assign_role_response{.success = false,
                                           .error_message = std::string{no_account_answer}});
                return;
            }
            if (!svc.has_permission(*caller_id, domain::permissions::roles_assign)) {
                BOOST_LOG_SEV(authorization_handler_lg(), warn)
                    << msg.subject << " denied: caller lacks iam::roles:assign permission";
                reply(nats_,
                      msg,
                      assign_role_response{.success = false,
                                           .error_message =
                                               "Permission denied: iam::roles:assign required"});
                return;
            }
            svc.assign_role(sg(req->account_id),
                            sg(req->role_id),
                            ctx.actor(),
                            req->change_commentary.empty() ? "Role assigned to account" :
                                                             req->change_commentary,
                            req->change_reason_code);
            BOOST_LOG_SEV(authorization_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, assign_role_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(authorization_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, assign_role_response{.success = false, .error_message = e.what()});
        }
    }

    void revoke(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(authorization_handler_lg(), msg);
        auto req = decode<revoke_role_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(authorization_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            auto ctx_expected = ores::service::service::make_request_context(
                ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>{signer_});
            if (!ctx_expected) {
                error_reply(nats_, msg, ctx_expected.error());
                return;
            }
            const auto& ctx = *ctx_expected;
            boost::uuids::string_generator sg;
            service::authorization_service svc(ctx);
            const auto caller_id = svc.caller_account();
            if (!caller_id) {
                reply(nats_,
                      msg,
                      revoke_role_response{.success = false,
                                           .error_message = std::string{no_account_answer}});
                return;
            }
            if (!svc.has_permission(*caller_id, domain::permissions::roles_revoke)) {
                BOOST_LOG_SEV(authorization_handler_lg(), warn)
                    << msg.subject << " denied: caller lacks iam::roles:revoke permission";
                reply(nats_,
                      msg,
                      revoke_role_response{.success = false,
                                           .error_message =
                                               "Permission denied: iam::roles:revoke required"});
                return;
            }
            const auto account_id = sg(req->account_id);
            if (const auto refusal = authorization_revoke_refusal(*caller_id, account_id)) {
                reply(
                    nats_, msg, revoke_role_response{.success = false, .error_message = *refusal});
                return;
            }
            svc.revoke_role(account_id, sg(req->role_id));
            BOOST_LOG_SEV(authorization_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, revoke_role_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(authorization_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, revoke_role_response{.success = false, .error_message = e.what()});
        }
    }

    void by_account(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(authorization_handler_lg(), msg);
        auto req = decode<get_account_roles_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(authorization_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            auto ctx_expected = ores::service::service::make_request_context(
                ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>{signer_});
            if (!ctx_expected) {
                error_reply(nats_, msg, ctx_expected.error());
                return;
            }
            const auto& ctx = *ctx_expected;
            boost::uuids::string_generator sg;
            service::authorization_service svc(ctx);
            const auto caller_id = svc.caller_account();
            if (!caller_id) {
                reply(nats_,
                      msg,
                      get_account_roles_response{
                          .result = failed_result(std::string{no_account_answer})});
                return;
            }
            auto answer = svc.read_account_access(*caller_id, sg(req->account_id));
            BOOST_LOG_SEV(authorization_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, to_response(std::move(answer)));
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(authorization_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_account_roles_response{.result = failed_result(e.what())});
        }
    }

    void mine(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(authorization_handler_lg(), msg);
        try {
            auto ctx_expected = ores::service::service::make_request_context(
                ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>{signer_});
            if (!ctx_expected) {
                error_reply(nats_, msg, ctx_expected.error());
                return;
            }
            const auto& ctx = *ctx_expected;
            service::authorization_service svc(ctx);
            const auto caller_id = svc.caller_account();
            if (!caller_id) {
                reply(nats_,
                      msg,
                      get_account_roles_response{
                          .result = failed_result(std::string{no_account_answer})});
                return;
            }
            auto answer = svc.read_own_access(*caller_id);
            BOOST_LOG_SEV(authorization_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, to_response(std::move(answer)));
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(authorization_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_account_roles_response{.result = failed_result(e.what())});
        }
    }

    void permissions(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(authorization_handler_lg(), msg);
        auto req = decode<get_role_permissions_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(authorization_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            auto ctx_expected = ores::service::service::make_request_context(
                ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>{signer_});
            if (!ctx_expected) {
                error_reply(nats_, msg, ctx_expected.error());
                return;
            }
            const auto& ctx = *ctx_expected;
            service::authorization_service svc(ctx);
            boost::uuids::string_generator sg;
            auto codes = svc.get_role_permissions(sg(req->role_id));
            BOOST_LOG_SEV(authorization_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, get_role_permissions_response{.permission_codes = std::move(codes)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(authorization_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_role_permissions_response{.result = failed_result(e.what())});
        }
    }

    void put_permissions(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(authorization_handler_lg(), msg);
        auto req = decode<put_role_permissions_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(authorization_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            auto ctx_expected = ores::service::service::make_request_context(
                ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>{signer_});
            if (!ctx_expected) {
                error_reply(nats_, msg, ctx_expected.error());
                return;
            }
            const auto& ctx = *ctx_expected;
            boost::uuids::string_generator sg;
            service::authorization_service svc(ctx);
            const auto caller_id = svc.caller_account();
            if (!caller_id) {
                reply(nats_,
                      msg,
                      get_role_permissions_response{
                          .result = failed_result(std::string{no_account_answer})});
                return;
            }
            auto answer = svc.replace_role_permissions(*caller_id,
                                                       sg(req->role_id),
                                                       req->permission_codes,
                                                       req->change_reason_code,
                                                       req->change_commentary);
            BOOST_LOG_SEV(authorization_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_,
                  msg,
                  get_role_permissions_response{.result = std::move(answer.result),
                                                .permission_codes =
                                                    std::move(answer.permission_codes)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(authorization_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_role_permissions_response{.result = failed_result(e.what())});
        }
    }

    void assign_by_name(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(authorization_handler_lg(), msg);
        auto req = decode<assign_role_by_name_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(authorization_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            auto ctx_expected = ores::service::service::make_request_context(
                ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>{signer_});
            if (!ctx_expected) {
                error_reply(nats_, msg, ctx_expected.error());
                return;
            }
            const auto& ctx = *ctx_expected;
            service::authorization_service caller_svc(ctx);
            const auto caller_id = caller_svc.caller_account();
            if (!caller_id) {
                reply(nats_,
                      msg,
                      assign_role_by_name_response{
                          .success = false, .error_message = std::string{no_account_answer}});
                return;
            }
            if (!caller_svc.has_permission(*caller_id, domain::permissions::roles_assign)) {
                BOOST_LOG_SEV(authorization_handler_lg(), warn)
                    << msg.subject << " denied: caller lacks iam::roles:assign permission";
                reply(nats_,
                      msg,
                      assign_role_by_name_response{
                          .success = false,
                          .error_message = "Permission denied: iam::roles:assign required"});
                return;
            }

            // Parse principal: username@hostname
            const auto principal = split_principal(req->principal);
            if (!principal.has_hostname) {
                reply(nats_,
                      msg,
                      assign_role_by_name_response{
                          .success = false,
                          .error_message = "Principal must be in username@hostname format"});
                return;
            }
            const auto& username = principal.username;
            const auto& hostname = principal.hostname;

            // Resolve tenant by hostname
            const auto tenants = repository::read_active_tenant_by_hostname(ctx_, hostname);
            if (tenants.empty()) {
                reply(nats_,
                      msg,
                      assign_role_by_name_response{
                          .success = false,
                          .error_message = "Tenant not found for hostname: " + hostname});
                return;
            }

            using ores::database::service::tenant_context;
            auto tenant_ctx =
                tenant_context::with_tenant(ctx_, boost::uuids::to_string(tenants.front().id));

            // Look up account by username in the target tenant
            repository::account_repository acct_repo;
            auto accounts = acct_repo.read_latest_by_username(tenant_ctx, username);
            if (accounts.empty()) {
                reply(nats_,
                      msg,
                      assign_role_by_name_response{
                          .success = false, .error_message = "Account not found: " + username});
                return;
            }

            // Resolve role by name
            service::authorization_service auth_svc(tenant_ctx);
            auto role = auth_svc.find_role_by_name(req->role_name);
            if (!role) {
                reply(nats_,
                      msg,
                      assign_role_by_name_response{
                          .success = false, .error_message = "Role not found: " + req->role_name});
                return;
            }

            auth_svc.assign_role(accounts.front().id, role->id, ctx.actor());
            BOOST_LOG_SEV(authorization_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, assign_role_by_name_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(authorization_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_,
                  msg,
                  assign_role_by_name_response{.success = false, .error_message = e.what()});
        }
    }

    void revoke_by_name(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(authorization_handler_lg(), msg);
        auto req = decode<revoke_role_by_name_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(authorization_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            // Authenticate and check roles:revoke permission
            auto ctx_expected = ores::service::service::make_request_context(
                ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>{signer_});
            if (!ctx_expected) {
                error_reply(nats_, msg, ctx_expected.error());
                return;
            }
            const auto& ctx = *ctx_expected;
            service::authorization_service caller_svc(ctx);
            const auto caller_id = caller_svc.caller_account();
            if (!caller_id) {
                reply(nats_,
                      msg,
                      revoke_role_by_name_response{
                          .success = false, .error_message = std::string{no_account_answer}});
                return;
            }
            if (!caller_svc.has_permission(*caller_id, domain::permissions::roles_revoke)) {
                BOOST_LOG_SEV(authorization_handler_lg(), warn)
                    << msg.subject << " denied: caller lacks iam::roles:revoke permission";
                reply(nats_,
                      msg,
                      revoke_role_by_name_response{
                          .success = false,
                          .error_message = "Permission denied: iam::roles:revoke required"});
                return;
            }

            // Parse principal: username@hostname
            const auto principal = split_principal(req->principal);
            if (!principal.has_hostname) {
                reply(nats_,
                      msg,
                      revoke_role_by_name_response{
                          .success = false,
                          .error_message = "Principal must be in username@hostname format"});
                return;
            }
            const auto& username = principal.username;
            const auto& hostname = principal.hostname;

            // Resolve tenant by hostname
            const auto tenants = repository::read_active_tenant_by_hostname(ctx_, hostname);
            if (tenants.empty()) {
                reply(nats_,
                      msg,
                      revoke_role_by_name_response{
                          .success = false,
                          .error_message = "Tenant not found for hostname: " + hostname});
                return;
            }

            using ores::database::service::tenant_context;
            auto tenant_ctx =
                tenant_context::with_tenant(ctx_, boost::uuids::to_string(tenants.front().id));

            // Look up account by username in the target tenant
            repository::account_repository acct_repo;
            auto accounts = acct_repo.read_latest_by_username(tenant_ctx, username);
            if (accounts.empty()) {
                reply(nats_,
                      msg,
                      revoke_role_by_name_response{
                          .success = false, .error_message = "Account not found: " + username});
                return;
            }

            // Resolve role by name
            service::authorization_service auth_svc(tenant_ctx);
            auto role = auth_svc.find_role_by_name(req->role_name);
            if (!role) {
                reply(nats_,
                      msg,
                      revoke_role_by_name_response{
                          .success = false, .error_message = "Role not found: " + req->role_name});
                return;
            }

            if (const auto refusal =
                    authorization_revoke_refusal(*caller_id, accounts.front().id)) {
                reply(nats_,
                      msg,
                      revoke_role_by_name_response{.success = false, .error_message = *refusal});
                return;
            }
            auth_svc.revoke_role(accounts.front().id, role->id);
            BOOST_LOG_SEV(authorization_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, revoke_role_by_name_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(authorization_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_,
                  msg,
                  revoke_role_by_name_response{.success = false, .error_message = e.what()});
        }
    }

    /**
     * @brief Serves iam.v1.roles.suggest-commands.
     *
     * The answer names an account's id and its tenant's hostname, so it is an
     * administrator's read: the caller needs iam::roles:assign, and naming a
     * tenant other than the caller's own, or naming one by hostname, needs
     * iam::tenants:read as well, which only system administration holds.
     */
    void suggest_commands(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(authorization_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>{signer_});
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        auto req = decode<suggest_role_commands_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(authorization_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        const auto& caller = *ctx_expected;
        const bool own_tenant =
            !req->tenant_id.empty() && req->tenant_id == caller.tenant_id().to_string();
        if (!ores::service::messaging::has_permission(caller, "iam::roles:assign") ||
            (!own_tenant &&
             !ores::service::messaging::has_permission(caller, "iam::tenants:read"))) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        try {
            using ores::database::repository::execute_parameterized_string_query;
            std::vector<std::string> results;
            if (!req->tenant_id.empty()) {
                results = execute_parameterized_string_query(
                    ctx_,
                    "SELECT command FROM "
                    "ores_iam_generate_role_commands_fn($1, NULL, $2::uuid)",
                    {req->username, req->tenant_id},
                    authorization_handler_lg(),
                    "Suggest role commands by tenant_id");
            } else if (!req->hostname.empty()) {
                results =
                    execute_parameterized_string_query(ctx_,
                                                       "SELECT command FROM "
                                                       "ores_iam_generate_role_commands_fn($1, $2)",
                                                       {req->username, req->hostname},
                                                       authorization_handler_lg(),
                                                       "Suggest role commands by hostname");
            } else {
                reply(nats_, msg, suggest_role_commands_response{});
                return;
            }
            BOOST_LOG_SEV(authorization_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, suggest_role_commands_response{.commands = std::move(results)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(authorization_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, suggest_role_commands_response{});
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    ores::security::jwt::jwt_authenticator signer_;
};

} // namespace ores::iam::messaging
#endif
