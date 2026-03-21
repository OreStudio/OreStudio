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
#ifndef ORES_IAM_MESSAGING_ROLE_HANDLER_HPP
#define ORES_IAM_MESSAGING_ROLE_HANDLER_HPP

#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.iam/messaging/authorization_protocol.hpp"
#include "ores.iam/domain/permission.hpp"
#include "ores.iam/repository/account_repository.hpp"
#include "ores.iam/repository/tenant_repository.hpp"
#include "ores.iam/service/authorization_service.hpp"

namespace ores::iam::messaging {

namespace {

inline auto& role_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.iam.messaging.role_handler");
    return instance;
}

} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;

class role_handler {
public:
    role_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        ores::security::jwt::jwt_authenticator signer)
        : nats_(nats), ctx_(std::move(ctx)), signer_(std::move(signer)) {}

    void list(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(role_handler_lg(), debug)
            << "Handling " << msg.subject;
        try {
            auto ctx_expected = ores::service::service::make_request_context(
                ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>{signer_});
            if (!ctx_expected) {
                error_reply(nats_, msg, ctx_expected.error());
                return;
            }
            const auto& ctx = *ctx_expected;
            service::authorization_service svc(ctx);
            auto roles = svc.list_roles();
            BOOST_LOG_SEV(role_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg,
                list_roles_response{.roles = std::move(roles)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(role_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, list_roles_response{});
        }
    }

    void assign(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(role_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req = decode<assign_role_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(role_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
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
            svc.assign_role(sg(req->account_id),
                sg(req->role_id), ctx.actor());
            BOOST_LOG_SEV(role_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg,
                assign_role_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(role_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, assign_role_response{
                .success = false,
                .error_message = e.what()});
        }
    }

    void revoke(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(role_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req = decode<revoke_role_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(role_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
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
            const auto caller_id = sg(ctx.actor());
            service::authorization_service svc(ctx);
            if (!svc.has_permission(caller_id,
                    domain::permissions::roles_revoke)) {
                BOOST_LOG_SEV(role_handler_lg(), warn)
                    << msg.subject
                    << " denied: caller lacks iam::roles:revoke permission";
                reply(nats_, msg, revoke_role_response{
                    .success = false,
                    .error_message =
                        "Permission denied: iam::roles:revoke required"});
                return;
            }
            svc.revoke_role(sg(req->account_id), sg(req->role_id));
            BOOST_LOG_SEV(role_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg,
                revoke_role_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(role_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, revoke_role_response{
                .success = false,
                .error_message = e.what()});
        }
    }

    void by_account(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(role_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req = decode<get_account_roles_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(role_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
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
            auto roles = svc.get_account_roles(
                sg(req->account_id));
            BOOST_LOG_SEV(role_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg,
                get_account_roles_response{
                    .roles = std::move(roles)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(role_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_account_roles_response{});
        }
    }

    void assign_by_name(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(role_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req = decode<assign_role_by_name_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(role_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
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

            // Parse principal: username@hostname
            const auto at_pos = req->principal.rfind('@');
            if (at_pos == std::string::npos) {
                reply(nats_, msg, assign_role_by_name_response{
                    .success = false,
                    .error_message =
                        "Principal must be in username@hostname format"});
                return;
            }
            const auto username = req->principal.substr(0, at_pos);
            const auto hostname = req->principal.substr(at_pos + 1);

            // Resolve tenant by hostname
            repository::tenant_repository tenant_repo(ctx_);
            auto tenants = tenant_repo.read_latest_by_hostname(hostname);
            if (tenants.empty()) {
                reply(nats_, msg, assign_role_by_name_response{
                    .success = false,
                    .error_message =
                        "Tenant not found for hostname: " + hostname});
                return;
            }

            using ores::database::service::tenant_context;
            auto tenant_ctx = tenant_context::with_tenant(
                ctx_, boost::uuids::to_string(tenants.front().id));

            // Look up account by username in the target tenant
            repository::account_repository acct_repo(tenant_ctx);
            auto accounts = acct_repo.read_latest_by_username(username);
            if (accounts.empty()) {
                reply(nats_, msg, assign_role_by_name_response{
                    .success = false,
                    .error_message = "Account not found: " + username});
                return;
            }

            // Resolve role by name
            service::authorization_service auth_svc(tenant_ctx);
            auto role = auth_svc.find_role_by_name(req->role_name);
            if (!role) {
                reply(nats_, msg, assign_role_by_name_response{
                    .success = false,
                    .error_message = "Role not found: " + req->role_name});
                return;
            }

            auth_svc.assign_role(accounts.front().id, role->id, ctx.actor());
            BOOST_LOG_SEV(role_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, assign_role_by_name_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(role_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, assign_role_by_name_response{
                .success = false,
                .error_message = e.what()});
        }
    }

    void revoke_by_name(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(role_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req = decode<revoke_role_by_name_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(role_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
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
            boost::uuids::string_generator sg;
            const auto caller_id = sg(ctx.actor());
            service::authorization_service caller_svc(ctx);
            if (!caller_svc.has_permission(caller_id,
                    domain::permissions::roles_revoke)) {
                BOOST_LOG_SEV(role_handler_lg(), warn)
                    << msg.subject
                    << " denied: caller lacks iam::roles:revoke permission";
                reply(nats_, msg, revoke_role_by_name_response{
                    .success = false,
                    .error_message =
                        "Permission denied: iam::roles:revoke required"});
                return;
            }

            // Parse principal: username@hostname
            const auto at_pos = req->principal.rfind('@');
            if (at_pos == std::string::npos) {
                reply(nats_, msg, revoke_role_by_name_response{
                    .success = false,
                    .error_message =
                        "Principal must be in username@hostname format"});
                return;
            }
            const auto username = req->principal.substr(0, at_pos);
            const auto hostname = req->principal.substr(at_pos + 1);

            // Resolve tenant by hostname
            repository::tenant_repository tenant_repo(ctx_);
            auto tenants = tenant_repo.read_latest_by_hostname(hostname);
            if (tenants.empty()) {
                reply(nats_, msg, revoke_role_by_name_response{
                    .success = false,
                    .error_message =
                        "Tenant not found for hostname: " + hostname});
                return;
            }

            using ores::database::service::tenant_context;
            auto tenant_ctx = tenant_context::with_tenant(
                ctx_, boost::uuids::to_string(tenants.front().id));

            // Look up account by username in the target tenant
            repository::account_repository acct_repo(tenant_ctx);
            auto accounts = acct_repo.read_latest_by_username(username);
            if (accounts.empty()) {
                reply(nats_, msg, revoke_role_by_name_response{
                    .success = false,
                    .error_message = "Account not found: " + username});
                return;
            }

            // Resolve role by name
            service::authorization_service auth_svc(tenant_ctx);
            auto role = auth_svc.find_role_by_name(req->role_name);
            if (!role) {
                reply(nats_, msg, revoke_role_by_name_response{
                    .success = false,
                    .error_message = "Role not found: " + req->role_name});
                return;
            }

            auth_svc.revoke_role(accounts.front().id, role->id);
            BOOST_LOG_SEV(role_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, revoke_role_by_name_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(role_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, revoke_role_by_name_response{
                .success = false,
                .error_message = e.what()});
        }
    }

    void suggest_commands(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(role_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req = decode<suggest_role_commands_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(role_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            using ores::database::repository::execute_parameterized_string_query;
            std::vector<std::string> results;
            if (!req->tenant_id.empty()) {
                results = execute_parameterized_string_query(ctx_,
                    "SELECT command FROM "
                    "ores_iam_generate_role_commands_fn($1, NULL, $2::uuid)",
                    {req->username, req->tenant_id},
                    role_handler_lg(), "Suggest role commands by tenant_id");
            } else if (!req->hostname.empty()) {
                results = execute_parameterized_string_query(ctx_,
                    "SELECT command FROM "
                    "ores_iam_generate_role_commands_fn($1, $2)",
                    {req->username, req->hostname},
                    role_handler_lg(), "Suggest role commands by hostname");
            } else {
                reply(nats_, msg, suggest_role_commands_response{});
                return;
            }
            BOOST_LOG_SEV(role_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, suggest_role_commands_response{
                .commands = std::move(results)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(role_handler_lg(), error)
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
