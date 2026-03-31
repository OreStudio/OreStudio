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
#ifndef ORES_IAM_MESSAGING_ACCOUNT_HANDLER_HPP
#define ORES_IAM_MESSAGING_ACCOUNT_HANDLER_HPP

#include <chrono>
#include <stdexcept>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.security/jwt/jwt_claims.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.iam.api/messaging/account_protocol.hpp"
#include "ores.iam.api/messaging/account_history_protocol.hpp"
#include "ores.iam.api/messaging/login_protocol.hpp"
#include "ores.iam.api/domain/account_version.hpp"
#include "ores.iam.api/domain/session.hpp"
#include "ores.iam.core/repository/account_party_repository.hpp"
#include "ores.iam.core/repository/tenant_repository.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.iam.core/service/account_service.hpp"
#include "ores.iam.core/service/authorization_service.hpp"
#include "ores.iam.core/service/account_setup_service.hpp"
#include "ores.variability.core/service/system_settings_service.hpp"
#include "ores.iam.core/domain/token_settings.hpp"

namespace ores::iam::messaging {

namespace {

inline auto& account_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.iam.messaging.account_handler");
    return instance;
}

inline std::string acct_extract_bearer_token(const ores::nats::message& msg) {
    auto it = msg.headers.find(std::string(ores::nats::headers::authorization));
    if (it == msg.headers.end())
        return {};
    const auto& val = it->second;
    if (!val.starts_with(ores::nats::headers::bearer_prefix))
        return {};
    return val.substr(ores::nats::headers::bearer_prefix.size());
}

inline std::vector<boost::uuids::uuid> acct_compute_visible_party_ids(
    const ores::database::context& ctx,
    const boost::uuids::uuid& party_id) {
    try {
        refdata::repository::party_repository repo(ctx);
        auto ids = repo.read_descendants(party_id);
        if (ids.empty())
            return {party_id};
        return ids;
    } catch (const std::exception& e) {
        using namespace ores::logging;
        BOOST_LOG_SEV(account_handler_lg(), warn)
            << "Failed to compute visible party IDs: " << e.what();
        return {party_id};
    }
}

inline std::optional<refdata::domain::party> acct_lookup_party(
    const ores::database::context& ctx,
    const boost::uuids::uuid& party_id) {
    try {
        refdata::repository::party_repository repo(ctx);
        auto parties = repo.read_latest(party_id);
        if (!parties.empty())
            return parties.front();
    } catch (const std::exception& e) {
        using namespace ores::logging;
        BOOST_LOG_SEV(account_handler_lg(), warn)
            << "Failed to look up party: " << e.what();
    }
    return std::nullopt;
}

inline std::string acct_lookup_tenant_name(
    const ores::database::context& ctx,
    const boost::uuids::uuid& tenant_id) {
    try {
        repository::tenant_repository repo(ctx);
        auto tenants = repo.read_latest(tenant_id);
        if (!tenants.empty())
            return tenants.front().name;
    } catch (const std::exception& e) {
        using namespace ores::logging;
        BOOST_LOG_SEV(account_handler_lg(), warn)
            << "Failed to look up tenant name: " << e.what();
    }
    return {};
}

} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;

class account_handler {
public:
    account_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        ores::security::jwt::jwt_authenticator signer)
        : nats_(nats), ctx_(std::move(ctx)), signer_(std::move(signer)) {
        reload_token_settings();
    }

    void reload_token_settings() {
        try {
            variability::service::system_settings_service svc(ctx_);
            svc.refresh();
            token_settings_ = domain::token_settings::load(svc);
        } catch (const std::exception& e) {
            using namespace ores::logging;
            BOOST_LOG_SEV(account_handler_lg(), warn)
                << "Failed to load token settings, using defaults: " << e.what();
        }
    }

    void list(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(account_handler_lg(), debug)
            << "Handling " << msg.subject;
        try {
            auto ctx_expected = ores::service::service::make_request_context(
                ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>{signer_});
            if (!ctx_expected) {
                error_reply(nats_, msg, ctx_expected.error());
                return;
            }
            const auto& ctx = *ctx_expected;
            service::account_service svc(ctx);
            auto accounts = svc.list_accounts();
            get_accounts_response resp;
            resp.total_available_count =
                static_cast<int>(accounts.size());
            resp.accounts = std::move(accounts);
            BOOST_LOG_SEV(account_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(account_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_accounts_response{});
        }
    }

    void save(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(account_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req = decode<save_account_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(account_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            auto base_ctx_expected = ores::service::service::make_request_context(
                ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>{signer_});
            if (!base_ctx_expected) {
                error_reply(nats_, msg, base_ctx_expected.error());
                return;
            }
            const auto& base_ctx = *base_ctx_expected;
            if (!has_permission(base_ctx, "iam::accounts:create")) {
                error_reply(nats_, msg, ores::service::error_code::forbidden);
                return;
            }

            // Parse principal: if username@hostname, route to that tenant's
            // context so accounts can be created in any tenant by a system
            // admin whose JWT is in the system tenant.
            std::string username = req->principal;
            ores::database::context op_ctx = base_ctx;
            const auto at_pos = req->principal.rfind('@');
            if (at_pos != std::string::npos) {
                username = req->principal.substr(0, at_pos);
                const auto hostname = req->principal.substr(at_pos + 1);
                repository::tenant_repository tenant_repo(ctx_);
                auto tenants = tenant_repo.read_latest_by_hostname(hostname);
                if (!tenants.empty()) {
                    using ores::database::service::tenant_context;
                    op_ctx = tenant_context::with_tenant(
                        ctx_, boost::uuids::to_string(tenants.front().id));
                } else {
                    throw std::runtime_error(
                        "Tenant not found for hostname: " + hostname +
                        ". Cannot create account in an unknown tenant.");
                }
            }

            service::account_service acct_svc(op_ctx);
            auto auth_svc =
                std::make_shared<service::authorization_service>(op_ctx);
            service::account_setup_service setup_svc(acct_svc, auth_svc);
            auto acct = setup_svc.create_account(
                username, req->email, req->password, base_ctx.actor());
            BOOST_LOG_SEV(account_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, save_account_response{
                .success = true,
                .account_id = boost::uuids::to_string(acct.id)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(account_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, save_account_response{
                .success = false, .message = e.what()});
        }
    }

    void del(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(account_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req = decode<delete_account_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(account_handler_lg(), warn)
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
            if (!has_permission(ctx, "iam::accounts:delete")) {
                error_reply(nats_, msg, ores::service::error_code::forbidden);
                return;
            }
            service::account_service svc(ctx);
            boost::uuids::string_generator sg;
            svc.delete_account(sg(req->account_id));
            BOOST_LOG_SEV(account_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg,
                delete_account_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(account_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, delete_account_response{
                .success = false, .message = e.what()});
        }
    }

    void lock(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(account_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req = decode<lock_account_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(account_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        lock_account_response resp;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>{signer_});
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "iam::accounts:lock")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::account_service svc(ctx);
        boost::uuids::string_generator sg;
        for (const auto& id : req->account_ids) {
            try {
                svc.lock_account(sg(id));
                resp.results.push_back({.success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(account_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                resp.results.push_back({
                    .success = false, .message = e.what()});
            }
        }
        BOOST_LOG_SEV(account_handler_lg(), debug)
            << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

    void unlock(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(account_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req = decode<unlock_account_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(account_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        unlock_account_response resp;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>{signer_});
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "iam::accounts:unlock")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::account_service svc(ctx);
        boost::uuids::string_generator sg;
        for (const auto& id : req->account_ids) {
            try {
                svc.unlock_account(sg(id));
                resp.results.push_back({.success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(account_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                resp.results.push_back({
                    .success = false, .message = e.what()});
            }
        }
        BOOST_LOG_SEV(account_handler_lg(), debug)
            << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

    void login_info(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(account_handler_lg(), debug)
            << "Handling " << msg.subject;
        try {
            auto ctx_expected = ores::service::service::make_request_context(
                ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>{signer_});
            if (!ctx_expected) {
                error_reply(nats_, msg, ctx_expected.error());
                return;
            }
            const auto& ctx = *ctx_expected;
            service::account_service svc(ctx);
            auto infos = svc.list_login_info();
            BOOST_LOG_SEV(account_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg,
                list_login_info_response{
                    .login_infos = std::move(infos)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(account_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, list_login_info_response{});
        }
    }

    void reset_password(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(account_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req = decode<reset_password_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(account_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        reset_password_response resp;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>{signer_});
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "iam::accounts:reset_password")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::account_service svc(ctx);
        boost::uuids::string_generator sg;
        for (const auto& id_str : req->account_ids) {
            try {
                auto account_id = sg(id_str);
                auto err = svc.change_password(
                    account_id, req->new_password);
                if (err.empty()) {
                    resp.results.push_back({.success = true});
                } else {
                    resp.results.push_back({
                        .success = false, .message = err});
                }
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(account_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                resp.results.push_back({
                    .success = false, .message = e.what()});
            }
        }
        resp.success = true;
        BOOST_LOG_SEV(account_handler_lg(), debug)
            << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

    void change_password(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(account_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req = decode<change_password_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(account_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            auto token = acct_extract_bearer_token(msg);
            if (token.empty()) {
                reply(nats_, msg, change_password_response{
                    .success = false,
                    .message = "Missing authorization token"});
                return;
            }
            auto claims_result = signer_.validate(token);
            if (!claims_result) {
                reply(nats_, msg, change_password_response{
                    .success = false,
                    .message = "Invalid or expired token"});
                return;
            }
            boost::uuids::string_generator sg;
            auto account_id = sg(claims_result->subject);
            auto ctx_expected = ores::service::service::make_request_context(
                ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>{signer_});
            if (!ctx_expected) {
                error_reply(nats_, msg, ctx_expected.error());
                return;
            }
            const auto& ctx = *ctx_expected;
            service::account_service svc(ctx);
            auto err = svc.change_password(account_id,
                req->new_password);
            if (err.empty()) {
                BOOST_LOG_SEV(account_handler_lg(), debug)
                    << "Completed " << msg.subject;
                reply(nats_, msg, change_password_response{
                    .success = true});
            } else {
                reply(nats_, msg, change_password_response{
                    .success = false, .message = err});
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(account_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, change_password_response{
                .success = false, .message = e.what()});
        }
    }

    void update(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(account_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req = decode<update_account_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(account_handler_lg(), warn)
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
            if (!has_permission(ctx, "iam::accounts:update")) {
                error_reply(nats_, msg, ores::service::error_code::forbidden);
                return;
            }
            service::account_service svc(ctx);
            boost::uuids::string_generator sg;
            svc.update_account(sg(req->account_id), req->email,
                ctx.actor(), req->change_reason_code,
                req->change_commentary);
            BOOST_LOG_SEV(account_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg,
                update_account_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(account_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, update_account_response{
                .success = false, .message = e.what()});
        }
    }

    void update_email(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(account_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req = decode<update_my_email_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(account_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            auto token = acct_extract_bearer_token(msg);
            if (token.empty()) {
                reply(nats_, msg, update_my_email_response{
                    .success = false,
                    .message = "Missing authorization token"});
                return;
            }
            auto claims_result = signer_.validate(token);
            if (!claims_result) {
                reply(nats_, msg, update_my_email_response{
                    .success = false,
                    .message = "Invalid or expired token"});
                return;
            }
            boost::uuids::string_generator sg;
            auto account_id = sg(claims_result->subject);
            auto ctx_expected = ores::service::service::make_request_context(
                ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>{signer_});
            if (!ctx_expected) {
                error_reply(nats_, msg, ctx_expected.error());
                return;
            }
            const auto& ctx = *ctx_expected;
            service::account_service svc(ctx);
            auto err = svc.update_my_email(account_id, req->email);
            if (err.empty()) {
                BOOST_LOG_SEV(account_handler_lg(), debug)
                    << "Completed " << msg.subject;
                reply(nats_, msg, update_my_email_response{
                    .success = true});
            } else {
                reply(nats_, msg, update_my_email_response{
                    .success = false, .message = err});
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(account_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, update_my_email_response{
                .success = false, .message = e.what()});
        }
    }

    void select_party(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(account_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req = decode<select_party_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(account_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            auto token = acct_extract_bearer_token(msg);
            if (token.empty()) {
                reply(nats_, msg, select_party_response{
                    .success = false,
                    .message = "Missing authorization token"});
                return;
            }

            auto claims_result = signer_.validate(token);
            if (!claims_result ||
                claims_result->audience != "select_party_only") {
                reply(nats_, msg, select_party_response{
                    .success = false,
                    .message = "Invalid or expired token"});
                return;
            }

            boost::uuids::string_generator sg;
            auto account_id = sg(claims_result->subject);

            auto ctx_expected = ores::service::service::make_request_context(
                ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>{signer_});
            if (!ctx_expected) {
                error_reply(nats_, msg, ctx_expected.error());
                return;
            }
            const auto& ctx = *ctx_expected;
            boost::uuids::uuid requested_party_id =
                sg(req->party_id);
            repository::account_party_repository ap_repo(ctx);
            auto parties =
                ap_repo.read_latest_by_account(account_id);

            bool is_member = false;
            for (const auto& ap : parties) {
                if (ap.party_id == requested_party_id) {
                    is_member = true;
                    break;
                }
            }

            if (!is_member) {
                reply(nats_, msg, select_party_response{
                    .success = false,
                    .message =
                        "User is not a member of requested party"});
                return;
            }

            auto tenant_id_str = claims_result->tenant_id.value_or(
                ctx_.tenant_id().to_string());
            auto visible = acct_compute_visible_party_ids(
                ctx, requested_party_id);

            security::jwt::jwt_claims new_claims;
            new_claims.subject = claims_result->subject;
            new_claims.issued_at = std::chrono::system_clock::now();
            new_claims.expires_at =
                new_claims.issued_at + std::chrono::seconds(
                    token_settings_.access_lifetime_s);
            new_claims.username = claims_result->username;
            new_claims.email = claims_result->email;
            new_claims.tenant_id = tenant_id_str;
            new_claims.party_id =
                boost::uuids::to_string(requested_party_id);
            // Carry the session identifiers forward so logout can end
            // the session record created at login.
            new_claims.session_id = claims_result->session_id;
            new_claims.session_start_time = claims_result->session_start_time;
            for (const auto& vid : visible)
                new_claims.visible_party_ids.push_back(
                    boost::uuids::to_string(vid));

            auto new_token =
                signer_.create_token(new_claims).value_or("");

            std::string t_name;
            std::string p_name;
            try {
                auto tid = sg(tenant_id_str);
                t_name = acct_lookup_tenant_name(ctx, tid);
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(account_handler_lg(), warn)
                    << "Failed to look up tenant name during "
                       "party selection: " << e.what();
            }
            bool party_setup_required = false;
            if (const auto p = acct_lookup_party(ctx, requested_party_id)) {
                p_name = p->full_name;
                party_setup_required = p->status == "Inactive";
            }

            BOOST_LOG_SEV(account_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, select_party_response{
                .success = true,
                .message = "Party selected",
                .token = new_token,
                .username = claims_result->username.value_or(""),
                .tenant_name = t_name,
                .party_name = p_name,
                .party_setup_required = party_setup_required
            });
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(account_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, select_party_response{
                .success = false, .message = e.what()});
        }
    }

    void history(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(account_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req = decode<get_account_history_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(account_handler_lg(), warn)
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
            service::account_service svc(ctx);
            auto accounts = svc.get_account_history(req->username);
            account_version_history avh;
            int vnum = static_cast<int>(accounts.size());
            for (const auto& a : accounts) {
                ores::iam::domain::account_version av;
                av.data = a;
                av.version_number = vnum--;
                av.modified_by = a.modified_by;
                av.recorded_at = a.recorded_at;
                avh.versions.push_back(std::move(av));
            }
            BOOST_LOG_SEV(account_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, get_account_history_response{
                .success = true,
                .history = std::move(avh)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(account_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_account_history_response{
                .success = false, .message = e.what()});
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    ores::security::jwt::jwt_authenticator signer_;
    domain::token_settings token_settings_;
};

} // namespace ores::iam::messaging
#endif
