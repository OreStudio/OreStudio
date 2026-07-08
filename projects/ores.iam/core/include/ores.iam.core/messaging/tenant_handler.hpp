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
#ifndef ORES_IAM_MESSAGING_TENANT_HANDLER_HPP
#define ORES_IAM_MESSAGING_TENANT_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.iam.api/messaging/tenant_protocol.hpp"
#include "ores.iam.core/repository/tenant_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.variability.api/messaging/system_settings_protocol.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <rfl/json.hpp>
#include <stdexcept>

namespace ores::iam::messaging {

namespace {

inline auto& tenant_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.iam.messaging.tenant_handler");
    return instance;
}

} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::stamp;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::log_handler_entry;
using ores::database::service::tenant_context;
using ores::database::repository::execute_parameterized_string_query;
using ores::database::repository::execute_parameterized_command;
using namespace ores::logging;

class tenant_handler {
public:
    tenant_handler(ores::nats::service::client& nats,
                   ores::database::context ctx,
                   ores::security::jwt::jwt_authenticator signer)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , signer_(std::move(signer)) {}

    void list(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(tenant_handler_lg(), msg);
        try {
            repository::tenant_repository repo(ctx_);
            get_tenants_response resp;
            resp.tenants = repo.read_latest();
            BOOST_LOG_SEV(tenant_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(tenant_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_tenants_response{});
        }
    }

    void save(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(tenant_handler_lg(), msg);
        auto req = decode<save_tenant_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(tenant_handler_lg(), warn) << "Failed to decode: " << msg.subject;
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
            if (!has_permission(ctx, "iam::tenants:write")) {
                error_reply(nats_, msg, ores::service::error_code::forbidden);
                return;
            }
            if (req->data.id.is_nil())
                req->data.id = boost::uuids::random_generator()();
            repository::tenant_repository repo(ctx);
            stamp(req->data, ctx);
            repo.write(req->data);
            BOOST_LOG_SEV(tenant_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, save_tenant_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(tenant_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, save_tenant_response{.success = false, .message = e.what()});
        }
    }

    void remove(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(tenant_handler_lg(), msg);
        auto req = decode<delete_tenant_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(tenant_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            repository::tenant_repository repo(ctx_);
            boost::uuids::string_generator sg;
            for (const auto& id_str : req->ids)
                repo.remove(sg(id_str));
            BOOST_LOG_SEV(tenant_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, delete_tenant_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(tenant_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, delete_tenant_response{.success = false, .message = e.what()});
        }
    }

    void history(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(tenant_handler_lg(), msg);
        auto req = decode<get_tenant_history_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(tenant_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            repository::tenant_repository repo(ctx_);
            boost::uuids::string_generator sg;
            auto hist = repo.read_history(sg(req->id));
            BOOST_LOG_SEV(tenant_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_,
                  msg,
                  get_tenant_history_response{.success = true, .versions = std::move(hist)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(tenant_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_tenant_history_response{.success = false, .message = e.what()});
        }
    }

    void complete_provisioning(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(tenant_handler_lg(), msg);
        try {
            auto ctx_expected = ores::service::service::make_request_context(
                ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>{signer_});
            if (!ctx_expected) {
                error_reply(nats_, msg, ctx_expected.error());
                return;
            }

            const auto actor = ctx_expected->actor();
            auto ids =
                execute_parameterized_string_query(*ctx_expected,
                                                   "SELECT ores_iam_current_tenant_id_fn()::text",
                                                   {},
                                                   tenant_handler_lg(),
                                                   "complete_provisioning");
            if (ids.empty()) {
                BOOST_LOG_SEV(tenant_handler_lg(), error)
                    << "complete_provisioning: no tenant ID in context";
                reply(nats_,
                      msg,
                      complete_tenant_provisioning_response{.success = false,
                                                            .message = "No tenant context"});
                return;
            }

            auto sys_ctx = tenant_context::with_system_tenant(ctx_);
            execute_parameterized_command(sys_ctx,
                                          "SELECT ores_iam_mark_tenant_active_fn($1::uuid, $2)",
                                          {ids.front(), actor},
                                          tenant_handler_lg(),
                                          "complete_provisioning");

            BOOST_LOG_SEV(tenant_handler_lg(), info) << "Tenant marked active: " << ids.front();

            // Clear the bootstrap_mode flag via the variability service over
            // NATS, so the write happens under its own (correctly-granted) DB
            // role rather than IAM's. The variability-side handler has no
            // permission check on this subject — independent of the user
            // having variability::flags:create — so forwarding the original
            // bearer (scoping the tenant) is sufficient.
            try {
                using namespace ores::variability::messaging;
                const clear_bootstrap_mode_request req{};
                const auto json = rfl::json::write(req);
                const auto* p = reinterpret_cast<const std::byte*>(json.data());

                std::unordered_map<std::string, std::string> hdrs;
                if (const auto bearer = extract_bearer(msg); !bearer.empty())
                    hdrs[std::string(ores::nats::headers::delegated_authorization)] =
                        std::string(ores::nats::headers::bearer_prefix) + bearer;

                const auto resp_msg =
                    nats_.request_sync(clear_bootstrap_mode_request::nats_subject,
                                       std::span<const std::byte>(p, json.size()),
                                       std::move(hdrs),
                                       std::chrono::seconds(5));
                const std::string_view sv(reinterpret_cast<const char*>(resp_msg.data.data()),
                                          resp_msg.data.size());
                const auto resp = rfl::json::read<clear_bootstrap_mode_response>(sv);
                if (resp && resp->success) {
                    BOOST_LOG_SEV(tenant_handler_lg(), info)
                        << "Bootstrap mode cleared for tenant: " << ids.front();
                } else {
                    BOOST_LOG_SEV(tenant_handler_lg(), warn)
                        << "Failed to clear bootstrap mode for tenant " << ids.front() << ": "
                        << (resp ? resp->message : "malformed response");
                }
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(tenant_handler_lg(), warn)
                    << "Failed to clear bootstrap mode for tenant " << ids.front() << ": "
                    << e.what();
            }

            reply(nats_, msg, complete_tenant_provisioning_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(tenant_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_,
                  msg,
                  complete_tenant_provisioning_response{.success = false, .message = e.what()});
        }
    }

private:
    // Extract the raw JWT from an incoming message, preferring the delegated
    // header so the original end-user context propagates to downstream calls.
    static std::string extract_bearer(const ores::nats::message& msg) {
        using namespace ores::nats::headers;
        for (auto hdr : {delegated_authorization, authorization}) {
            const auto it = msg.headers.find(std::string(hdr));
            if (it != msg.headers.end() && it->second.starts_with(bearer_prefix))
                return std::string(it->second.substr(bearer_prefix.size()));
        }
        return {};
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    ores::security::jwt::jwt_authenticator signer_;
};

} // namespace ores::iam::messaging
#endif
