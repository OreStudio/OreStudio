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
#ifndef ORES_IAM_MESSAGING_RESET_HANDLER_HPP
#define ORES_IAM_MESSAGING_RESET_HANDLER_HPP

#include <stdexcept>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.iam.api/messaging/reset_protocol.hpp"

namespace ores::iam::messaging {

namespace {

inline auto& reset_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.iam.messaging.reset_handler");
    return instance;
}

} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::log_handler_entry;
using namespace ores::logging;

class reset_handler {
public:
    reset_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        ores::security::jwt::jwt_authenticator signer)
        : nats_(nats), ctx_(std::move(ctx)), signer_(std::move(signer)) {}

    void reset_tenant(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(reset_handler_lg(), msg);
        auto req = decode<reset_tenant_command>(msg);
        if (!req) {
            BOOST_LOG_SEV(reset_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            auto ctx_expected = ores::service::service::make_request_context(
                ctx_, msg,
                std::optional<ores::security::jwt::jwt_authenticator>{signer_});
            if (!ctx_expected) {
                error_reply(nats_, msg, ctx_expected.error());
                return;
            }
            if (!has_permission(*ctx_expected, "iam::system:reset-tenant")) {
                error_reply(nats_, msg, ores::service::error_code::forbidden);
                return;
            }

            using ores::database::service::tenant_context;
            using ores::database::repository::execute_parameterized_multi_column_query;

            auto sys_ctx = tenant_context::with_system_tenant(ctx_);
            execute_parameterized_multi_column_query(sys_ctx,
                "SELECT ores_iam_reset_tenant_bootstrap_fn($1)",
                {req->tenant_code},
                reset_handler_lg(), "reset_tenant");

            BOOST_LOG_SEV(reset_handler_lg(), info)
                << "Tenant bootstrap reset complete: " << req->tenant_code;
            reply(nats_, msg, reset_tenant_result{.success = true,
                .message = "Tenant '" + req->tenant_code + "' has been reset"});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(reset_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg,
                reset_tenant_result{.success = false, .message = e.what()});
        }
    }

    void reset_system(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(reset_handler_lg(), msg);
        try {
            auto ctx_expected = ores::service::service::make_request_context(
                ctx_, msg,
                std::optional<ores::security::jwt::jwt_authenticator>{signer_});
            if (!ctx_expected) {
                error_reply(nats_, msg, ctx_expected.error());
                return;
            }
            if (!has_permission(*ctx_expected, "iam::system:reset")) {
                error_reply(nats_, msg, ores::service::error_code::forbidden);
                return;
            }

            using ores::database::service::tenant_context;
            using ores::database::repository::execute_parameterized_multi_column_query;

            auto sys_ctx = tenant_context::with_system_tenant(ctx_);
            execute_parameterized_multi_column_query(sys_ctx,
                "SELECT ores_iam_reset_system_fn()",
                {},
                reset_handler_lg(), "reset_system");

            BOOST_LOG_SEV(reset_handler_lg(), info) << "System bootstrap reset complete";
            reply(nats_, msg, reset_system_result{.success = true,
                .message = "System has been reset to pre-bootstrap state"});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(reset_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg,
                reset_system_result{.success = false, .message = e.what()});
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    ores::security::jwt::jwt_authenticator signer_;
};

} // namespace ores::iam::messaging
#endif
