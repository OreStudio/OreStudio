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
#ifndef ORES_REPORTING_MESSAGING_CONCURRENCY_POLICY_HANDLER_HPP
#define ORES_REPORTING_MESSAGING_CONCURRENCY_POLICY_HANDLER_HPP

#include <optional>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.reporting.api/messaging/concurrency_policy_protocol.hpp"
#include "ores.reporting.core/service/concurrency_policy_service.hpp"

namespace ores::reporting::messaging {

namespace {
inline auto& concurrency_policy_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.reporting.messaging.concurrency_policy_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::stamp;
using ores::service::messaging::error_reply;
using namespace ores::logging;

class concurrency_policy_handler {
public:
    concurrency_policy_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(concurrency_policy_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::concurrency_policy_service svc(ctx);
        get_concurrency_policies_response resp;
        try {
            resp.policies = svc.list_policies();
            resp.total_available_count =
                static_cast<int>(resp.policies.size());
        } catch (...) {}
        reply(nats_, msg, resp);
        BOOST_LOG_SEV(concurrency_policy_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

    void save(ores::nats::message msg) {
        BOOST_LOG_SEV(concurrency_policy_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (auto req = decode<save_concurrency_policy_request>(msg)) {
            service::concurrency_policy_service svc(ctx);
            try {
                stamp(req->policy, ctx);
                svc.save_policy(req->policy);
                reply(nats_, msg,
                    save_concurrency_policy_response{.success = true});
            } catch (const std::exception& e) {
                reply(nats_, msg, save_concurrency_policy_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(concurrency_policy_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
        BOOST_LOG_SEV(concurrency_policy_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

    void del(ores::nats::message msg) {
        BOOST_LOG_SEV(concurrency_policy_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (auto req = decode<delete_concurrency_policy_request>(msg)) {
            service::concurrency_policy_service svc(ctx);
            try {
                for (const auto& code : req->codes)
                    svc.remove_policy(code);
                reply(nats_, msg,
                    delete_concurrency_policy_response{.success = true});
            } catch (const std::exception& e) {
                reply(nats_, msg, delete_concurrency_policy_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(concurrency_policy_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
        BOOST_LOG_SEV(concurrency_policy_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

    void history(ores::nats::message msg) {
        BOOST_LOG_SEV(concurrency_policy_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (auto req = decode<get_concurrency_policy_history_request>(msg)) {
            service::concurrency_policy_service svc(ctx);
            try {
                auto hist = svc.get_policy_history(req->code);
                reply(nats_, msg, get_concurrency_policy_history_response{
                    .success = true,
                    .history = std::move(hist)});
            } catch (const std::exception& e) {
                reply(nats_, msg, get_concurrency_policy_history_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(concurrency_policy_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
        BOOST_LOG_SEV(concurrency_policy_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::reporting::messaging

#endif
