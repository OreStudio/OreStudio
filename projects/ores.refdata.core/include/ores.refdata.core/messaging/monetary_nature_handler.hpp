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
#ifndef ORES_REFDATA_CORE_MESSAGING_MONETARY_NATURE_HANDLER_HPP
#define ORES_REFDATA_CORE_MESSAGING_MONETARY_NATURE_HANDLER_HPP

#include <optional>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.refdata.api/messaging/monetary_nature_protocol.hpp"
#include "ores.refdata.core/service/monetary_nature_service.hpp"

namespace ores::refdata::messaging {

namespace {
inline auto& monetary_nature_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.refdata.messaging.monetary_nature_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

class monetary_nature_handler {
public:
    monetary_nature_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(monetary_nature_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::monetary_nature_service svc(ctx);
        get_monetary_natures_response resp;
        try {
            resp.monetary_natures = svc.list_types();
            resp.total_available_count =
                static_cast<int>(resp.monetary_natures.size());
            BOOST_LOG_SEV(monetary_nature_handler_lg(), debug)
                << "Completed " << msg.subject;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(monetary_nature_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
        }
        reply(nats_, msg, resp);
    }

    void save(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(monetary_nature_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "refdata::monetary_natures:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::monetary_nature_service svc(ctx);
        auto req = decode<save_monetary_nature_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(monetary_nature_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            svc.save_type(req->data);
            BOOST_LOG_SEV(monetary_nature_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg,
                save_monetary_nature_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(monetary_nature_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, save_monetary_nature_response{
                .success = false, .message = e.what()});
        }
    }

    void del(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(monetary_nature_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "refdata::monetary_natures:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::monetary_nature_service svc(ctx);
        auto req = decode<delete_monetary_nature_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(monetary_nature_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            svc.remove_type(req->nature);
            BOOST_LOG_SEV(monetary_nature_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg,
                delete_monetary_nature_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(monetary_nature_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, delete_monetary_nature_response{
                .success = false, .message = e.what()});
        }
    }

    void history(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(monetary_nature_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::monetary_nature_service svc(ctx);
        auto req = decode<get_monetary_nature_history_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(monetary_nature_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            auto h = svc.get_type_history(req->nature);
            BOOST_LOG_SEV(monetary_nature_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, get_monetary_nature_history_response{
                .success = true, .history = std::move(h)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(monetary_nature_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_monetary_nature_history_response{
                .success = false, .message = e.what()});
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::refdata::messaging
#endif
