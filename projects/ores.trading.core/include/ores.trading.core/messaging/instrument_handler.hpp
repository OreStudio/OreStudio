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
#ifndef ORES_TRADING_MESSAGING_INSTRUMENT_HANDLER_HPP
#define ORES_TRADING_MESSAGING_INSTRUMENT_HANDLER_HPP

#include <optional>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"
#include "ores.trading.core/service/instrument_service.hpp"

namespace ores::trading::messaging {

namespace {
inline auto& instrument_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.trading.messaging.instrument_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using namespace ores::logging;

class instrument_handler {
public:
    instrument_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(instrument_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::instrument_service svc(ctx);
        get_instruments_response resp;
        try {
            if (auto req = decode<get_instruments_request>(msg)) {
                const auto offset =
                    static_cast<std::uint32_t>(req->offset);
                const auto limit =
                    static_cast<std::uint32_t>(req->limit);
                resp.instruments = svc.list_instruments(offset, limit);
                resp.total_available_count =
                    static_cast<int>(svc.count_instruments());
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(instrument_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp.success = false;
            resp.message = e.what();
        }
        BOOST_LOG_SEV(instrument_handler_lg(), debug)
            << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

    void save(ores::nats::message msg) {
        BOOST_LOG_SEV(instrument_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::instrument_service svc(ctx);
        if (auto req = decode<save_instrument_request>(msg)) {
            try {
                svc.save_instrument(req->data, req->legs);
                BOOST_LOG_SEV(instrument_handler_lg(), debug)
                    << "Completed " << msg.subject;
                reply(nats_, msg, save_instrument_response{.success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(instrument_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_, msg, save_instrument_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(instrument_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
    }

    void remove(ores::nats::message msg) {
        BOOST_LOG_SEV(instrument_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::instrument_service svc(ctx);
        if (auto req = decode<delete_instrument_request>(msg)) {
            try {
                for (const auto& id : req->ids)
                    svc.remove_instrument(id);
                BOOST_LOG_SEV(instrument_handler_lg(), debug)
                    << "Completed " << msg.subject;
                reply(nats_, msg, delete_instrument_response{.success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(instrument_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_, msg, delete_instrument_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(instrument_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
    }

    void history(ores::nats::message msg) {
        BOOST_LOG_SEV(instrument_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::instrument_service svc(ctx);
        if (auto req = decode<get_instrument_history_request>(msg)) {
            try {
                auto versions = svc.get_instrument_history(req->id);
                BOOST_LOG_SEV(instrument_handler_lg(), debug)
                    << "Completed " << msg.subject;
                reply(nats_, msg, get_instrument_history_response{
                    .success = true,
                    .history = std::move(versions)});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(instrument_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_, msg, get_instrument_history_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(instrument_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
    }

    void list_legs(ores::nats::message msg) {
        BOOST_LOG_SEV(instrument_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::instrument_service svc(ctx);
        get_swap_legs_response resp;
        try {
            if (auto req = decode<get_swap_legs_request>(msg)) {
                resp.legs = svc.get_legs(req->instrument_id);
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(instrument_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp.success = false;
            resp.message = e.what();
        }
        BOOST_LOG_SEV(instrument_handler_lg(), debug)
            << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::trading::messaging

#endif
