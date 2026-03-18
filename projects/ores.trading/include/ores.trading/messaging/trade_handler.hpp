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
#ifndef ORES_TRADING_MESSAGING_TRADE_HANDLER_HPP
#define ORES_TRADING_MESSAGING_TRADE_HANDLER_HPP

#include <optional>
#include <boost/uuid/string_generator.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.trading/messaging/trade_protocol.hpp"
#include "ores.trading/service/trade_service.hpp"

namespace ores::trading::messaging {

namespace {
inline auto& trade_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.trading.messaging.trade_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::stamp;
using namespace ores::logging;

class trade_handler {
public:
    trade_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(trade_handler_lg(), debug)
            << "Handling " << msg.subject;
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        service::trade_service svc(ctx);
        get_trades_response resp;
        try {
            if (auto req = decode<get_trades_request>(msg)) {
                const auto offset =
                    static_cast<std::uint32_t>(req->offset);
                const auto limit =
                    static_cast<std::uint32_t>(req->limit);
                if (!req->book_id.empty()) {
                    boost::uuids::string_generator gen;
                    const auto book_uuid = gen(req->book_id);
                    resp.trades = svc.list_trades_filtered(
                        offset, limit,
                        std::optional<boost::uuids::uuid>(book_uuid),
                        std::nullopt);
                    resp.total_available_count =
                        static_cast<int>(svc.count_trades_filtered(
                            std::optional<boost::uuids::uuid>(book_uuid),
                            std::nullopt));
                } else {
                    resp.trades = svc.list_trades(offset, limit);
                    resp.total_available_count =
                        static_cast<int>(svc.count_trades());
                }
            }
        } catch (...) {}
        BOOST_LOG_SEV(trade_handler_lg(), debug)
            << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

    void save(ores::nats::message msg) {
        BOOST_LOG_SEV(trade_handler_lg(), debug)
            << "Handling " << msg.subject;
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        service::trade_service svc(ctx);
        if (auto req = decode<save_trade_request>(msg)) {
            try {
                for (auto& t : req->trades)
                    stamp(t, ctx);
                svc.save_trades(req->trades);
                BOOST_LOG_SEV(trade_handler_lg(), debug)
                    << "Completed " << msg.subject;
                reply(nats_, msg, save_trade_response{.success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(trade_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_, msg, save_trade_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(trade_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
    }

    void remove(ores::nats::message msg) {
        BOOST_LOG_SEV(trade_handler_lg(), debug)
            << "Handling " << msg.subject;
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        service::trade_service svc(ctx);
        if (auto req = decode<delete_trade_request>(msg)) {
            try {
                for (const auto& id : req->ids)
                    svc.remove_trade(id);
                BOOST_LOG_SEV(trade_handler_lg(), debug)
                    << "Completed " << msg.subject;
                reply(nats_, msg, delete_trade_response{.success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(trade_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_, msg, delete_trade_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(trade_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
    }

    void history(ores::nats::message msg) {
        BOOST_LOG_SEV(trade_handler_lg(), debug)
            << "Handling " << msg.subject;
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        service::trade_service svc(ctx);
        if (auto req = decode<get_trade_history_request>(msg)) {
            try {
                auto versions = svc.get_trade_history(req->id);
                BOOST_LOG_SEV(trade_handler_lg(), debug)
                    << "Completed " << msg.subject;
                reply(nats_, msg, get_trade_history_response{
                    .success = true,
                    .versions = std::move(versions)});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(trade_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_, msg, get_trade_history_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(trade_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::trading::messaging

#endif
