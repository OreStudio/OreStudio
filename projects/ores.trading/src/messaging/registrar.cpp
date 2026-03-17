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
#include "ores.trading/messaging/registrar.hpp"

#include <optional>
#include <span>
#include <string_view>
#include <rfl/json.hpp>
#include "ores.utility/rfl/reflectors.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.trading/messaging/trade_protocol.hpp"
#include "ores.trading/service/trade_service.hpp"
#include "ores.service/service/request_context.hpp"

namespace ores::trading::messaging {

namespace {

template<typename Resp>
void reply(ores::nats::service::client& nats,
           const ores::nats::message& msg,
           const Resp& resp) {
    if (msg.reply_subject.empty())
        return;
    const auto json = rfl::json::write(resp);
    const auto* p = reinterpret_cast<const std::byte*>(json.data());
    nats.publish(msg.reply_subject, std::span<const std::byte>(p, json.size()));
}

template<typename Req>
std::optional<Req> decode(const ores::nats::message& msg) {
    const std::string_view sv(
        reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto r = rfl::json::read<Req>(sv);
    if (!r)
        return std::nullopt;
    return *r;
}

} // namespace


std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;

    subs.push_back(nats.queue_subscribe(
        "trading.v1.>", "ores.trading.service",
        [&nats, base_ctx = ctx, verifier](ores::nats::message msg) mutable {
            const auto ctx = ores::service::service::make_request_context(base_ctx, msg, verifier);
            const auto& subj = msg.subject;

            // ----------------------------------------------------------------
            // Trades
            // ----------------------------------------------------------------
            if (subj.ends_with(".trades.list")) {
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
                reply(nats, msg, resp);

            } else if (subj.ends_with(".trades.save")) {
                service::trade_service svc(ctx);
                if (auto req = decode<save_trade_request>(msg)) {
                    try {
                        svc.save_trades(req->trades);
                        reply(nats, msg,
                            save_trade_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, save_trade_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".trades.delete")) {
                service::trade_service svc(ctx);
                if (auto req = decode<delete_trade_request>(msg)) {
                    try {
                        for (const auto& id : req->ids)
                            svc.remove_trade(id);
                        reply(nats, msg,
                            delete_trade_response{.success = true});
                    } catch (const std::exception& e) {
                        reply(nats, msg, delete_trade_response{
                            .success = false, .message = e.what()});
                    }
                }

            } else if (subj.ends_with(".trades.history")) {
                service::trade_service svc(ctx);
                if (auto req = decode<get_trade_history_request>(msg)) {
                    try {
                        auto versions = svc.get_trade_history(req->id);
                        reply(nats, msg, get_trade_history_response{
                            .success = true,
                            .versions = std::move(versions)});
                    } catch (const std::exception& e) {
                        reply(nats, msg, get_trade_history_response{
                            .success = false, .message = e.what()});
                    }
                }
            }
        }));

    return subs;
}

}
