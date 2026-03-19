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
#include "ores.trading/messaging/trade_handler.hpp"

namespace ores::trading::messaging {

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;
    constexpr auto queue = "ores.trading.service";

    subs.push_back(nats.queue_subscribe(
        std::string(get_activity_types_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            trade_handler h(nats, ctx, verifier);
            h.list_activity_types(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(get_trades_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            trade_handler h(nats, ctx, verifier);
            h.list(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(save_trade_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            trade_handler h(nats, ctx, verifier);
            h.save(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(delete_trade_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            trade_handler h(nats, ctx, verifier);
            h.remove(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(get_trade_history_request::nats_subject), queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            trade_handler h(nats, ctx, verifier);
            h.history(std::move(msg));
        }));

    return subs;
}

}
