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
#include "ores.trading.api/messaging/trade_protocol.hpp"
#include "ores.trading.core/messaging/registrar_detail.hpp"
#include "ores.trading.core/messaging/trade_handler.hpp"
#include "ores.trading.core/messaging/trade_registrar.hpp"
#include "ores.trading.core/messaging/trade_type_registrar.hpp"

namespace ores::trading::messaging::detail {

std::vector<ores::nats::service::subscription>
register_trade_handlers(ores::nats::service::client& nats,
                        ores::database::context ctx,
                        std::optional<ores::security::jwt::jwt_authenticator> verifier,
                        const std::string& http_base_url) {
    // The trade entity's own verbs come from its generated registrar.
    auto subs = ores::trading::messaging::register_trade_handlers(nats, ctx, verifier);

    constexpr auto queue = queue_name;

    // The three operations below have no model and stay hand-written: the
    // instrument rebuild, the portfolio export, and the storage export.
    subs.push_back(nats.queue_subscribe(std::string(get_trade_instrument_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            trade_handler h(nats, ctx, verifier);
                                            h.instrument(std::move(msg));
                                        }));

    subs.push_back(nats.queue_subscribe(std::string(export_portfolio_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            trade_handler h(nats, ctx, verifier);
                                            h.export_portfolio(std::move(msg));
                                        }));

    subs.push_back(nats.queue_subscribe(
        std::string(export_trades_to_storage_request::nats_subject),
        queue,
        [&nats, ctx, verifier, http_base_url](ores::nats::message msg) mutable {
            trade_handler h(nats, ctx, verifier, http_base_url);
            h.export_trades_to_storage(std::move(msg));
        }));

    // activity_type has no model yet, so its lookup verb is served here.
    subs.push_back(nats.queue_subscribe(std::string(get_activity_types_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            trade_handler h(nats, ctx, verifier);
                                            h.list_activity_types(std::move(msg));
                                        }));

    // Instrument reference data — floating index types and leg types moved
    // to ores.refdata (see ores.refdata.core/messaging/registrar.cpp); trade
    // types are handled by the entity-shaped trade_type handler stack.
    auto trade_type_subs = register_trade_type_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(trade_type_subs.begin()),
                std::make_move_iterator(trade_type_subs.end()));

    return subs;
}

}
