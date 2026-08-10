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
#include "ores.trading.core/messaging/registrar_detail.hpp"
#include "ores.trading.core/messaging/trade_handler.hpp"
#include "ores.trading.core/messaging/trade_type_registrar.hpp"

namespace ores::trading::messaging::detail {

std::vector<ores::nats::service::subscription>
register_trade_handlers(ores::nats::service::client& nats,
                        ores::database::context ctx,
                        std::optional<ores::security::jwt::jwt_authenticator> verifier,
                        const std::string& http_base_url) {
    std::vector<ores::nats::service::subscription> subs;
    constexpr auto queue = queue_name;

    subs.push_back(nats.queue_subscribe(std::string(get_activity_types_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            trade_handler h(nats, ctx, verifier);
                                            h.list_activity_types(std::move(msg));
                                        }));

    subs.push_back(nats.queue_subscribe(std::string(get_trades_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            trade_handler h(nats, ctx, verifier);
                                            h.list(std::move(msg));
                                        }));

    subs.push_back(nats.queue_subscribe(std::string(save_trade_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            trade_handler h(nats, ctx, verifier);
                                            h.save(std::move(msg));
                                        }));

    subs.push_back(nats.queue_subscribe(std::string(delete_trade_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            trade_handler h(nats, ctx, verifier);
                                            h.remove(std::move(msg));
                                        }));

    subs.push_back(nats.queue_subscribe(std::string(get_trade_history_request::nats_subject),
                                        queue,
                                        [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                            trade_handler h(nats, ctx, verifier);
                                            h.history(std::move(msg));
                                        }));

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

    // Instrument reference data — floating index types and leg types moved
    // to ores.refdata (see ores.refdata.core/messaging/registrar.cpp); trade
    // types are handled by the entity-shaped trade_type handler stack.

    // Instrument reference data — trade types
    auto trade_type_subs = register_trade_type_handlers(nats, ctx, verifier);
    subs.insert(subs.end(), std::make_move_iterator(trade_type_subs.begin()),
                std::make_move_iterator(trade_type_subs.end()));

    return subs;
}

}
