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
#include "ores.trading.core/messaging/equity_accumulator_instrument_registrar.hpp"
#include "ores.trading.core/messaging/equity_barrier_option_instrument_registrar.hpp"
#include "ores.trading.core/messaging/equity_forward_instrument_registrar.hpp"
#include "ores.trading.core/messaging/equity_option_instrument_registrar.hpp"
#include "ores.trading.core/messaging/equity_position_instrument_registrar.hpp"
#include "ores.trading.core/messaging/equity_swap_instrument_registrar.hpp"
#include "ores.trading.core/messaging/equity_variance_swap_instrument_registrar.hpp"
#include "ores.trading.core/messaging/registrar_detail.hpp"
#include "ores.trading.core/messaging/typed_equity_instrument_handler.hpp"

namespace ores::trading::messaging::detail {

std::vector<ores::nats::service::subscription>
register_equity_handlers(ores::nats::service::client& nats,
                         ores::database::context ctx,
                         std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;
    constexpr auto queue = queue_name;

    subs.push_back(nats.queue_subscribe(
        std::string(save_equity_digital_option_instrument_request::nats_subject),
        queue,
        [&nats, ctx, verifier](ores::nats::message msg) mutable {
            typed_equity_instrument_handler h(nats, ctx, verifier);
            h.save_digital_option(std::move(msg));
        }));

    subs.push_back(
        nats.queue_subscribe(std::string(save_equity_asian_option_instrument_request::nats_subject),
                             queue,
                             [&nats, ctx, verifier](ores::nats::message msg) mutable {
                                 typed_equity_instrument_handler h(nats, ctx, verifier);
                                 h.save_asian_option(std::move(msg));
                             }));

    auto equity_accumulator_instrument_subs =
        register_equity_accumulator_instrument_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(equity_accumulator_instrument_subs.begin()),
                std::make_move_iterator(equity_accumulator_instrument_subs.end()));

    auto equity_barrier_option_instrument_subs =
        register_equity_barrier_option_instrument_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(equity_barrier_option_instrument_subs.begin()),
                std::make_move_iterator(equity_barrier_option_instrument_subs.end()));

    auto equity_option_instrument_subs =
        register_equity_option_instrument_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(equity_option_instrument_subs.begin()),
                std::make_move_iterator(equity_option_instrument_subs.end()));

    auto equity_position_instrument_subs =
        register_equity_position_instrument_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(equity_position_instrument_subs.begin()),
                std::make_move_iterator(equity_position_instrument_subs.end()));

    auto equity_swap_instrument_subs =
        register_equity_swap_instrument_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(equity_swap_instrument_subs.begin()),
                std::make_move_iterator(equity_swap_instrument_subs.end()));

    auto equity_variance_swap_instrument_subs =
        register_equity_variance_swap_instrument_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(equity_variance_swap_instrument_subs.begin()),
                std::make_move_iterator(equity_variance_swap_instrument_subs.end()));

    auto equity_forward_instrument_subs =
        register_equity_forward_instrument_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(equity_forward_instrument_subs.begin()),
                std::make_move_iterator(equity_forward_instrument_subs.end()));

    return subs;
}

}
