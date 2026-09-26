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
#include "ores.trading.core/messaging/fra_instrument_registrar.hpp"
#include "ores.trading.core/messaging/vanilla_swap_instrument_registrar.hpp"
#include "ores.trading.core/messaging/cap_floor_instrument_registrar.hpp"
#include "ores.trading.core/messaging/swaption_instrument_registrar.hpp"
#include "ores.trading.core/messaging/balance_guaranteed_swap_instrument_registrar.hpp"
#include "ores.trading.core/messaging/callable_swap_instrument_registrar.hpp"
#include "ores.trading.core/messaging/knock_out_swap_instrument_registrar.hpp"
#include "ores.trading.core/messaging/inflation_swap_instrument_registrar.hpp"
#include "ores.trading.core/messaging/rpa_instrument_registrar.hpp"
#include "ores.trading.core/messaging/registrar_detail.hpp"

namespace ores::trading::messaging::detail {

std::vector<ores::nats::service::subscription>
register_rates_handlers(ores::nats::service::client& nats,
                        ores::database::context ctx,
                        std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;

    auto fra_instrument_subs = register_fra_instrument_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(fra_instrument_subs.begin()),
                std::make_move_iterator(fra_instrument_subs.end()));

    auto vanilla_swap_instrument_subs = register_vanilla_swap_instrument_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(vanilla_swap_instrument_subs.begin()),
                std::make_move_iterator(vanilla_swap_instrument_subs.end()));

    auto cap_floor_instrument_subs = register_cap_floor_instrument_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(cap_floor_instrument_subs.begin()),
                std::make_move_iterator(cap_floor_instrument_subs.end()));

    auto swaption_instrument_subs = register_swaption_instrument_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(swaption_instrument_subs.begin()),
                std::make_move_iterator(swaption_instrument_subs.end()));

    auto balance_guaranteed_swap_instrument_subs = register_balance_guaranteed_swap_instrument_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(balance_guaranteed_swap_instrument_subs.begin()),
                std::make_move_iterator(balance_guaranteed_swap_instrument_subs.end()));

    auto callable_swap_instrument_subs = register_callable_swap_instrument_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(callable_swap_instrument_subs.begin()),
                std::make_move_iterator(callable_swap_instrument_subs.end()));

    auto knock_out_swap_instrument_subs = register_knock_out_swap_instrument_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(knock_out_swap_instrument_subs.begin()),
                std::make_move_iterator(knock_out_swap_instrument_subs.end()));

    auto inflation_swap_instrument_subs = register_inflation_swap_instrument_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(inflation_swap_instrument_subs.begin()),
                std::make_move_iterator(inflation_swap_instrument_subs.end()));

    auto rpa_instrument_subs = register_rpa_instrument_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(rpa_instrument_subs.begin()),
                std::make_move_iterator(rpa_instrument_subs.end()));

    return subs;
}

}
