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
#include "ores.trading.core/messaging/balance_guaranteed_swap_instrument_registrar.hpp"
#include "ores.trading.core/messaging/balance_guaranteed_swap_tranche_registrar.hpp"
#include "ores.trading.core/messaging/balance_guaranteed_swap_tranche_notional_registrar.hpp"
#include "ores.trading.core/messaging/callable_swap_call_date_registrar.hpp"
#include "ores.trading.core/messaging/callable_swap_instrument_registrar.hpp"
#include "ores.trading.core/messaging/cap_floor_instrument_registrar.hpp"
#include "ores.trading.core/messaging/flexi_swap_instrument_registrar.hpp"
#include "ores.trading.core/messaging/flexi_swap_lower_notional_registrar.hpp"
#include "ores.trading.core/messaging/fra_instrument_registrar.hpp"
#include "ores.trading.core/messaging/inflation_swap_instrument_registrar.hpp"
#include "ores.trading.core/messaging/instrument_option_exercise_fee_registrar.hpp"
#include "ores.trading.core/messaging/instrument_option_exercise_price_registrar.hpp"
#include "ores.trading.core/messaging/instrument_option_payment_date_registrar.hpp"
#include "ores.trading.core/messaging/instrument_option_premium_registrar.hpp"
#include "ores.trading.core/messaging/instrument_option_registrar.hpp"
#include "ores.trading.core/messaging/instrument_schedule_date_registrar.hpp"
#include "ores.trading.core/messaging/instrument_schedule_registrar.hpp"
#include "ores.trading.core/messaging/instrument_strike_registrar.hpp"
#include "ores.trading.core/messaging/knock_out_swap_instrument_registrar.hpp"
#include "ores.trading.core/messaging/rate_instrument_registrar.hpp"
#include "ores.trading.core/messaging/registrar_detail.hpp"
#include "ores.trading.core/messaging/rpa_instrument_registrar.hpp"
#include "ores.trading.core/messaging/swap_leg_registrar.hpp"
#include "ores.trading.core/messaging/swaption_instrument_registrar.hpp"
#include "ores.trading.core/messaging/vanilla_swap_instrument_registrar.hpp"

namespace ores::trading::messaging::detail {

std::vector<ores::nats::service::subscription>
register_rates_handlers(ores::nats::service::client& nats,
                        ores::database::context ctx,
                        std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;

    auto rate_instrument_subs = register_rate_instrument_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(rate_instrument_subs.begin()),
                std::make_move_iterator(rate_instrument_subs.end()));

    auto fra_instrument_subs = register_fra_instrument_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(fra_instrument_subs.begin()),
                std::make_move_iterator(fra_instrument_subs.end()));

    auto vanilla_swap_instrument_subs =
        register_vanilla_swap_instrument_handlers(nats, ctx, verifier);
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

    auto balance_guaranteed_swap_instrument_subs =
        register_balance_guaranteed_swap_instrument_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(balance_guaranteed_swap_instrument_subs.begin()),
                std::make_move_iterator(balance_guaranteed_swap_instrument_subs.end()));

    auto balance_guaranteed_swap_tranche_subs =
        register_balance_guaranteed_swap_tranche_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(balance_guaranteed_swap_tranche_subs.begin()),
                std::make_move_iterator(balance_guaranteed_swap_tranche_subs.end()));

    auto balance_guaranteed_swap_tranche_notional_subs =
        register_balance_guaranteed_swap_tranche_notional_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(balance_guaranteed_swap_tranche_notional_subs.begin()),
                std::make_move_iterator(balance_guaranteed_swap_tranche_notional_subs.end()));

    auto flexi_swap_instrument_subs =
        register_flexi_swap_instrument_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(flexi_swap_instrument_subs.begin()),
                std::make_move_iterator(flexi_swap_instrument_subs.end()));

    auto flexi_swap_lower_notional_subs =
        register_flexi_swap_lower_notional_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(flexi_swap_lower_notional_subs.begin()),
                std::make_move_iterator(flexi_swap_lower_notional_subs.end()));

    auto callable_swap_instrument_subs =
        register_callable_swap_instrument_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(callable_swap_instrument_subs.begin()),
                std::make_move_iterator(callable_swap_instrument_subs.end()));

    auto callable_swap_call_date_subs =
        register_callable_swap_call_date_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(callable_swap_call_date_subs.begin()),
                std::make_move_iterator(callable_swap_call_date_subs.end()));

    auto knock_out_swap_instrument_subs =
        register_knock_out_swap_instrument_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(knock_out_swap_instrument_subs.begin()),
                std::make_move_iterator(knock_out_swap_instrument_subs.end()));

    auto inflation_swap_instrument_subs =
        register_inflation_swap_instrument_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(inflation_swap_instrument_subs.begin()),
                std::make_move_iterator(inflation_swap_instrument_subs.end()));

    auto rpa_instrument_subs = register_rpa_instrument_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(rpa_instrument_subs.begin()),
                std::make_move_iterator(rpa_instrument_subs.end()));

    auto swap_leg_subs = register_swap_leg_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(swap_leg_subs.begin()),
                std::make_move_iterator(swap_leg_subs.end()));

    // The shared children the rates family writes. They are registered here,
    // in the root that owns them, rather than by the bond family's root.
    auto instrument_strike_subs = register_instrument_strike_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(instrument_strike_subs.begin()),
                std::make_move_iterator(instrument_strike_subs.end()));

    auto instrument_schedule_subs = register_instrument_schedule_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(instrument_schedule_subs.begin()),
                std::make_move_iterator(instrument_schedule_subs.end()));

    auto instrument_schedule_date_subs =
        register_instrument_schedule_date_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(instrument_schedule_date_subs.begin()),
                std::make_move_iterator(instrument_schedule_date_subs.end()));

    auto instrument_option_subs = register_instrument_option_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(instrument_option_subs.begin()),
                std::make_move_iterator(instrument_option_subs.end()));

    auto instrument_option_premium_subs =
        register_instrument_option_premium_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(instrument_option_premium_subs.begin()),
                std::make_move_iterator(instrument_option_premium_subs.end()));

    auto instrument_option_exercise_fee_subs =
        register_instrument_option_exercise_fee_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(instrument_option_exercise_fee_subs.begin()),
                std::make_move_iterator(instrument_option_exercise_fee_subs.end()));

    auto instrument_option_payment_date_subs =
        register_instrument_option_payment_date_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(instrument_option_payment_date_subs.begin()),
                std::make_move_iterator(instrument_option_payment_date_subs.end()));

    auto instrument_option_exercise_price_subs =
        register_instrument_option_exercise_price_handlers(nats, ctx, verifier);
    subs.insert(subs.end(),
                std::make_move_iterator(instrument_option_exercise_price_subs.begin()),
                std::make_move_iterator(instrument_option_exercise_price_subs.end()));

    return subs;
}

}
