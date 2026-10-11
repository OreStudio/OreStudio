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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_trade_type_digest.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_TRADING_CORE_REPOSITORY_INSTRUMENT_ECONOMIC_SHAPE_HPP
#define ORES_TRADING_CORE_REPOSITORY_INSTRUMENT_ECONOMIC_SHAPE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.trading.api/domain/trade_type_routing.hpp"
#include "ores.trading.core/repository/balance_guaranteed_swap_instrument_repository.hpp"
#include "ores.trading.core/repository/bond_instrument_repository.hpp"
#include "ores.trading.core/repository/callable_swap_instrument_repository.hpp"
#include "ores.trading.core/repository/cap_floor_instrument_repository.hpp"
#include "ores.trading.core/repository/commodity_instrument_repository.hpp"
#include "ores.trading.core/repository/composite_instrument_repository.hpp"
#include "ores.trading.core/repository/credit_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_accumulator_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_asian_option_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_barrier_option_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_digital_option_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_forward_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_option_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_position_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_swap_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_variance_swap_instrument_repository.hpp"
#include "ores.trading.core/repository/fra_instrument_repository.hpp"
#include "ores.trading.core/repository/fx_accumulator_instrument_repository.hpp"
#include "ores.trading.core/repository/fx_asian_forward_instrument_repository.hpp"
#include "ores.trading.core/repository/fx_barrier_option_instrument_repository.hpp"
#include "ores.trading.core/repository/fx_digital_option_instrument_repository.hpp"
#include "ores.trading.core/repository/fx_forward_instrument_repository.hpp"
#include "ores.trading.core/repository/fx_vanilla_option_instrument_repository.hpp"
#include "ores.trading.core/repository/fx_variance_swap_instrument_repository.hpp"
#include "ores.trading.core/repository/inflation_swap_instrument_repository.hpp"
#include "ores.trading.core/repository/knock_out_swap_instrument_repository.hpp"
#include "ores.trading.core/repository/parent_scoped_queries.hpp"
#include "ores.trading.core/repository/rate_instrument_repository.hpp"
#include "ores.trading.core/repository/scripted_instrument_repository.hpp"
#include "ores.trading.core/repository/swaption_instrument_repository.hpp"
#include "ores.trading.core/repository/vanilla_swap_instrument_repository.hpp"
#include <string>
#include <vector>

namespace ores::trading::repository {

/**
 * @brief Hands @p append the latest rows of the instrument components of a
 * trade.
 *
 * Generated from the digest section of ores.trading.trade_type_catalogue. A
 * component with no row for the trade appends an empty result.
 */
template <typename Append>
void read_instrument_components(ores::database::context ctx,
                                const std::string& trade_id,
                                domain::instrument_table table,
                                Append&& append) {
    switch (table) {
        case domain::instrument_table::bond_instrument:
            append(bond_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::commodity_instrument:
            append(commodity_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::composite_instrument:
            append(composite_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::credit_instrument:
            append(credit_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::equity_accumulator_instrument:
            append(equity_accumulator_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::equity_asian_option_instrument:
            append(equity_asian_option_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::equity_barrier_option_instrument:
            append(equity_barrier_option_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::equity_digital_option_instrument:
            append(equity_digital_option_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::equity_forward_instrument:
            append(equity_forward_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::equity_option_instrument:
            append(equity_option_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::equity_position_instrument:
            append(equity_position_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::equity_swap_instrument:
            append(equity_swap_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::equity_variance_swap_instrument:
            append(equity_variance_swap_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::fx_accumulator_instrument:
            append(fx_accumulator_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::fx_asian_forward_instrument:
            append(fx_asian_forward_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::fx_barrier_option_instrument:
            append(fx_barrier_option_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::fx_digital_option_instrument:
            append(fx_digital_option_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::fx_forward_instrument:
            append(fx_forward_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::fx_vanilla_option_instrument:
            append(fx_vanilla_option_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::fx_variance_swap_instrument:
            append(fx_variance_swap_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::rate_instrument:
            append(rate_instrument_repository{}.read_latest(ctx, trade_id));
            append(balance_guaranteed_swap_instrument_repository{}.read_latest(ctx, trade_id));
            append(callable_swap_instrument_repository{}.read_latest(ctx, trade_id));
            append(cap_floor_instrument_repository{}.read_latest(ctx, trade_id));
            append(fra_instrument_repository{}.read_latest(ctx, trade_id));
            append(inflation_swap_instrument_repository{}.read_latest(ctx, trade_id));
            append(knock_out_swap_instrument_repository{}.read_latest(ctx, trade_id));
            append(swaption_instrument_repository{}.read_latest(ctx, trade_id));
            append(vanilla_swap_instrument_repository{}.read_latest(ctx, trade_id));
            break;
        case domain::instrument_table::scripted_instrument:
            append(scripted_instrument_repository{}.read_latest(ctx, trade_id));
            break;
    }
}

/**
 * @brief Hands @p append the rows every instrument shares: the legs, the
 * strikes and the option and schedule blocks.
 *
 * Generated from the digest section of ores.trading.trade_type_catalogue.
 */
template <typename Append>
void read_shared_components(ores::database::context ctx,
                            const std::string& trade_id,
                            Append&& append) {
    const auto ids = std::vector<std::string>{trade_id};
    append(read_legs_by_trade_ids(ctx, ids));
    append(read_leg_amounts_by_trade_ids(ctx, ids));
    append(read_leg_rates_by_trade_ids(ctx, ids));
    append(read_leg_amortizations_by_trade_ids(ctx, ids));
    append(read_forwards_by_trade_ids(ctx, ids));
    append(read_strikes_by_trade_ids(ctx, ids));
    append(read_options_by_trade_ids(ctx, ids));
    append(read_option_exercise_fees_by_trade_ids(ctx, ids));
    append(read_option_payment_dates_by_trade_ids(ctx, ids));
    append(read_option_premiums_by_trade_ids(ctx, ids));
    append(read_schedules_by_trade_ids(ctx, ids));
    append(read_schedule_dates_by_trade_ids(ctx, ids));
}

}

#endif
