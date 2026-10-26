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
 * Template: cpp_trade_type_writer.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_TRADING_CORE_SERVICE_INSTRUMENT_BATCH_WRITER_HPP
#define ORES_TRADING_CORE_SERVICE_INSTRUMENT_BATCH_WRITER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.trading.api/domain/instrument_batch.hpp"
#include "ores.trading.api/domain/trade_type_routing.hpp"
#include "ores.trading.core/repository/balance_guaranteed_swap_instrument_repository.hpp"
#include "ores.trading.core/repository/balance_guaranteed_swap_tranche_notional_repository.hpp"
#include "ores.trading.core/repository/balance_guaranteed_swap_tranche_repository.hpp"
#include "ores.trading.core/repository/bond_instrument_repository.hpp"
#include "ores.trading.core/repository/callable_swap_call_date_repository.hpp"
#include "ores.trading.core/repository/callable_swap_instrument_repository.hpp"
#include "ores.trading.core/repository/cap_floor_instrument_repository.hpp"
#include "ores.trading.core/repository/commodity_basket_constituent_repository.hpp"
#include "ores.trading.core/repository/commodity_instrument_repository.hpp"
#include "ores.trading.core/repository/composite_instrument_repository.hpp"
#include "ores.trading.core/repository/composite_leg_repository.hpp"
#include "ores.trading.core/repository/credit_basket_constituent_repository.hpp"
#include "ores.trading.core/repository/credit_default_swap_instrument_repository.hpp"
#include "ores.trading.core/repository/credit_instrument_repository.hpp"
#include "ores.trading.core/repository/credit_linked_swap_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_accumulator_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_asian_option_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_barrier_option_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_basket_constituent_repository.hpp"
#include "ores.trading.core/repository/equity_digital_option_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_forward_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_option_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_position_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_position_option_underlying_repository.hpp"
#include "ores.trading.core/repository/equity_swap_instrument_repository.hpp"
#include "ores.trading.core/repository/equity_variance_swap_instrument_repository.hpp"
#include "ores.trading.core/repository/flexi_swap_instrument_repository.hpp"
#include "ores.trading.core/repository/flexi_swap_lower_notional_repository.hpp"
#include "ores.trading.core/repository/fra_instrument_repository.hpp"
#include "ores.trading.core/repository/fx_accumulator_instrument_repository.hpp"
#include "ores.trading.core/repository/fx_asian_forward_instrument_repository.hpp"
#include "ores.trading.core/repository/fx_barrier_option_instrument_repository.hpp"
#include "ores.trading.core/repository/fx_digital_option_instrument_repository.hpp"
#include "ores.trading.core/repository/fx_forward_instrument_repository.hpp"
#include "ores.trading.core/repository/fx_vanilla_option_instrument_repository.hpp"
#include "ores.trading.core/repository/fx_variance_swap_instrument_repository.hpp"
#include "ores.trading.core/repository/index_credit_default_swap_instrument_repository.hpp"
#include "ores.trading.core/repository/index_credit_default_swap_option_instrument_repository.hpp"
#include "ores.trading.core/repository/inflation_swap_instrument_repository.hpp"
#include "ores.trading.core/repository/instrument_option_exercise_fee_repository.hpp"
#include "ores.trading.core/repository/instrument_option_exercise_price_repository.hpp"
#include "ores.trading.core/repository/instrument_option_payment_date_repository.hpp"
#include "ores.trading.core/repository/instrument_option_premium_repository.hpp"
#include "ores.trading.core/repository/instrument_option_repository.hpp"
#include "ores.trading.core/repository/instrument_schedule_date_repository.hpp"
#include "ores.trading.core/repository/instrument_schedule_repository.hpp"
#include "ores.trading.core/repository/instrument_strike_repository.hpp"
#include "ores.trading.core/repository/knock_out_swap_instrument_repository.hpp"
#include "ores.trading.core/repository/range_bound_repository.hpp"
#include "ores.trading.core/repository/rate_instrument_repository.hpp"
#include "ores.trading.core/repository/risk_participation_agreement_instrument_repository.hpp"
#include "ores.trading.core/repository/scripted_instrument_repository.hpp"
#include "ores.trading.core/repository/swap_leg_amount_repository.hpp"
#include "ores.trading.core/repository/swap_leg_rate_repository.hpp"
#include "ores.trading.core/repository/swap_leg_repository.hpp"
#include "ores.trading.core/repository/swaption_instrument_repository.hpp"
#include "ores.trading.core/repository/synthetic_cdo_instrument_repository.hpp"
#include "ores.trading.core/repository/vanilla_swap_instrument_repository.hpp"
#include <boost/uuid/uuid.hpp>
#include <string>
#include <string_view>
#include <vector>

namespace ores::trading::service {

/**
 * @brief The keys a booking states on every row of its instrument batch.
 */
struct instrument_keys {
    boost::uuids::uuid trade_id;
    std::string trade_type;
    boost::uuids::uuid activity_id;
};

/**
 * @brief States the trade id, the trade type and the activity id on a row, and
 * starts its version at zero.
 *
 * A document container passes the call to the entity it holds, and an entity
 * that nests its key in an identity group states it there.
 */
template <typename T>
void state_keys(T& row, const instrument_keys& keys) {
    if constexpr (requires { row.instrument; }) {
        state_keys(row.instrument, keys);
    } else if constexpr (requires { row.identity; }) {
        state_keys(row.identity, keys);
    } else {
        if constexpr (requires { row.trade_id; })
            row.trade_id = keys.trade_id;
        if constexpr (requires { row.trade_type_code; })
            row.trade_type_code = keys.trade_type;
        if constexpr (requires { row.trade_activity_id; })
            row.trade_activity_id = keys.activity_id;
        if constexpr (requires { row.version; })
            row.version = 0;
    }
}

/**
 * @brief Stamps a row, and the identity group and the audit group it nests.
 */
template <typename T, typename Stamp>
void stamp_row(T& row, Stamp& stamp) {
    if constexpr (requires { row.instrument; }) {
        stamp_row(row.instrument, stamp);
    } else {
        stamp(row);
        if constexpr (requires { row.identity; })
            stamp(row.identity);
        if constexpr (requires { row.audit; })
            stamp(row.audit);
    }
}

/**
 * @brief The entity a batch member writes: the member, or the entity that a
 * document container holds.
 */
template <typename T>
[[nodiscard]] const auto& entity_of(const T& row) {
    if constexpr (requires { row.instrument; })
        return row.instrument;
    else
        return row;
}

/**
 * @brief Writes every row of one array, each under the claim given.
 *
 * A claim of must_not_exist makes each write a create, so a booking never
 * replaces a row.
 */
template <typename Repository, typename Row, typename Stamp, typename Claim>
void write_rows(ores::database::context ctx,
                std::vector<Row>& rows,
                const instrument_keys& keys,
                Stamp& stamp,
                const Claim& claim) {
    Repository repository;
    for (auto& row : rows) {
        state_keys(row, keys);
        stamp_row(row, stamp);
        repository.write(ctx, entity_of(row), claim);
    }
}

/**
 * @brief Writes every array of the batch.
 *
 * Generated from ores.trading.trade_type_catalogue: one write for each member,
 * in the order the catalogue lists them.
 */
template <typename Stamp, typename Claim>
void write_instrument_batch(ores::database::context ctx,
                            domain::instrument_batch& batch,
                            const instrument_keys& keys,
                            Stamp&& stamp,
                            const Claim& claim) {
    write_rows<repository::bond_instrument_repository>(
        ctx, batch.bond_instruments, keys, stamp, claim);
    write_rows<repository::commodity_instrument_repository>(
        ctx, batch.commodity_instruments, keys, stamp, claim);
    write_rows<repository::composite_instrument_repository>(
        ctx, batch.composite_instruments, keys, stamp, claim);
    write_rows<repository::credit_instrument_repository>(
        ctx, batch.credit_instruments, keys, stamp, claim);
    write_rows<repository::equity_accumulator_instrument_repository>(
        ctx, batch.equity_accumulator_instruments, keys, stamp, claim);
    write_rows<repository::equity_asian_option_instrument_repository>(
        ctx, batch.equity_asian_option_instruments, keys, stamp, claim);
    write_rows<repository::equity_barrier_option_instrument_repository>(
        ctx, batch.equity_barrier_option_instruments, keys, stamp, claim);
    write_rows<repository::equity_digital_option_instrument_repository>(
        ctx, batch.equity_digital_option_instruments, keys, stamp, claim);
    write_rows<repository::equity_forward_instrument_repository>(
        ctx, batch.equity_forward_instruments, keys, stamp, claim);
    write_rows<repository::equity_option_instrument_repository>(
        ctx, batch.equity_option_instruments, keys, stamp, claim);
    write_rows<repository::equity_position_instrument_repository>(
        ctx, batch.equity_position_instruments, keys, stamp, claim);
    write_rows<repository::equity_swap_instrument_repository>(
        ctx, batch.equity_swap_instruments, keys, stamp, claim);
    write_rows<repository::equity_variance_swap_instrument_repository>(
        ctx, batch.equity_variance_swap_instruments, keys, stamp, claim);
    write_rows<repository::fx_accumulator_instrument_repository>(
        ctx, batch.fx_accumulator_instruments, keys, stamp, claim);
    write_rows<repository::fx_asian_forward_instrument_repository>(
        ctx, batch.fx_asian_forward_instruments, keys, stamp, claim);
    write_rows<repository::fx_barrier_option_instrument_repository>(
        ctx, batch.fx_barrier_option_instruments, keys, stamp, claim);
    write_rows<repository::fx_digital_option_instrument_repository>(
        ctx, batch.fx_digital_option_instruments, keys, stamp, claim);
    write_rows<repository::fx_forward_instrument_repository>(
        ctx, batch.fx_forward_instruments, keys, stamp, claim);
    write_rows<repository::fx_vanilla_option_instrument_repository>(
        ctx, batch.fx_vanilla_option_instruments, keys, stamp, claim);
    write_rows<repository::fx_variance_swap_instrument_repository>(
        ctx, batch.fx_variance_swap_instruments, keys, stamp, claim);
    write_rows<repository::rate_instrument_repository>(
        ctx, batch.rate_instruments, keys, stamp, claim);
    write_rows<repository::scripted_instrument_repository>(
        ctx, batch.scripted_instruments, keys, stamp, claim);
    write_rows<repository::balance_guaranteed_swap_instrument_repository>(
        ctx, batch.balance_guaranteed_swap_instruments, keys, stamp, claim);
    write_rows<repository::balance_guaranteed_swap_tranche_repository>(
        ctx, batch.balance_guaranteed_swap_tranches, keys, stamp, claim);
    write_rows<repository::balance_guaranteed_swap_tranche_notional_repository>(
        ctx, batch.balance_guaranteed_swap_tranche_notionals, keys, stamp, claim);
    write_rows<repository::callable_swap_call_date_repository>(
        ctx, batch.callable_swap_call_dates, keys, stamp, claim);
    write_rows<repository::callable_swap_instrument_repository>(
        ctx, batch.callable_swap_instruments, keys, stamp, claim);
    write_rows<repository::cap_floor_instrument_repository>(
        ctx, batch.cap_floor_instruments, keys, stamp, claim);
    write_rows<repository::commodity_basket_constituent_repository>(
        ctx, batch.commodity_basket_constituents, keys, stamp, claim);
    write_rows<repository::composite_leg_repository>(ctx, batch.composite_legs, keys, stamp, claim);
    write_rows<repository::credit_basket_constituent_repository>(
        ctx, batch.credit_basket_constituents, keys, stamp, claim);
    write_rows<repository::credit_default_swap_instrument_repository>(
        ctx, batch.credit_default_swap_instruments, keys, stamp, claim);
    write_rows<repository::credit_linked_swap_instrument_repository>(
        ctx, batch.credit_linked_swap_instruments, keys, stamp, claim);
    write_rows<repository::equity_basket_constituent_repository>(
        ctx, batch.equity_basket_constituents, keys, stamp, claim);
    write_rows<repository::equity_position_option_underlying_repository>(
        ctx, batch.equity_position_option_underlyings, keys, stamp, claim);
    write_rows<repository::flexi_swap_instrument_repository>(
        ctx, batch.flexi_swap_instruments, keys, stamp, claim);
    write_rows<repository::flexi_swap_lower_notional_repository>(
        ctx, batch.flexi_swap_lower_notionals, keys, stamp, claim);
    write_rows<repository::fra_instrument_repository>(
        ctx, batch.fra_instruments, keys, stamp, claim);
    write_rows<repository::index_credit_default_swap_instrument_repository>(
        ctx, batch.index_credit_default_swap_instruments, keys, stamp, claim);
    write_rows<repository::index_credit_default_swap_option_instrument_repository>(
        ctx, batch.index_credit_default_swap_option_instruments, keys, stamp, claim);
    write_rows<repository::inflation_swap_instrument_repository>(
        ctx, batch.inflation_swap_instruments, keys, stamp, claim);
    write_rows<repository::instrument_option_repository>(
        ctx, batch.instrument_options, keys, stamp, claim);
    write_rows<repository::instrument_option_exercise_fee_repository>(
        ctx, batch.instrument_option_exercise_fees, keys, stamp, claim);
    write_rows<repository::instrument_option_exercise_price_repository>(
        ctx, batch.instrument_option_exercise_prices, keys, stamp, claim);
    write_rows<repository::instrument_option_payment_date_repository>(
        ctx, batch.instrument_option_payment_dates, keys, stamp, claim);
    write_rows<repository::instrument_option_premium_repository>(
        ctx, batch.instrument_option_premiums, keys, stamp, claim);
    write_rows<repository::instrument_schedule_repository>(
        ctx, batch.instrument_schedules, keys, stamp, claim);
    write_rows<repository::instrument_schedule_date_repository>(
        ctx, batch.instrument_schedule_dates, keys, stamp, claim);
    write_rows<repository::instrument_strike_repository>(
        ctx, batch.instrument_strikes, keys, stamp, claim);
    write_rows<repository::knock_out_swap_instrument_repository>(
        ctx, batch.knock_out_swap_instruments, keys, stamp, claim);
    write_rows<repository::range_bound_repository>(ctx, batch.range_bounds, keys, stamp, claim);
    write_rows<repository::risk_participation_agreement_instrument_repository>(
        ctx, batch.risk_participation_agreement_instruments, keys, stamp, claim);
    write_rows<repository::swap_leg_repository>(ctx, batch.swap_legs, keys, stamp, claim);
    write_rows<repository::swap_leg_amount_repository>(
        ctx, batch.swap_leg_amounts, keys, stamp, claim);
    write_rows<repository::swap_leg_rate_repository>(ctx, batch.swap_leg_rates, keys, stamp, claim);
    write_rows<repository::swaption_instrument_repository>(
        ctx, batch.swaption_instruments, keys, stamp, claim);
    write_rows<repository::synthetic_cdo_instrument_repository>(
        ctx, batch.synthetic_cdo_instruments, keys, stamp, claim);
    write_rows<repository::vanilla_swap_instrument_repository>(
        ctx, batch.vanilla_swap_instruments, keys, stamp, claim);
}

/**
 * @brief The routed arrays of the batch that hold rows and are not the table
 * the catalogue routes @p trade_type to.
 *
 * A trade type no table holds names every routed array that holds rows.
 */
[[nodiscard]] inline std::vector<std::string>
misrouted_arrays(const domain::instrument_batch& batch, std::string_view trade_type) {
    std::vector<std::string> names;
    const auto table = domain::instrument_table_for(trade_type);
    if (!batch.bond_instruments.empty() && table != domain::instrument_table::bond_instrument)
        names.emplace_back("bond_instruments");
    if (!batch.commodity_instruments.empty() &&
        table != domain::instrument_table::commodity_instrument)
        names.emplace_back("commodity_instruments");
    if (!batch.composite_instruments.empty() &&
        table != domain::instrument_table::composite_instrument)
        names.emplace_back("composite_instruments");
    if (!batch.credit_instruments.empty() && table != domain::instrument_table::credit_instrument)
        names.emplace_back("credit_instruments");
    if (!batch.equity_accumulator_instruments.empty() &&
        table != domain::instrument_table::equity_accumulator_instrument)
        names.emplace_back("equity_accumulator_instruments");
    if (!batch.equity_asian_option_instruments.empty() &&
        table != domain::instrument_table::equity_asian_option_instrument)
        names.emplace_back("equity_asian_option_instruments");
    if (!batch.equity_barrier_option_instruments.empty() &&
        table != domain::instrument_table::equity_barrier_option_instrument)
        names.emplace_back("equity_barrier_option_instruments");
    if (!batch.equity_digital_option_instruments.empty() &&
        table != domain::instrument_table::equity_digital_option_instrument)
        names.emplace_back("equity_digital_option_instruments");
    if (!batch.equity_forward_instruments.empty() &&
        table != domain::instrument_table::equity_forward_instrument)
        names.emplace_back("equity_forward_instruments");
    if (!batch.equity_option_instruments.empty() &&
        table != domain::instrument_table::equity_option_instrument)
        names.emplace_back("equity_option_instruments");
    if (!batch.equity_position_instruments.empty() &&
        table != domain::instrument_table::equity_position_instrument)
        names.emplace_back("equity_position_instruments");
    if (!batch.equity_swap_instruments.empty() &&
        table != domain::instrument_table::equity_swap_instrument)
        names.emplace_back("equity_swap_instruments");
    if (!batch.equity_variance_swap_instruments.empty() &&
        table != domain::instrument_table::equity_variance_swap_instrument)
        names.emplace_back("equity_variance_swap_instruments");
    if (!batch.fx_accumulator_instruments.empty() &&
        table != domain::instrument_table::fx_accumulator_instrument)
        names.emplace_back("fx_accumulator_instruments");
    if (!batch.fx_asian_forward_instruments.empty() &&
        table != domain::instrument_table::fx_asian_forward_instrument)
        names.emplace_back("fx_asian_forward_instruments");
    if (!batch.fx_barrier_option_instruments.empty() &&
        table != domain::instrument_table::fx_barrier_option_instrument)
        names.emplace_back("fx_barrier_option_instruments");
    if (!batch.fx_digital_option_instruments.empty() &&
        table != domain::instrument_table::fx_digital_option_instrument)
        names.emplace_back("fx_digital_option_instruments");
    if (!batch.fx_forward_instruments.empty() &&
        table != domain::instrument_table::fx_forward_instrument)
        names.emplace_back("fx_forward_instruments");
    if (!batch.fx_vanilla_option_instruments.empty() &&
        table != domain::instrument_table::fx_vanilla_option_instrument)
        names.emplace_back("fx_vanilla_option_instruments");
    if (!batch.fx_variance_swap_instruments.empty() &&
        table != domain::instrument_table::fx_variance_swap_instrument)
        names.emplace_back("fx_variance_swap_instruments");
    if (!batch.rate_instruments.empty() && table != domain::instrument_table::rate_instrument)
        names.emplace_back("rate_instruments");
    if (!batch.scripted_instruments.empty() &&
        table != domain::instrument_table::scripted_instrument)
        names.emplace_back("scripted_instruments");
    return names;
}

/**
 * @brief The child arrays of the batch that hold rows when no routed array
 * holds one.
 *
 * A child belongs to the header of its family, so a batch with children and no
 * header is not a booking.
 */
[[nodiscard]] inline std::vector<std::string> orphan_arrays(const domain::instrument_batch& batch) {
    std::vector<std::string> names;
    const bool headed =
        !batch.bond_instruments.empty() || !batch.commodity_instruments.empty() ||
        !batch.composite_instruments.empty() || !batch.credit_instruments.empty() ||
        !batch.equity_accumulator_instruments.empty() ||
        !batch.equity_asian_option_instruments.empty() ||
        !batch.equity_barrier_option_instruments.empty() ||
        !batch.equity_digital_option_instruments.empty() ||
        !batch.equity_forward_instruments.empty() || !batch.equity_option_instruments.empty() ||
        !batch.equity_position_instruments.empty() || !batch.equity_swap_instruments.empty() ||
        !batch.equity_variance_swap_instruments.empty() ||
        !batch.fx_accumulator_instruments.empty() || !batch.fx_asian_forward_instruments.empty() ||
        !batch.fx_barrier_option_instruments.empty() ||
        !batch.fx_digital_option_instruments.empty() || !batch.fx_forward_instruments.empty() ||
        !batch.fx_vanilla_option_instruments.empty() ||
        !batch.fx_variance_swap_instruments.empty() || !batch.rate_instruments.empty() ||
        !batch.scripted_instruments.empty() || false;
    if (headed)
        return names;
    if (!batch.balance_guaranteed_swap_instruments.empty())
        names.emplace_back("balance_guaranteed_swap_instruments");
    if (!batch.balance_guaranteed_swap_tranches.empty())
        names.emplace_back("balance_guaranteed_swap_tranches");
    if (!batch.balance_guaranteed_swap_tranche_notionals.empty())
        names.emplace_back("balance_guaranteed_swap_tranche_notionals");
    if (!batch.callable_swap_call_dates.empty())
        names.emplace_back("callable_swap_call_dates");
    if (!batch.callable_swap_instruments.empty())
        names.emplace_back("callable_swap_instruments");
    if (!batch.cap_floor_instruments.empty())
        names.emplace_back("cap_floor_instruments");
    if (!batch.commodity_basket_constituents.empty())
        names.emplace_back("commodity_basket_constituents");
    if (!batch.composite_legs.empty())
        names.emplace_back("composite_legs");
    if (!batch.credit_basket_constituents.empty())
        names.emplace_back("credit_basket_constituents");
    if (!batch.credit_default_swap_instruments.empty())
        names.emplace_back("credit_default_swap_instruments");
    if (!batch.credit_linked_swap_instruments.empty())
        names.emplace_back("credit_linked_swap_instruments");
    if (!batch.equity_basket_constituents.empty())
        names.emplace_back("equity_basket_constituents");
    if (!batch.equity_position_option_underlyings.empty())
        names.emplace_back("equity_position_option_underlyings");
    if (!batch.flexi_swap_instruments.empty())
        names.emplace_back("flexi_swap_instruments");
    if (!batch.flexi_swap_lower_notionals.empty())
        names.emplace_back("flexi_swap_lower_notionals");
    if (!batch.fra_instruments.empty())
        names.emplace_back("fra_instruments");
    if (!batch.index_credit_default_swap_instruments.empty())
        names.emplace_back("index_credit_default_swap_instruments");
    if (!batch.index_credit_default_swap_option_instruments.empty())
        names.emplace_back("index_credit_default_swap_option_instruments");
    if (!batch.inflation_swap_instruments.empty())
        names.emplace_back("inflation_swap_instruments");
    if (!batch.instrument_options.empty())
        names.emplace_back("instrument_options");
    if (!batch.instrument_option_exercise_fees.empty())
        names.emplace_back("instrument_option_exercise_fees");
    if (!batch.instrument_option_exercise_prices.empty())
        names.emplace_back("instrument_option_exercise_prices");
    if (!batch.instrument_option_payment_dates.empty())
        names.emplace_back("instrument_option_payment_dates");
    if (!batch.instrument_option_premiums.empty())
        names.emplace_back("instrument_option_premiums");
    if (!batch.instrument_schedules.empty())
        names.emplace_back("instrument_schedules");
    if (!batch.instrument_schedule_dates.empty())
        names.emplace_back("instrument_schedule_dates");
    if (!batch.instrument_strikes.empty())
        names.emplace_back("instrument_strikes");
    if (!batch.knock_out_swap_instruments.empty())
        names.emplace_back("knock_out_swap_instruments");
    if (!batch.range_bounds.empty())
        names.emplace_back("range_bounds");
    if (!batch.risk_participation_agreement_instruments.empty())
        names.emplace_back("risk_participation_agreement_instruments");
    if (!batch.swap_legs.empty())
        names.emplace_back("swap_legs");
    if (!batch.swap_leg_amounts.empty())
        names.emplace_back("swap_leg_amounts");
    if (!batch.swap_leg_rates.empty())
        names.emplace_back("swap_leg_rates");
    if (!batch.swaption_instruments.empty())
        names.emplace_back("swaption_instruments");
    if (!batch.synthetic_cdo_instruments.empty())
        names.emplace_back("synthetic_cdo_instruments");
    if (!batch.vanilla_swap_instruments.empty())
        names.emplace_back("vanilla_swap_instruments");
    return names;
}

}

#endif
