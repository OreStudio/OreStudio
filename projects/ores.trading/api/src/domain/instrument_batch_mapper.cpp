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
#include "ores.trading.api/domain/instrument_batch_mapper.hpp"
#include <type_traits>
#include <variant>

namespace ores::trading::domain {
namespace {

/**
 * A rates leaf and the children it owns, which are the parts of the family
 * carrier the batch stores separately.
 */
template <typename Leaf>
swap_instrument_data
as_swap(const instrument_batch& batch, const Leaf& leaf, boost::uuids::uuid trade_id) {
    return swap_instrument_data{leaf,
                                find_all(batch.swap_legs, trade_id),
                                find_all(batch.swap_leg_amounts, trade_id),
                                find_all(batch.swap_leg_rates, trade_id),
                                find_all(batch.callable_swap_call_dates, trade_id)};
}

/**
 * An equity leaf and the position entries it owns.
 */
template <typename Leaf>
equity_instrument_data
as_equity(const instrument_batch& batch, const Leaf& leaf, boost::uuids::uuid trade_id) {
    return equity_instrument_data{equity_instrument_variant{leaf},
                                  find_all(batch.equity_position_option_underlyings, trade_id)};
}

/**
 * A leaf inside a variant, appended as its own type. The variant never
 * reaches the batch, only its alternatives do.
 */
template <typename Variant>
void append_variant(instrument_batch& batch, const Variant& variant) {
    std::visit([&](const auto& leaf) { append(batch, leaf); }, variant);
}

}

void append_instrument(instrument_batch& batch, const trade_instrument& instrument) {
    std::visit(
        [&]<typename T>(const T& v) {
            if constexpr (std::is_same_v<T, std::monostate>) {
                return;
            } else if constexpr (std::is_same_v<T, swap_instrument_data>) {
                append_variant(batch, v.instrument);
                batch.swap_legs.insert(batch.swap_legs.end(), v.legs.begin(), v.legs.end());
                batch.swap_leg_amounts.insert(
                    batch.swap_leg_amounts.end(), v.leg_amounts.begin(), v.leg_amounts.end());
                batch.swap_leg_rates.insert(
                    batch.swap_leg_rates.end(), v.leg_rates.begin(), v.leg_rates.end());
                batch.callable_swap_call_dates.insert(
                    batch.callable_swap_call_dates.end(), v.call_dates.begin(), v.call_dates.end());
            } else if constexpr (std::is_same_v<T, fx_instrument_variant>) {
                append_variant(batch, v);
            } else if constexpr (std::is_same_v<T, equity_instrument_data>) {
                append_variant(batch, v.instrument);
                batch.equity_position_option_underlyings.insert(
                    batch.equity_position_option_underlyings.end(),
                    v.underlyings.begin(),
                    v.underlyings.end());
            } else if constexpr (std::is_same_v<T, commodity_instrument_data>) {
                append(batch, v.instrument);
                batch.commodity_basket_constituents.insert(
                    batch.commodity_basket_constituents.end(),
                    v.constituents.begin(),
                    v.constituents.end());
            } else if constexpr (std::is_same_v<T, composite_instrument_data>) {
                append(batch, v.instrument);
                batch.composite_legs.insert(
                    batch.composite_legs.end(), v.legs.begin(), v.legs.end());
            } else {
                append(batch, v);
            }
        },
        instrument);
}

trade_instrument rebuild_instrument(const instrument_batch& batch, boost::uuids::uuid trade_id) {
    if (const auto* v = find(batch.fra_instruments, trade_id))
        return as_swap(batch, *v, trade_id);
    if (const auto* v = find(batch.vanilla_swap_instruments, trade_id))
        return as_swap(batch, *v, trade_id);
    if (const auto* v = find(batch.cap_floor_instruments, trade_id))
        return as_swap(batch, *v, trade_id);
    if (const auto* v = find(batch.swaption_instruments, trade_id))
        return as_swap(batch, *v, trade_id);
    if (const auto* v = find(batch.balance_guaranteed_swap_instruments, trade_id))
        return as_swap(batch, *v, trade_id);
    if (const auto* v = find(batch.callable_swap_instruments, trade_id))
        return as_swap(batch, *v, trade_id);
    if (const auto* v = find(batch.knock_out_swap_instruments, trade_id))
        return as_swap(batch, *v, trade_id);
    if (const auto* v = find(batch.inflation_swap_instruments, trade_id))
        return as_swap(batch, *v, trade_id);

    if (const auto* v = find(batch.fx_forward_instruments, trade_id))
        return fx_instrument_variant{*v};
    if (const auto* v = find(batch.fx_vanilla_option_instruments, trade_id))
        return fx_instrument_variant{*v};
    if (const auto* v = find(batch.fx_barrier_option_instruments, trade_id))
        return fx_instrument_variant{*v};
    if (const auto* v = find(batch.fx_digital_option_instruments, trade_id))
        return fx_instrument_variant{*v};
    if (const auto* v = find(batch.fx_asian_forward_instruments, trade_id))
        return fx_instrument_variant{*v};
    if (const auto* v = find(batch.fx_accumulator_instruments, trade_id))
        return fx_instrument_variant{*v};
    if (const auto* v = find(batch.fx_variance_swap_instruments, trade_id))
        return fx_instrument_variant{*v};

    if (const auto* v = find(batch.equity_option_instruments, trade_id))
        return as_equity(batch, *v, trade_id);
    if (const auto* v = find(batch.equity_digital_option_instruments, trade_id))
        return as_equity(batch, *v, trade_id);
    if (const auto* v = find(batch.equity_barrier_option_instruments, trade_id))
        return as_equity(batch, *v, trade_id);
    if (const auto* v = find(batch.equity_asian_option_instruments, trade_id))
        return as_equity(batch, *v, trade_id);
    if (const auto* v = find(batch.equity_forward_instruments, trade_id))
        return as_equity(batch, *v, trade_id);
    if (const auto* v = find(batch.equity_variance_swap_instruments, trade_id))
        return as_equity(batch, *v, trade_id);
    if (const auto* v = find(batch.equity_swap_instruments, trade_id))
        return as_equity(batch, *v, trade_id);
    if (const auto* v = find(batch.equity_accumulator_instruments, trade_id))
        return as_equity(batch, *v, trade_id);
    if (const auto* v = find(batch.equity_position_instruments, trade_id))
        return as_equity(batch, *v, trade_id);

    if (const auto* v = find(batch.bond_instruments, trade_id))
        return *v;
    if (const auto* v = find(batch.credit_instruments, trade_id))
        return *v;
    if (const auto* v = find(batch.commodity_instruments, trade_id))
        return commodity_instrument_data{*v,
                                         find_all(batch.commodity_basket_constituents, trade_id)};
    if (const auto* v = find(batch.composite_instruments, trade_id))
        return composite_instrument_data{*v, find_all(batch.composite_legs, trade_id)};
    if (const auto* v = find(batch.scripted_instruments, trade_id))
        return *v;

    return std::monostate{};
}

}
