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
#include <cstddef>
#include <tuple>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

namespace ores::trading::domain {
namespace {

/**
 * A rates header, the product's fact row and the children the family owns,
 * which are the parts of the family carrier the batch stores separately.
 */
template <typename Leaf>
swap_instrument_data as_swap(const instrument_batch& batch,
                             const rate_instrument& header,
                             const Leaf& leaf,
                             boost::uuids::uuid trade_id) {
    return swap_instrument_data{
        header,
        leaf,
        find_all(batch.swap_legs, trade_id),
        find_all(batch.swap_leg_amounts, trade_id),
        find_all(batch.swap_leg_rates, trade_id),
        find_all(batch.callable_swap_call_dates, trade_id),
        find_all(batch.instrument_strikes, trade_id),
        find_all(batch.instrument_schedules, trade_id),
        find_all(batch.instrument_schedule_dates, trade_id),
        find_all(batch.instrument_options, trade_id),
        find_all(batch.instrument_option_premiums, trade_id),
        find_all(batch.instrument_option_exercise_fees, trade_id),
        find_all(batch.instrument_option_payment_dates, trade_id),
        find_all(batch.instrument_option_exercise_prices, trade_id),
        find_all(batch.flexi_swap_lower_notionals, trade_id),
        find_all(batch.balance_guaranteed_swap_tranches, trade_id),
        find_all(batch.balance_guaranteed_swap_tranche_notionals, trade_id)};
}

/**
 * The batch's copy of each credit fact array, in the order credit_fact_arrays
 * declares. The fold below walks both tuples together, so appending a credit
 * carrier is one call rather than six inserts that a new product can be left
 * out of.
 */
inline constexpr auto credit_fact_arrays_batch =
    std::tuple{&instrument_batch::credit_default_swap_instruments,
               &instrument_batch::index_credit_default_swap_instruments,
               &instrument_batch::index_credit_default_swap_option_instruments,
               &instrument_batch::synthetic_cdo_instruments,
               &instrument_batch::credit_linked_swap_instruments,
               &instrument_batch::risk_participation_agreement_instruments};

static_assert(std::tuple_size_v<decltype(credit_fact_arrays_batch)> ==
              std::tuple_size_v<decltype(credit_fact_arrays)>);

template <typename T>
void append_rows(std::vector<T>& to, const std::vector<T>& from) {
    to.insert(to.end(), from.begin(), from.end());
}

template <std::size_t... I>
void append_credit_fact_arrays(instrument_batch& batch,
                               const credit_instrument_data& carrier,
                               std::index_sequence<I...>) {
    (append_rows(batch.*std::get<I>(credit_fact_arrays_batch),
                 carrier.*std::get<I>(credit_fact_arrays)),
     ...);
}

/**
 * A credit header, the product's fact row and the children the family owns,
 * which are the parts of the family carrier the batch stores separately.
 */
template <typename Leaf>
credit_instrument_data as_credit(const instrument_batch& batch,
                                 const credit_instrument& header,
                                 const Leaf& leaf,
                                 boost::uuids::uuid trade_id) {
    static_assert(is_credit_fact_type<Leaf>,
                  "not a credit fact type: add it to credit_fact_types and credit_fact_arrays");
    credit_instrument_data data{
        .legs = find_all(batch.swap_legs, trade_id),
        .leg_amounts = find_all(batch.swap_leg_amounts, trade_id),
        .leg_rates = find_all(batch.swap_leg_rates, trade_id),
        .constituents = find_all(batch.credit_basket_constituents, trade_id),
        .strikes = find_all(batch.instrument_strikes, trade_id),
        .schedules = find_all(batch.instrument_schedules, trade_id),
        .schedule_dates = find_all(batch.instrument_schedule_dates, trade_id),
        .options = find_all(batch.instrument_options, trade_id),
        .option_premiums = find_all(batch.instrument_option_premiums, trade_id),
        .option_exercise_fees = find_all(batch.instrument_option_exercise_fees, trade_id),
        .option_payment_dates = find_all(batch.instrument_option_payment_dates, trade_id),
        .option_exercise_prices = find_all(batch.instrument_option_exercise_prices, trade_id)};
    data.instrument = header;
    for_each_credit_fact_array(data, [&leaf](auto& rows) {
        using Row = typename std::decay_t<decltype(rows)>::value_type;
        if constexpr (std::is_same_v<Row, Leaf>)
            rows.push_back(leaf);
    });
    return data;
}

/**
 * An equity leaf and the position entries it owns.
 */
template <typename Leaf>
equity_instrument_data
as_equity(const instrument_batch& batch, const Leaf& leaf, boost::uuids::uuid trade_id) {
    return equity_instrument_data{equity_instrument_variant{leaf},
                                  find_all(batch.equity_position_option_underlyings, trade_id),
                                  find_all(batch.equity_basket_constituents, trade_id),
                                  find_all(batch.equity_swap_periods, trade_id)};
}

/**
 * A leaf inside a variant, appended as its own type. The variant never
 * reaches the batch, only its alternatives do.
 */
template <typename Variant>
void append_variant(instrument_batch& batch, const Variant& variant) {
    std::visit([&](const auto& leaf) { append(batch, leaf); }, variant);
}

/**
 * The family's arrays paired with the batch columns they land in, in the
 * family's product order. Both directions of the join read this one list, so a
 * product the family gains is one entry here and no walk changes.
 */
inline constexpr auto fx_batch_arrays = std::tuple{
    std::pair{&fx_instrument_data::forwards, &instrument_batch::fx_forward_instruments},
    std::pair{&fx_instrument_data::vanilla_options,
              &instrument_batch::fx_vanilla_option_instruments},
    std::pair{&fx_instrument_data::barrier_options,
              &instrument_batch::fx_barrier_option_instruments},
    std::pair{&fx_instrument_data::digital_options,
              &instrument_batch::fx_digital_option_instruments},
    std::pair{&fx_instrument_data::asian_forwards, &instrument_batch::fx_asian_forward_instruments},
    std::pair{&fx_instrument_data::accumulators, &instrument_batch::fx_accumulator_instruments},
    std::pair{&fx_instrument_data::variance_swaps,
              &instrument_batch::fx_variance_swap_instruments}};

/**
 * The family's child arrays paired with the batch columns they land in, in the
 * family's product order. A child states no product, so this list is separate
 * from the product list the join uses to pick one.
 *
 * make_tuple, because tuple's pair deduction guide flattens a lone pair into
 * its two members, and the walk reads one pair per child.
 */
inline constexpr auto fx_batch_child_arrays =
    std::make_tuple(std::pair{&fx_instrument_data::range_bounds, &instrument_batch::range_bounds});

/**
 * A family carrier whose parts are typed arrays rather than alternatives. Each
 * array, product or child, is a part in its own right, so every one of them
 * reaches the batch; a carrier that states none leaves the batch alone.
 */
void append_fx_arrays(instrument_batch& batch, const fx_instrument_data& carrier) {
    const auto append_one = [&](const auto& columns) {
        const auto& rows = carrier.*(columns.first);
        auto& column = batch.*(columns.second);
        column.insert(column.end(), rows.begin(), rows.end());
    };
    std::apply([&](const auto... columns) { (append_one(columns), ...); },
               std::tuple_cat(fx_batch_arrays, fx_batch_child_arrays));
}

}

void append_instrument(instrument_batch& batch, const trade_instrument& instrument) {
    std::visit(
        [&]<typename T>(const T& v) {
            if constexpr (std::is_same_v<T, std::monostate>) {
                return;
            } else if constexpr (std::is_same_v<T, swap_instrument_data>) {
                append(batch, v.header);
                append_variant(batch, v.facts);
                batch.swap_legs.insert(batch.swap_legs.end(), v.legs.begin(), v.legs.end());
                batch.swap_leg_amounts.insert(
                    batch.swap_leg_amounts.end(), v.leg_amounts.begin(), v.leg_amounts.end());
                batch.swap_leg_rates.insert(
                    batch.swap_leg_rates.end(), v.leg_rates.begin(), v.leg_rates.end());
                batch.callable_swap_call_dates.insert(
                    batch.callable_swap_call_dates.end(), v.call_dates.begin(), v.call_dates.end());
                batch.instrument_strikes.insert(
                    batch.instrument_strikes.end(), v.strikes.begin(), v.strikes.end());
                batch.instrument_schedules.insert(
                    batch.instrument_schedules.end(), v.schedules.begin(), v.schedules.end());
                batch.instrument_schedule_dates.insert(batch.instrument_schedule_dates.end(),
                                                       v.schedule_dates.begin(),
                                                       v.schedule_dates.end());
                batch.instrument_options.insert(
                    batch.instrument_options.end(), v.options.begin(), v.options.end());
                batch.instrument_option_premiums.insert(batch.instrument_option_premiums.end(),
                                                        v.option_premiums.begin(),
                                                        v.option_premiums.end());
                batch.instrument_option_exercise_fees.insert(
                    batch.instrument_option_exercise_fees.end(),
                    v.option_exercise_fees.begin(),
                    v.option_exercise_fees.end());
                batch.instrument_option_payment_dates.insert(
                    batch.instrument_option_payment_dates.end(),
                    v.option_payment_dates.begin(),
                    v.option_payment_dates.end());
                batch.instrument_option_exercise_prices.insert(
                    batch.instrument_option_exercise_prices.end(),
                    v.option_exercise_prices.begin(),
                    v.option_exercise_prices.end());
                batch.flexi_swap_lower_notionals.insert(batch.flexi_swap_lower_notionals.end(),
                                                        v.lower_notionals.begin(),
                                                        v.lower_notionals.end());
                batch.balance_guaranteed_swap_tranches.insert(
                    batch.balance_guaranteed_swap_tranches.end(),
                    v.tranches.begin(),
                    v.tranches.end());
                batch.balance_guaranteed_swap_tranche_notionals.insert(
                    batch.balance_guaranteed_swap_tranche_notionals.end(),
                    v.tranche_notionals.begin(),
                    v.tranche_notionals.end());
            } else if constexpr (std::is_same_v<T, fx_instrument_data>) {
                append_fx_arrays(batch, v);
            } else if constexpr (std::is_same_v<T, equity_instrument_data>) {
                append_variant(batch, v.instrument);
                batch.equity_position_option_underlyings.insert(
                    batch.equity_position_option_underlyings.end(),
                    v.underlyings.begin(),
                    v.underlyings.end());
                batch.equity_basket_constituents.insert(
                    batch.equity_basket_constituents.end(),
                    v.basket_constituents.begin(),
                    v.basket_constituents.end());
                batch.equity_swap_periods.insert(batch.equity_swap_periods.end(),
                                                 v.periods.begin(),
                                                 v.periods.end());
            } else if constexpr (std::is_same_v<T, commodity_instrument_data>) {
                append(batch, v.instrument);
                batch.commodity_basket_constituents.insert(
                    batch.commodity_basket_constituents.end(),
                    v.constituents.begin(),
                    v.constituents.end());
            } else if constexpr (std::is_same_v<T, credit_instrument_data>) {
                append(batch, v.instrument);
                batch.swap_legs.insert(batch.swap_legs.end(), v.legs.begin(), v.legs.end());
                batch.swap_leg_amounts.insert(
                    batch.swap_leg_amounts.end(), v.leg_amounts.begin(), v.leg_amounts.end());
                batch.swap_leg_rates.insert(
                    batch.swap_leg_rates.end(), v.leg_rates.begin(), v.leg_rates.end());
                batch.credit_basket_constituents.insert(batch.credit_basket_constituents.end(),
                                                        v.constituents.begin(),
                                                        v.constituents.end());
                append_credit_fact_arrays(
                    batch,
                    v,
                    std::make_index_sequence<std::tuple_size_v<decltype(credit_fact_arrays)>>{});
                batch.instrument_strikes.insert(
                    batch.instrument_strikes.end(), v.strikes.begin(), v.strikes.end());
                batch.instrument_schedules.insert(
                    batch.instrument_schedules.end(), v.schedules.begin(), v.schedules.end());
                batch.instrument_schedule_dates.insert(batch.instrument_schedule_dates.end(),
                                                       v.schedule_dates.begin(),
                                                       v.schedule_dates.end());
                if (v.wrapped_index_swap)
                    append(batch, *v.wrapped_index_swap);
                batch.instrument_options.insert(
                    batch.instrument_options.end(), v.options.begin(), v.options.end());
                batch.instrument_option_premiums.insert(batch.instrument_option_premiums.end(),
                                                        v.option_premiums.begin(),
                                                        v.option_premiums.end());
                batch.instrument_option_exercise_fees.insert(
                    batch.instrument_option_exercise_fees.end(),
                    v.option_exercise_fees.begin(),
                    v.option_exercise_fees.end());
                batch.instrument_option_payment_dates.insert(
                    batch.instrument_option_payment_dates.end(),
                    v.option_payment_dates.begin(),
                    v.option_payment_dates.end());
                batch.instrument_option_exercise_prices.insert(
                    batch.instrument_option_exercise_prices.end(),
                    v.option_exercise_prices.begin(),
                    v.option_exercise_prices.end());
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
    // The family header states whether a rates instrument exists at all; a
    // fact row without one belongs to no instrument.
    if (const auto* header = find(batch.rate_instruments, trade_id)) {
        if (const auto* v = find(batch.fra_instruments, trade_id))
            return as_swap(batch, *header, *v, trade_id);
        if (const auto* v = find(batch.vanilla_swap_instruments, trade_id))
            return as_swap(batch, *header, *v, trade_id);
        if (const auto* v = find(batch.cap_floor_instruments, trade_id))
            return as_swap(batch, *header, *v, trade_id);
        if (const auto* v = find(batch.swaption_instruments, trade_id))
            return as_swap(batch, *header, *v, trade_id);
        if (const auto* v = find(batch.balance_guaranteed_swap_instruments, trade_id))
            return as_swap(batch, *header, *v, trade_id);
        if (const auto* v = find(batch.flexi_swap_instruments, trade_id))
            return as_swap(batch, *header, *v, trade_id);
        if (const auto* v = find(batch.callable_swap_instruments, trade_id))
            return as_swap(batch, *header, *v, trade_id);
        if (const auto* v = find(batch.knock_out_swap_instruments, trade_id))
            return as_swap(batch, *header, *v, trade_id);
        if (const auto* v = find(batch.inflation_swap_instruments, trade_id))
            return as_swap(batch, *header, *v, trade_id);
    }

    // The FX carrier states one array per product, so the join fills the one
    // array that holds the trade rather than an alternative of a variant. The
    // first row that matches decides, as the variant's alternative did.
    fx_instrument_data fx;
    bool matched = false;
    const auto take_one = [&](const auto& columns) {
        if (matched)
            return;
        const auto& column = batch.*(columns.second);
        if (const auto* row = find(column, trade_id)) {
            (fx.*(columns.first)).push_back(*row);
            matched = true;
        }
    };
    std::apply([&](const auto... columns) { (take_one(columns), ...); }, fx_batch_arrays);
    if (matched) {
        const auto take_children = [&](const auto& columns) {
            const auto& column = batch.*(columns.second);
            auto& rows = fx.*(columns.first);
            rows = find_all(column, trade_id);
        };
        std::apply([&](const auto... columns) { (take_children(columns), ...); },
                   fx_batch_child_arrays);
        return fx;
    }

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
    if (const auto* header = find(batch.credit_instruments, trade_id)) {
        if (const auto* v = find(batch.credit_default_swap_instruments, trade_id))
            return as_credit(batch, *header, *v, trade_id);
        if (const auto* v = find(batch.index_credit_default_swap_option_instruments, trade_id)) {
            auto data = as_credit(batch, *header, *v, trade_id);
            if (const auto* wrapped = find(batch.index_credit_default_swap_instruments, trade_id))
                data.wrapped_index_swap = *wrapped;
            return data;
        }
        if (const auto* v = find(batch.index_credit_default_swap_instruments, trade_id))
            return as_credit(batch, *header, *v, trade_id);
        if (const auto* v = find(batch.synthetic_cdo_instruments, trade_id))
            return as_credit(batch, *header, *v, trade_id);
        if (const auto* v = find(batch.credit_linked_swap_instruments, trade_id))
            return as_credit(batch, *header, *v, trade_id);
        if (const auto* v = find(batch.risk_participation_agreement_instruments, trade_id))
            return as_credit(batch, *header, *v, trade_id);
    }
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
