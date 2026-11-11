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
#include "ores.trading.api/builders/fx_builders.hpp"
#include "ores.trading.api/builders/fx_builder_rows.hpp"
#include "refusals.hpp"
#include <algorithm>
#include <utility>

namespace ores::trading::builders {

using namespace fx_detail;
using utility::decimal::decimal;
using ymd = std::chrono::year_month_day;


fx_forward_builder::fx_forward_builder(fx_forward_terms terms)
    : terms_(std::move(terms)) {}

fx_result fx_forward_builder::build() const {
    if (auto refusal = exchange_refusal(terms_.exchange))
        return std::unexpected(std::move(*refusal));
    auto row = exchange_row<domain::fx_forward_instrument>(trade_type, terms_.exchange);
    row.value_date = terms_.value_date.get();
    row.settlement = settlement_of(terms_.settles.get());
    domain::fx_instrument_data out;
    out.forwards.push_back(std::move(row));
    return out;
}

fx_swap_builder::fx_swap_builder(fx_swap_terms terms)
    : terms_(std::move(terms)) {}

fx_result fx_swap_builder::build() const {
    if (auto refusal = exchange_refusal(terms_.near_leg))
        return std::unexpected(std::move(*refusal));
    if (terms_.far_date.get() <= terms_.near_date.get())
        return std::unexpected(build_error{"far_date", "must be after the near date"});
    if (terms_.far_bought_amount.get() <= decimal{} || terms_.far_sold_amount.get() <= decimal{})
        return std::unexpected(build_error{"far_amounts", "must be positive"});
    auto row = exchange_row<domain::fx_forward_instrument>(trade_type, terms_.near_leg);
    row.value_date = terms_.near_date.get();
    row.far_date = terms_.far_date.get();
    row.far_bought_amount = terms_.far_bought_amount.get();
    row.far_sold_amount = terms_.far_sold_amount.get();
    row.settlement = "Physical";
    domain::fx_instrument_data out;
    out.forwards.push_back(std::move(row));
    return out;
}

fx_option_builder::fx_option_builder(fx_option_terms terms)
    : terms_(std::move(terms)) {}

fx_result fx_option_builder::build() const {
    if (auto refusal = exchange_refusal(terms_.exchange))
        return std::unexpected(std::move(*refusal));
    auto row = exchange_row<domain::fx_vanilla_option_instrument>(trade_type, terms_.exchange);
    row.option_type = option_type_of(terms_.kind.get());
    row.expiry_date = terms_.expiry.get();
    row.exercise_style = "European";
    row.settlement = "Cash";
    domain::fx_instrument_data out;
    out.vanilla_options.push_back(std::move(row));
    out.options.push_back(option_row(terms_.position.get(), "European"));
    return out;
}

fx_generic_barrier_option_builder::fx_generic_barrier_option_builder(
    fx_generic_barrier_option_terms terms)
    : terms_(std::move(terms)) {}

fx_result fx_generic_barrier_option_builder::build() const {
    const auto& option = terms_.option;
    if (auto refusal = exchange_refusal(option.exchange))
        return std::unexpected(std::move(*refusal));
    const std::vector<fx_barrier> barriers{terms_.barrier.get()};
    if (auto refusal = barrier_refusal(barriers, is_single_type, 1, "one UpAnd or DownAnd barrier"))
        return std::unexpected(std::move(*refusal));
    if (terms_.monitoring_start.get() >= option.expiry.get())
        return std::unexpected(build_error{"monitoring_start", "must be before the expiry"});

    // The vanilla payoff on the bought amount, struck at the exchange's own rate.
    auto row = exchange_row<domain::fx_barrier_option_instrument>(trade_type, option.exchange);
    row.option_type = option_type_of(option.kind.get());
    row.expiry_date = option.expiry.get();
    row.settlement = "Cash";
    row.quantity = option.exchange.bought_amount.get().to_double();
    const auto strike = decimal::from_double(option.exchange.sold_amount.get().to_double() /
                                             option.exchange.bought_amount.get().to_double());
    if (!strike)
        return std::unexpected(build_error{"strike", "the exchange states no rate a strike holds"});
    row.strike = *strike;
    set_barrier_columns(row, barriers);
    domain::fx_instrument_data out;
    out.barrier_options.push_back(std::move(row));
    out.options.push_back(option_row(option.position.get(), std::nullopt, "Vanilla"));
    add_underlying(out, terms_.fx_index.get());
    add_barriers(out, barriers);

    add_schedule(out, "barrier_schedule", "1D", terms_.monitoring_start.get(), option.expiry.get());
    return out;
}

fx_average_forward_builder::fx_average_forward_builder(fx_average_forward_terms terms)
    : terms_(std::move(terms)) {}

fx_result fx_average_forward_builder::build() const {
    if (auto refusal = refusals::period(terms_.start.get(), terms_.payment_date.get()))
        return std::unexpected(std::move(*refusal));
    if (terms_.reference_notional.get() <= decimal{} ||
        terms_.settlement_notional.get() <= decimal{})
        return std::unexpected(build_error{"notionals", "must be positive"});
    domain::fx_asian_forward_instrument row;
    row.identity.trade_type_code = std::string(trade_type);
    row.fx_index = terms_.fx_index.get().uri();
    row.reference_currency = terms_.reference_currency.get();
    row.reference_notional = terms_.reference_notional.get();
    row.settlement_currency = terms_.settlement_currency.get();
    row.settlement_notional = terms_.settlement_notional.get();
    row.payment_date = terms_.payment_date.get();
    row.currency = terms_.settlement_currency.get();
    row.long_short = "Long";
    row.fixed_payer = false;
    domain::fx_instrument_data out;
    out.asian_forwards.push_back(std::move(row));
    add_schedule(out, "observation_schedule", "1M", terms_.start.get(), terms_.payment_date.get());
    return out;
}

fx_tarf_builder::fx_tarf_builder(fx_tarf_terms terms)
    : terms_(std::move(terms)) {}

fx_result fx_tarf_builder::build() const {
    if (auto refusal = refusals::period(terms_.start.get(), terms_.end.get()))
        return std::unexpected(std::move(*refusal));
    if (terms_.strike.get() <= decimal{} || terms_.fixing_amount.get() <= decimal{} ||
        terms_.target_amount.get() <= decimal{})
        return std::unexpected(
            build_error{"amounts", "the strike, fixing amount and target must be positive"});
    domain::fx_asian_forward_instrument row;
    row.identity.trade_type_code = std::string(trade_type);
    row.fx_index = terms_.fx_index.get().uri();
    row.currency = terms_.currency.get();
    row.fixing_amount = terms_.fixing_amount.get();
    row.strike = terms_.strike.get();
    row.target_amount = terms_.target_amount.get();
    row.long_short = long_short_of(terms_.position.get());
    domain::fx_instrument_data out;
    out.asian_forwards.push_back(std::move(row));
    add_underlying(out, terms_.fx_index.get());
    out.options.push_back(
        option_row(terms_.position.get(), std::nullopt, std::string("TargetExact")));
    add_schedule(out, "product_schedule", "1M", terms_.start.get(), terms_.end.get());
    out.range_bounds = strike_leverage_bounds(terms_.strike.get());
    add_barriers(out, {fx_barrier{"CumulatedProfitCap", {terms_.target_amount.get()}}});
    return out;
}

fx_variance_swap_builder::fx_variance_swap_builder(fx_variance_swap_terms terms)
    : terms_(std::move(terms)) {}

fx_result fx_variance_swap_builder::build() const {
    if (auto refusal = refusals::period(terms_.start.get(), terms_.end.get()))
        return std::unexpected(std::move(*refusal));
    if (!(terms_.strike.get() > 0.0))
        return std::unexpected(build_error{"strike", "must be positive"});
    if (terms_.notional.get() <= decimal{})
        return std::unexpected(build_error{"notional", "must be positive"});
    domain::fx_variance_swap_instrument row;
    row.identity.trade_type_code = std::string(trade_type);
    row.start_date = terms_.start.get();
    row.end_date = terms_.end.get();
    row.currency = terms_.currency.get();
    row.underlying_code = terms_.fx_index.get().uri();
    row.long_short = long_short_of(terms_.position.get());
    row.strike = terms_.strike.get();
    row.notional = terms_.notional.get();
    row.moment_type = "Variance";
    domain::fx_instrument_data out;
    out.variance_swaps.push_back(std::move(row));
    add_underlying(out, terms_.fx_index.get());
    return out;
}

fx_accumulator_builder::fx_accumulator_builder(fx_accumulator_terms terms)
    : terms_(std::move(terms)) {}

fx_result fx_accumulator_builder::build() const {
    if (auto refusal = refusals::period(terms_.start.get(), terms_.end.get()))
        return std::unexpected(std::move(*refusal));
    if (terms_.strike.get() <= decimal{} || terms_.fixing_amount.get() <= decimal{})
        return std::unexpected(
            build_error{"amounts", "the strike and fixing amount must be positive"});
    if (terms_.knock_out.get() <= terms_.strike.get())
        return std::unexpected(build_error{"knock_out", "must be above the strike"});
    domain::fx_accumulator_instrument row;
    row.identity.trade_type_code = std::string(trade_type);
    row.currency = terms_.currency.get();
    row.fixing_amount = terms_.fixing_amount.get();
    row.strike = terms_.strike.get();
    row.underlying_code = terms_.fx_index.get().uri();
    row.long_short = long_short_of(terms_.position.get());
    row.start_date = terms_.start.get();
    row.knock_out_barrier = terms_.knock_out.get();
    domain::fx_instrument_data out;
    out.accumulators.push_back(std::move(row));
    add_underlying(out, terms_.fx_index.get());
    out.options.push_back(
        option_row(terms_.position.get(), std::nullopt, std::string("Accumulator")));
    add_schedule(out, "observation_schedule", "1M", terms_.start.get(), terms_.end.get());
    out.range_bounds = strike_leverage_bounds(terms_.strike.get());
    add_barriers(out, {fx_barrier{"UpAndOut", {terms_.knock_out.get()}}});
    return out;
}


}
