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
#include "refusals.hpp"
#include <algorithm>
#include <utility>

namespace ores::trading::builders {

namespace {

using utility::decimal::decimal;
using ymd = std::chrono::year_month_day;

std::optional<build_error> exchange_refusal(const fx_exchange& exchange) {
    if (exchange.bought_currency.get().empty() || exchange.sold_currency.get().empty())
        return build_error{"exchange", "must name both currencies"};
    if (exchange.bought_currency.get() == exchange.sold_currency.get())
        return build_error{"exchange", "must name two different currencies"};
    if (exchange.bought_amount.get() <= decimal{} || exchange.sold_amount.get() <= decimal{})
        return build_error{"exchange", "must state positive amounts"};
    return std::nullopt;
}

template <typename Row>
Row exchange_row(std::string_view trade_type, const fx_exchange& exchange) {
    Row row;
    row.identity.trade_type_code = std::string(trade_type);
    row.bought_currency = exchange.bought_currency.get();
    row.bought_amount = exchange.bought_amount.get();
    row.sold_currency = exchange.sold_currency.get();
    row.sold_amount = exchange.sold_amount.get();
    return row;
}

domain::instrument_option option_row(option_position position,
                                     std::optional<std::string> style,
                                     std::optional<std::string> payoff_type = {},
                                     std::optional<std::string> pay_off_at_expiry = {}) {
    domain::instrument_option row;
    row.long_short = long_short_of(position);
    row.style = std::move(style);
    if (row.style)
        row.settlement = "Cash";
    row.payoff_type = std::move(payoff_type);
    row.pay_off_at_expiry = std::move(pay_off_at_expiry);
    return row;
}

void add_barriers(domain::fx_instrument_data& out, const std::vector<fx_barrier>& barriers) {
    int number = 0;
    for (const auto& barrier : barriers) {
        domain::instrument_barrier row;
        row.barrier_role = "barrier";
        row.sequence_number = ++number;
        row.barrier_type = barrier.type;
        out.barriers.push_back(std::move(row));
        int level_number = 0;
        for (const auto& value : barrier.levels) {
            domain::instrument_barrier_level level;
            level.barrier_role = "barrier";
            level.barrier_sequence_number = number;
            level.sequence_number = ++level_number;
            level.level = value;
            out.barrier_levels.push_back(std::move(level));
        }
    }
}

/// The pair an FX product observes, as the Underlying block states it.
void add_underlying(domain::fx_instrument_data& out,
                    const marketdata::datum::market_reference& fx_index) {
    domain::instrument_underlying row;
    row.underlying_role = "underlying";
    row.sequence_number = 1;
    row.underlying_type = "FX";
    row.name = fx_index.uri();
    out.instrument_underlyings.push_back(std::move(row));
}

/*
 * A schedule kept as the product's own, from @p start to @p end every @p tenor.
 * Every generated FX schedule observes on the TARGET calendar, rolling forward,
 * as ORE's samples do.
 */
void add_schedule(
    domain::fx_instrument_data& out, std::string role, std::string tenor, ymd start, ymd end) {
    domain::instrument_schedule row;
    row.owner_role = "option";
    row.owner_number = 1;
    row.schedule_role = std::move(role);
    row.sequence_number = 1;
    row.schedule_kind = "rules";
    row.start_date = start;
    row.end_date = end;
    row.tenor = std::move(tenor);
    row.calendar = "TARGET";
    row.convention = "F";
    row.rule = "Forward";
    out.schedules.push_back(std::move(row));
}

/// Fixings below the strike are taken twice, those above it once.
std::vector<domain::range_bound> strike_leverage_bounds(decimal strike) {
    domain::range_bound below;
    below.sequence_number = 1;
    below.range_to = strike;
    below.leverage = 2.0;
    domain::range_bound above;
    above.sequence_number = 2;
    above.range_from = strike;
    above.leverage = 1.0;
    return {below, above};
}

bool is_single_type(std::string_view type) {
    return type == "UpAndIn" || type == "UpAndOut" || type == "DownAndIn" || type == "DownAndOut";
}

bool is_double_type(std::string_view type) {
    return type == "KnockIn" || type == "KnockOut";
}

bool knocks_in(std::string_view type) {
    return type.ends_with("In");
}

/// One barrier of the type set @p fits and of @p levels levels.
std::optional<build_error> barrier_refusal(const std::vector<fx_barrier>& barriers,
                                           bool (*fits)(std::string_view),
                                           std::size_t levels,
                                           std::string_view shape) {
    if (barriers.size() != 1 || !fits(barriers.front().type) ||
        barriers.front().levels.size() != levels)
        return build_error{"barriers", "must be " + std::string(shape)};
    for (const auto& level : barriers.front().levels)
        if (level <= decimal{})
            return build_error{"barriers", "must state positive levels"};
    return std::nullopt;
}

/// The barrier columns of a row: the type of the first barrier and its levels.
template <typename Row>
void set_barrier_columns(Row& row, const std::vector<fx_barrier>& barriers) {
    if (barriers.empty())
        return;
    row.barrier_type = barriers.front().type;
    std::vector<decimal> levels;
    for (const auto& barrier : barriers)
        levels.insert(levels.end(), barrier.levels.begin(), barrier.levels.end());
    std::ranges::sort(levels);
    if (levels.size() == 1) {
        if (barriers.front().type.starts_with("Up"))
            row.upper_barrier = levels.front();
        else
            row.lower_barrier = levels.front();
        return;
    }
    row.lower_barrier = levels.front();
    row.upper_barrier = levels.back();
}

}

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
    if (auto refusal = exchange_refusal(terms_.near))
        return std::unexpected(std::move(*refusal));
    if (terms_.far_date.get() <= terms_.near_date.get())
        return std::unexpected(build_error{"far_date", "must be after the near date"});
    if (terms_.far_bought_amount.get() <= decimal{} || terms_.far_sold_amount.get() <= decimal{})
        return std::unexpected(build_error{"far_amounts", "must be positive"});
    auto row = exchange_row<domain::fx_forward_instrument>(trade_type, terms_.near);
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

template <fx_barrier_product Product>
fx_result fx_barrier_option_builder<Product>::build() const {
    const auto& option = terms_.option;
    if (auto refusal = exchange_refusal(option.exchange))
        return std::unexpected(std::move(*refusal));
    const auto& barriers = terms_.barriers.get();
    if constexpr (Product == fx_barrier_product::double_barrier) {
        if (auto refusal = barrier_refusal(
                barriers, is_double_type, 2, "one KnockIn or KnockOut barrier of two levels"))
            return std::unexpected(std::move(*refusal));
    } else if constexpr (Product == fx_barrier_product::kiko) {
        const bool shaped = barriers.size() == 2 && is_single_type(barriers[0].type) &&
                            is_single_type(barriers[1].type) &&
                            knocks_in(barriers[0].type) != knocks_in(barriers[1].type) &&
                            barriers[0].levels.size() == 1 && barriers[1].levels.size() == 1;
        if (!shaped)
            return std::unexpected(build_error{
                "barriers", "must be a knock-in and a knock-out barrier of one level each"});
    } else {
        if (auto refusal =
                barrier_refusal(barriers, is_single_type, 1, "one UpAnd or DownAnd barrier"))
            return std::unexpected(std::move(*refusal));
    }

    auto row = exchange_row<domain::fx_barrier_option_instrument>(trade_type, option.exchange);
    row.option_type = option_type_of(option.kind.get());
    row.expiry_date = option.expiry.get();
    row.settlement = "Cash";
    set_barrier_columns(row, barriers);
    row.barrier_fixing_index = terms_.fx_index.get().uri();
    domain::fx_instrument_data out;
    out.options.push_back(option_row(option.position.get(), "European"));
    out.barrier_options.push_back(std::move(row));
    add_barriers(out, barriers);
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

template <fx_digital_product Product>
fx_result fx_digital_option_builder<Product>::build() const {
    if (terms_.foreign_currency.get() == terms_.domestic_currency.get())
        return std::unexpected(build_error{"currencies", "must be two different currencies"});
    if (terms_.payoff_amount.get() <= decimal{})
        return std::unexpected(build_error{"payoff_amount", "must be positive"});
    constexpr bool struck =
        Product == fx_digital_product::digital || Product == fx_digital_product::digital_barrier;
    if (struck && !(terms_.strike && *terms_.strike > decimal{}))
        return std::unexpected(build_error{"strike", "must be positive"});
    if (!struck && terms_.strike)
        return std::unexpected(build_error{"strike", "a touch pays on its barrier, not a strike"});
    if constexpr (Product == fx_digital_product::digital) {
        if (!terms_.barriers.empty())
            return std::unexpected(build_error{"barriers", "must be none"});
    } else if constexpr (Product == fx_digital_product::double_touch) {
        if (auto refusal = barrier_refusal(terms_.barriers,
                                           is_double_type,
                                           2,
                                           "one KnockIn or KnockOut barrier of two levels"))
            return std::unexpected(std::move(*refusal));
    } else {
        if (auto refusal =
                barrier_refusal(terms_.barriers, is_single_type, 1, "one UpAnd or DownAnd barrier"))
            return std::unexpected(std::move(*refusal));
    }

    domain::fx_digital_option_instrument row;
    row.identity.trade_type_code = std::string(trade_type);
    row.foreign_currency = terms_.foreign_currency.get();
    row.domestic_currency = terms_.domestic_currency.get();
    // Paid in the domestic currency, as ORE's samples pay.
    row.payoff_currency = terms_.domestic_currency.get();
    row.payoff_amount = terms_.payoff_amount.get();
    row.option_type = option_type_of(terms_.kind.get());
    row.expiry_date = terms_.expiry.get();
    row.long_short = long_short_of(terms_.position.get());
    row.strike = terms_.strike;
    set_barrier_columns(row, terms_.barriers);
    if constexpr (Product == fx_digital_product::digital_barrier)
        row.barrier_fixing_index = terms_.fx_index.get().uri();

    domain::fx_instrument_data out;
    out.digital_options.push_back(std::move(row));
    // A touch pays when touched, so ORE states no exercise style for it.
    constexpr bool touch =
        Product == fx_digital_product::touch || Product == fx_digital_product::double_touch;
    out.options.push_back(touch ? option_row(terms_.position.get(), std::nullopt, {}, "true") :
                                  option_row(terms_.position.get(), "European"));
    add_barriers(out, terms_.barriers);
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

template class ORES_TRADING_API_EXPORT fx_barrier_option_builder<fx_barrier_product::barrier>;
template class ORES_TRADING_API_EXPORT
    fx_barrier_option_builder<fx_barrier_product::double_barrier>;
template class ORES_TRADING_API_EXPORT
    fx_barrier_option_builder<fx_barrier_product::european_barrier>;
template class ORES_TRADING_API_EXPORT fx_barrier_option_builder<fx_barrier_product::kiko>;
template class ORES_TRADING_API_EXPORT fx_digital_option_builder<fx_digital_product::digital>;
template class ORES_TRADING_API_EXPORT fx_digital_option_builder<fx_digital_product::touch>;
template class ORES_TRADING_API_EXPORT fx_digital_option_builder<fx_digital_product::double_touch>;
template class ORES_TRADING_API_EXPORT
    fx_digital_option_builder<fx_digital_product::digital_barrier>;

}
