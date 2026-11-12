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
#include "ores.trading.api/builders/commodity_builders.hpp"
#include "leg_rows.hpp"
#include "refusals.hpp"
#include <cmath>
#include <optional>
#include <tuple>
#include <utility>
#include <variant>

namespace ores::trading::builders {

namespace {

using utility::decimal::decimal;
using ymd = std::chrono::year_month_day;

std::optional<build_error> commodity_amount_refusal(const std::string& currency,
                                                    const decimal& amount) {
    if (currency.empty())
        return build_error{"currency", "must not be empty"};
    if (amount <= decimal{})
        return build_error{"quantity", "must be positive"};
    return std::nullopt;
}

domain::commodity_instrument commodity_row_of(std::string_view trade_type,
                                              const marketdata::datum::market_reference& underlying,
                                              const std::string& currency,
                                              const decimal& quantity,
                                              const std::string& unit) {
    domain::commodity_instrument row{};
    row.identity.trade_type_code = std::string(trade_type);
    row.commodity_code = underlying.uri();
    row.currency = currency;
    row.quantity = quantity.to_double();
    row.unit = unit;
    return row;
}

domain::instrument_option commodity_option_row(option_position position,
                                               std::optional<std::string> payoff_type = {}) {
    domain::instrument_option row{};
    row.long_short = long_short_of(position);
    row.style = "European";
    row.settlement = "Cash";
    row.payoff_type = std::move(payoff_type);
    return row;
}

/*
 * A commodity leg pays five days after its period on the NYSE calendar,
 * following, which is what every commodity example ORE ships states. A leg on
 * another exchange's calendar needs these as terms.
 */
domain::swap_leg
commodity_paying_leg(int leg_number, bool payer, std::string type, std::string currency) {
    auto row = rows::swap_leg(leg_number, payer, std::move(type), std::move(currency));
    row.payment_lag = "5";
    row.payment_calendar = "US-NYSE";
    row.business_day_convention_code = "Following";
    return row;
}

domain::swap_leg commodity_floating_leg(int leg_number,
                                        bool payer,
                                        const std::string& currency,
                                        const marketdata::datum::market_reference& underlying) {
    auto row = commodity_paying_leg(leg_number, payer, "CommodityFloating", currency);
    row.floating_index_code = underlying.uri();
    row.commodity_price_type = "FutureSettlement";
    return row;
}

/// A leg's periods, rolled back from its end without adjustment.
domain::instrument_schedule
commodity_leg_schedule(int leg_number, ymd start, ymd end, std::string tenor) {
    domain::instrument_schedule row{};
    row.owner_role = "swap";
    row.owner_number = leg_number;
    row.schedule_role = "schedule";
    row.sequence_number = 1;
    row.schedule_kind = "rules";
    row.start_date = start;
    row.end_date = end;
    row.tenor = std::move(tenor);
    row.calendar = "NullCalendar";
    row.convention = "Unadjusted";
    row.rule = "Backward";
    return row;
}

std::optional<build_error> commodity_swap_refusal(const commodity_swap_terms& terms) {
    if (auto refusal = commodity_amount_refusal(terms.currency.get(), terms.quantity.get()))
        return refusal;
    if (terms.price.get() <= decimal{})
        return build_error{"price", "must be positive"};
    return refusals::period(terms.start.get(), terms.maturity.get());
}

/// The fixed and floating legs of a monthly commodity swap.
void add_commodity_swap_legs(domain::commodity_instrument_data& out,
                             const commodity_swap_terms& terms) {
    const bool fixed_pays = terms.fixed_side.get() == side::pay;
    const auto& currency = terms.currency.get();
    out.legs.push_back(commodity_paying_leg(1, fixed_pays, "CommodityFixed", currency));
    auto floating = commodity_floating_leg(2, !fixed_pays, currency, terms.underlying.get());
    floating.is_averaged = true;
    out.legs.push_back(std::move(floating));
    for (int leg = 1; leg <= 2; ++leg) {
        out.leg_amounts.push_back(rows::leg_amount(leg, terms.quantity.get()));
        out.schedules.push_back(
            commodity_leg_schedule(leg, terms.start.get(), terms.maturity.get(), "1M"));
    }
    out.leg_rates.push_back(rows::leg_rate(1, "price", terms.price.get()));
}

domain::commodity_instrument commodity_swap_row(std::string_view trade_type,
                                                const commodity_swap_terms& terms) {
    auto row = commodity_row_of(
        trade_type, terms.underlying.get(), terms.currency.get(), terms.quantity.get(), terms.unit);
    row.start_date = terms.start.get();
    row.maturity_date = terms.maturity.get();
    row.fixed_price = terms.price.get();
    return row;
}

std::optional<build_error> commodity_option_refusal(const commodity_option_terms& terms) {
    if (auto refusal = commodity_amount_refusal(terms.currency.get(), terms.quantity.get()))
        return refusal;
    if (terms.strike.get() <= decimal{})
        return build_error{"strike", "must be positive"};
    return std::nullopt;
}

/// The side of an option strip a strike sits on.
std::string strike_role_of(option_kind kind) {
    return kind == option_kind::call ? "call" : "put";
}

domain::commodity_instrument commodity_option_row_of(std::string_view trade_type,
                                                     const commodity_option_terms& terms) {
    auto row = commodity_row_of(
        trade_type, terms.underlying.get(), terms.currency.get(), terms.quantity.get(), terms.unit);
    row.option_type = option_type_of(terms.kind.get());
    row.strike_price = terms.strike.get();
    row.exercise_type = "European";
    row.maturity_date = terms.expiry.get();
    return row;
}

std::optional<build_error> commodity_digital_refusal(const commodity_digital_option_terms& terms) {
    if (terms.currency.get().empty())
        return build_error{"currency", "must not be empty"};
    if (terms.payoff.get() <= decimal{})
        return build_error{"payoff", "must be positive"};
    if (terms.strike.get() <= decimal{})
        return build_error{"strike", "must be positive"};
    return std::nullopt;
}

/// A digital option's row keeps the cash it pays as its quantity, which counts no unit.
domain::commodity_instrument commodity_digital_row_of(std::string_view trade_type,
                                                      const commodity_digital_option_terms& terms) {
    auto row = commodity_row_of(
        trade_type, terms.underlying.get(), terms.currency.get(), terms.payoff.get(), {});
    row.option_type = option_type_of(terms.kind.get());
    row.strike_price = terms.strike.get();
    row.exercise_type = "European";
    row.maturity_date = terms.expiry.get();
    return row;
}

/*
 * A delivery day counts 86400 seconds. A day that gains an hour when the
 * clocks change counts more, and ORE states that hour by a flag these terms
 * do not carry.
 */
constexpr int commodity_seconds_in_a_day = 86400;

/// The commodity a product names in its Underlying block.
void commodity_add_underlying(domain::commodity_instrument_data& out,
                              const marketdata::datum::market_reference& underlying,
                              std::optional<double> weight = std::nullopt) {
    domain::instrument_underlying row{};
    row.underlying_role = "underlying";
    row.sequence_number = 1;
    row.underlying_type = "Commodity";
    row.name = underlying.uri();
    row.weight = weight;
    out.instrument_underlyings.push_back(std::move(row));
}

/*
 * A scripted product's monthly dates from start to end, kept as its own. They
 * roll on the NYSE calendar, as the commodity legs pay on it. A commodity that
 * trades on another exchange's calendar needs the calendar as a term.
 */
domain::instrument_schedule commodity_scripted_schedule(std::string role, ymd start, ymd end) {
    domain::instrument_schedule row{};
    row.owner_role = "option";
    row.owner_number = 1;
    row.schedule_role = std::move(role);
    row.sequence_number = 1;
    row.schedule_kind = "rules";
    row.start_date = start;
    row.end_date = end;
    row.tenor = "1M";
    row.calendar = "US-NYSE";
    row.convention = "F";
    row.rule = "Forward";
    return row;
}

/// Fixings below the strike are taken twice, those above it once.
std::vector<domain::range_bound> commodity_strike_bounds(decimal strike) {
    domain::range_bound below{};
    below.sequence_number = 1;
    below.range_to = strike;
    below.leverage = 2.0;
    domain::range_bound above{};
    above.sequence_number = 2;
    above.range_from = strike;
    above.leverage = 1.0;
    return {below, above};
}

/// One barrier of one level, kept as the product's own.
void commodity_add_barrier(domain::commodity_instrument_data& out,
                           std::string type,
                           decimal level) {
    domain::instrument_barrier barrier{};
    barrier.barrier_role = "barrier";
    barrier.sequence_number = 1;
    barrier.barrier_type = std::move(type);
    out.barriers.push_back(std::move(barrier));
    domain::instrument_barrier_level row{};
    row.barrier_role = "barrier";
    row.barrier_sequence_number = 1;
    row.sequence_number = 1;
    row.level = level;
    out.barrier_levels.push_back(std::move(row));
}

std::string commodity_barrier_type_of(barrier_kind kind) {
    switch (kind) {
        case barrier_kind::up_and_in:
            return "UpAndIn";
        case barrier_kind::up_and_out:
            return "UpAndOut";
        case barrier_kind::down_and_in:
            return "DownAndIn";
        case barrier_kind::down_and_out:
            return "DownAndOut";
    }
    return {};
}

std::optional<build_error> commodity_fixing_refusal(
    const std::string& currency, decimal fixing_amount, decimal strike, ymd start, ymd expiry) {
    if (currency.empty())
        return build_error{"currency", "must not be empty"};
    if (strike <= decimal{} || fixing_amount <= decimal{})
        return build_error{"amounts", "the strike and fixing amount must be positive"};
    return refusals::period(start, expiry);
}

std::optional<build_error> commodity_basket_refusal(
    const std::vector<commodity_basket_member>& basket) {
    if (basket.size() < 2)
        return build_error{"basket", "must hold at least two commodities"};
    for (std::size_t i = 0; i < basket.size(); ++i) {
        if (!(basket[i].weight > 0.0) || !std::isfinite(basket[i].weight))
            return build_error{"basket", "weights must be positive"};
        for (std::size_t j = i + 1; j < basket.size(); ++j)
            if (basket[i].underlying == basket[j].underlying)
                return build_error{"basket", "must not hold a commodity twice"};
    }
    return std::nullopt;
}

/// The basket's constituents in its order.
void commodity_add_basket(domain::commodity_instrument_data& out,
                          const std::vector<commodity_basket_member>& basket) {
    int number = 0;
    for (const auto& member : basket) {
        domain::commodity_basket_constituent constituent{};
        constituent.sequence_number = ++number;
        constituent.underlying_code = member.underlying.uri();
        constituent.weight = member.weight;
        out.constituents.push_back(std::move(constituent));
    }
}

/// An accumulating product's row keeps its fixing amount, and its quantity is none.
domain::commodity_instrument commodity_fixing_row_of(std::string_view trade_type,
                                                     const marketdata::datum::market_reference& u,
                                                     const std::string& currency,
                                                     decimal fixing_amount,
                                                     decimal strike,
                                                     const std::string& unit) {
    auto row = commodity_row_of(trade_type, u, currency, decimal{}, unit);
    row.accumulation_amount = fixing_amount;
    row.strike_price = strike;
    return row;
}

}

commodity_forward_builder::commodity_forward_builder(commodity_forward_terms terms)
    : terms_(std::move(terms)) {}

commodity_result commodity_forward_builder::build() const {
    if (auto refusal = commodity_amount_refusal(terms_.currency.get(), terms_.quantity.get()))
        return std::unexpected(std::move(*refusal));
    if (terms_.price.get() <= decimal{})
        return std::unexpected(build_error{"price", "must be positive"});
    auto row = commodity_row_of(trade_type,
                                terms_.underlying.get(),
                                terms_.currency.get(),
                                terms_.quantity.get(),
                                terms_.unit);
    row.fixed_price = terms_.price.get();
    row.maturity_date = terms_.maturity.get();
    row.position = long_short_of(terms_.position.get());
    domain::commodity_instrument_data out{};
    out.instrument = std::move(row);
    return out;
}

commodity_option_builder::commodity_option_builder(commodity_option_terms terms)
    : terms_(std::move(terms)) {}

commodity_result commodity_option_builder::build() const {
    if (auto refusal = commodity_option_refusal(terms_))
        return std::unexpected(std::move(*refusal));
    domain::commodity_instrument_data out{};
    out.instrument = commodity_option_row_of(trade_type, terms_);
    out.options.push_back(commodity_option_row(terms_.position.get()));
    return out;
}

commodity_average_price_option_builder::commodity_average_price_option_builder(
    commodity_average_price_option_terms terms)
    : terms_(std::move(terms)) {}

commodity_result commodity_average_price_option_builder::build() const {
    const auto& option = terms_.option;
    if (auto refusal = commodity_option_refusal(option))
        return std::unexpected(std::move(*refusal));
    if (auto refusal = refusals::period(terms_.averaging_start.get(), option.expiry.get()))
        return std::unexpected(std::move(*refusal));
    auto row = commodity_option_row_of(trade_type, option);
    row.average_type = "Arithmetic";
    row.averaging_start_date = terms_.averaging_start.get();
    row.averaging_end_date = option.expiry.get();
    domain::commodity_instrument_data out{};
    out.instrument = std::move(row);
    out.options.push_back(commodity_option_row(option.position.get()));
    return out;
}

commodity_option_strip_builder::commodity_option_strip_builder(commodity_option_strip_terms terms)
    : terms_(std::move(terms)) {}

commodity_result commodity_option_strip_builder::build() const {
    if (auto refusal = commodity_amount_refusal(terms_.currency.get(), terms_.quantity.get()))
        return std::unexpected(std::move(*refusal));
    if (terms_.strike.get() <= decimal{})
        return std::unexpected(build_error{"strike", "must be positive"});
    if (auto refusal = refusals::period(terms_.start.get(), terms_.maturity.get()))
        return std::unexpected(std::move(*refusal));
    if (terms_.roll_tenor.get().empty())
        return std::unexpected(build_error{"roll_tenor", "must not be empty"});

    auto row = commodity_row_of(trade_type,
                                terms_.underlying.get(),
                                terms_.currency.get(),
                                terms_.quantity.get(),
                                terms_.unit);
    row.option_type = option_type_of(terms_.kind.get());
    row.strike_price = terms_.strike.get();
    row.start_date = terms_.start.get();
    row.maturity_date = terms_.maturity.get();
    row.strip_frequency_code = terms_.roll_tenor.get();
    domain::commodity_instrument_data out{};
    out.instrument = std::move(row);
    out.legs.push_back(
        commodity_floating_leg(1, false, terms_.currency.get(), terms_.underlying.get()));
    out.leg_amounts.push_back(rows::leg_amount(1, terms_.quantity.get()));
    out.schedules.push_back(commodity_leg_schedule(
        1, terms_.start.get(), terms_.maturity.get(), terms_.roll_tenor.get()));

    domain::instrument_strike strike{};
    strike.strike_role = strike_role_of(terms_.kind.get());
    strike.sequence_number = 1;
    strike.bare_value = terms_.strike.get();
    strike.long_short = long_short_of(terms_.position.get());
    out.strikes.push_back(std::move(strike));
    return out;
}

commodity_swap_builder::commodity_swap_builder(commodity_swap_terms terms)
    : terms_(std::move(terms)) {}

commodity_result commodity_swap_builder::build() const {
    if (auto refusal = commodity_swap_refusal(terms_))
        return std::unexpected(std::move(*refusal));
    domain::commodity_instrument_data out{};
    out.instrument = commodity_swap_row(trade_type, terms_);
    add_commodity_swap_legs(out, terms_);
    return out;
}

commodity_swaption_builder::commodity_swaption_builder(commodity_swaption_terms terms)
    : terms_(std::move(terms)) {}

commodity_result commodity_swaption_builder::build() const {
    if (auto refusal = commodity_swap_refusal(terms_.swap))
        return std::unexpected(std::move(*refusal));
    if (terms_.expiry.get() > terms_.swap.start.get())
        return std::unexpected(build_error{"expiry", "must be no later than the swap's start"});
    domain::commodity_instrument_data out{};
    auto row = commodity_swap_row(trade_type, terms_.swap);
    row.swaption_expiry_date = terms_.expiry.get();
    out.instrument = std::move(row);
    add_commodity_swap_legs(out, terms_.swap);
    out.options.push_back(commodity_option_row(terms_.position.get()));
    return out;
}

commodity_variance_swap_builder::commodity_variance_swap_builder(
    commodity_variance_swap_terms terms)
    : terms_(std::move(terms)) {}

commodity_result commodity_variance_swap_builder::build() const {
    if (auto refusal = commodity_amount_refusal(terms_.currency.get(), terms_.notional.get()))
        return std::unexpected(std::move(*refusal));
    const auto strike = terms_.variance_strike.get();
    if (!(strike > 0.0) || !std::isfinite(strike))
        return std::unexpected(build_error{"variance_strike", "must be positive"});
    if (auto refusal = refusals::period(terms_.start.get(), terms_.maturity.get()))
        return std::unexpected(std::move(*refusal));
    auto row = commodity_row_of(trade_type,
                                terms_.underlying.get(),
                                terms_.currency.get(),
                                terms_.notional.get(),
                                terms_.unit);
    row.variance_strike = strike;
    row.start_date = terms_.start.get();
    row.maturity_date = terms_.maturity.get();
    domain::commodity_instrument_data out{};
    out.instrument = std::move(row);
    return out;
}

commodity_basket_option_builder::commodity_basket_option_builder(
    commodity_basket_option_terms terms)
    : terms_(std::move(terms)) {}

commodity_result commodity_basket_option_builder::build() const {
    if (auto refusal = commodity_amount_refusal(terms_.currency.get(), terms_.notional.get()))
        return std::unexpected(std::move(*refusal));
    if (terms_.strike.get() <= decimal{})
        return std::unexpected(build_error{"strike", "must be positive"});
    const auto& basket = terms_.basket.get();
    if (auto refusal = commodity_basket_refusal(basket))
        return std::unexpected(std::move(*refusal));

    /*
     * The row names the basket's first member, so it states a commodity as
     * every row does. It states no unit, because the members may count
     * different ones.
     */
    auto row = commodity_row_of(
        trade_type, basket.front().underlying, terms_.currency.get(), terms_.notional.get(), {});
    row.option_type = option_type_of(terms_.kind.get());
    row.strike_price = terms_.strike.get();
    row.exercise_type = "European";
    row.maturity_date = terms_.expiry.get();
    domain::commodity_instrument_data out{};
    out.instrument = std::move(row);
    commodity_add_basket(out, basket);
    out.options.push_back(commodity_option_row(terms_.position.get(), "Vanilla"));
    return out;
}

commodity_digital_option_builder::commodity_digital_option_builder(
    commodity_digital_option_terms terms)
    : terms_(std::move(terms)) {}

commodity_result commodity_digital_option_builder::build() const {
    if (auto refusal = commodity_digital_refusal(terms_))
        return std::unexpected(std::move(*refusal));
    domain::commodity_instrument_data out{};
    out.instrument = commodity_digital_row_of(trade_type, terms_);
    out.options.push_back(commodity_option_row(terms_.position.get()));
    return out;
}

commodity_digital_average_price_option_builder::commodity_digital_average_price_option_builder(
    commodity_digital_average_price_option_terms terms)
    : terms_(std::move(terms)) {}

commodity_result commodity_digital_average_price_option_builder::build() const {
    const auto& option = terms_.option;
    if (auto refusal = commodity_digital_refusal(option))
        return std::unexpected(std::move(*refusal));
    if (auto refusal = refusals::period(terms_.averaging_start.get(), option.expiry.get()))
        return std::unexpected(std::move(*refusal));
    auto row = commodity_digital_row_of(trade_type, option);
    row.average_type = "Arithmetic";
    row.averaging_start_date = terms_.averaging_start.get();
    row.averaging_end_date = option.expiry.get();
    domain::commodity_instrument_data out{};
    out.instrument = std::move(row);
    out.options.push_back(commodity_option_row(option.position.get()));
    return out;
}

commodity_position_builder::commodity_position_builder(commodity_position_terms terms)
    : terms_(std::move(terms)) {}

commodity_result commodity_position_builder::build() const {
    if (auto refusal = commodity_amount_refusal(terms_.currency.get(), terms_.quantity.get()))
        return std::unexpected(std::move(*refusal));
    domain::commodity_instrument_data out{};
    out.instrument = commodity_row_of(trade_type,
                                      terms_.underlying.get(),
                                      terms_.currency.get(),
                                      terms_.quantity.get(),
                                      terms_.unit);
    commodity_add_underlying(out, terms_.underlying.get(), 1.0);
    return out;
}

commodity_spread_option_builder::commodity_spread_option_builder(
    commodity_spread_option_terms terms)
    : terms_(std::move(terms)) {}

commodity_result commodity_spread_option_builder::build() const {
    if (auto refusal = commodity_amount_refusal(terms_.currency.get(), terms_.quantity.get()))
        return std::unexpected(std::move(*refusal));
    if (terms_.long_underlying.get() == terms_.short_underlying.get())
        return std::unexpected(
            build_error{"short_underlying", "must differ from the long underlying"});
    if (auto refusal = refusals::period(terms_.start.get(), terms_.maturity.get()))
        return std::unexpected(std::move(*refusal));

    const auto& currency = terms_.currency.get();
    auto row = commodity_row_of(
        trade_type, terms_.long_underlying.get(), currency, terms_.quantity.get(), terms_.unit);
    row.spread_commodity_code = terms_.short_underlying.get().uri();
    row.option_type = option_type_of(terms_.kind.get());
    row.strike_price = terms_.spread_strike.get();
    row.exercise_type = "European";
    row.start_date = terms_.start.get();
    row.maturity_date = terms_.maturity.get();
    domain::commodity_instrument_data out{};
    out.instrument = std::move(row);
    // ORE is long the leg it receives and short the leg it pays.
    out.legs.push_back(commodity_floating_leg(1, false, currency, terms_.long_underlying.get()));
    out.legs.push_back(commodity_floating_leg(2, true, currency, terms_.short_underlying.get()));
    for (int leg = 1; leg <= 2; ++leg) {
        out.leg_amounts.push_back(rows::leg_amount(leg, terms_.quantity.get()));
        out.schedules.push_back(
            commodity_leg_schedule(leg, terms_.start.get(), terms_.maturity.get(), "1M"));
    }
    out.options.push_back(commodity_option_row(terms_.position.get()));
    return out;
}

intraday_power_forward_builder::intraday_power_forward_builder(intraday_power_forward_terms terms)
    : terms_(std::move(terms)) {}

commodity_result intraday_power_forward_builder::build() const {
    if (auto refusal = commodity_amount_refusal(terms_.currency.get(), terms_.quantity.get()))
        return std::unexpected(std::move(*refusal));
    if (terms_.price.get() <= decimal{})
        return std::unexpected(build_error{"price", "must be positive"});
    if (terms_.delivery_date.get() < terms_.maturity.get())
        return std::unexpected(build_error{"delivery_date", "must not be before the maturity"});
    const auto start = terms_.delivery_start.get();
    const auto end = terms_.delivery_end.get();
    if (start < 0 || end > commodity_seconds_in_a_day)
        return std::unexpected(build_error{"delivery", "must lie within the delivery day"});
    if (end <= start)
        return std::unexpected(build_error{"delivery_end", "must be after the delivery start"});

    auto row = commodity_row_of(trade_type,
                                terms_.underlying.get(),
                                terms_.currency.get(),
                                terms_.quantity.get(),
                                terms_.unit);
    row.fixed_price = terms_.price.get();
    row.maturity_date = terms_.maturity.get();
    row.position = long_short_of(terms_.position.get());
    row.delivery_date = terms_.delivery_date.get();
    row.delivery_start = start;
    row.delivery_end = end;
    domain::commodity_instrument_data out{};
    out.instrument = std::move(row);
    return out;
}

commodity_asian_option_builder::commodity_asian_option_builder(commodity_asian_option_terms terms)
    : terms_(std::move(terms)) {}

commodity_result commodity_asian_option_builder::build() const {
    const auto& option = terms_.option;
    if (auto refusal = commodity_option_refusal(option))
        return std::unexpected(std::move(*refusal));
    if (auto refusal = refusals::period(terms_.averaging_start.get(), option.expiry.get()))
        return std::unexpected(std::move(*refusal));
    domain::commodity_instrument_data out{};
    out.instrument = commodity_option_row_of(trade_type, option);
    out.options.push_back(commodity_option_row(option.position.get(), std::string("Asian")));
    out.schedules.push_back(commodity_scripted_schedule(
        "observation_schedule", terms_.averaging_start.get(), option.expiry.get()));
    commodity_add_underlying(out, option.underlying.get());
    return out;
}

commodity_accumulator_builder::commodity_accumulator_builder(commodity_accumulator_terms terms)
    : terms_(std::move(terms)) {}

commodity_result commodity_accumulator_builder::build() const {
    if (auto refusal = commodity_fixing_refusal(terms_.currency.get(),
                                                terms_.fixing_amount.get(),
                                                terms_.strike.get(),
                                                terms_.start.get(),
                                                terms_.expiry.get()))
        return std::unexpected(std::move(*refusal));
    if (terms_.knock_out.get() <= terms_.strike.get())
        return std::unexpected(build_error{"knock_out", "must be above the strike"});
    domain::commodity_instrument_data out{};
    out.instrument = commodity_fixing_row_of(trade_type,
                                             terms_.underlying.get(),
                                             terms_.currency.get(),
                                             terms_.fixing_amount.get(),
                                             terms_.strike.get(),
                                             terms_.unit);
    out.instrument.start_date = terms_.start.get();
    domain::instrument_option option{};
    option.long_short = long_short_of(terms_.position.get());
    option.payoff_type = "Accumulator";
    out.options.push_back(std::move(option));
    out.schedules.push_back(commodity_scripted_schedule(
        "observation_schedule", terms_.start.get(), terms_.expiry.get()));
    out.range_bounds = commodity_strike_bounds(terms_.strike.get());
    commodity_add_barrier(out, "UpAndOut", terms_.knock_out.get());
    commodity_add_underlying(out, terms_.underlying.get());
    return out;
}

commodity_tarf_builder::commodity_tarf_builder(commodity_tarf_terms terms)
    : terms_(std::move(terms)) {}

commodity_result commodity_tarf_builder::build() const {
    if (auto refusal = commodity_fixing_refusal(terms_.currency.get(),
                                                terms_.fixing_amount.get(),
                                                terms_.strike.get(),
                                                terms_.start.get(),
                                                terms_.expiry.get()))
        return std::unexpected(std::move(*refusal));
    if (terms_.target_amount.get() <= decimal{})
        return std::unexpected(build_error{"target_amount", "must be positive"});
    domain::commodity_instrument_data out{};
    out.instrument = commodity_fixing_row_of(trade_type,
                                             terms_.underlying.get(),
                                             terms_.currency.get(),
                                             terms_.fixing_amount.get(),
                                             terms_.strike.get(),
                                             terms_.unit);
    out.instrument.target_amount = terms_.target_amount.get();
    domain::instrument_option option{};
    option.long_short = long_short_of(terms_.position.get());
    option.payoff_type = "TargetExact";
    out.options.push_back(std::move(option));
    out.schedules.push_back(
        commodity_scripted_schedule("product_schedule", terms_.start.get(), terms_.expiry.get()));
    out.range_bounds = commodity_strike_bounds(terms_.strike.get());
    commodity_add_barrier(out, "CumulatedProfitCap", terms_.target_amount.get());
    commodity_add_underlying(out, terms_.underlying.get());
    return out;
}

commodity_generic_barrier_option_builder::commodity_generic_barrier_option_builder(
    commodity_generic_barrier_option_terms terms)
    : terms_(std::move(terms)) {}

commodity_result commodity_generic_barrier_option_builder::build() const {
    const auto& option = terms_.option;
    if (auto refusal = commodity_option_refusal(option))
        return std::unexpected(std::move(*refusal));
    if (terms_.barrier_level.get() <= decimal{})
        return std::unexpected(build_error{"barrier_level", "must be positive"});
    if (auto refusal = refusals::period(terms_.monitoring_start.get(), option.expiry.get()))
        return std::unexpected(std::move(*refusal));
    domain::commodity_instrument_data out{};
    out.instrument = commodity_option_row_of(trade_type, option);
    out.options.push_back(commodity_option_row(option.position.get()));
    commodity_add_barrier(
        out, commodity_barrier_type_of(terms_.barrier.get()), terms_.barrier_level.get());
    out.schedules.push_back(commodity_scripted_schedule(
        "barrier_schedule", terms_.monitoring_start.get(), option.expiry.get()));
    commodity_add_underlying(out, option.underlying.get());
    return out;
}

commodity_strike_resettable_option_builder::commodity_strike_resettable_option_builder(
    commodity_strike_resettable_option_terms terms)
    : terms_(std::move(terms)) {}

commodity_result commodity_strike_resettable_option_builder::build() const {
    const auto& option = terms_.option;
    if (auto refusal = commodity_option_refusal(option))
        return std::unexpected(std::move(*refusal));
    if (terms_.reset_strike.get() <= decimal{})
        return std::unexpected(build_error{"reset_strike", "must be positive"});
    if (terms_.trigger_price.get() <= decimal{})
        return std::unexpected(build_error{"trigger_price", "must be positive"});
    if (terms_.premium.get() < decimal{})
        return std::unexpected(build_error{"premium", "must not be negative"});
    if (auto refusal = refusals::period(terms_.observation_start.get(), option.expiry.get()))
        return std::unexpected(std::move(*refusal));
    if (terms_.settlement.get() < option.expiry.get())
        return std::unexpected(build_error{"settlement", "must not be before the expiry"});
    auto row = commodity_option_row_of(trade_type, option);
    // The product states no OptionData, so the row keeps no exercise style.
    row.exercise_type.clear();
    row.position = long_short_of(option.position.get());
    row.reset_strike = terms_.reset_strike.get();
    row.trigger_type = terms_.trigger.get() == trigger_direction::up ? "Up" : "Down";
    row.trigger_price = terms_.trigger_price.get();
    row.settlement_date = terms_.settlement.get();
    row.premium_amount = terms_.premium.get();
    row.premium_date = terms_.premium_date.get();
    domain::commodity_instrument_data out{};
    out.instrument = std::move(row);
    out.schedules.push_back(commodity_scripted_schedule(
        "observation_schedule", terms_.observation_start.get(), option.expiry.get()));
    commodity_add_underlying(out, option.underlying.get());
    return out;
}

commodity_forward_volatility_agreement_builder::commodity_forward_volatility_agreement_builder(
    commodity_forward_volatility_agreement_terms terms)
    : terms_(std::move(terms)) {}

commodity_result commodity_forward_volatility_agreement_builder::build() const {
    if (auto refusal = commodity_amount_refusal(terms_.currency.get(), terms_.quantity.get()))
        return std::unexpected(std::move(*refusal));
    if (terms_.implied_vol_strike.get() <= decimal{})
        return std::unexpected(build_error{"implied_vol_strike", "must be positive"});
    if (!(terms_.fva_date.get() < terms_.expiry.get()))
        return std::unexpected(build_error{"fva_date", "must be before the expiry"});
    if (terms_.settlement.get() < terms_.expiry.get())
        return std::unexpected(build_error{"settlement", "must not be before the expiry"});
    auto row = commodity_row_of(trade_type,
                                terms_.underlying.get(),
                                terms_.currency.get(),
                                terms_.quantity.get(),
                                terms_.unit);
    row.maturity_date = terms_.expiry.get();
    row.position = long_short_of(terms_.position.get());
    row.fva_date = terms_.fva_date.get();
    row.settlement_date = terms_.settlement.get();
    row.premium_date = terms_.premium_date.get();
    row.implied_vol_strike = terms_.implied_vol_strike.get();
    domain::commodity_instrument_data out{};
    out.instrument = std::move(row);
    commodity_add_underlying(out, terms_.underlying.get());
    return out;
}

namespace {

std::string commodity_rainbow_payoff_of(rainbow_payoff payoff) {
    switch (payoff) {
        case rainbow_payoff::best_of_asset_or_cash:
            return "BestOfAssetOrCash";
        case rainbow_payoff::worst_of_asset_or_cash:
            return "WorstOfAssetOrCash";
        case rainbow_payoff::max_rainbow:
            return "MaxRainbow";
        case rainbow_payoff::min_rainbow:
            return "MinRainbow";
    }
    return {};
}

}

commodity_rainbow_option_builder::commodity_rainbow_option_builder(
    commodity_rainbow_option_terms terms)
    : terms_(std::move(terms)) {}

commodity_result commodity_rainbow_option_builder::build() const {
    if (auto refusal = commodity_amount_refusal(terms_.currency.get(), terms_.notional.get()))
        return std::unexpected(std::move(*refusal));
    if (terms_.strike.get() <= decimal{})
        return std::unexpected(build_error{"strike", "must be positive"});
    const auto& basket = terms_.basket.get();
    if (auto refusal = commodity_basket_refusal(basket))
        return std::unexpected(std::move(*refusal));
    // The members may count different units, so the row states none.
    auto row = commodity_row_of(
        trade_type, basket.front().underlying, terms_.currency.get(), terms_.notional.get(), {});
    row.option_type = option_type_of(terms_.kind.get());
    row.strike_price = terms_.strike.get();
    row.exercise_type = "European";
    row.maturity_date = terms_.expiry.get();
    domain::commodity_instrument_data out{};
    out.instrument = std::move(row);
    commodity_add_basket(out, basket);
    out.options.push_back(commodity_option_row(terms_.position.get(),
                                               commodity_rainbow_payoff_of(terms_.payoff.get())));
    return out;
}

commodity_basket_variance_swap_builder::commodity_basket_variance_swap_builder(
    commodity_basket_variance_swap_terms terms)
    : terms_(std::move(terms)) {}

commodity_result commodity_basket_variance_swap_builder::build() const {
    if (auto refusal = commodity_amount_refusal(terms_.currency.get(), terms_.notional.get()))
        return std::unexpected(std::move(*refusal));
    if (!(terms_.variance_strike.get() > 0.0))
        return std::unexpected(build_error{"variance_strike", "must be positive"});
    if (auto refusal = refusals::period(terms_.start.get(), terms_.maturity.get()))
        return std::unexpected(std::move(*refusal));
    if (terms_.settlement.get() < terms_.maturity.get())
        return std::unexpected(build_error{"settlement", "must not be before the maturity"});
    const auto& basket = terms_.basket.get();
    if (auto refusal = commodity_basket_refusal(basket))
        return std::unexpected(std::move(*refusal));
    auto row = commodity_row_of(
        trade_type, basket.front().underlying, terms_.currency.get(), terms_.notional.get(), {});
    row.variance_strike = terms_.variance_strike.get();
    row.moment_type = "Variance";
    row.start_date = terms_.start.get();
    row.maturity_date = terms_.maturity.get();
    row.settlement_date = terms_.settlement.get();
    row.position = long_short_of(terms_.position.get());
    domain::commodity_instrument_data out{};
    out.instrument = std::move(row);
    commodity_add_basket(out, basket);
    out.schedules.push_back(commodity_scripted_schedule(
        "valuation_schedule", terms_.start.get(), terms_.maturity.get()));
    return out;
}

commodity_best_entry_option_builder::commodity_best_entry_option_builder(
    commodity_best_entry_option_terms terms)
    : terms_(std::move(terms)) {}

commodity_result commodity_best_entry_option_builder::build() const {
    if (auto refusal = commodity_amount_refusal(terms_.currency.get(), terms_.notional.get()))
        return std::unexpected(std::move(*refusal));
    if (terms_.strike.get() <= decimal{})
        return std::unexpected(build_error{"strike", "must be positive"});
    if (terms_.multiplier.get() <= decimal{} || terms_.cap.get() <= decimal{})
        return std::unexpected(build_error{"cap", "the multiplier and cap must be positive"});
    const decimal one = *decimal::from_string("1");
    for (const auto& [term, value] : {std::pair{"trigger_level", terms_.trigger_level.get()},
                                      std::pair{"reset_minimum", terms_.reset_minimum.get()}})
        if (value <= decimal{} || one < value)
            return std::unexpected(
                build_error{term, "must be a fraction of the initial price above zero"});
    if (terms_.premium.get() < decimal{})
        return std::unexpected(build_error{"premium", "must not be negative"});
    if (auto refusal = refusals::period(terms_.strike_date.get(), terms_.expiry.get()))
        return std::unexpected(std::move(*refusal));
    if (terms_.settlement.get() < terms_.expiry.get())
        return std::unexpected(build_error{"settlement", "must not be before the expiry"});
    auto row = commodity_row_of(trade_type,
                                terms_.underlying.get(),
                                terms_.currency.get(),
                                terms_.notional.get(),
                                terms_.unit);
    row.strike_price = terms_.strike.get();
    row.maturity_date = terms_.expiry.get();
    row.position = long_short_of(terms_.position.get());
    row.multiplier = terms_.multiplier.get();
    row.cap = terms_.cap.get();
    row.trigger_level = terms_.trigger_level.get();
    row.reset_minimum = terms_.reset_minimum.get();
    row.premium_amount = terms_.premium.get();
    row.premium_date = terms_.premium_date.get();
    row.strike_date = terms_.strike_date.get();
    row.settlement_date = terms_.settlement.get();
    domain::commodity_instrument_data out{};
    out.instrument = std::move(row);
    out.schedules.push_back(commodity_scripted_schedule(
        "observation_schedule", terms_.strike_date.get(), terms_.expiry.get()));
    commodity_add_underlying(out, terms_.underlying.get());
    return out;
}

commodity_worst_of_basket_swap_builder::commodity_worst_of_basket_swap_builder(
    const marketdata::conventions::convention_lookup& conventions,
    commodity_worst_of_basket_swap_terms terms)
    : conventions_(conventions)
    , terms_(std::move(terms)) {}

commodity_result commodity_worst_of_basket_swap_builder::build() const {
    const auto& basket = terms_.basket.get();
    if (basket.size() < 2)
        return std::unexpected(build_error{"basket", "must hold at least two commodities"});
    for (std::size_t i = 0; i < basket.size(); ++i) {
        if (basket[i].initial_price <= decimal{})
            return std::unexpected(build_error{"basket", "initial prices must be positive"});
        for (std::size_t j = i + 1; j < basket.size(); ++j)
            if (basket[i].underlying == basket[j].underlying)
                return std::unexpected(build_error{"basket", "must not hold a commodity twice"});
    }
    if (auto refusal = commodity_amount_refusal(terms_.currency.get(), terms_.quantity.get()))
        return std::unexpected(std::move(*refusal));
    if (terms_.fixed_rate.get() < decimal{})
        return std::unexpected(build_error{"fixed_rate", "must not be negative"});
    if (auto refusal = refusals::period(terms_.start.get(), terms_.maturity.get()))
        return std::unexpected(std::move(*refusal));
    const auto& observations = terms_.observation_dates.get();
    if (observations.empty())
        return std::unexpected(build_error{"observation_dates", "must state at least one date"});
    for (std::size_t i = 0; i < observations.size(); ++i) {
        const auto previous = i == 0 ? terms_.start.get() : observations[i - 1];
        if (observations[i] <= previous || observations[i] > terms_.maturity.get())
            return std::unexpected(build_error{
                "observation_dates", "must ascend after the start and end no later than maturity"});
    }
    const auto& funding = terms_.funding_index.get();
    if (funding.index() == nullptr || funding.index()->subject() != terms_.currency.get())
        return std::unexpected(build_error{"funding_index", "must be an index in the currency"});
    const auto definition = conventions_.index_definition_of(funding);
    if (!definition)
        return std::unexpected(build_error{
            "funding_index", definition.error().reference + ": " + definition.error().reason});
    // The funding leg accrues and rolls as its index's convention defines.
    const auto [day_count, calendar, adjustment] = std::visit(
        [](const auto& index) {
            std::string rolls = "MF";
            if constexpr (requires { index.business_day_convention; })
                rolls = index.business_day_convention;
            return std::tuple{index.day_count_fraction, index.fixing_calendar, rolls};
        },
        *definition);

    auto row = commodity_row_of(
        trade_type, basket.front().underlying, terms_.currency.get(), terms_.quantity.get(), {});
    row.start_date = terms_.start.get();
    row.maturity_date = terms_.maturity.get();
    row.position = long_short_of(terms_.position.get());
    domain::commodity_instrument_data out{};
    out.instrument = std::move(row);
    int number = 0;
    for (const auto& member : basket) {
        domain::commodity_basket_constituent constituent{};
        constituent.sequence_number = ++number;
        constituent.underlying_code = member.underlying.uri();
        constituent.weight = 1.0 / static_cast<double>(basket.size());
        constituent.initial_price = member.initial_price;
        out.constituents.push_back(std::move(constituent));
    }

    // The funding leg floats on its index; the coupon leg pays the fixed rate.
    auto funding_leg = rows::swap_leg(1, true, "Floating", terms_.currency.get());
    funding_leg.floating_index_code = funding.uri();
    funding_leg.day_count_fraction_code = day_count;
    out.legs.push_back(std::move(funding_leg));
    out.legs.push_back(rows::swap_leg(2, false, "Fixed", terms_.currency.get()));
    domain::swap_leg_rate coupon{};
    coupon.leg_number = 2;
    coupon.rate_role = "fixed";
    coupon.sequence_number = 1;
    coupon.value = terms_.fixed_rate.get();
    out.leg_rates.push_back(std::move(coupon));

    /*
     * ORE accrues between consecutive schedule dates and reads one level per
     * accrual period, so both schedules are the start then each period's end.
     */
    for (const auto* role : {"floating_period_schedule", "floating_pay_schedule"}) {
        domain::instrument_schedule schedule{};
        schedule.owner_role = "option";
        schedule.owner_number = 1;
        schedule.schedule_role = role;
        schedule.sequence_number = 1;
        schedule.schedule_kind = "dates";
        schedule.calendar = calendar;
        schedule.convention = adjustment;
        out.schedules.push_back(std::move(schedule));
        int date_number = 0;
        std::vector<ymd> dates{terms_.start.get()};
        dates.insert(dates.end(), observations.begin(), observations.end());
        for (const auto& when : dates) {
            domain::instrument_schedule_date date{};
            date.owner_role = "option";
            date.owner_number = 1;
            date.schedule_role = role;
            date.schedule_sequence_number = 1;
            date.sequence_number = ++date_number;
            date.schedule_date = when;
            out.schedule_dates.push_back(std::move(date));
        }
    }
    number = 0;
    for (const auto& when : observations) {
        domain::basket_swap_period period{};
        period.sequence_number = ++number;
        period.period_date = when;
        period.knock_out_level = 1.0;
        period.fixed_trigger_level = 0.75;
        out.periods.push_back(std::move(period));
    }
    return out;
}

}
