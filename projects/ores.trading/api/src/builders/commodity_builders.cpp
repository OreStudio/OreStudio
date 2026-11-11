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
#include <utility>

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
                                              const decimal& quantity) {
    domain::commodity_instrument row{};
    row.identity.trade_type_code = std::string(trade_type);
    row.commodity_code = underlying.uri();
    row.currency = currency;
    row.quantity = quantity.to_double();
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

/// A commodity leg that pays five days after its period on the NYSE calendar.
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
        trade_type, terms.underlying.get(), terms.currency.get(), terms.quantity.get());
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

domain::commodity_instrument commodity_option_row_of(std::string_view trade_type,
                                                     const commodity_option_terms& terms) {
    auto row = commodity_row_of(
        trade_type, terms.underlying.get(), terms.currency.get(), terms.quantity.get());
    row.option_type = option_type_of(terms.kind.get());
    row.strike_price = terms.strike.get();
    row.exercise_type = "European";
    row.maturity_date = terms.expiry.get();
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
    auto row = commodity_row_of(
        trade_type, terms_.underlying.get(), terms_.currency.get(), terms_.quantity.get());
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

    auto row = commodity_row_of(
        trade_type, terms_.underlying.get(), terms_.currency.get(), terms_.quantity.get());
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
    strike.strike_role = terms_.kind.get() == option_kind::call ? "call" : "put";
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
    auto row = commodity_row_of(
        trade_type, terms_.underlying.get(), terms_.currency.get(), terms_.notional.get());
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
    const auto& basket = terms_.basket.get();
    if (basket.size() < 2)
        return std::unexpected(build_error{"basket", "must hold at least two commodities"});
    for (std::size_t i = 0; i < basket.size(); ++i) {
        if (!(basket[i].weight > 0.0) || !std::isfinite(basket[i].weight))
            return std::unexpected(build_error{"basket", "weights must be positive"});
        for (std::size_t j = i + 1; j < basket.size(); ++j)
            if (basket[i].underlying == basket[j].underlying)
                return std::unexpected(build_error{"basket", "must not hold a commodity twice"});
    }
    if (auto refusal = commodity_amount_refusal(terms_.currency.get(), terms_.notional.get()))
        return std::unexpected(std::move(*refusal));
    if (terms_.strike.get() <= decimal{})
        return std::unexpected(build_error{"strike", "must be positive"});

    // The row names the basket's first member, so it states a commodity as every row does.
    auto row = commodity_row_of(
        trade_type, basket.front().underlying, terms_.currency.get(), terms_.notional.get());
    row.option_type = option_type_of(terms_.kind.get());
    row.strike_price = terms_.strike.get();
    row.exercise_type = "European";
    row.maturity_date = terms_.expiry.get();
    domain::commodity_instrument_data out{};
    out.instrument = std::move(row);
    int number = 0;
    for (const auto& member : basket) {
        domain::commodity_basket_constituent constituent{};
        constituent.sequence_number = ++number;
        constituent.underlying_code = member.underlying.uri();
        constituent.weight = member.weight;
        out.constituents.push_back(std::move(constituent));
    }
    out.options.push_back(commodity_option_row(terms_.position.get(), "Vanilla"));
    return out;
}

}
