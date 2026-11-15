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
#include "ores.trading.api/builders/equity_builders.hpp"
#include "leg_rows.hpp"
#include "refusals.hpp"
#include <tuple>
#include <utility>
#include <variant>

namespace ores::trading::builders {

namespace {

using utility::decimal::decimal;
using ymd = std::chrono::year_month_day;

/**
 * @brief The name an underlying states: its index's subject when it names one,
 * else the URI it carries.
 *
 * An underlying is ORE's one-or-the-other: an index or a URI. Every place that
 * writes the name reads it here rather than deciding for itself.
 */
template <typename Underlying>
std::string underlying_name_of(const Underlying& underlying) {
    const auto* index = underlying.index();
    return index != nullptr ? index->subject() : underlying.uri();
}

/// A schedule kept as the product's own, from @p start to @p end every @p tenor.
domain::instrument_schedule
equity_schedule(std::string role, std::string tenor, ymd start, ymd end) {
    domain::instrument_schedule row{};
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
    return row;
}

/// Fixings below the strike are taken twice, those above it once.
std::vector<domain::range_bound> equity_strike_bounds(decimal strike) {
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
void equity_add_barrier(domain::equity_instrument_data& out, std::string type, decimal level) {
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

/// The equity an accumulating product observes, as its Underlying block states it.
void equity_add_underlying(domain::equity_instrument_data& out, std::string address) {
    domain::instrument_underlying row{};
    row.underlying_role = "underlying";
    row.sequence_number = 1;
    row.underlying_type = "Equity";
    row.name = std::move(address);
    out.instrument_underlyings.push_back(std::move(row));
}

std::optional<build_error>
equity_fixing_refusal(decimal strike, decimal fixing_amount, ymd start, ymd expiry) {
    if (strike <= decimal{} || fixing_amount <= decimal{})
        return build_error{"amounts", "the strike and fixing amount must be positive"};
    return refusals::period(start, expiry);
}

}

namespace equity_detail {

std::optional<build_error> option_refusal(const equity_option_terms& terms) {
    if (terms.currency.get().empty())
        return build_error{"currency", "must not be empty"};
    if (terms.quantity.get() <= decimal{})
        return build_error{"quantity", "must be positive"};
    if (terms.strike.get() <= decimal{})
        return build_error{"strike", "must be positive"};
    return std::nullopt;
}

domain::instrument_option option_row(option_position position,
                                     std::optional<std::string> style,
                                     std::optional<std::string> payoff_type,
                                     std::optional<std::string> pay_off_at_expiry) {
    domain::instrument_option row{};
    row.long_short = long_short_of(position);
    row.style = std::move(style);
    if (row.style)
        row.settlement = "Cash";
    row.payoff_type = std::move(payoff_type);
    row.pay_off_at_expiry = std::move(pay_off_at_expiry);
    return row;
}

std::optional<build_error>
barrier_refusal(std::string_view type, const std::vector<decimal>& levels, bool double_barrier) {
    const bool single_type =
        type == "UpAndIn" || type == "UpAndOut" || type == "DownAndIn" || type == "DownAndOut";
    const bool double_type = type == "KnockIn" || type == "KnockOut";
    if (double_barrier ? !(double_type && levels.size() == 2) :
                         !(single_type && levels.size() == 1))
        return build_error{"barrier",
                           double_barrier ? "must be KnockIn or KnockOut with two levels" :
                                            "must be one UpAnd or DownAnd level"};
    for (const auto& level : levels)
        if (level <= decimal{})
            return build_error{"barrier", "levels must be positive"};
    if (double_barrier && !(levels.front() < levels.back()))
        return build_error{"barrier", "the lower level must be below the upper"};
    return std::nullopt;
}

}

namespace {

domain::equity_option_instrument equity_option_row(std::string_view trade_type,
                                                   const equity_option_terms& terms) {
    domain::equity_option_instrument row{};
    row.identity.trade_type_code = std::string(trade_type);
    row.underlying_name = terms.underlying.get().uri();
    row.currency = terms.currency.get();
    row.notional = terms.quantity.get();
    row.option_type = option_type_of(terms.kind.get());
    row.strike = terms.strike.get();
    row.expiry_date = terms.expiry.get();
    row.exercise_type = "European";
    row.long_short = long_short_of(terms.position.get());
    row.settlement_type = "Cash";
    return row;
}

}

equity_option_builder::equity_option_builder(equity_option_terms terms)
    : terms_(std::move(terms)) {}

equity_result equity_option_builder::build() const {
    if (auto refusal = equity_detail::option_refusal(terms_))
        return std::unexpected(std::move(*refusal));
    domain::equity_instrument_data out;
    out.instrument = equity_option_row(trade_type, terms_);
    out.options.push_back(equity_detail::option_row(terms_.position.get(), "European"));
    return out;
}

equity_cliquet_option_builder::equity_cliquet_option_builder(equity_cliquet_option_terms terms)
    : terms_(std::move(terms)) {}

equity_result equity_cliquet_option_builder::build() const {
    if (auto refusal = equity_detail::option_refusal(terms_.option))
        return std::unexpected(std::move(*refusal));
    if (auto refusal = refusals::period(terms_.start.get(), terms_.option.expiry.get()))
        return std::unexpected(std::move(*refusal));
    if (terms_.reset_tenor.get().empty())
        return std::unexpected(build_error{"reset_tenor", "must not be empty"});
    auto row = equity_option_row(trade_type, terms_.option);
    row.cliquet_frequency = terms_.reset_tenor.get();
    domain::equity_instrument_data out;
    out.instrument = std::move(row);
    out.schedules.push_back(equity_schedule("product_schedule",
                                            terms_.reset_tenor.get(),
                                            terms_.start.get(),
                                            terms_.option.expiry.get()));
    return out;
}

equity_asian_option_builder::equity_asian_option_builder(equity_asian_option_terms terms)
    : terms_(std::move(terms)) {}

equity_result equity_asian_option_builder::build() const {
    const auto& option = terms_.option;
    if (auto refusal = equity_detail::option_refusal(option))
        return std::unexpected(std::move(*refusal));
    if (auto refusal = refusals::period(terms_.averaging_start.get(), option.expiry.get()))
        return std::unexpected(std::move(*refusal));
    domain::equity_asian_option_instrument row{};
    row.identity.trade_type_code = std::string(trade_type);
    row.underlying_name = option.underlying.get().uri();
    row.currency = option.currency.get();
    row.notional = option.quantity.get();
    row.option_type = option_type_of(option.kind.get());
    row.strike = option.strike.get();
    row.expiry_date = option.expiry.get();
    row.exercise_type = "European";
    row.long_short = long_short_of(option.position.get());
    row.average_type = "Arithmetic";
    row.averaging_start_date = terms_.averaging_start.get();
    row.averaging_end_date = option.expiry.get();
    domain::equity_instrument_data out;
    out.instrument = std::move(row);
    // ORE's Asian option states its payoff type and observation dates, not a style.
    out.options.push_back(
        equity_detail::option_row(option.position.get(), std::nullopt, std::string("Asian")));
    out.schedules.push_back(equity_schedule(
        "observation_schedule", "1M", terms_.averaging_start.get(), option.expiry.get()));
    equity_add_underlying(out, option.underlying.get().uri());
    return out;
}

equity_digital_option_builder::equity_digital_option_builder(equity_digital_option_terms terms)
    : terms_(std::move(terms)) {}

equity_result equity_digital_option_builder::build() const {
    const auto& option = terms_.option;
    if (auto refusal = equity_detail::option_refusal(option))
        return std::unexpected(std::move(*refusal));
    if (terms_.payout.get() <= decimal{})
        return std::unexpected(build_error{"payout", "must be positive"});
    domain::equity_digital_option_instrument row{};
    row.identity.trade_type_code = std::string(trade_type);
    row.underlying_name = option.underlying.get().uri();
    row.currency = option.currency.get();
    row.notional = terms_.payout.get();
    row.option_type = option_type_of(option.kind.get());
    row.strike = option.strike.get();
    row.expiry_date = option.expiry.get();
    row.long_short = long_short_of(option.position.get());
    row.payout_amount = terms_.payout.get();
    row.quantity = option.quantity.get().to_double();
    domain::equity_instrument_data out;
    out.instrument = std::move(row);
    out.options.push_back(equity_detail::option_row(option.position.get(), "European"));
    return out;
}

equity_touch_option_builder::equity_touch_option_builder(equity_touch_option_terms terms)
    : terms_(std::move(terms)) {}

equity_result equity_touch_option_builder::build() const {
    if (terms_.payout.get() <= decimal{})
        return std::unexpected(build_error{"payout", "must be positive"});
    if (terms_.level.get() <= decimal{})
        return std::unexpected(build_error{"level", "must be positive"});
    if (auto refusal =
            equity_detail::barrier_refusal(terms_.barrier_type.get(), {terms_.level.get()}, false))
        return std::unexpected(std::move(*refusal));
    domain::equity_digital_option_instrument row{};
    row.identity.trade_type_code = std::string(trade_type);
    row.underlying_name = terms_.underlying.get().uri();
    row.currency = terms_.currency.get();
    row.notional = terms_.payout.get();
    row.barrier_level = terms_.level.get();
    row.barrier_type = terms_.barrier_type.get();
    row.expiry_date = terms_.expiry.get();
    row.long_short = long_short_of(terms_.position.get());
    row.payout_amount = terms_.payout.get();
    /*
     * ORE watches a touch against the equity's index. The catalogue keeps no
     * exchange calendar for an equity, so the touch is watched on TARGET.
     */
    row.barrier_fixing_index = terms_.underlying.get().uri();
    row.barrier_calendar = "TARGET";
    domain::equity_instrument_data out;
    out.instrument = std::move(row);
    out.options.push_back(
        equity_detail::option_row(terms_.position.get(), std::nullopt, {}, std::string("true")));
    return out;
}

equity_forward_builder::equity_forward_builder(equity_forward_terms terms)
    : terms_(std::move(terms)) {}

equity_result equity_forward_builder::build() const {
    if (terms_.currency.get().empty())
        return std::unexpected(build_error{"currency", "must not be empty"});
    if (!(terms_.quantity.get() > 0.0))
        return std::unexpected(build_error{"quantity", "must be positive"});
    if (terms_.forward_price.get() <= decimal{})
        return std::unexpected(build_error{"forward_price", "must be positive"});
    domain::equity_forward_instrument row{};
    row.identity.trade_type_code = std::string(trade_type);
    row.underlying_name = terms_.underlying.get().uri();
    row.currency = terms_.currency.get();
    row.quantity = terms_.quantity.get();
    row.forward_price = terms_.forward_price.get();
    row.expiry_date = terms_.maturity.get();
    row.long_short = long_short_of(terms_.position.get());
    row.settlement_type = "Cash";
    domain::equity_instrument_data out;
    out.instrument = std::move(row);
    return out;
}

equity_swap_builder::equity_swap_builder(
    const marketdata::conventions::convention_lookup& conventions, equity_swap_terms terms)
    : conventions_(conventions)
    , terms_(std::move(terms)) {}

equity_result equity_swap_builder::build() const {
    if (!(terms_.quantity.get() > 0.0))
        return std::unexpected(build_error{"quantity", "must be positive"});
    const auto& funding = terms_.funding;
    const auto swap = vanilla_swap_builder(conventions_, funding).build();
    if (!swap)
        return std::unexpected(build_error{"funding." + swap.error().term, swap.error().message});

    domain::equity_swap_instrument row{};
    row.identity.trade_type_code = std::string(trade_type);
    row.underlying_name = terms_.underlying.get().uri();
    row.currency = funding.currency.get();
    row.notional = funding.notional.get();
    row.return_type = "Price";
    row.start_date = funding.start.get();
    row.maturity_date = funding.maturity.get();
    row.long_short = "Long";

    domain::equity_instrument_data out;
    // The floating leg is the funding swap's, renumbered first; the equity leg pays on its
    // schedule.
    for (auto leg : swap->legs)
        if (leg.identity.leg_number == 2) {
            leg.identity.leg_number = 1;
            leg.payer = false;
            row.payment_frequency_code = leg.payment_frequency_code;
            out.legs.push_back(std::move(leg));
        }
    for (auto amount : swap->leg_amounts)
        if (amount.leg_number == 2) {
            amount.leg_number = 1;
            out.leg_amounts.push_back(std::move(amount));
        }
    for (auto rate : swap->leg_rates)
        if (rate.leg_number == 2) {
            rate.leg_number = 1;
            out.leg_rates.push_back(std::move(rate));
        }
    for (const auto& schedule : swap->schedules)
        if (schedule.owner_number == 2 && schedule.schedule_role == "schedule") {
            auto floating = schedule;
            floating.owner_number = 1;
            out.schedules.push_back(floating);
            auto equity = schedule;
            equity.owner_number = 2;
            out.schedules.push_back(std::move(equity));
        }
    auto equity_leg = rows::swap_leg(2, true, "Equity", funding.currency.get());
    equity_leg.day_count_fraction_code = "A360";
    equity_leg.business_day_convention_code = "MF";
    equity_leg.payment_frequency_code = row.payment_frequency_code;
    equity_leg.equity_return_type = "Price";
    equity_leg.equity_underlying_type = "Equity";
    equity_leg.equity_name = terms_.underlying.get().uri();
    equity_leg.equity_quantity = terms_.quantity.get();
    out.legs.push_back(std::move(equity_leg));
    out.instrument = std::move(row);
    return out;
}

equity_worst_of_basket_swap_builder::equity_worst_of_basket_swap_builder(
    const marketdata::conventions::convention_lookup& conventions,
    equity_worst_of_basket_swap_terms terms)
    : conventions_(conventions)
    , terms_(std::move(terms)) {}

equity_result equity_worst_of_basket_swap_builder::build() const {
    const auto& basket = terms_.basket.get();
    if (basket.size() < 2)
        return std::unexpected(build_error{"basket", "must hold at least two equities"});
    for (std::size_t i = 0; i < basket.size(); ++i) {
        if (basket[i].initial_price <= decimal{})
            return std::unexpected(build_error{"basket", "initial prices must be positive"});
        for (std::size_t j = i + 1; j < basket.size(); ++j)
            if (basket[i].underlying == basket[j].underlying)
                return std::unexpected(build_error{"basket", "must not hold an equity twice"});
    }
    if (terms_.currency.get().empty())
        return std::unexpected(build_error{"currency", "must not be empty"});
    if (terms_.notional.get() <= decimal{})
        return std::unexpected(build_error{"notional", "must be positive"});
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

    domain::equity_swap_instrument row{};
    row.identity.trade_type_code = std::string(trade_type);
    row.currency = terms_.currency.get();
    row.notional = terms_.notional.get();
    row.return_type = "Price";
    row.start_date = terms_.start.get();
    row.maturity_date = terms_.maturity.get();
    row.long_short = long_short_of(terms_.position.get());
    row.payment_frequency_code = "Quarterly";
    row.fixed_rate = decimal{};
    row.floating_index = terms_.funding_index.get().uri();
    row.floating_spread = decimal{};
    row.floating_day_count_fraction = day_count;
    domain::equity_instrument_data out;
    out.instrument = std::move(row);
    int number = 0;
    for (const auto& member : basket) {
        domain::equity_basket_constituent constituent{};
        constituent.sequence_number = ++number;
        /*
         * The store resolves a constituent against the equity catalogue by its
         * code, so a member is stored by the code its address names until that
         * check reads addresses; the projection writes a code as stated.
         */
        constituent.underlying_name = underlying_name_of(member.underlying);
        // ORE's type for an equity underlying; the store reads the kind case-insensitively.
        constituent.underlying_kind = "Equity";
        constituent.weight = 1.0 / static_cast<double>(basket.size());
        constituent.initial_price = member.initial_price;
        out.basket_constituents.push_back(std::move(constituent));
    }
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

equity_variance_swap_builder::equity_variance_swap_builder(equity_variance_swap_terms terms)
    : terms_(std::move(terms)) {}

equity_result equity_variance_swap_builder::build() const {
    if (auto refusal = refusals::period(terms_.start.get(), terms_.maturity.get()))
        return std::unexpected(std::move(*refusal));
    if (terms_.currency.get().empty())
        return std::unexpected(build_error{"currency", "must not be empty"});
    if (!(terms_.variance_strike.get() > 0.0))
        return std::unexpected(build_error{"variance_strike", "must be positive"});
    if (terms_.notional.get() <= decimal{})
        return std::unexpected(build_error{"notional", "must be positive"});
    domain::equity_variance_swap_instrument row{};
    row.identity.trade_type_code = std::string(trade_type);
    row.underlying_name = terms_.underlying.get().uri();
    row.currency = terms_.currency.get();
    row.notional = terms_.notional.get();
    row.variance_strike = terms_.variance_strike.get();
    row.start_date = terms_.start.get();
    row.maturity_date = terms_.maturity.get();
    row.long_short = long_short_of(terms_.position.get());
    row.moment_type = "Variance";
    domain::equity_instrument_data out;
    out.instrument = std::move(row);
    return out;
}

equity_accumulator_builder::equity_accumulator_builder(equity_accumulator_terms terms)
    : terms_(std::move(terms)) {}

equity_result equity_accumulator_builder::build() const {
    if (auto refusal = equity_fixing_refusal(terms_.strike.get(),
                                             terms_.fixing_amount.get(),
                                             terms_.start.get(),
                                             terms_.expiry.get()))
        return std::unexpected(std::move(*refusal));
    if (terms_.knock_out.get() <= terms_.strike.get())
        return std::unexpected(build_error{"knock_out", "must be above the strike"});
    domain::equity_accumulator_instrument row{};
    row.identity.trade_type_code = std::string(trade_type);
    row.underlying_name = terms_.underlying.get().uri();
    row.currency = terms_.currency.get();
    row.strike = terms_.strike.get();
    row.fixing_amount = terms_.fixing_amount.get();
    row.start_date = terms_.start.get();
    row.expiry_date = terms_.expiry.get();
    row.fixing_frequency = "Monthly";
    row.long_short = long_short_of(terms_.position.get());
    row.knock_out_level = terms_.knock_out.get();
    row.payoff_type = "Accumulator";
    domain::equity_instrument_data out;
    out.instrument = std::move(row);
    out.options.push_back(
        equity_detail::option_row(terms_.position.get(), std::nullopt, std::string("Accumulator")));
    out.schedules.push_back(
        equity_schedule("observation_schedule", "1M", terms_.start.get(), terms_.expiry.get()));
    out.range_bounds = equity_strike_bounds(terms_.strike.get());
    equity_add_barrier(out, "UpAndOut", terms_.knock_out.get());
    equity_add_underlying(out, terms_.underlying.get().uri());
    return out;
}

equity_tarf_builder::equity_tarf_builder(equity_tarf_terms terms)
    : terms_(std::move(terms)) {}

equity_result equity_tarf_builder::build() const {
    if (auto refusal = equity_fixing_refusal(terms_.strike.get(),
                                             terms_.fixing_amount.get(),
                                             terms_.start.get(),
                                             terms_.expiry.get()))
        return std::unexpected(std::move(*refusal));
    if (terms_.target_amount.get() <= decimal{})
        return std::unexpected(build_error{"target_amount", "must be positive"});
    domain::equity_accumulator_instrument row{};
    row.identity.trade_type_code = std::string(trade_type);
    row.underlying_name = terms_.underlying.get().uri();
    row.currency = terms_.currency.get();
    row.strike = terms_.strike.get();
    row.fixing_amount = terms_.fixing_amount.get();
    row.start_date = terms_.start.get();
    row.expiry_date = terms_.expiry.get();
    row.fixing_frequency = "Monthly";
    row.long_short = long_short_of(terms_.position.get());
    row.target_amount = terms_.target_amount.get();
    row.target_type = "TargetExact";
    row.payoff_type = "TargetExact";
    domain::equity_instrument_data out;
    out.instrument = std::move(row);
    out.options.push_back(
        equity_detail::option_row(terms_.position.get(), std::nullopt, std::string("TargetExact")));
    out.schedules.push_back(
        equity_schedule("product_schedule", "1M", terms_.start.get(), terms_.expiry.get()));
    out.range_bounds = equity_strike_bounds(terms_.strike.get());
    equity_add_barrier(out, "CumulatedProfitCap", terms_.target_amount.get());
    equity_add_underlying(out, terms_.underlying.get().uri());
    return out;
}

namespace {

equity_result equity_position_of(std::string_view trade_type, const equity_position_terms& terms) {
    if (terms.currency.get().empty())
        return std::unexpected(build_error{"currency", "must not be empty"});
    if (!(terms.quantity.get() > 0.0))
        return std::unexpected(build_error{"quantity", "must be positive"});
    if (terms.price.get() <= decimal{})
        return std::unexpected(build_error{"price", "must be positive"});
    domain::equity_position_instrument row{};
    row.identity.trade_type_code = std::string(trade_type);
    row.underlying_name = terms.underlying.get().uri();
    row.currency = terms.currency.get();
    row.quantity = terms.quantity.get();
    row.price = terms.price.get();
    domain::equity_instrument_data out;
    out.instrument = std::move(row);
    return out;
}

}

equity_position_builder::equity_position_builder(equity_position_terms terms)
    : terms_(std::move(terms)) {}

equity_result equity_position_builder::build() const {
    return equity_position_of(trade_type, terms_);
}

equity_option_position_builder::equity_option_position_builder(equity_option_position_terms terms)
    : terms_(std::move(terms)) {}

equity_result equity_option_position_builder::build() const {
    if (terms_.currency.get().empty())
        return std::unexpected(build_error{"currency", "must not be empty"});
    if (!(terms_.quantity.get() > 0.0))
        return std::unexpected(build_error{"quantity", "must be positive"});
    if (terms_.price.get() <= decimal{})
        return std::unexpected(build_error{"price", "must be positive"});
    const auto& entries = terms_.entries.get();
    if (entries.empty())
        return std::unexpected(
            build_error{"entries", "must name at least one underlying for the position to hold"});

    domain::equity_position_instrument row{};
    row.identity.trade_type_code = std::string(trade_type);
    row.currency = terms_.currency.get();
    row.quantity = terms_.quantity.get();
    row.price = terms_.price.get();
    domain::equity_instrument_data out;
    /*
     * The store refuses a position that names no underlying on its own row, and
     * the document's first entry is the name that row carries. The projection
     * writes the entries and never this name, so the two cannot disagree.
     */
    row.underlying_name = underlying_name_of(entries.front().underlying.get());
    out.instrument = std::move(row);

    int number = 0;
    for (const auto& entry : entries) {
        domain::equity_position_option_underlying underlying{};
        underlying.sequence_number = ++number;
        underlying.underlying_name = underlying_name_of(entry.underlying.get());
        underlying.strike = entry.strike.get();
        underlying.long_short = entry.long_short.get();
        underlying.option_type = entry.option_type;
        underlying.exercise_type = entry.exercise_type;
        underlying.settlement_type = entry.settlement_type;
        underlying.exercise_date = entry.exercise_date;
        underlying.weight = entry.weight;
        out.underlyings.push_back(std::move(underlying));
    }
    return out;
}

}
