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
#include "ores.trading.api/builders/rate_builders.hpp"
#include "leg_rows.hpp"
#include <algorithm>
#include <array>
#include <utility>
#include <variant>

namespace ores::trading::builders {

namespace {

using marketdata::conventions::convention_lookup;
using utility::decimal::decimal;

/// A refusal of a wrapped swap names the role the swap plays, so the term is unambiguous.
build_error within(std::string_view role, build_error error) {
    return {std::string(role) + "." + error.term, std::move(error.message)};
}

swap_result swap_of(const convention_lookup& conventions, const vanilla_swap_terms& terms) {
    return vanilla_swap_builder(conventions, terms).build();
}

/*
 * The rows of one leg of @p from, renumbered as leg @p to of @p into. A leg is
 * its row, its amounts, its rates and its schedules, which all carry its number.
 */
void take_leg(domain::swap_instrument_data& into,
              const domain::swap_instrument_data& from,
              int leg,
              int to) {
    for (auto row : from.legs)
        if (row.identity.leg_number == leg) {
            row.identity.leg_number = to;
            into.legs.push_back(std::move(row));
        }
    for (auto row : from.leg_amounts)
        if (row.leg_number == leg) {
            row.leg_number = to;
            into.leg_amounts.push_back(std::move(row));
        }
    for (auto row : from.leg_rates)
        if (row.leg_number == leg) {
            row.leg_number = to;
            into.leg_rates.push_back(std::move(row));
        }
    for (auto row : from.schedules)
        if (row.owner_role == "swap" && row.owner_number == leg) {
            row.owner_number = to;
            into.schedules.push_back(std::move(row));
        }
}

domain::instrument_option option_of(option_position position,
                                    std::string style,
                                    std::string settles,
                                    std::optional<std::string> method = {}) {
    domain::instrument_option option{};
    option.long_short = long_short_of(position);
    option.style = std::move(style);
    option.settlement = std::move(settles);
    option.settlement_method = std::move(method);
    return option;
}

void add_exercise_dates(domain::swap_instrument_data& out,
                        const std::vector<std::chrono::year_month_day>& dates) {
    domain::instrument_schedule exercise{};
    exercise.owner_role = "option";
    exercise.owner_number = 1;
    exercise.schedule_role = "exercise_dates";
    exercise.sequence_number = 1;
    exercise.schedule_kind = "dates";
    out.schedules.push_back(std::move(exercise));
    int number = 0;
    for (const auto& when : dates) {
        domain::instrument_schedule_date date{};
        date.owner_role = "option";
        date.owner_number = 1;
        date.schedule_role = "exercise_dates";
        date.schedule_sequence_number = 1;
        date.sequence_number = ++number;
        date.schedule_date = when;
        out.schedule_dates.push_back(std::move(date));
    }
}

constexpr std::array<std::string_view, 4> barrier_types{
    "UpAndOut", "DownAndOut", "UpAndIn", "DownAndIn"};

}

cross_currency_swap_builder::cross_currency_swap_builder(conventions_ref conventions,
                                                         cross_currency_swap_terms terms)
    : conventions_(conventions)
    , terms_(std::move(terms)) {}

swap_result cross_currency_swap_builder::build() const {
    const auto& domestic_terms = terms_.domestic;
    if (terms_.foreign_currency.get() == domestic_terms.currency.get())
        return std::unexpected(
            build_error{"foreign_currency", "must differ from the domestic currency"});
    if (terms_.foreign_notional.get() <= decimal{})
        return std::unexpected(build_error{"foreign_notional", "must be positive"});

    const auto domestic = swap_of(conventions_, domestic_terms);
    if (!domestic)
        return std::unexpected(within("domestic", domestic.error()));
    const vanilla_swap_terms foreign_terms{.notional = terms_.foreign_notional.get(),
                                           .currency = terms_.foreign_currency.get(),
                                           .start = domestic_terms.start.get(),
                                           .maturity = domestic_terms.maturity.get(),
                                           .fixed_rate = domestic_terms.fixed_rate.get(),
                                           .index = terms_.foreign_index.get(),
                                           .fixed_side = domestic_terms.fixed_side.get(),
                                           .convention = terms_.foreign_convention};
    const auto foreign = swap_of(conventions_, foreign_terms);
    if (!foreign)
        return std::unexpected(within("foreign", foreign.error()));

    // The fixed leg is the domestic swap's and the floating leg the foreign one's.
    domain::swap_instrument_data out{};
    out.header = domestic->header;
    out.facts = domain::vanilla_swap_instrument{};
    take_leg(out, *domestic, 1, 1);
    take_leg(out, *foreign, 2, 2);
    for (auto& leg : out.legs) {
        leg.notional_initial_exchange = true;
        leg.notional_final_exchange = true;
    }
    return out;
}

forward_rate_agreement_builder::forward_rate_agreement_builder(conventions_ref conventions,
                                                               forward_rate_agreement_terms terms)
    : conventions_(conventions)
    , terms_(std::move(terms)) {}

swap_result forward_rate_agreement_builder::build() const {
    const auto& period = terms_.period;
    const auto definition = conventions_.index_definition_of(period.index.get());
    if (!definition)
        return std::unexpected(
            build_error{"index", definition.error().reference + ": " + definition.error().reason});
    if (!std::holds_alternative<refdata::domain::ibor_index_convention>(*definition))
        return std::unexpected(
            build_error{"index", "a forward rate agreement needs an Ibor index"});

    auto out = swap_of(conventions_, period);
    if (!out)
        return out;
    domain::fra_instrument fact{};
    fact.currency = period.currency.get();
    fact.rate_index = period.index.get().uri();
    fact.long_short = long_short_of(terms_.position.get());
    fact.strike = period.fixed_rate.get();
    fact.notional = period.notional.get();
    out->facts = std::move(fact);
    return out;
}

cap_floor_builder::cap_floor_builder(conventions_ref conventions, cap_floor_terms terms)
    : conventions_(conventions)
    , terms_(std::move(terms)) {}

swap_result cap_floor_builder::build() const {
    const auto swap = swap_of(conventions_, terms_.swap);
    if (!swap)
        return std::unexpected(swap.error());

    // A cap or a floor pays on the floating leg alone.
    domain::swap_instrument_data out{};
    out.header = swap->header;
    domain::cap_floor_instrument fact{};
    fact.long_short = long_short_of(terms_.position.get());
    out.facts = std::move(fact);
    take_leg(out, *swap, 2, 1);
    out.legs.front().payer = false;

    domain::instrument_strike strike{};
    strike.strike_role = terms_.kind.get() == cap_floor_kind::cap ? "cap" : "floor";
    strike.sequence_number = 1;
    strike.bare_value = terms_.swap.fixed_rate.get();
    strike.bare_currency = terms_.swap.currency.get();
    out.strikes.push_back(std::move(strike));
    return out;
}

swaption_builder::swaption_builder(conventions_ref conventions, swaption_terms terms)
    : conventions_(conventions)
    , terms_(std::move(terms)) {}

swap_result swaption_builder::build() const {
    if (terms_.expiry.get() > terms_.swap.start.get())
        return std::unexpected(build_error{"expiry", "must not be after the swap starts"});
    auto out = swap_of(conventions_, terms_.swap);
    if (!out)
        return out;

    const bool cash = terms_.settles.get() == settlement::cash;
    domain::swaption_instrument fact{};
    fact.expiry_date = terms_.expiry.get();
    fact.exercise_type = "European";
    fact.settlement_type = cash ? "Cash" : "Physical";
    fact.long_short = long_short_of(terms_.position.get());
    out->facts = std::move(fact);
    // A cash settled swaption states how the cash is computed; ORE's samples use the par yield
    // curve.
    out->options.push_back(
        option_of(terms_.position.get(),
                  "European",
                  cash ? "Cash" : "Physical",
                  cash ? std::optional<std::string>("ParYieldCurve") : std::nullopt));
    add_exercise_dates(*out, {terms_.expiry.get()});
    return out;
}

callable_swap_builder::callable_swap_builder(conventions_ref conventions, callable_swap_terms terms)
    : conventions_(conventions)
    , terms_(std::move(terms)) {}

swap_result callable_swap_builder::build() const {
    const auto first = terms_.first_call.get();
    if (first <= terms_.swap.start.get() || first >= terms_.swap.maturity.get())
        return std::unexpected(
            build_error{"first_call", "must be after the start and before the maturity"});
    auto out = swap_of(conventions_, terms_.swap);
    if (!out)
        return out;

    out->facts = domain::callable_swap_instrument{};
    out->options.push_back(option_of(terms_.position.get(), "Bermudan", "Physical"));
    // The calls fall once a year on the first call's anniversary, whatever the
    // legs pay; an anniversary of 29 February falls on the last day of February.
    int number = 0;
    for (auto years = std::chrono::years{0};; years += std::chrono::years{1}) {
        auto call = first + years;
        if (!call.ok())
            call = call.year() / call.month() / std::chrono::last;
        if (call >= terms_.swap.maturity.get())
            break;
        domain::callable_swap_call_date row{};
        row.sequence_number = ++number;
        row.call_date = call;
        out->call_dates.push_back(std::move(row));
    }
    return out;
}

knock_out_swap_builder::knock_out_swap_builder(conventions_ref conventions,
                                               knock_out_swap_terms terms)
    : conventions_(conventions)
    , terms_(std::move(terms)) {}

swap_result knock_out_swap_builder::build() const {
    const auto& type = terms_.barrier_type.get();
    if (std::ranges::find(barrier_types, type) == barrier_types.end())
        return std::unexpected(build_error{"barrier_type", "must be one of ORE's barrier types"});
    if (terms_.barrier_level.get() <= decimal{})
        return std::unexpected(build_error{"barrier_level", "must be positive"});
    const auto barrier_start = terms_.barrier_start.get();
    if (barrier_start < terms_.swap.start.get() || barrier_start >= terms_.swap.maturity.get())
        return std::unexpected(build_error{"barrier_start", "must be within the swap's life"});
    auto out = swap_of(conventions_, terms_.swap);
    if (!out)
        return out;

    domain::knock_out_swap_instrument fact{};
    fact.barrier_type = type;
    fact.barrier_level = terms_.barrier_level.get();
    fact.barrier_start_date = barrier_start;
    out->facts = std::move(fact);
    return out;
}

flexi_swap_builder::flexi_swap_builder(conventions_ref conventions, flexi_swap_terms terms)
    : conventions_(conventions)
    , terms_(std::move(terms)) {}

swap_result flexi_swap_builder::build() const {
    auto out = swap_of(conventions_, terms_.swap);
    if (!out)
        return out;
    domain::flexi_swap_instrument fact{};
    fact.option_long_short = long_short_of(terms_.position.get());
    out->facts = std::move(fact);
    return out;
}

balance_guaranteed_swap_builder::balance_guaranteed_swap_builder(
    conventions_ref conventions, balance_guaranteed_swap_terms terms)
    : conventions_(conventions)
    , terms_(std::move(terms)) {}

swap_result balance_guaranteed_swap_builder::build() const {
    const auto& security = terms_.reference_security.get();
    if (security.empty())
        return std::unexpected(build_error{"reference_security", "must not be empty"});
    auto out = swap_of(conventions_, terms_.swap);
    if (!out)
        return out;

    domain::balance_guaranteed_swap_instrument fact{};
    fact.reference_security = security;
    out->facts = std::move(fact);

    // One tranche, the whole security, amortising on the fixed leg's schedule.
    domain::balance_guaranteed_swap_tranche tranche{};
    tranche.sequence_number = 1;
    tranche.security_id = security;
    tranche.seniority = 1;
    out->tranches.push_back(std::move(tranche));
    domain::balance_guaranteed_swap_tranche_notional notional{};
    notional.tranche_number = 1;
    notional.sequence_number = 1;
    notional.notional = terms_.swap.notional.get();
    out->tranche_notionals.push_back(std::move(notional));
    auto schedule = out->schedules.front();
    schedule.owner_role = "tranches";
    schedule.owner_number = 1;
    out->schedules.push_back(std::move(schedule));
    return out;
}

inflation_swap_builder::inflation_swap_builder(conventions_ref conventions,
                                               inflation_swap_terms terms)
    : conventions_(conventions)
    , terms_(std::move(terms)) {}

swap_result inflation_swap_builder::build() const {
    if (terms_.notional.get() <= decimal{})
        return std::unexpected(build_error{"notional", "must be positive"});
    if (terms_.currency.get().empty())
        return std::unexpected(build_error{"currency", "must not be empty"});
    if (terms_.maturity.get() <= terms_.start.get())
        return std::unexpected(build_error{"maturity", "must be after the start"});
    if (!(terms_.base_cpi.get() > 0.0))
        return std::unexpected(build_error{"base_cpi", "must be positive"});
    const auto convention = conventions_.inflation_swap_convention_of(terms_.index.get());
    if (!convention)
        return std::unexpected(
            build_error{"index", convention.error().reference + ": " + convention.error().reason});

    const auto start = terms_.start.get();
    const auto maturity = terms_.maturity.get();
    const bool fixed_pays = terms_.fixed_side.get() == side::pay;
    const auto leg_of = [&](int number, bool payer, std::string type) {
        auto row = rows::swap_leg(number, payer, std::move(type), terms_.currency.get());
        row.day_count_fraction_code = convention->day_count_fraction;
        row.business_day_convention_code = convention->fix_convention;
        row.payment_frequency_code = "Once";
        return row;
    };
    const auto schedule_of = [&](int number, std::string kind) {
        domain::instrument_schedule row{};
        row.owner_role = "swap";
        row.owner_number = number;
        row.schedule_role = "schedule";
        row.sequence_number = 1;
        row.schedule_kind = std::move(kind);
        row.calendar = convention->fix_calendar;
        row.convention = convention->fix_convention;
        return row;
    };

    domain::swap_instrument_data out{};
    out.header.start_date = start;
    out.header.maturity_date = maturity;
    domain::inflation_swap_instrument fact{};
    fact.inflation_index_code = terms_.index.get().uri();
    fact.base_cpi = terms_.base_cpi.get();
    fact.lag_convention = convention->observation_lag;
    out.facts = std::move(fact);

    // The fixed side compounds its rate over the whole life and pays once.
    auto fixed = leg_of(1, fixed_pays, "ZeroCouponFixed");
    fixed.zero_coupon_compounding = "Compounded";
    out.legs.push_back(std::move(fixed));
    out.leg_amounts.push_back(rows::leg_amount(1, terms_.notional.get()));
    out.leg_rates.push_back(rows::leg_rate(1, "fixed", terms_.fixed_rate.get()));
    auto fixed_schedule = schedule_of(1, "rules");
    fixed_schedule.start_date = start;
    fixed_schedule.end_date = maturity;
    fixed_schedule.tenor = "0D";
    fixed_schedule.rule = "Zero";
    out.schedules.push_back(std::move(fixed_schedule));

    // The CPI side pays the index's growth on the notional at maturity alone.
    auto cpi = leg_of(2, !fixed_pays, "CPI");
    cpi.floating_index_code = terms_.index.get().uri();
    cpi.cpi_base = terms_.base_cpi.get();
    cpi.cpi_start_date = start;
    cpi.observation_lag = convention->observation_lag;
    cpi.cpi_interpolation = convention->interpolated ? "Linear" : "Flat";
    cpi.subtract_inflation_notional = true;
    cpi.notional_final_exchange = true;
    out.legs.push_back(std::move(cpi));
    out.leg_amounts.push_back(rows::leg_amount(2, terms_.notional.get()));
    out.leg_rates.push_back(rows::leg_rate(2, "fixed", decimal{}));
    out.schedules.push_back(schedule_of(2, "dates"));
    domain::instrument_schedule_date payment{};
    payment.owner_role = "swap";
    payment.owner_number = 2;
    payment.schedule_role = "schedule";
    payment.schedule_sequence_number = 1;
    payment.sequence_number = 1;
    payment.schedule_date = maturity;
    out.schedule_dates.push_back(std::move(payment));
    return out;
}

}
