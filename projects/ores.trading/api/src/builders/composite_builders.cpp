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
#include "ores.trading.api/builders/composite_builders.hpp"
#include "refusals.hpp"
#include <utility>

namespace ores::trading::builders {

namespace {

using utility::decimal::decimal;

domain::composite_instrument header_of(std::string_view trade_type) {
    domain::composite_instrument row{};
    row.identity.trade_type_code = std::string(trade_type);
    return row;
}

std::optional<build_error> refusal_of(const total_return_terms& terms) {
    if (terms.currency.get().empty())
        return build_error{"currency", "must name a currency"};
    if (terms.initial_price.get() <= decimal{})
        return build_error{"initial_price", "must be positive"};
    return refusals::period(terms.start.get(), terms.end.get());
}

/*
 * The return leg's terms and its schedule, which runs from the start to the
 * end in one period: ORE measures the return over the whole of it.
 */
domain::composite_instrument_data return_leg_of(std::string_view trade_type,
                                                const total_return_terms& terms) {
    domain::composite_instrument_data out{header_of(trade_type)};
    auto& row = out.instrument;
    row.return_payer = terms.return_side.get() == side::pay ? "true" : "false";
    row.return_currency = terms.currency.get();
    row.return_initial_price = terms.initial_price.get();
    row.return_initial_price_currency = terms.currency.get();

    domain::instrument_schedule schedule{};
    schedule.owner_role = "trs";
    schedule.owner_number = 1;
    schedule.schedule_role = "return_schedule";
    schedule.sequence_number = 1;
    schedule.schedule_kind = "rules";
    schedule.start_date = terms.start.get();
    schedule.end_date = terms.end.get();
    schedule.tenor = "0D";
    schedule.calendar = "TARGET";
    schedule.convention = "F";
    schedule.rule = "Forward";
    out.schedules.push_back(std::move(schedule));
    return out;
}

}

composite_trade_builder::composite_trade_builder(composite_trade_terms terms)
    : terms_(std::move(terms)) {}

composite_result composite_trade_builder::build() const {
    if (terms_.currency.get().empty())
        return std::unexpected(build_error{"currency", "must name a currency"});
    if (terms_.notional_calculation.get().empty())
        return std::unexpected(build_error{"notional_calculation", "must name a calculation"});
    domain::composite_instrument_data out{header_of(trade_type)};
    out.instrument.currency = terms_.currency.get();
    out.instrument.notional_calculation = terms_.notional_calculation.get();
    return out;
}

contract_for_difference_builder::contract_for_difference_builder(total_return_terms terms)
    : terms_(std::move(terms)) {}

composite_result contract_for_difference_builder::build() const {
    if (auto refusal = refusal_of(terms_))
        return std::unexpected(std::move(*refusal));
    return return_leg_of(trade_type, terms_);
}

total_return_swap_builder::total_return_swap_builder(
    const marketdata::conventions::convention_lookup& conventions, total_return_swap_terms terms)
    : conventions_(conventions)
    , terms_(std::move(terms)) {}

composite_result total_return_swap_builder::build() const {
    const auto& returns = terms_.return_leg;
    if (auto refusal = refusal_of(returns))
        return std::unexpected(std::move(*refusal));

    // The swap's fixed leg pays on the side that pays the return, so its
    // floating leg, kept as the funding leg, pays on the other.
    const auto swap = vanilla_swap_builder(conventions_,
                                           {.notional = terms_.funding_notional.get(),
                                            .currency = returns.currency.get(),
                                            .start = returns.start.get(),
                                            .maturity = returns.end.get(),
                                            .fixed_rate = decimal{},
                                            .index = terms_.funding_index.get(),
                                            .fixed_side = returns.return_side.get()})
                          .build();
    if (!swap)
        return std::unexpected(build_error{"funding_" + swap.error().term, swap.error().message});

    auto out = return_leg_of(trade_type, returns);
    constexpr int floating_leg = 2;
    for (auto leg : swap->legs)
        if (leg.identity.leg_number == floating_leg) {
            leg.identity.leg_number = 1;
            out.legs.push_back(std::move(leg));
        }
    for (auto amount : swap->leg_amounts)
        if (amount.leg_number == floating_leg) {
            amount.leg_number = 1;
            out.leg_amounts.push_back(std::move(amount));
        }
    for (auto rate : swap->leg_rates)
        if (rate.leg_number == floating_leg) {
            rate.leg_number = 1;
            out.leg_rates.push_back(std::move(rate));
        }
    for (auto schedule : swap->schedules)
        if (schedule.owner_role == "swap" && schedule.owner_number == floating_leg) {
            schedule.owner_number = 1;
            out.schedules.push_back(std::move(schedule));
        }
    if (out.legs.size() != 1)
        return std::unexpected(build_error{"funding_index", "states no floating leg"});
    return out;
}

}
