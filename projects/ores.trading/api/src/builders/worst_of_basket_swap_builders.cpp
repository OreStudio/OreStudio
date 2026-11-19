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
#include "ores.trading.api/builders/worst_of_basket_swap_builders.hpp"
#include <optional>
#include <string>
#include <utility>

namespace ores::trading::builders {

namespace {

using utility::decimal::decimal;
using ymd = std::chrono::year_month_day;

std::optional<build_error> worst_of_positive(std::string term, const decimal& value) {
    if (value <= decimal{})
        return build_error{std::move(term), "must be positive"};
    return std::nullopt;
}

std::optional<build_error> refusal_of(const fx_worst_of_basket_swap_terms& terms) {
    if (terms.currency.get().empty())
        return build_error{"currency", "must name a currency"};
    if (terms.floating_index.get().empty())
        return build_error{"floating_index", "must name an index"};
    if (auto refusal = worst_of_positive("quantity", terms.quantity.get()))
        return refusal;
    const auto& members = terms.members.get();
    if (members.size() < 2)
        return build_error{"members", "a basket names at least two pairs"};
    for (std::size_t i = 0; i < members.size(); ++i) {
        if (auto refusal = worst_of_positive("initial_price", members[i].initial_price))
            return refusal;
        for (std::size_t j = 0; j < i; ++j)
            if (members[j].underlying.uri() == members[i].underlying.uri())
                return build_error{"members", "names a pair twice"};
    }
    const auto& periods = terms.period_dates.get();
    if (periods.size() < 2)
        return build_error{"period_dates", "must bound at least one period"};
    for (std::size_t i = 1; i < periods.size(); ++i)
        if (!(periods[i - 1] < periods[i]))
            return build_error{"period_dates", "must be in increasing order"};
    const auto count = periods.size() - 1;
    const auto& pays = terms.pay_dates.get();
    if (pays.size() != count)
        return build_error{"pay_dates", "must state one date a period"};
    for (std::size_t i = 0; i < count; ++i)
        if (pays[i] < periods[i + 1])
            return build_error{"pay_dates", "must not be before the period ends"};
    if (terms.knock_out_levels.get().size() != count ||
        terms.fixed_trigger_levels.get().size() != count)
        return build_error{"knock_out_levels", "the level lists must state one level a period"};
    return std::nullopt;
}

void add_values(domain::fx_instrument_data& out,
                std::string_view role,
                const std::vector<decimal>& values) {
    int sequence = 0;
    for (const auto& value : values) {
        domain::instrument_value row{};
        row.value_role = std::string(role);
        row.sequence_number = ++sequence;
        row.value = value;
        out.values.push_back(std::move(row));
    }
}

domain::instrument_schedule schedule_row(std::string_view role, std::string_view kind) {
    domain::instrument_schedule row{};
    row.owner_role = "option";
    row.owner_number = 1;
    row.schedule_role = std::string(role);
    row.sequence_number = 1;
    row.schedule_kind = std::string(kind);
    return row;
}

/// A schedule stated as a list of dates, adjusted on TARGET by modified following.
void add_dated_schedule(domain::fx_instrument_data& out,
                        std::string_view role,
                        const std::vector<ymd>& dates) {
    auto schedule = schedule_row(role, "dates");
    schedule.calendar = "TARGET";
    schedule.convention = "MF";
    out.schedules.push_back(std::move(schedule));
    int sequence = 0;
    for (const auto& date : dates) {
        domain::instrument_schedule_date row{};
        row.owner_role = "option";
        row.owner_number = 1;
        row.schedule_role = std::string(role);
        row.schedule_sequence_number = 1;
        row.sequence_number = ++sequence;
        row.schedule_date = date;
        out.schedule_dates.push_back(std::move(row));
    }
}

}

fx_worst_of_basket_swap_builder::fx_worst_of_basket_swap_builder(
    fx_worst_of_basket_swap_terms terms)
    : terms_(std::move(terms)) {}

fx_result fx_worst_of_basket_swap_builder::build() const {
    const auto& terms = terms_;
    if (auto refusal = refusal_of(terms))
        return std::unexpected(std::move(*refusal));

    domain::fx_instrument_data out{};
    domain::fx_worst_of_basket_swap_instrument row{};
    row.identity.trade_type_code = std::string(trade_type);
    row.long_short = long_short_of(terms.position.get());
    row.currency = terms.currency.get();
    row.quantity = terms.quantity.get().to_double();
    row.initial_fixed_rate = terms.initial_fixed_rate.get();
    row.fixed_rate = terms.fixed_rate.get();
    row.bermudan_knock_in = false;
    row.knock_in_level = terms.knock_in_level.get();
    row.accumulating_fixed_coupons = false;
    row.accruing_fixed_coupons = false;
    row.floating_index = terms.floating_index.get();
    row.floating_spread = terms.floating_spread.get();
    row.floating_day_count_fraction = terms.floating_day_count.get();
    row.strike = terms.strike.get();
    out.worst_of_basket_swaps.push_back(std::move(row));

    std::vector<decimal> initial_prices;
    int sequence = 0;
    for (const auto& member : terms.members.get()) {
        domain::instrument_underlying underlying{};
        underlying.underlying_role = "underlying";
        underlying.sequence_number = ++sequence;
        underlying.underlying_type = "FX";
        underlying.name = member.underlying.uri();
        out.instrument_underlyings.push_back(std::move(underlying));
        initial_prices.push_back(member.initial_price);
    }
    add_values(out, "initial_price", initial_prices);
    add_values(out, "knock_out_level", terms.knock_out_levels.get());
    add_values(out, "fixed_trigger_level", terms.fixed_trigger_levels.get());

    add_dated_schedule(out, "floating_period_schedule", terms.period_dates.get());
    add_dated_schedule(out, "floating_pay_schedule", terms.pay_dates.get());
    // The floating leg fixes two business days before each period starts.
    auto fixing = schedule_row("floating_fixing_schedule", "derived");
    fixing.base_schedule = "FloatingPeriodSchedule";
    fixing.shift = "-2D";
    fixing.calendar = "TARGET";
    fixing.convention = "P";
    out.schedules.push_back(std::move(fixing));
    return out;
}

}
