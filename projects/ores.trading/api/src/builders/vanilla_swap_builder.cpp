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
#include "ores.trading.api/builders/vanilla_swap_builder.hpp"
#include "leg_rows.hpp"
#include "ores.trading.api/builders/cadence.hpp"
#include <optional>
#include <string_view>
#include <utility>
#include <variant>

namespace ores::trading::builders {

namespace {

using marketdata::conventions::lookup_error;

struct leg_conventions final {
    cadence period;
    std::string day_count;
    std::string calendar;
    std::string adjustment;
};

build_error index_error(const lookup_error& error) {
    return {"index", error.reference + ": " + error.reason};
}

/// An overnight index states no adjustment of its own, and accrues modified following.
template <typename Definition>
std::string adjustment_of(const Definition& definition) {
    if constexpr (requires { definition.business_day_convention; })
        return definition.business_day_convention;
    else
        return "MF";
}

/// An Ibor index name ends in its tenor, which is the floating leg's frequency.
std::string tenor_of(const std::string& index_name) {
    return index_name.substr(index_name.rfind('-') + 1);
}

domain::instrument_schedule
schedule_of(int leg_number, const vanilla_swap_terms& terms, const leg_conventions& leg) {
    domain::instrument_schedule row{};
    row.owner_role = "swap";
    row.owner_number = leg_number;
    row.schedule_role = "schedule";
    row.sequence_number = 1;
    row.schedule_kind = "rules";
    row.start_date = terms.start.get();
    row.end_date = terms.maturity.get();
    row.tenor = std::string(leg.period.tenor);
    row.calendar = leg.calendar;
    row.convention = leg.adjustment;
    row.term_convention = leg.adjustment;
    row.rule = "Forward";
    return row;
}

domain::swap_leg conventional_leg(int leg_number,
                                  bool payer,
                                  std::string type,
                                  const vanilla_swap_terms& terms,
                                  const leg_conventions& leg) {
    auto row = rows::swap_leg(leg_number, payer, std::move(type), terms.currency.get());
    row.day_count_fraction_code = leg.day_count;
    row.business_day_convention_code = leg.adjustment;
    row.payment_frequency_code = std::string(leg.period.frequency);
    return row;
}

}

vanilla_swap_builder::vanilla_swap_builder(
    const marketdata::conventions::convention_lookup& conventions, vanilla_swap_terms terms)
    : conventions_(conventions)
    , terms_(std::move(terms)) {}

vanilla_swap_builder& vanilla_swap_builder::fixed_day_count(std::string value) {
    fixed_day_count_ = std::move(value);
    return *this;
}

vanilla_swap_builder& vanilla_swap_builder::fixed_adjustment(std::string value) {
    fixed_adjustment_ = std::move(value);
    return *this;
}

std::expected<domain::swap_instrument_data, build_error> vanilla_swap_builder::build() const {
    if (terms_.notional.get() <= utility::decimal::decimal{})
        return std::unexpected(build_error{"notional", "must be positive"});
    if (terms_.currency.get().empty())
        return std::unexpected(build_error{"currency", "must not be empty"});
    if (terms_.maturity.get() <= terms_.start.get())
        return std::unexpected(build_error{"maturity", "must be after the start"});

    const auto swap = terms_.convention ? conventions_.swap_convention_by_id(*terms_.convention,
                                                                             terms_.index.get()) :
                                          conventions_.swap_convention_of(terms_.index.get());
    if (!swap)
        return std::unexpected(index_error(swap.error()));
    const auto index = conventions_.index_definition_of(terms_.index.get());
    if (!index)
        return std::unexpected(index_error(index.error()));

    const auto float_stated = swap->float_frequency.value_or(tenor_of(swap->index));
    const auto float_period = cadence_of(float_stated);
    if (!float_period)
        return std::unexpected(build_error{"index", swap->id + ": no tenor for " + float_stated});
    const auto fixed_period = cadence_of(swap->fixed_frequency);
    if (!fixed_period)
        return std::unexpected(
            build_error{"index", swap->id + ": no tenor for " + swap->fixed_frequency});

    const auto [floating, fixing_days] = std::visit(
        [&](const auto& definition) {
            return std::pair{leg_conventions{*float_period,
                                             definition.day_count_fraction,
                                             definition.fixing_calendar,
                                             adjustment_of(definition)},
                             definition.settlement_days};
        },
        *index);
    const leg_conventions fixed{
        *fixed_period,
        fixed_day_count_.value_or(swap->fixed_day_count_fraction),
        swap->fixed_calendar.value_or(floating.calendar),
        fixed_adjustment_.value_or(swap->fixed_convention.value_or(floating.adjustment))};

    const bool fixed_pays = terms_.fixed_side.get() == side::pay;
    domain::swap_instrument_data out{};
    out.header.start_date = terms_.start.get();
    out.header.maturity_date = terms_.maturity.get();
    out.facts = domain::vanilla_swap_instrument{};
    out.legs.push_back(conventional_leg(1, fixed_pays, "Fixed", terms_, fixed));
    auto floating_leg = conventional_leg(2, !fixed_pays, "Floating", terms_, floating);
    floating_leg.floating_index_code = terms_.index.get().uri();
    floating_leg.fixing_days = fixing_days;
    floating_leg.is_in_arrears = false;
    out.legs.push_back(std::move(floating_leg));
    out.leg_amounts = {rows::leg_amount(1, terms_.notional.get()),
                       rows::leg_amount(2, terms_.notional.get())};
    out.leg_rates = {rows::leg_rate(1, "fixed", terms_.fixed_rate.get()),
                     rows::leg_rate(2, "spread", utility::decimal::decimal{})};
    out.schedules = {schedule_of(1, terms_, fixed), schedule_of(2, terms_, floating)};
    return out;
}

}
