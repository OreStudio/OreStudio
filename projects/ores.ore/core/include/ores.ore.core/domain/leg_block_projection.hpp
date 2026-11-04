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
#ifndef ORES_ORE_CORE_DOMAIN_LEG_BLOCK_PROJECTION_HPP
#define ORES_ORE_CORE_DOMAIN_LEG_BLOCK_PROJECTION_HPP

#include "ores.ore.core/domain/option_block_projection.hpp"
#include "ores.ore.core/domain/payment_frequency_conversion.hpp"
#include "ores.trading.api/domain/swap_leg.hpp"
#include "ores.trading.api/domain/swap_leg_amortization.hpp"
#include "ores.trading.api/domain/swap_leg_amount.hpp"
#include "ores.trading.api/domain/swap_leg_rate.hpp"
#include <algorithm>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

/**
 * @file leg_block_projection.hpp
 * @brief ORE's LegData block, read into and written from the shared swap leg
 * rows, for any family whose composite holds them.
 *
 * A composite reads and writes legs when it holds the vectors the rows land
 * in: legs, leg_amounts, leg_rates, leg_amortizations, schedules and
 * schedule_dates. ORE states a leg the same way in a swap, a commodity swap,
 * an equity swap and a total return swap's funding, so one reader and one
 * writer serve them all.
 */

namespace ores::ore::domain::leg_block {

using ores::utility::decimal::decimal;
using trading::domain::swap_leg;
using trading::domain::swap_leg_amortization;
using trading::domain::swap_leg_amount;
using trading::domain::swap_leg_rate;
using xml::record_reader;
using xml::record_writer;

using option_block::decimal_of;
using option_block::decimal_text;
using option_block::flag_text;
using option_block::is_flag_on;
using option_block::set_spelling_or;
using option_block::spelling_of;
using option_block::stamp_row;
using option_block::text_of;
using option_block::to_domain_date;
using option_block::to_optional_domain_date;
using option_block::to_ore_date;

inline void stamp_leg(swap_leg& row) {
    row.audit.modified_by = std::string(option_block::block_actor);
    row.audit.performed_by = std::string(option_block::block_actor);
    row.audit.change_reason_code = std::string(option_block::block_change_reason);
    row.audit.change_commentary = std::string(option_block::block_change_commentary);
}

/// A flag the element states, or nothing when it states none.
inline std::optional<bool> optional_flag(const record_reader& r, std::string_view name) {
    if (!r.has(name))
        return std::nullopt;
    return is_flag_on(text_of(r, name));
}

/// The first schedule rule's tenor, which is the leg's payment frequency.
inline auto first_tenor(const std::optional<record_reader>& schedule) -> std::string {
    if (!schedule)
        return {};
    const auto rules = schedule->children("Rules");
    if (rules.empty())
        return {};
    return text_of(rules.front(), "Tenor");
}

inline swap_leg read_leg(const record_reader& leg, int leg_number) {
    swap_leg row;
    row.identity.leg_number = leg_number;
    row.payer = is_flag_on(text_of(leg, "Payer"));
    row.leg_type_code = spelling_of(leg, "LegType");
    row.currency = text_of(leg, "Currency");
    row.day_count_fraction_code = spelling_of(leg, "DayCounter");
    row.business_day_convention_code = spelling_of(leg, "PaymentConvention");
    row.payment_frequency_code = tenor_to_payment_frequency(first_tenor(leg.child("ScheduleData")));
    if (leg.has("PaymentLag"))
        row.payment_lag = text_of(leg, "PaymentLag");
    if (leg.has("PaymentCalendar"))
        row.payment_calendar = text_of(leg, "PaymentCalendar");
    if (const auto notionals = leg.child("Notionals")) {
        if (const auto all = notionals->children("Exchanges"); !all.empty()) {
            const auto& exchanges = all.front();
            const auto spelled = [&](std::string_view name) -> std::optional<bool> {
                const auto value = exchanges.spelling(name);
                if (!value)
                    return std::nullopt;
                return is_flag_on(*value);
            };
            row.notional_initial_exchange = spelled("NotionalInitialExchange");
            row.notional_final_exchange = spelled("NotionalFinalExchange");
        }
    }
    if (const auto floating = leg.child("FloatingLegData")) {
        row.floating_index_code = text_of(*floating, "Index");
        if (const auto days = floating->integer("FixingDays"))
            row.fixing_days = static_cast<int>(*days);
        row.is_in_arrears = optional_flag(*floating, "IsInArrears");
        row.include_spread = optional_flag(*floating, "IncludeSpread");
        row.is_averaged = optional_flag(*floating, "IsAveraged");
        row.has_sub_periods = optional_flag(*floating, "HasSubPeriods");
    }
    stamp_leg(row);
    return row;
}

inline std::vector<swap_leg_amount> read_leg_amounts(const record_reader& leg, int leg_number) {
    std::vector<swap_leg_amount> amounts;
    const auto append = [&](const decimal& value, const std::string& start_date) {
        swap_leg_amount row;
        row.leg_number = leg_number;
        row.sequence_number = static_cast<int>(amounts.size()) + 1;
        row.amount = value;
        if (!start_date.empty())
            row.start_date = to_domain_date(start_date);
        stamp_row(row);
        amounts.push_back(std::move(row));
    };

    // The document states its notional as a list, and a cashflow leg states its
    // amounts as dated payments. Both arms land in the same column. An
    // amortization schedule is a rule over the notional, not an amount, so it
    // keeps rows of its own.
    if (const auto notionals = leg.child("Notionals")) {
        for (const auto& notional : notionals->children("Notional"))
            append(decimal_text(notional), text_of(notional, "startDate"));
    } else if (const auto cashflow_data = leg.child("CashflowData")) {
        if (const auto cashflow = cashflow_data->child("Cashflow"))
            for (const auto& amount : cashflow->children("Amount"))
                append(decimal_text(amount), text_of(amount, "date"));
    }
    return amounts;
}

inline std::vector<swap_leg_amortization> read_leg_amortizations(const record_reader& leg,
                                                                 int leg_number) {
    std::vector<swap_leg_amortization> steps;
    const auto amortizations = leg.child("Amortizations");
    if (!amortizations)
        return steps;
    for (const auto& step : amortizations->children("AmortizationData")) {
        swap_leg_amortization row;
        row.leg_number = leg_number;
        row.sequence_number = static_cast<int>(steps.size()) + 1;
        row.amortization_type = spelling_of(step, "Type");
        if (step.has("Value"))
            row.value = decimal_of(step, "Value");
        row.start_date = to_optional_domain_date(text_of(step, "StartDate"));
        row.end_date = to_optional_domain_date(text_of(step, "EndDate"));
        if (step.has("Frequency"))
            row.frequency = text_of(step, "Frequency");
        row.underflow = optional_flag(step, "Underflow");
        stamp_row(row);
        steps.push_back(std::move(row));
    }
    return steps;
}

/// The schedules a floating leg states, each as its list, its item and the
/// role a rate row keeps it under.
struct floating_rate_list {
    std::string_view element;
    std::string_view item;
    std::string_view role;
};

inline constexpr floating_rate_list floating_rate_lists[] = {
    {"Spreads", "Spread", "spread"},
    {"Gearings", "Gearing", "gearing"},
    {"Caps", "Cap", "cap"},
    {"Floors", "Floor", "floor"},
};

/// Writes each schedule a floating leg's rows state, in the order the schema
/// gives them.
inline void write_floating_rates(record_writer& floating, const std::vector<swap_leg_rate>& rates) {
    for (const auto& [element, item, role] : floating_rate_lists) {
        const auto states = std::any_of(
            rates.begin(), rates.end(), [&](const auto& rate) { return rate.rate_role == role; });
        if (!states)
            continue;
        auto list = floating.child(element);
        for (const auto& rate : rates) {
            if (rate.rate_role != role)
                continue;
            auto writer = list.append(item);
            writer.set_own_text(rate.value.to_string());
            if (rate.start_date)
                writer.set_text("startDate", to_ore_date(*rate.start_date));
        }
    }
}

inline std::vector<swap_leg_rate> read_leg_rates(const record_reader& leg, int leg_number) {
    std::vector<swap_leg_rate> rates;
    const auto append = [&](std::string_view role, const record_reader& value) {
        swap_leg_rate row;
        row.leg_number = leg_number;
        row.rate_role = std::string(role);
        row.sequence_number = static_cast<int>(rates.size()) + 1;
        row.value = decimal_text(value);
        if (const auto start = text_of(value, "startDate"); !start.empty())
            row.start_date = to_domain_date(start);
        stamp_row(row);
        rates.push_back(std::move(row));
    };

    if (const auto fixed = leg.child("FixedLegData")) {
        if (const auto list = fixed->child("Rates"))
            for (const auto& rate : list->children("Rate"))
                append("fixed", rate);
    }
    if (const auto floating = leg.child("FloatingLegData")) {
        for (const auto& [element, item, role] : floating_rate_lists)
            if (const auto list = floating->child(element))
                for (const auto& value : list->children(item))
                    append(role, value);
    }
    return rates;
}

template <typename Data>
void append_leg(Data& result, const record_reader& leg, int leg_number) {
    result.legs.push_back(read_leg(leg, leg_number));
    for (auto& amount : read_leg_amounts(leg, leg_number))
        result.leg_amounts.push_back(std::move(amount));
    for (auto& rate : read_leg_rates(leg, leg_number))
        result.leg_rates.push_back(std::move(rate));
    for (auto& step : read_leg_amortizations(leg, leg_number))
        result.leg_amortizations.push_back(std::move(step));
    if (const auto schedule = leg.child("ScheduleData"))
        option_block::read_schedule(result, *schedule, "swap", leg_number, "schedule");
}

template <typename Data>
void append_legs(Data& result, const record_reader& data) {
    int leg_number = 1;
    for (const auto& leg : data.children("LegData"))
        append_leg(result, leg, leg_number++);
}

inline void write_schedule_rules(record_writer& schedule,
                                 const std::optional<std::chrono::year_month_day>& start_date,
                                 const std::optional<std::chrono::year_month_day>& end_date,
                                 const std::string& tenor) {
    auto rule = schedule.append("Rules");
    rule.set_text("StartDate", to_ore_date(start_date));
    if (end_date)
        rule.set_text("EndDate", to_ore_date(*end_date));
    rule.set_text("Tenor", tenor);
    rule.set_spelling("Convention", "MF", "business day convention");
}

template <typename Data>
std::vector<swap_leg_amount> amounts_for_leg(const Data& v, int leg_number) {
    std::vector<swap_leg_amount> out;
    for (const auto& row : v.leg_amounts)
        if (row.leg_number == leg_number)
            out.push_back(row);
    return out;
}

template <typename Data>
std::vector<swap_leg_rate> rates_for_leg(const Data& v, int leg_number) {
    std::vector<swap_leg_rate> out;
    for (const auto& row : v.leg_rates)
        if (row.leg_number == leg_number)
            out.push_back(row);
    return out;
}

/**
 * @brief Writes one leg from its rows.
 *
 * The start and maturity are the instrument's own dates: a leg stored before
 * its schedule rows existed falls back to them.
 */
template <typename Data>
void write_leg(record_writer& leg,
               const std::optional<std::chrono::year_month_day>& start_date,
               const std::optional<std::chrono::year_month_day>& maturity_date,
               const swap_leg& row,
               const Data& v) {
    const auto leg_number = row.identity.leg_number;
    const auto amounts = amounts_for_leg(v, leg_number);
    const auto rates = rates_for_leg(v, leg_number);

    leg.set_spelling("LegType", row.leg_type_code, "leg type");
    leg.set_text("Payer", flag_text(row.payer.value_or(false)));
    leg.set_text("Currency", row.currency);
    if (!row.day_count_fraction_code.empty())
        set_spelling_or(leg, "DayCounter", row.day_count_fraction_code, "A360", "day counter");
    if (!row.business_day_convention_code.empty())
        set_spelling_or(leg,
                        "PaymentConvention",
                        row.business_day_convention_code,
                        "MF",
                        "business day convention");
    if (row.payment_lag)
        leg.set_text("PaymentLag", *row.payment_lag);
    if (row.payment_calendar)
        leg.set_text("PaymentCalendar", *row.payment_calendar);

    // A cashflow leg has dated payments and no notional or schedule.
    if (row.leg_type_code == "Cashflow") {
        auto cashflow = leg.child("CashflowData").child("Cashflow");
        for (const auto& amount : amounts) {
            auto writer = cashflow.append("Amount");
            writer.set_own_text(amount.amount.to_string());
            if (amount.start_date)
                writer.set_text("date", to_ore_date(*amount.start_date));
        }
        return;
    }

    if (!amounts.empty()) {
        auto notionals = leg.child("Notionals");
        for (const auto& amount : amounts) {
            auto writer = notionals.append("Notional");
            writer.set_own_text(amount.amount.to_string());
            if (amount.start_date)
                writer.set_text("startDate", to_ore_date(*amount.start_date));
        }
    }
    if (row.notional_initial_exchange || row.notional_final_exchange) {
        auto exchanges = leg.child("Notionals").append("Exchanges");
        if (row.notional_initial_exchange)
            exchanges.set_spelling(
                "NotionalInitialExchange", flag_text(*row.notional_initial_exchange), "boolean");
        if (row.notional_final_exchange)
            exchanges.set_spelling(
                "NotionalFinalExchange", flag_text(*row.notional_final_exchange), "boolean");
    }

    std::optional<record_writer> amortizations;
    for (const auto& step : v.leg_amortizations) {
        if (step.leg_number != leg_number)
            continue;
        if (!amortizations)
            amortizations = leg.child("Amortizations");
        auto writer = amortizations->append("AmortizationData");
        set_spelling_or(writer, "Type", step.amortization_type, "FixedAmount", "amortization type");
        if (step.value)
            writer.set_decimal("Value", *step.value);
        if (step.start_date)
            writer.set_text("StartDate", to_ore_date(step.start_date));
        if (step.end_date)
            writer.set_text("EndDate", to_ore_date(step.end_date));
        if (step.frequency)
            writer.set_text("Frequency", *step.frequency);
        if (step.underflow)
            writer.set_text("Underflow", flag_text(*step.underflow));
    }

    auto schedule = leg.child("ScheduleData");
    if (!option_block::write_stored_schedule(schedule, v, "swap", leg_number, "schedule"))
        write_schedule_rules(schedule,
                             start_date,
                             maturity_date,
                             payment_frequency_to_tenor(row.payment_frequency_code));

    const auto has_role = [&](std::string_view role) {
        return std::any_of(rates.begin(), rates.end(), [&](const swap_leg_rate& rate) {
            return rate.rate_role == role;
        });
    };
    if (row.leg_type_code == "Fixed" && has_role("fixed")) {
        auto list = leg.child("FixedLegData").child("Rates");
        for (const auto& rate : rates)
            if (rate.rate_role == "fixed")
                list.append("Rate").set_own_text(rate.value.to_string());
    } else if (row.leg_type_code == "Floating") {
        auto floating = leg.child("FloatingLegData");
        floating.set_text("Index", row.floating_index_code);
        if (row.fixing_days)
            floating.set_integer("FixingDays", *row.fixing_days);
        const auto write_flag = [&floating](std::string_view name,
                                            const std::optional<bool>& flag) {
            if (flag)
                floating.set_text(name, flag_text(*flag));
        };
        write_flag("IsInArrears", row.is_in_arrears);
        write_flag("IncludeSpread", row.include_spread);
        write_flag("IsAveraged", row.is_averaged);
        write_flag("HasSubPeriods", row.has_sub_periods);
        write_floating_rates(floating, rates);
    }
}

template <typename Data>
void write_legs(record_writer& data,
                const std::optional<std::chrono::year_month_day>& start_date,
                const std::optional<std::chrono::year_month_day>& maturity_date,
                const Data& v) {
    for (const auto& leg : v.legs) {
        auto writer = data.append("LegData");
        write_leg(writer, start_date, maturity_date, leg, v);
    }
}

}

#endif
