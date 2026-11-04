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
#include "ores.ore.core/domain/option_block_projection.hpp"
#include "ores.ore.core/domain/payment_frequency_conversion.hpp"
#include "ores.ore.core/domain/portfolio_instrument_projection.hpp"
#include "ores.ore.core/xml/binding.hpp"
#include "ores.platform/time/datetime.hpp"
#include <algorithm>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::ore::domain {

namespace {

using ores::utility::decimal::decimal;

using trading::domain::balance_guaranteed_swap_instrument;
using trading::domain::balance_guaranteed_swap_tranche;
using trading::domain::balance_guaranteed_swap_tranche_notional;
using trading::domain::callable_swap_call_date;
using trading::domain::callable_swap_instrument;
using trading::domain::cap_floor_instrument;
using trading::domain::flexi_swap_instrument;
using trading::domain::flexi_swap_lower_notional;
using trading::domain::fra_instrument;
using trading::domain::inflation_swap_instrument;
using trading::domain::instrument_option;
using trading::domain::instrument_option_exercise_fee;
using trading::domain::instrument_option_exercise_price;
using trading::domain::instrument_option_payment_date;
using trading::domain::instrument_option_premium;
using trading::domain::instrument_schedule;
using trading::domain::instrument_schedule_date;
using trading::domain::instrument_strike;
using trading::domain::knock_out_swap_instrument;
using trading::domain::rate_instrument;
using trading::domain::swap_instrument_data;
using trading::domain::swap_leg;
using trading::domain::swap_leg_amortization;
using trading::domain::swap_leg_amount;
using trading::domain::swap_leg_rate;
using trading::domain::swaption_instrument;
using trading::domain::vanilla_swap_instrument;

using xml::record_reader;
using xml::record_writer;

// ---------------------------------------------------------------------------
// The vocabulary both directions share
// ---------------------------------------------------------------------------

/// The actor and reason every imported row states.
constexpr std::string_view rate_instrument_projection_actor = "ores";
constexpr std::string_view rate_instrument_projection_change_reason = "system.external_data_import";
constexpr std::string_view rate_instrument_projection_change_commentary = "Imported from ORE XML";

/// The trade types the family states, which are the ones the old dispatcher took.
constexpr bool is_rate_trade_type(std::string_view type) {
    return type == "Swap" || type == "CrossCurrencySwap" || type == "ForwardRateAgreement" ||
           type == "CapFloor" || type == "Swaption" || type == "CallableSwap" ||
           type == "KnockOutSwap" || type == "InflationSwap" || type == "FlexiSwap" ||
           type == "BalanceGuaranteedSwap";
}

void rate_instrument_projection_stamp(trading::domain::swap_leg& row) {
    row.audit.modified_by = std::string(rate_instrument_projection_actor);
    row.audit.performed_by = std::string(rate_instrument_projection_actor);
    row.audit.change_reason_code = std::string(rate_instrument_projection_change_reason);
    row.audit.change_commentary = std::string(rate_instrument_projection_change_commentary);
}

template <typename Row>
void rate_instrument_projection_stamp_row(Row& row) {
    row.modified_by = std::string(rate_instrument_projection_actor);
    row.performed_by = std::string(rate_instrument_projection_actor);
    row.change_reason_code = std::string(rate_instrument_projection_change_reason);
    row.change_commentary = std::string(rate_instrument_projection_change_commentary);
}

auto rate_instrument_projection_make_header(std::string_view trade_type_code) -> rate_instrument {
    rate_instrument r;
    r.identity.trade_type_code = std::string(trade_type_code);
    r.audit.modified_by = std::string(rate_instrument_projection_actor);
    r.audit.performed_by = std::string(rate_instrument_projection_actor);
    r.audit.change_reason_code = std::string(rate_instrument_projection_change_reason);
    r.audit.change_commentary = std::string(rate_instrument_projection_change_commentary);
    return r;
}

// ---------------------------------------------------------------------------
// Reading: the values, dates and spellings a row states
// ---------------------------------------------------------------------------

auto rate_instrument_projection_to_domain_date(const std::string& text)
    -> std::chrono::year_month_day {
    if (text.empty())
        return {};
    return ores::platform::time::datetime::from_iso8601_date(text);
}

auto rate_instrument_projection_to_optional_domain_date(const std::string& text)
    -> std::optional<std::chrono::year_month_day> {
    if (text.empty())
        return std::nullopt;
    return ores::platform::time::datetime::from_iso8601_date(text);
}

auto rate_instrument_projection_to_ore_date(const std::chrono::year_month_day& d) -> std::string {
    return d.ok() ? ores::platform::time::datetime::to_iso8601_date(d) : std::string{};
}

auto rate_instrument_projection_to_ore_date(const std::optional<std::chrono::year_month_day>& d)
    -> std::string {
    return d ? ores::platform::time::datetime::to_iso8601_date(*d) : std::string{};
}

auto rate_instrument_projection_text_of(const record_reader& r, std::string_view name)
    -> std::string {
    if (const auto value = r.text(name))
        return *value;
    return {};
}

auto rate_instrument_projection_spelling_of(const record_reader& r, std::string_view name)
    -> std::string {
    if (const auto value = r.spelling(name))
        return *value;
    return {};
}

// A boolean member the schema types as text keeps the spelling the document
// wrote, so the reading accepts every spelling the binder accepts for a flag.
auto rate_instrument_projection_is_flag_on(std::string_view text) -> bool {
    return text == "Y" || text == "YES" || text == "TRUE" || text == "True" || text == "true" ||
           text == "1";
}

auto rate_instrument_projection_flag_text(bool value) -> std::string_view {
    return value ? "true" : "false";
}

/// The exact decimal an xs:simpleContent numeric leaf carries.
auto rate_instrument_projection_decimal_text(const record_reader& r) -> decimal {
    return r.own_decimal().value_or(decimal{});
}

/// A schema decimal element as the domain's exact decimal.
auto rate_instrument_projection_decimal_of(const record_reader& r, std::string_view name)
    -> decimal {
    return decimal_from_double(decimal_as_double(r, name).value_or(0.0));
}

/// The first schedule rule's tenor, which is the leg's payment frequency.
auto rate_instrument_projection_first_tenor(const std::optional<record_reader>& schedule)
    -> std::string {
    if (!schedule)
        return {};
    const auto rules = schedule->children("Rules");
    if (rules.empty())
        return {};
    return rate_instrument_projection_text_of(rules.front(), "Tenor");
}

auto start_date_of(const record_reader& schedule) -> std::chrono::year_month_day {
    const auto rules = schedule.children("Rules");
    if (!rules.empty())
        return rate_instrument_projection_to_domain_date(
            rate_instrument_projection_text_of(rules.front(), "StartDate"));
    const auto blocks = schedule.children("Dates");
    if (!blocks.empty()) {
        const auto dates = blocks.front().child("Dates");
        if (dates) {
            const auto values = dates->values("Date");
            if (!values.empty())
                return rate_instrument_projection_to_domain_date(values.front());
        }
    }
    return {};
}

auto end_date_of(const record_reader& schedule) -> std::chrono::year_month_day {
    const auto rules = schedule.children("Rules");
    if (!rules.empty() && rules.front().has("EndDate"))
        return rate_instrument_projection_to_domain_date(
            rate_instrument_projection_text_of(rules.front(), "EndDate"));
    const auto blocks = schedule.children("Dates");
    if (!blocks.empty()) {
        const auto dates = blocks.front().child("Dates");
        if (dates) {
            const auto values = dates->values("Date");
            if (!values.empty())
                return rate_instrument_projection_to_domain_date(values.back());
        }
    }
    return {};
}

/// The exercise dates a document states as a plain list, in document order.
auto exercise_date_list(const std::optional<record_reader>& group) -> std::vector<std::string> {
    if (!group)
        return {};
    return group->values("ExerciseDate");
}

// ---------------------------------------------------------------------------
// Reading: the shared children
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Reading: the legs and their economics
// ---------------------------------------------------------------------------

/// A flag the element states, or nothing when it states none.
std::optional<bool> rate_instrument_projection_optional_flag(const record_reader& r,
                                                             std::string_view name) {
    if (!r.has(name))
        return std::nullopt;
    return rate_instrument_projection_is_flag_on(rate_instrument_projection_text_of(r, name));
}

swap_leg rate_instrument_projection_read_leg(const record_reader& leg, int leg_number) {
    swap_leg row;
    row.identity.leg_number = leg_number;
    row.payer =
        rate_instrument_projection_is_flag_on(rate_instrument_projection_text_of(leg, "Payer"));
    row.leg_type_code = rate_instrument_projection_spelling_of(leg, "LegType");
    row.currency = rate_instrument_projection_text_of(leg, "Currency");
    row.day_count_fraction_code = rate_instrument_projection_spelling_of(leg, "DayCounter");
    row.business_day_convention_code =
        rate_instrument_projection_spelling_of(leg, "PaymentConvention");
    row.payment_frequency_code = tenor_to_payment_frequency(
        rate_instrument_projection_first_tenor(leg.child("ScheduleData")));
    if (leg.has("PaymentLag"))
        row.payment_lag = rate_instrument_projection_text_of(leg, "PaymentLag");
    if (leg.has("PaymentCalendar"))
        row.payment_calendar = rate_instrument_projection_text_of(leg, "PaymentCalendar");
    if (const auto notionals = leg.child("Notionals")) {
        if (const auto all = notionals->children("Exchanges"); !all.empty()) {
            const auto& exchanges = all.front();
            const auto spelled = [&](std::string_view name) -> std::optional<bool> {
                const auto value = exchanges.spelling(name);
                if (!value)
                    return std::nullopt;
                return rate_instrument_projection_is_flag_on(*value);
            };
            row.notional_initial_exchange = spelled("NotionalInitialExchange");
            row.notional_final_exchange = spelled("NotionalFinalExchange");
        }
    }
    if (const auto floating = leg.child("FloatingLegData")) {
        row.floating_index_code = rate_instrument_projection_text_of(*floating, "Index");
        if (const auto days = floating->integer("FixingDays"))
            row.fixing_days = static_cast<int>(*days);
        row.is_in_arrears = rate_instrument_projection_optional_flag(*floating, "IsInArrears");
        row.include_spread = rate_instrument_projection_optional_flag(*floating, "IncludeSpread");
        row.is_averaged = rate_instrument_projection_optional_flag(*floating, "IsAveraged");
        row.has_sub_periods = rate_instrument_projection_optional_flag(*floating, "HasSubPeriods");
    }
    rate_instrument_projection_stamp(row);
    return row;
}

std::vector<swap_leg_amount> rate_instrument_projection_read_leg_amounts(const record_reader& leg,
                                                                         int leg_number) {
    std::vector<swap_leg_amount> amounts;
    const auto append = [&](const decimal& value, const std::string& start_date) {
        swap_leg_amount row;
        row.leg_number = leg_number;
        row.sequence_number = static_cast<int>(amounts.size()) + 1;
        row.amount = value;
        if (!start_date.empty())
            row.start_date = rate_instrument_projection_to_domain_date(start_date);
        rate_instrument_projection_stamp_row(row);
        amounts.push_back(std::move(row));
    };

    // The document states its notional as a list, and a cashflow leg states its
    // amounts as dated payments. Both arms land in the same column. An
    // amortization schedule is a rule over the notional, not an amount, so it
    // keeps rows of its own.
    if (const auto notionals = leg.child("Notionals")) {
        for (const auto& notional : notionals->children("Notional"))
            append(rate_instrument_projection_decimal_text(notional),
                   rate_instrument_projection_text_of(notional, "startDate"));
    } else if (const auto cashflow_data = leg.child("CashflowData")) {
        if (const auto cashflow = cashflow_data->child("Cashflow"))
            for (const auto& amount : cashflow->children("Amount"))
                append(rate_instrument_projection_decimal_text(amount),
                       rate_instrument_projection_text_of(amount, "date"));
    }
    return amounts;
}

std::vector<swap_leg_amortization>
rate_instrument_projection_read_leg_amortizations(const record_reader& leg, int leg_number) {
    std::vector<swap_leg_amortization> steps;
    const auto amortizations = leg.child("Amortizations");
    if (!amortizations)
        return steps;
    for (const auto& step : amortizations->children("AmortizationData")) {
        swap_leg_amortization row;
        row.leg_number = leg_number;
        row.sequence_number = static_cast<int>(steps.size()) + 1;
        row.amortization_type = rate_instrument_projection_spelling_of(step, "Type");
        if (step.has("Value"))
            row.value = rate_instrument_projection_decimal_of(step, "Value");
        row.start_date = rate_instrument_projection_to_optional_domain_date(
            rate_instrument_projection_text_of(step, "StartDate"));
        row.end_date = rate_instrument_projection_to_optional_domain_date(
            rate_instrument_projection_text_of(step, "EndDate"));
        if (step.has("Frequency"))
            row.frequency = rate_instrument_projection_text_of(step, "Frequency");
        row.underflow = rate_instrument_projection_optional_flag(step, "Underflow");
        rate_instrument_projection_stamp_row(row);
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

constexpr floating_rate_list floating_rate_lists[] = {
    {"Spreads", "Spread", "spread"},
    {"Gearings", "Gearing", "gearing"},
    {"Caps", "Cap", "cap"},
    {"Floors", "Floor", "floor"},
};

/// Writes each schedule a floating leg's rows state, in the order the schema
/// gives them.
void write_floating_rates(record_writer& floating, const std::vector<swap_leg_rate>& rates) {
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
                writer.set_text("startDate",
                                rate_instrument_projection_to_ore_date(*rate.start_date));
        }
    }
}

std::vector<swap_leg_rate> rate_instrument_projection_read_leg_rates(const record_reader& leg,
                                                                     int leg_number) {
    std::vector<swap_leg_rate> rates;
    const auto append = [&](std::string_view role, const record_reader& value) {
        swap_leg_rate row;
        row.leg_number = leg_number;
        row.rate_role = std::string(role);
        row.sequence_number = static_cast<int>(rates.size()) + 1;
        row.value = rate_instrument_projection_decimal_text(value);
        if (const auto start = rate_instrument_projection_text_of(value, "startDate");
            !start.empty())
            row.start_date = rate_instrument_projection_to_domain_date(start);
        rate_instrument_projection_stamp_row(row);
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

void rate_instrument_projection_append_leg(swap_instrument_data& result,
                                           const record_reader& leg,
                                           int leg_number) {
    result.legs.push_back(rate_instrument_projection_read_leg(leg, leg_number));
    for (auto& amount : rate_instrument_projection_read_leg_amounts(leg, leg_number))
        result.leg_amounts.push_back(std::move(amount));
    for (auto& rate : rate_instrument_projection_read_leg_rates(leg, leg_number))
        result.leg_rates.push_back(std::move(rate));
    for (auto& step : rate_instrument_projection_read_leg_amortizations(leg, leg_number))
        result.leg_amortizations.push_back(std::move(step));
    if (const auto schedule = leg.child("ScheduleData"))
        option_block::read_schedule(result, *schedule, "swap", leg_number, "schedule");
}

void append_legs(swap_instrument_data& result, const record_reader& data) {
    int leg_number = 1;
    for (const auto& leg : data.children("LegData"))
        rate_instrument_projection_append_leg(result, leg, leg_number++);
}

/// The first leg's schedule states the instrument's own dates.
void read_header_dates(swap_instrument_data& result, const record_reader& data) {
    const auto legs = data.children("LegData");
    if (legs.empty())
        return;
    const auto schedule = legs.front().child("ScheduleData");
    if (!schedule)
        return;
    result.header.start_date = start_date_of(*schedule);
    result.header.maturity_date = end_date_of(*schedule);
}

// ---------------------------------------------------------------------------
// Reading: one function per product
// ---------------------------------------------------------------------------

auto read_swap(const record_reader& trade, std::string_view trade_type) -> swap_instrument_data {
    swap_instrument_data result;
    result.header = rate_instrument_projection_make_header(trade_type);
    result.facts = vanilla_swap_instrument{};

    // The schema states a cross-currency swap in its own element of the same
    // type as a plain swap, so which one the document used is the product.
    const auto data = trade_type == "CrossCurrencySwap" ? trade.child("CrossCurrencySwapData") :
                                                          trade.child("SwapData");
    if (!data)
        return result;

    read_header_dates(result, *data);
    append_legs(result, *data);
    return result;
}

auto read_knock_out_swap(const record_reader& trade) -> swap_instrument_data {
    swap_instrument_data result;
    result.header = rate_instrument_projection_make_header("KnockOutSwap");
    result.facts = knock_out_swap_instrument{};

    const auto data = trade.child("KnockOutSwapData");
    if (!data)
        return result;

    auto& fact = std::get<knock_out_swap_instrument>(result.facts);
    if (const auto barrier = data->child("BarrierData")) {
        fact.barrier_type = rate_instrument_projection_spelling_of(*barrier, "Type");
        if (const auto levels = barrier->child("Levels")) {
            const auto values = levels->decimals("Level");
            if (!values.empty())
                fact.barrier_level = values.front();
        }
    }
    fact.barrier_start_date = rate_instrument_projection_to_domain_date(
        rate_instrument_projection_text_of(*data, "BarrierStartDate"));

    read_header_dates(result, *data);
    append_legs(result, *data);
    return result;
}

auto read_inflation_swap(const record_reader& trade) -> swap_instrument_data {
    swap_instrument_data result;
    result.header = rate_instrument_projection_make_header("InflationSwap");
    result.facts = inflation_swap_instrument{};

    const auto data = trade.child("InflationSwapData");
    if (!data)
        return result;

    auto& fact = std::get<inflation_swap_instrument>(result.facts);

    // The document states the index, its base level and the observation lag on
    // the CPI leg rather than on the swap. A swap states one such leg; the
    // reading takes the first and stops, as the old dispatcher did.
    for (const auto& leg : data->children("LegData")) {
        if (rate_instrument_projection_spelling_of(leg, "LegType") != "CPI")
            continue;
        const auto cpi = leg.child("CPILegData");
        if (!cpi)
            continue;
        const auto indices = cpi->values("Index");
        if (!indices.empty())
            fact.inflation_index_code = indices.front();
        const auto base = cpi->decimals("BaseCPI");
        if (!base.empty())
            fact.base_cpi = base.front().to_double();
        const auto lags = cpi->values("ObservationLag");
        if (!lags.empty())
            fact.lag_convention = lags.front();
        break;
    }

    read_header_dates(result, *data);
    append_legs(result, *data);
    return result;
}

auto read_fra(const record_reader& trade) -> swap_instrument_data {
    swap_instrument_data result;
    result.header = rate_instrument_projection_make_header("ForwardRateAgreement");
    result.facts = fra_instrument{};

    const auto data = trade.child("ForwardRateAgreementData");
    if (!data)
        return result;

    auto& fact = std::get<fra_instrument>(result.facts);
    result.header.start_date = rate_instrument_projection_to_domain_date(
        rate_instrument_projection_text_of(*data, "StartDate"));
    result.header.maturity_date = rate_instrument_projection_to_domain_date(
        rate_instrument_projection_text_of(*data, "EndDate"));
    fact.currency = rate_instrument_projection_spelling_of(*data, "Currency");
    fact.notional = rate_instrument_projection_decimal_of(*data, "Notional");
    fact.rate_index = rate_instrument_projection_text_of(*data, "Index");
    fact.strike = rate_instrument_projection_decimal_of(*data, "Strike");
    fact.long_short = rate_instrument_projection_spelling_of(*data, "LongShort");

    // A FRA is one fixed leg on the index it fixes, stated whole by the
    // product rather than by a leg element.
    swap_leg leg;
    leg.identity.leg_number = 1;
    leg.leg_type_code = "Fixed";
    leg.currency = fact.currency;
    leg.floating_index_code = fact.rate_index;
    rate_instrument_projection_stamp(leg);
    result.legs.push_back(std::move(leg));

    swap_leg_amount amount;
    amount.leg_number = 1;
    amount.sequence_number = 1;
    amount.amount = fact.notional;
    rate_instrument_projection_stamp_row(amount);
    result.leg_amounts.push_back(std::move(amount));

    swap_leg_rate rate;
    rate.leg_number = 1;
    rate.rate_role = "fixed";
    rate.sequence_number = 1;
    rate.value = fact.strike;
    rate_instrument_projection_stamp_row(rate);
    result.leg_rates.push_back(std::move(rate));

    return result;
}

auto read_capfloor(const record_reader& trade) -> swap_instrument_data {
    swap_instrument_data result;
    result.header = rate_instrument_projection_make_header("CapFloor");
    result.facts = cap_floor_instrument{};

    const auto data = trade.child("CapFloorData");
    if (!data)
        return result;

    const auto leg = data->child("LegData");
    if (!leg)
        return result;

    if (const auto schedule = leg->child("ScheduleData")) {
        result.header.start_date = start_date_of(*schedule);
        result.header.maturity_date = end_date_of(*schedule);
    }

    swap_leg row;
    row.identity.leg_number = 1;
    row.leg_type_code = rate_instrument_projection_spelling_of(*leg, "LegType");
    row.currency = rate_instrument_projection_spelling_of(*leg, "Currency");
    row.day_count_fraction_code = rate_instrument_projection_spelling_of(*leg, "DayCounter");
    row.business_day_convention_code =
        rate_instrument_projection_spelling_of(*leg, "PaymentConvention");
    row.payment_frequency_code = tenor_to_payment_frequency(
        rate_instrument_projection_first_tenor(leg->child("ScheduleData")));
    if (const auto floating = leg->child("FloatingLegData"))
        row.floating_index_code = rate_instrument_projection_text_of(*floating, "Index");
    rate_instrument_projection_stamp(row);
    result.legs.push_back(std::move(row));

    int amount_number = 0;
    if (const auto notionals = leg->child("Notionals"))
        for (const auto& notional : notionals->children("Notional")) {
            swap_leg_amount amount;
            amount.leg_number = 1;
            amount.sequence_number = ++amount_number;
            amount.amount = rate_instrument_projection_decimal_text(notional);
            rate_instrument_projection_stamp_row(amount);
            result.leg_amounts.push_back(std::move(amount));
        }

    int rate_number = 0;
    if (const auto fixed = leg->child("FixedLegData")) {
        if (const auto list = fixed->child("Rates"))
            for (const auto& rate : list->children("Rate")) {
                swap_leg_rate leg_rate;
                leg_rate.leg_number = 1;
                leg_rate.rate_role = "fixed";
                leg_rate.sequence_number = ++rate_number;
                leg_rate.value = rate_instrument_projection_decimal_text(rate);
                rate_instrument_projection_stamp_row(leg_rate);
                result.leg_rates.push_back(std::move(leg_rate));
            }
    }
    int spread_number = 0;
    if (const auto floating = leg->child("FloatingLegData")) {
        if (const auto list = floating->child("Spreads"))
            for (const auto& spread : list->children("Spread")) {
                swap_leg_rate leg_rate;
                leg_rate.leg_number = 1;
                leg_rate.rate_role = "spread";
                leg_rate.sequence_number = ++spread_number;
                leg_rate.value = rate_instrument_projection_decimal_text(spread);
                rate_instrument_projection_stamp_row(leg_rate);
                result.leg_rates.push_back(std::move(leg_rate));
            }
    }

    // The schema states a cap/floor's strikes as two lists, so every cap and
    // every floor is its own row, carrying the list it came from and its
    // ordinal within that list. A collar states both lists and holds all of
    // them.
    const auto append_strikes =
        [&result](const record_reader& holder, std::string_view element, const char* role) {
            int sequence_number = 0;
            for (const auto& value : holder.children(element)) {
                instrument_strike row;
                row.strike_role = role;
                row.sequence_number = ++sequence_number;
                row.bare_value = rate_instrument_projection_decimal_text(value);
                rate_instrument_projection_stamp_row(row);
                result.strikes.push_back(std::move(row));
            }
        };
    if (const auto caps = data->child("Caps"))
        append_strikes(*caps, "Cap", "cap");
    if (const auto floors = data->child("Floors"))
        append_strikes(*floors, "Floor", "floor");

    if (const auto schedule = leg->child("ScheduleData"))
        option_block::read_schedule(result, *schedule, "swap", 1, "schedule");
    return result;
}

auto read_swaption(const record_reader& trade) -> swap_instrument_data {
    swap_instrument_data result;
    result.header = rate_instrument_projection_make_header("Swaption");
    result.facts = swaption_instrument{};

    const auto data = trade.child("SwaptionData");
    if (!data)
        return result;

    auto& fact = std::get<swaption_instrument>(result.facts);
    if (const auto option = data->child("OptionData")) {
        if (option->has("Style"))
            fact.exercise_type = rate_instrument_projection_text_of(*option, "Style");
        const auto list = exercise_date_list(option->child("ExerciseDates"));
        if (!list.empty())
            fact.expiry_date = rate_instrument_projection_to_domain_date(list.front());
        option_block::read_option_block(result, *option);
    }

    append_legs(result, *data);

    const auto legs = data->children("LegData");
    if (!legs.empty()) {
        if (!result.header.maturity_date) {
            const auto schedule = legs.front().child("ScheduleData");
            if (schedule) {
                const auto maturity = end_date_of(*schedule);
                if (maturity.ok())
                    result.header.maturity_date = maturity;
            }
        }
        if (!result.header.start_date) {
            const auto schedule = legs.front().child("ScheduleData");
            if (schedule) {
                const auto start = start_date_of(*schedule);
                if (start.ok())
                    result.header.start_date = start;
            }
        }
    }
    return result;
}

auto read_callable_swap(const record_reader& trade) -> swap_instrument_data {
    swap_instrument_data result;
    result.header = rate_instrument_projection_make_header("CallableSwap");
    result.facts = callable_swap_instrument{};

    const auto data = trade.child("CallableSwapData");
    if (!data)
        return result;

    if (const auto option = data->child("OptionData")) {
        int sequence_number = 1;
        for (const auto& date : exercise_date_list(option->child("ExerciseDates"))) {
            callable_swap_call_date row;
            row.sequence_number = sequence_number++;
            row.call_date = rate_instrument_projection_to_domain_date(date);
            rate_instrument_projection_stamp_row(row);
            result.call_dates.push_back(std::move(row));
        }
    }

    append_legs(result, *data);
    read_header_dates(result, *data);
    return result;
}

auto read_flexi_swap(const record_reader& trade) -> swap_instrument_data {
    swap_instrument_data result;
    result.header = rate_instrument_projection_make_header("FlexiSwap");
    result.facts = flexi_swap_instrument{};

    const auto data = trade.child("FlexiSwapData");
    if (!data)
        return result;

    auto& fact = std::get<flexi_swap_instrument>(result.facts);
    fact.option_long_short = rate_instrument_projection_spelling_of(*data, "OptionLongShort");

    int bound_number = 0;
    int sequence_number = 0;
    for (const auto& bound : data->children("LowerNotionalBounds")) {
        ++bound_number;
        for (const auto& notional : bound.children("Notional")) {
            flexi_swap_lower_notional row;
            row.sequence_number = ++sequence_number;
            row.bound_number = bound_number;
            if (bound.has("currency"))
                row.currency = rate_instrument_projection_text_of(bound, "currency");
            if (notional.has("startDate"))
                row.start_date = rate_instrument_projection_to_optional_domain_date(
                    rate_instrument_projection_text_of(notional, "startDate"));
            row.notional = rate_instrument_projection_decimal_text(notional);
            rate_instrument_projection_stamp_row(row);
            result.lower_notionals.push_back(std::move(row));
        }
    }

    append_legs(result, *data);
    read_header_dates(result, *data);
    return result;
}

auto read_balance_guaranteed_swap(const record_reader& trade) -> swap_instrument_data {
    swap_instrument_data result;
    result.header = rate_instrument_projection_make_header("BalanceGuaranteedSwap");
    result.facts = balance_guaranteed_swap_instrument{};

    const auto data = trade.child("BalanceGuaranteedSwapData");
    if (!data)
        return result;

    auto& fact = std::get<balance_guaranteed_swap_instrument>(result.facts);
    fact.reference_security = rate_instrument_projection_text_of(*data, "ReferenceSecurity");

    const auto tranches = data->child("Tranches");
    if (tranches) {
        int tranche_number = 0;
        for (const auto& tranche : tranches->children("Tranche")) {
            ++tranche_number;
            balance_guaranteed_swap_tranche row;
            row.sequence_number = tranche_number;
            if (tranche.has("Description"))
                row.description = rate_instrument_projection_text_of(tranche, "Description");
            row.security_id = rate_instrument_projection_text_of(tranche, "SecurityId");
            row.seniority = tranche.integer("Seniority").value_or(0);
            rate_instrument_projection_stamp_row(row);
            result.tranches.push_back(std::move(row));

            int notional_number = 0;
            if (const auto notionals = tranche.child("Notionals"))
                for (const auto& notional : notionals->children("Notional")) {
                    balance_guaranteed_swap_tranche_notional child;
                    child.tranche_number = tranche_number;
                    child.sequence_number = ++notional_number;
                    if (notional.has("startDate"))
                        child.start_date = rate_instrument_projection_to_optional_domain_date(
                            rate_instrument_projection_text_of(notional, "startDate"));
                    child.notional = rate_instrument_projection_decimal_text(notional);
                    rate_instrument_projection_stamp_row(child);
                    result.tranche_notionals.push_back(std::move(child));
                }
        }
        if (const auto schedule = tranches->child("ScheduleData"))
            option_block::read_schedule(result, *schedule, "tranches", 1, "schedule");
    }

    append_legs(result, *data);
    read_header_dates(result, *data);
    return result;
}

// ---------------------------------------------------------------------------
// Writing: the spellings a table refuses
// ---------------------------------------------------------------------------

auto rate_instrument_projection_accepts_spelling(const record_writer& w,
                                                 std::string_view name,
                                                 std::string_view value) -> bool {
    const auto* f = xml::find_field(w.type(), name, false);
    if (f == nullptr || f->tokens == nullptr)
        return false;
    for (std::int16_t i = 0; f->tokens[i] != nullptr; ++i)
        if (value == f->tokens[i])
            return true;
    return false;
}

// The old export replaced a stored code the schema does not state with a
// default rather than failing the document, so the default is stated here too.
void rate_instrument_projection_set_spelling_or(record_writer& w,
                                                std::string_view name,
                                                const std::string& value,
                                                std::string_view fallback,
                                                std::string_view what) {
    w.set_spelling(
        name, rate_instrument_projection_accepts_spelling(w, name, value) ? value : fallback, what);
}

// ---------------------------------------------------------------------------
// Writing: the shared children
// ---------------------------------------------------------------------------

void rate_instrument_projection_write_schedule_rules(
    record_writer& schedule,
    const std::optional<std::chrono::year_month_day>& start_date,
    const std::optional<std::chrono::year_month_day>& end_date,
    const std::string& tenor) {
    auto rule = schedule.append("Rules");
    rule.set_text("StartDate", rate_instrument_projection_to_ore_date(start_date));
    if (end_date)
        rule.set_text("EndDate", rate_instrument_projection_to_ore_date(*end_date));
    rule.set_text("Tenor", tenor);
    rule.set_spelling("Convention", "MF", "business day convention");
}

// ---------------------------------------------------------------------------
// Writing: the legs and their economics
// ---------------------------------------------------------------------------

std::vector<swap_leg_amount>
rate_instrument_projection_amounts_for_leg(const swap_instrument_data& v, int leg_number) {
    std::vector<swap_leg_amount> out;
    for (const auto& row : v.leg_amounts)
        if (row.leg_number == leg_number)
            out.push_back(row);
    return out;
}

std::vector<swap_leg_rate> rate_instrument_projection_rates_for_leg(const swap_instrument_data& v,
                                                                    int leg_number) {
    std::vector<swap_leg_rate> out;
    for (const auto& row : v.leg_rates)
        if (row.leg_number == leg_number)
            out.push_back(row);
    return out;
}

void rate_instrument_projection_write_leg(record_writer& leg,
                                          const rate_instrument& header,
                                          const swap_leg& row,
                                          const std::vector<swap_leg_amount>& amounts,
                                          const std::vector<swap_leg_rate>& rates,
                                          const swap_instrument_data& v) {
    leg.set_spelling("LegType", row.leg_type_code, "leg type");
    leg.set_text("Payer", rate_instrument_projection_flag_text(row.payer.value_or(false)));
    leg.set_text("Currency", row.currency);
    if (!row.day_count_fraction_code.empty())
        rate_instrument_projection_set_spelling_or(
            leg, "DayCounter", row.day_count_fraction_code, "A360", "day counter");
    if (!row.business_day_convention_code.empty())
        rate_instrument_projection_set_spelling_or(leg,
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
                writer.set_text("date", rate_instrument_projection_to_ore_date(*amount.start_date));
        }
        return;
    }

    if (!amounts.empty()) {
        auto notionals = leg.child("Notionals");
        for (const auto& amount : amounts) {
            auto writer = notionals.append("Notional");
            writer.set_own_text(amount.amount.to_string());
            if (amount.start_date)
                writer.set_text("startDate",
                                rate_instrument_projection_to_ore_date(*amount.start_date));
        }
    }
    if (row.notional_initial_exchange || row.notional_final_exchange) {
        auto exchanges = leg.child("Notionals").append("Exchanges");
        if (row.notional_initial_exchange)
            exchanges.set_spelling(
                "NotionalInitialExchange",
                rate_instrument_projection_flag_text(*row.notional_initial_exchange),
                "boolean");
        if (row.notional_final_exchange)
            exchanges.set_spelling(
                "NotionalFinalExchange",
                rate_instrument_projection_flag_text(*row.notional_final_exchange),
                "boolean");
    }

    std::optional<record_writer> amortizations;
    for (const auto& step : v.leg_amortizations) {
        if (step.leg_number != row.identity.leg_number)
            continue;
        if (!amortizations)
            amortizations = leg.child("Amortizations");
        auto writer = amortizations->append("AmortizationData");
        rate_instrument_projection_set_spelling_or(
            writer, "Type", step.amortization_type, "FixedAmount", "amortization type");
        if (step.value)
            writer.set_decimal("Value", *step.value);
        if (step.start_date)
            writer.set_text("StartDate", rate_instrument_projection_to_ore_date(step.start_date));
        if (step.end_date)
            writer.set_text("EndDate", rate_instrument_projection_to_ore_date(step.end_date));
        if (step.frequency)
            writer.set_text("Frequency", *step.frequency);
        if (step.underflow)
            writer.set_text("Underflow", rate_instrument_projection_flag_text(*step.underflow));
    }

    // The leg's own schedule is the one the document stated; a leg stored
    // before its schedule rows existed falls back to the header's dates.
    auto schedule = leg.child("ScheduleData");
    if (!option_block::write_stored_schedule(
            schedule, v, "swap", row.identity.leg_number, "schedule"))
        rate_instrument_projection_write_schedule_rules(
            schedule,
            header.start_date,
            header.maturity_date,
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
                floating.set_text(name, rate_instrument_projection_flag_text(*flag));
        };
        write_flag("IsInArrears", row.is_in_arrears);
        write_flag("IncludeSpread", row.include_spread);
        write_flag("IsAveraged", row.is_averaged);
        write_flag("HasSubPeriods", row.has_sub_periods);
        write_floating_rates(floating, rates);
    }
}

void write_legs(record_writer& data, const swap_instrument_data& v) {
    for (const auto& leg : v.legs) {
        auto writer = data.append("LegData");
        rate_instrument_projection_write_leg(
            writer,
            v.header,
            leg,
            rate_instrument_projection_amounts_for_leg(v, leg.identity.leg_number),
            rate_instrument_projection_rates_for_leg(v, leg.identity.leg_number),
            v);
    }
}

// ---------------------------------------------------------------------------
// Writing: one function per product
// ---------------------------------------------------------------------------

auto barrier_type_or_default(const std::string& code) -> std::string {
    for (const auto* spelling : {"UpAndOut",
                                 "UpAndIn",
                                 "DownAndIn",
                                 "KnockIn",
                                 "KnockOut",
                                 "CumulatedProfitCap",
                                 "CumulatedProfitCapPoints",
                                 "FixingCap",
                                 "FixingFloor"})
        if (code == spelling)
            return code;
    return "DownAndOut";
}

void write_swap(record_writer& trade, std::string_view trade_type, const swap_instrument_data& v) {
    // The schema gives a plain swap and a cross-currency swap an element each,
    // so the product type the row states decides which one carries the legs.
    auto data =
        trade.child(trade_type == "CrossCurrencySwap" ? "CrossCurrencySwapData" : "SwapData");
    write_legs(data, v);
}

void write_knock_out_swap(record_writer& trade, const swap_instrument_data& v) {
    const auto& fact = std::get<knock_out_swap_instrument>(v.facts);
    auto data = trade.child("KnockOutSwapData");
    auto barrier = data.child("BarrierData");
    barrier.set_spelling("Type", barrier_type_or_default(fact.barrier_type), "barrier type");
    barrier.child("Levels").append_decimal("Level", fact.barrier_level);
    data.set_text("BarrierStartDate",
                  rate_instrument_projection_to_ore_date(fact.barrier_start_date));
    write_legs(data, v);
}

void write_inflation_swap(record_writer& trade, const swap_instrument_data& v) {
    const auto& fact = std::get<inflation_swap_instrument>(v.facts);
    auto data = trade.child("InflationSwapData");

    // The shared leg writer states a cashflow, fixed or floating payload and
    // nothing for a CPI leg, so the product's own fields are written here.
    bool cpi_written = false;
    for (const auto& leg : v.legs) {
        auto writer = data.append("LegData");
        rate_instrument_projection_write_leg(
            writer,
            v.header,
            leg,
            rate_instrument_projection_amounts_for_leg(v, leg.identity.leg_number),
            rate_instrument_projection_rates_for_leg(v, leg.identity.leg_number),
            v);
        if (cpi_written || leg.leg_type_code != "CPI")
            continue;
        cpi_written = true;
        auto cpi = writer.child("CPILegData");
        if (!fact.inflation_index_code.empty())
            cpi.append_value("Index", fact.inflation_index_code);
        if (fact.base_cpi)
            cpi.append_decimal("BaseCPI", decimal_from_double(*fact.base_cpi));
        if (!fact.lag_convention.empty())
            cpi.append_value("ObservationLag", fact.lag_convention);
    }
}

void write_fra(record_writer& trade, const swap_instrument_data& v) {
    const auto& fact = std::get<fra_instrument>(v.facts);
    auto data = trade.child("ForwardRateAgreementData");
    data.set_text("StartDate", rate_instrument_projection_to_ore_date(v.header.start_date));
    data.set_text("EndDate", rate_instrument_projection_to_ore_date(v.header.maturity_date));
    data.set_spelling("Currency", fact.currency, "currency");
    set_decimal_from_double(data, "Notional", fact.notional.to_double());
    // The direction belongs to the instrument, so it is written whether or not
    // the leg rows survived. Its Index and Strike still come from those rows.
    rate_instrument_projection_set_spelling_or(
        data, "LongShort", fact.long_short, "Long", "long short");

    // The fact row states the index the forward resets on. A leg only carries
    // it when the document put it there, and the fixed leg states none, so
    // reading the first leg would write an empty index for a trade whose legs
    // are ordered fixed first.
    if (!fact.rate_index.empty())
        data.set_text("Index", fact.rate_index);
    else if (!v.legs.empty())
        data.set_text("Index", v.legs.front().floating_index_code);
    for (const auto& rate : v.leg_rates)
        if (rate.rate_role == "fixed") {
            set_decimal_from_double(data, "Strike", rate.value.to_double());
            break;
        }
    // The leg's own notional is the one the document stated on the leg; the
    // instrument's copy is the fallback for a row written before the children.
    if (!v.leg_amounts.empty())
        set_decimal_from_double(data, "Notional", v.leg_amounts.front().amount.to_double());
}

void write_capfloor(record_writer& trade, const swap_instrument_data& v) {
    auto data = trade.child("CapFloorData");
    data.set_spelling("LongShort", "Long", "long short");

    // A cap or a floor pays on the floating leg, so that is the leg the
    // document states. Taking the first leg would write a fixed one for a
    // trade whose fixed side happens to be numbered first.
    const auto floating = std::find_if(v.legs.begin(), v.legs.end(), [](const auto& candidate) {
        return candidate.leg_type_code == "Floating";
    });
    if (!v.legs.empty()) {
        const auto& row = floating != v.legs.end() ? *floating : v.legs.front();
        auto leg = data.child("LegData");
        rate_instrument_projection_set_spelling_or(
            leg, "LegType", row.leg_type_code, "Floating", "leg type");
        leg.set_spelling("Currency", row.currency, "currency");
        rate_instrument_projection_set_spelling_or(
            leg,
            "DayCounter",
            row.day_count_fraction_code.empty() ? "ACT/365" : row.day_count_fraction_code,
            "ACT/365",
            "day counter");
        if (!row.business_day_convention_code.empty())
            rate_instrument_projection_set_spelling_or(leg,
                                                       "PaymentConvention",
                                                       row.business_day_convention_code,
                                                       "MF",
                                                       "business day convention");

        // The stored schedule is the document's own; a row written before the
        // schedule children existed falls back to the header's dates.
        auto schedule = leg.child("ScheduleData");
        if (!option_block::write_stored_schedule(schedule, v, "swap", 1, "schedule"))
            rate_instrument_projection_write_schedule_rules(
                schedule,
                v.header.start_date,
                v.header.maturity_date,
                payment_frequency_to_tenor(row.payment_frequency_code));

        if (!v.leg_amounts.empty()) {
            auto notionals = leg.child("Notionals");
            for (const auto& amount : v.leg_amounts)
                notionals.append("Notional").set_own_text(amount.amount.to_string());
        }

        if (row.leg_type_code == "Floating") {
            auto floating = leg.child("FloatingLegData");
            floating.set_text("Index", row.floating_index_code);
            write_floating_rates(floating, v.leg_rates);
        }
    }

    // Each stored strike returns to the list its role names, so a floor comes
    // back as a floor and a collar keeps both arms. The row's ordinal decides
    // its place in that list, and a role this product does not state is left
    // out rather than guessed into a cap.
    auto ordered = v.strikes;
    std::sort(ordered.begin(), ordered.end(), [](const auto& lhs, const auto& rhs) {
        return lhs.sequence_number < rhs.sequence_number;
    });
    for (const auto& row : ordered) {
        if (!row.bare_value)
            continue;
        if (row.strike_role == "floor") {
            data.child("Floors").append("Floor").set_own_text(row.bare_value->to_string());
        } else {
            // A row that states no arm was read before the arm existed, and the
            // old mapper wrote that row as a cap.
            data.child("Caps").append("Cap").set_own_text(row.bare_value->to_string());
        }
    }
}

void write_swaption(record_writer& trade, const swap_instrument_data& v) {
    const auto& fact = std::get<swaption_instrument>(v.facts);
    auto data = trade.child("SwaptionData");
    auto option = data.child("OptionData");

    const auto dates = option_block::exercise_dates_of(v);
    if (!v.options.empty()) {
        option_block::write_option_block(option, v);
    } else {
        // A row written before the option child existed carries only the fact
        // row's style and expiry, which are the fallback here.
        option.set_text("LongShort", "Long");
        if (!fact.exercise_type.empty())
            option.set_text("Style", fact.exercise_type);
        if (dates.empty() &&
            option_block::schedules_of(v, "option", 1, "exercise_schedule").empty() &&
            fact.expiry_date.ok())
            option.child("ExerciseDates")
                .append_value("ExerciseDate",
                              rate_instrument_projection_to_ore_date(fact.expiry_date));
    }

    if (!dates.empty()) {
        auto list = option.child("ExerciseDates");
        for (const auto& date : dates)
            list.append_value("ExerciseDate", rate_instrument_projection_to_ore_date(date));
    } else if (!option_block::schedules_of(v, "option", 1, "exercise_schedule").empty()) {
        option_block::write_stored_schedule(
            option.child("ExerciseSchedule"), v, "option", 1, "exercise_schedule");
    }

    write_legs(data, v);
}

void write_callable_swap(record_writer& trade, const swap_instrument_data& v) {
    auto data = trade.child("CallableSwapData");
    if (!v.call_dates.empty()) {
        auto option = data.child("OptionData");
        option.set_text("LongShort", "Long");
        auto list = option.child("ExerciseDates");
        for (const auto& call_date : v.call_dates)
            list.append_value("ExerciseDate",
                              rate_instrument_projection_to_ore_date(call_date.call_date));
    }
    write_legs(data, v);
}

void write_flexi_swap(record_writer& trade, const swap_instrument_data& v) {
    const auto& fact = std::get<flexi_swap_instrument>(v.facts);
    auto data = trade.child("FlexiSwapData");
    data.set_spelling(
        "OptionLongShort", fact.option_long_short == "Short" ? "Short" : "Long", "long short");

    std::vector<const flexi_swap_lower_notional*> ordered;
    for (const auto& row : v.lower_notionals)
        ordered.push_back(&row);
    std::sort(ordered.begin(), ordered.end(), [](const auto* a, const auto* b) {
        return a->sequence_number < b->sequence_number;
    });

    // The rows of one block share its ordinal, so a new block starts where
    // the ordinal changes.
    std::optional<record_writer> block;
    int current_bound = 0;
    bool started = false;
    for (const auto* row : ordered) {
        if (!started || row->bound_number != current_bound) {
            current_bound = row->bound_number;
            started = true;
            block.emplace(data.append("LowerNotionalBounds"));
            if (row->currency)
                block->set_text("currency", *row->currency);
        }
        auto writer = block->append("Notional");
        writer.set_own_text(row->notional.to_string());
        if (row->start_date)
            writer.set_text("startDate", rate_instrument_projection_to_ore_date(*row->start_date));
    }

    write_legs(data, v);
}

void write_balance_guaranteed_swap(record_writer& trade, const swap_instrument_data& v) {
    const auto& fact = std::get<balance_guaranteed_swap_instrument>(v.facts);
    auto data = trade.child("BalanceGuaranteedSwapData");
    data.set_text("ReferenceSecurity", fact.reference_security);

    auto tranches = data.child("Tranches");
    std::vector<const balance_guaranteed_swap_tranche*> ordered;
    for (const auto& row : v.tranches)
        ordered.push_back(&row);
    std::sort(ordered.begin(), ordered.end(), [](const auto* a, const auto* b) {
        return a->sequence_number < b->sequence_number;
    });

    for (const auto* row : ordered) {
        auto tranche = tranches.append("Tranche");
        if (row->description)
            tranche.set_text("Description", *row->description);
        tranche.set_text("SecurityId", row->security_id);
        tranche.set_integer("Seniority", row->seniority);

        std::vector<const balance_guaranteed_swap_tranche_notional*> notionals;
        for (const auto& notional : v.tranche_notionals)
            if (notional.tranche_number == row->sequence_number)
                notionals.push_back(&notional);
        std::sort(notionals.begin(), notionals.end(), [](const auto* a, const auto* b) {
            return a->sequence_number < b->sequence_number;
        });
        for (const auto* notional : notionals) {
            auto writer = tranche.child("Notionals").append("Notional");
            writer.set_own_text(notional->notional.to_string());
            if (notional->start_date)
                writer.set_text("startDate",
                                rate_instrument_projection_to_ore_date(*notional->start_date));
        }
    }

    option_block::write_stored_schedule(
        tranches.child("ScheduleData"), v, "tranches", 1, "schedule");
    write_legs(data, v);
}

}

auto read_rate_instrument(xml::record_reader trade, std::string_view trade_type)
    -> std::optional<trading::domain::trade_instrument> {
    if (!is_rate_trade_type(trade_type))
        return std::nullopt;

    if (trade_type == "Swap" || trade_type == "CrossCurrencySwap")
        return trading::domain::trade_instrument{read_swap(trade, trade_type)};
    if (trade_type == "ForwardRateAgreement")
        return trading::domain::trade_instrument{read_fra(trade)};
    if (trade_type == "CapFloor")
        return trading::domain::trade_instrument{read_capfloor(trade)};
    if (trade_type == "Swaption")
        return trading::domain::trade_instrument{read_swaption(trade)};
    if (trade_type == "CallableSwap")
        return trading::domain::trade_instrument{read_callable_swap(trade)};
    if (trade_type == "KnockOutSwap")
        return trading::domain::trade_instrument{read_knock_out_swap(trade)};
    if (trade_type == "InflationSwap")
        return trading::domain::trade_instrument{read_inflation_swap(trade)};
    if (trade_type == "FlexiSwap")
        return trading::domain::trade_instrument{read_flexi_swap(trade)};
    return trading::domain::trade_instrument{read_balance_guaranteed_swap(trade)};
}

auto write_rate_instrument(xml::record_writer& trade,
                           std::string_view trade_type,
                           const trading::domain::trade_instrument& instrument,
                           const portfolio_reference_curves&) -> bool {
    const auto* v = std::get_if<swap_instrument_data>(&instrument);
    if (v == nullptr || !is_rate_trade_type(trade_type))
        return false;

    if (trade_type == "Swap" || trade_type == "CrossCurrencySwap")
        write_swap(trade, trade_type, *v);
    else if (trade_type == "ForwardRateAgreement")
        write_fra(trade, *v);
    else if (trade_type == "CapFloor")
        write_capfloor(trade, *v);
    else if (trade_type == "Swaption")
        write_swaption(trade, *v);
    else if (trade_type == "CallableSwap")
        write_callable_swap(trade, *v);
    else if (trade_type == "KnockOutSwap")
        write_knock_out_swap(trade, *v);
    else if (trade_type == "InflationSwap")
        write_inflation_swap(trade, *v);
    else if (trade_type == "FlexiSwap")
        write_flexi_swap(trade, *v);
    else
        write_balance_guaranteed_swap(trade, *v);
    return true;
}

}
