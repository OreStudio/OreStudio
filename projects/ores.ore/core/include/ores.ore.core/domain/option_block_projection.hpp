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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_ORE_CORE_DOMAIN_OPTION_BLOCK_PROJECTION_HPP
#define ORES_ORE_CORE_DOMAIN_OPTION_BLOCK_PROJECTION_HPP

#include "ores.ore.core/domain/portfolio_instrument_projection.hpp"
#include "ores.ore.core/xml/record.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.trading.api/domain/instrument_option.hpp"
#include "ores.trading.api/domain/instrument_option_exercise_fee.hpp"
#include "ores.trading.api/domain/instrument_option_exercise_price.hpp"
#include "ores.trading.api/domain/instrument_option_payment_date.hpp"
#include "ores.trading.api/domain/instrument_option_premium.hpp"
#include "ores.trading.api/domain/instrument_schedule.hpp"
#include "ores.trading.api/domain/instrument_schedule_date.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <algorithm>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

/**
 * @file projection.hpp
 * @brief ORE's OptionData block, read into and written from the shared option
 * rows, for any family whose composite holds them.
 *
 * A composite reads and writes the block when it holds the seven vectors the
 * rows land in: options, option_premiums, option_exercise_fees,
 * option_payment_dates, option_exercise_prices, schedules and schedule_dates.
 * The block was the rates family's; credit kept a copy, and the other families
 * kept none, so their options lost the block's terms.
 */

namespace ores::ore::domain::option_block {

using ores::utility::decimal::decimal;
using trading::domain::instrument_option;
using trading::domain::instrument_option_exercise_fee;
using trading::domain::instrument_option_exercise_price;
using trading::domain::instrument_option_payment_date;
using trading::domain::instrument_option_premium;
using trading::domain::instrument_schedule;
using trading::domain::instrument_schedule_date;
using xml::record_reader;
using xml::record_writer;

constexpr std::string_view block_actor = "ores";
constexpr std::string_view block_change_reason = "system.external_data_import";
constexpr std::string_view block_change_commentary = "Imported from ORE XML";

template <typename Row>
void stamp_row(Row& row) {
    row.modified_by = std::string(block_actor);
    row.performed_by = std::string(block_actor);
    row.change_reason_code = std::string(block_change_reason);
    row.change_commentary = std::string(block_change_commentary);
}

inline auto accepts_spelling(const record_writer& w, std::string_view name, std::string_view value)
    -> bool {
    const auto* f = xml::find_field(w.type(), name, false);
    if (f == nullptr || f->tokens == nullptr)
        return false;
    for (std::int16_t i = 0; f->tokens[i] != nullptr; ++i)
        if (value == f->tokens[i])
            return true;
    return false;
}

inline auto text_of(const record_reader& r, std::string_view name) -> std::string {
    if (const auto value = r.text(name))
        return *value;
    return {};
}

inline auto spelling_of(const record_reader& r, std::string_view name) -> std::string {
    if (const auto value = r.spelling(name))
        return *value;
    return {};
}

// The old export replaced a stored code the schema does not state with a
// default rather than failing the document, so the default is stated here too.
inline void set_spelling_or(record_writer& w,
                            std::string_view name,
                            const std::string& value,
                            std::string_view fallback,
                            std::string_view what) {
    w.set_spelling(name, accepts_spelling(w, name, value) ? value : fallback, what);
}

inline auto to_ore_date(const std::chrono::year_month_day& d) -> std::string {
    return d.ok() ? ores::platform::time::datetime::to_iso8601_date(d) : std::string{};
}

inline auto to_ore_date(const std::optional<std::chrono::year_month_day>& d) -> std::string {
    return d ? ores::platform::time::datetime::to_iso8601_date(*d) : std::string{};
}

inline auto to_optional_domain_date(const std::string& text)
    -> std::optional<std::chrono::year_month_day> {
    if (text.empty())
        return std::nullopt;
    return ores::platform::time::datetime::from_iso8601_date(text);
}

inline auto to_domain_date(const std::string& text) -> std::chrono::year_month_day {
    if (text.empty())
        return {};
    return ores::platform::time::datetime::from_iso8601_date(text);
}

inline void set_optional_date(record_writer& w,
                              std::string_view name,
                              const std::optional<std::chrono::year_month_day>& value) {
    if (value)
        w.set_text(name, to_ore_date(*value));
}

// A boolean member the schema types as text keeps the spelling the document
// wrote, so the reading accepts every spelling the binder accepts for a flag.
inline auto is_flag_on(std::string_view text) -> bool {
    return text == "Y" || text == "YES" || text == "TRUE" || text == "True" || text == "true" ||
           text == "1";
}

inline auto flag_text(bool value) -> std::string_view {
    return value ? "true" : "false";
}

// The exercise price list pairs one price with each exercise date the group
// states, in order.
inline auto split_numbers(const std::string& text) -> std::vector<std::string> {
    std::vector<std::string> out;
    std::string current;
    const auto flush = [&]() {
        if (!current.empty()) {
            out.push_back(current);
            current.clear();
        }
    };
    for (const char c : text) {
        if (c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == ',' || c == ';')
            flush();
        else
            current.push_back(c);
    }
    flush();
    return out;
}

inline auto optional_decimal_of(const record_reader& r, std::string_view name)
    -> std::optional<decimal> {
    const auto value = decimal_as_double(r, name);
    if (!value)
        return std::nullopt;
    return decimal_from_double(*value);
}

/// The exact decimal an xs:simpleContent numeric leaf carries.
inline auto decimal_text(const record_reader& r) -> decimal {
    return r.own_decimal().value_or(decimal{});
}

/// A schema decimal element as the domain's exact decimal.
inline auto decimal_of(const record_reader& r, std::string_view name) -> decimal {
    return decimal_from_double(decimal_as_double(r, name).value_or(0.0));
}

/// A number the document spelled as text, such as an option's premium amount.
inline auto decimal_from_text(const std::string& text) -> std::optional<decimal> {
    if (text.empty())
        return std::nullopt;
    if (auto parsed = decimal::from_string(text))
        return std::move(*parsed);
    return std::nullopt;
}

inline auto exercise_date_list(const std::optional<record_reader>& group)
    -> std::vector<std::string> {
    if (!group)
        return {};
    return group->values("ExerciseDate");
}

inline void stamp_schedule_rule(instrument_schedule& row, const record_reader& rule) {
    row.schedule_kind = "rules";
    row.start_date = to_domain_date(text_of(rule, "StartDate"));
    if (rule.has("EndDate"))
        row.end_date = to_optional_domain_date(text_of(rule, "EndDate"));
    if (const auto value = rule.spelling("AdjustEndDateToPreviousMonthEnd"))
        row.adjust_end_date_to_previous_month_end = *value;
    row.tenor = text_of(rule, "Tenor");
    if (rule.has("Calendar"))
        row.calendar = text_of(rule, "Calendar");
    row.convention = spelling_of(rule, "Convention");
    if (rule.has("TermConvention"))
        row.term_convention = spelling_of(rule, "TermConvention");
    if (rule.has("Rule"))
        row.rule = spelling_of(rule, "Rule");
    if (rule.has("EndOfMonth"))
        row.end_of_month = spelling_of(rule, "EndOfMonth");
    if (rule.has("EndOfMonthConvention"))
        row.end_of_month_convention = spelling_of(rule, "EndOfMonthConvention");
    if (rule.has("FirstDate"))
        row.first_date = to_optional_domain_date(text_of(rule, "FirstDate"));
    if (rule.has("LastDate"))
        row.last_date = to_optional_domain_date(text_of(rule, "LastDate"));
    if (rule.has("RemoveFirstDate"))
        row.remove_first_date = is_flag_on(text_of(rule, "RemoveFirstDate"));
    if (rule.has("RemoveLastDate"))
        row.remove_last_date = is_flag_on(text_of(rule, "RemoveLastDate"));
    stamp_row(row);
}

inline void stamp_schedule_dates(instrument_schedule& row, const record_reader& block) {
    row.schedule_kind = "dates";
    if (block.has("Calendar"))
        row.calendar = text_of(block, "Calendar");
    if (block.has("Convention"))
        row.convention = spelling_of(block, "Convention");
    if (block.has("Tenor"))
        row.tenor = text_of(block, "Tenor");
    if (block.has("EndOfMonth"))
        row.end_of_month = spelling_of(block, "EndOfMonth");
    if (block.has("IncludeDuplicateDates"))
        row.include_duplicate_dates = spelling_of(block, "IncludeDuplicateDates");
    stamp_row(row);
}

// One schedule of one owner, as rows: each rule block and each date block
// becomes a schedule row and a date block's dates become rows of their own.
// The ordinals run across both arms, in document order.
template <typename Data>
void read_schedule(Data& result,
                   const record_reader& schedule,
                   std::string_view owner_role,
                   int owner_number,
                   std::string_view schedule_role) {
    int sequence_number = 0;
    for (const auto& rule : schedule.children("Rules")) {
        instrument_schedule row;
        row.owner_role = std::string(owner_role);
        row.owner_number = owner_number;
        row.schedule_role = std::string(schedule_role);
        row.sequence_number = ++sequence_number;
        stamp_schedule_rule(row, rule);
        result.schedules.push_back(std::move(row));
    }
    for (const auto& block : schedule.children("Dates")) {
        const int parent_sequence = ++sequence_number;
        instrument_schedule row;
        row.owner_role = std::string(owner_role);
        row.owner_number = owner_number;
        row.schedule_role = std::string(schedule_role);
        row.sequence_number = parent_sequence;
        stamp_schedule_dates(row, block);
        result.schedules.push_back(std::move(row));

        const auto dates = block.child("Dates");
        if (!dates)
            continue;
        int date_number = 0;
        for (const auto& date : dates->values("Date")) {
            instrument_schedule_date date_row;
            date_row.owner_role = std::string(owner_role);
            date_row.owner_number = owner_number;
            date_row.schedule_role = std::string(schedule_role);
            date_row.schedule_sequence_number = parent_sequence;
            date_row.sequence_number = ++date_number;
            date_row.schedule_date = to_domain_date(date);
            stamp_row(date_row);
            result.schedule_dates.push_back(std::move(date_row));
        }
    }
}

template <typename Data>
void read_premiums(Data& result, const record_reader& option) {
    int sequence_number = 0;
    const auto block = option.child("Premiums");
    if (!block)
        return;
    for (const auto& premium : block->children("Premium")) {
        instrument_option_premium row;
        row.sequence_number = ++sequence_number;
        row.amount = premium.decimal("Amount").value_or(decimal{});
        row.currency = text_of(premium, "Currency");
        row.pay_date = to_domain_date(text_of(premium, "PayDate"));
        row.has_settlement = premium.has("SettlementData");
        if (const auto settlement = premium.child("SettlementData")) {
            row.settlement_pay_currency = spelling_of(*settlement, "PayCurrency");
            row.settlement_fx_index = text_of(*settlement, "FXIndex");
            if (settlement->has("FixingDate"))
                row.settlement_fixing_date =
                    to_optional_domain_date(text_of(*settlement, "FixingDate"));
        }
        stamp_row(row);
        result.option_premiums.push_back(std::move(row));
    }
}

template <typename Data>
void read_exercise_fees(Data& result, const record_reader& option) {
    int sequence_number = 0;
    const auto block = option.child("ExerciseFees");
    if (!block)
        return;
    for (const auto& fee : block->children("ExerciseFee")) {
        instrument_option_exercise_fee row;
        row.sequence_number = ++sequence_number;
        row.amount = decimal_text(fee);
        if (fee.has("type"))
            row.type = text_of(fee, "type");
        if (fee.has("startDate"))
            row.start_date = to_optional_domain_date(text_of(fee, "startDate"));
        if (fee.has("currency"))
            row.currency = text_of(fee, "currency");
        stamp_row(row);
        result.option_exercise_fees.push_back(std::move(row));
    }
}

template <typename Data>
void read_payment_dates(Data& result, const record_reader& option) {
    const auto payment = option.child("PaymentData");
    if (!payment)
        return;
    const auto dates = payment->child("Dates");
    if (!dates)
        return;
    int sequence_number = 0;
    for (const auto& date : dates->values("Date")) {
        instrument_option_payment_date row;
        row.sequence_number = ++sequence_number;
        row.payment_date = to_domain_date(date);
        stamp_row(row);
        result.option_payment_dates.push_back(std::move(row));
    }
}

template <typename Data>
void read_exercise_prices(Data& result,
                          const record_reader& option,
                          const std::vector<std::string>& exercise_dates) {
    const auto prices = option.text("ExercisePrices");
    if (!prices)
        return;
    const auto tokens = split_numbers(*prices);
    for (std::size_t i = 0; i < tokens.size() && i < exercise_dates.size(); ++i) {
        const auto parsed = decimal::from_string(tokens[i]);
        if (!parsed)
            continue;
        instrument_option_exercise_price row;
        row.sequence_number = static_cast<int>(i) + 1;
        row.exercise_date = to_domain_date(exercise_dates[i]);
        row.price = *parsed;
        stamp_row(row);
        result.option_exercise_prices.push_back(std::move(row));
    }
}

template <typename Data>
void read_option_block(Data& result, const record_reader& od) {
    instrument_option row;
    row.long_short = text_of(od, "LongShort");
    if (od.has("OptionType"))
        row.option_type = text_of(od, "OptionType");
    if (od.has("PayoffType"))
        row.payoff_type = text_of(od, "PayoffType");
    if (od.has("PayoffType2"))
        row.payoff_type_2 = text_of(od, "PayoffType2");
    if (od.has("Style"))
        row.style = text_of(od, "Style");
    if (od.has("NoticePeriod"))
        row.notice_period = text_of(od, "NoticePeriod");
    if (od.has("NoticeCalendar"))
        row.notice_calendar = text_of(od, "NoticeCalendar");
    if (od.has("NoticeConvention"))
        row.notice_convention = text_of(od, "NoticeConvention");
    if (od.has("MidCouponExercise"))
        row.mid_coupon_exercise = text_of(od, "MidCouponExercise");
    if (od.has("Settlement"))
        row.settlement = spelling_of(od, "Settlement");
    if (od.has("SettlementMethod"))
        row.settlement_method = spelling_of(od, "SettlementMethod");
    if (od.has("PayOffAtExpiry"))
        row.pay_off_at_expiry = text_of(od, "PayOffAtExpiry");
    if (od.has("PremiumAmount"))
        row.premium_amount = decimal_from_text(text_of(od, "PremiumAmount"));
    if (od.has("PremiumCurrency"))
        row.premium_currency = text_of(od, "PremiumCurrency");
    if (od.has("PremiumPayDate"))
        row.premium_pay_date = to_optional_domain_date(text_of(od, "PremiumPayDate"));
    if (od.has("ExerciseFeeSettlementPeriod"))
        row.exercise_fee_settlement_period = text_of(od, "ExerciseFeeSettlementPeriod");
    if (od.has("ExerciseFeeSettlementCalendar"))
        row.exercise_fee_settlement_calendar = text_of(od, "ExerciseFeeSettlementCalendar");
    if (od.has("ExerciseFeeSettlementConvention"))
        row.exercise_fee_settlement_convention = text_of(od, "ExerciseFeeSettlementConvention");
    if (od.has("AutomaticExercise"))
        row.automatic_exercise = spelling_of(od, "AutomaticExercise");

    row.has_exercise_data = od.has("ExerciseData");
    if (const auto exercise = od.child("ExerciseData")) {
        row.exercise_date = to_domain_date(text_of(*exercise, "Date"));
        if (exercise->has("Price"))
            row.exercise_price = optional_decimal_of(*exercise, "Price");
    }

    row.has_payment_data = od.has("PaymentData");
    if (const auto payment = od.child("PaymentData")) {
        if (const auto rules = payment->child("Rules")) {
            if (const auto lag = rules->integer("Lag"))
                row.payment_lag = *lag;
            row.payment_calendar = text_of(*rules, "Calendar");
            row.payment_convention = spelling_of(*rules, "Convention");
            if (rules->has("RelativeTo"))
                row.payment_relative_to = spelling_of(*rules, "RelativeTo");
        }
    }

    row.has_settlement_data = od.has("SettlementData");
    if (const auto settlement = od.child("SettlementData")) {
        row.settlement_pay_currency = spelling_of(*settlement, "PayCurrency");
        row.settlement_fx_index = text_of(*settlement, "FXIndex");
        if (settlement->has("FixingDate"))
            row.settlement_fixing_date =
                to_optional_domain_date(text_of(*settlement, "FixingDate"));
    }
    stamp_row(row);
    result.options.push_back(std::move(row));

    read_premiums(result, od);
    read_exercise_fees(result, od);
    read_payment_dates(result, od);

    // The exercise dates live in the shared schedule tables under the option
    // owner, whether the document stated a list or a schedule.
    const auto group = od.child("ExerciseDates");
    const auto list = exercise_date_list(group);
    read_exercise_prices(result, od, list);
    if (group) {
        instrument_schedule schedule_row;
        schedule_row.owner_role = "option";
        schedule_row.owner_number = 1;
        schedule_row.schedule_role = "exercise_dates";
        schedule_row.sequence_number = 1;
        schedule_row.schedule_kind = "dates";
        stamp_row(schedule_row);
        result.schedules.push_back(std::move(schedule_row));

        int date_number = 0;
        for (const auto& date : list) {
            instrument_schedule_date date_row;
            date_row.owner_role = "option";
            date_row.owner_number = 1;
            date_row.schedule_role = "exercise_dates";
            date_row.schedule_sequence_number = 1;
            date_row.sequence_number = ++date_number;
            date_row.schedule_date = to_domain_date(date);
            stamp_row(date_row);
            result.schedule_dates.push_back(std::move(date_row));
        }
    } else if (const auto schedule = od.child("ExerciseSchedule")) {
        read_schedule(result, *schedule, "option", 1, "exercise_schedule");
    }
}

template <typename Data>
std::vector<const instrument_schedule*> schedules_of(const Data& v,
                                                     std::string_view owner_role,
                                                     int owner_number,
                                                     std::string_view schedule_role) {
    std::vector<const instrument_schedule*> rows;
    for (const auto& row : v.schedules)
        if (row.owner_role == owner_role && row.owner_number == owner_number &&
            row.schedule_role == schedule_role)
            rows.push_back(&row);
    std::sort(rows.begin(), rows.end(), [](const auto* a, const auto* b) {
        return a->sequence_number < b->sequence_number;
    });
    return rows;
}

template <typename Data>
std::vector<const instrument_schedule_date*> schedule_dates_of(const Data& v,
                                                               std::string_view owner_role,
                                                               int owner_number,
                                                               std::string_view schedule_role,
                                                               int schedule_sequence_number) {
    std::vector<const instrument_schedule_date*> rows;
    for (const auto& row : v.schedule_dates)
        if (row.owner_role == owner_role && row.owner_number == owner_number &&
            row.schedule_role == schedule_role &&
            row.schedule_sequence_number == schedule_sequence_number)
            rows.push_back(&row);
    std::sort(rows.begin(), rows.end(), [](const auto* a, const auto* b) {
        return a->sequence_number < b->sequence_number;
    });
    return rows;
}

/// The stored schedule rows of one owner, written back as the document's own
/// rules and date lists, and whether they stated a schedule at all.
template <typename Data>
bool write_stored_schedule(record_writer schedule,
                           const Data& v,
                           std::string_view owner_role,
                           int owner_number,
                           std::string_view schedule_role) {
    const auto rows = schedules_of(v, owner_role, owner_number, schedule_role);
    if (rows.empty())
        return false;

    for (const auto* row : rows) {
        if (row->schedule_kind == "rules") {
            auto rule = schedule.append("Rules");
            rule.set_text("StartDate", to_ore_date(row->start_date));
            if (row->end_date)
                rule.set_text("EndDate", to_ore_date(*row->end_date));
            if (row->adjust_end_date_to_previous_month_end)
                set_spelling_or(rule,
                                "AdjustEndDateToPreviousMonthEnd",
                                *row->adjust_end_date_to_previous_month_end,
                                "N",
                                "boolean");
            rule.set_text("Tenor", row->tenor.value_or(""));
            if (row->calendar)
                rule.set_text("Calendar", *row->calendar);
            if (row->convention)
                set_spelling_or(
                    rule, "Convention", *row->convention, "MF", "business day convention");
            if (row->term_convention)
                set_spelling_or(
                    rule, "TermConvention", *row->term_convention, "MF", "business day convention");
            if (row->rule)
                set_spelling_or(rule, "Rule", *row->rule, "Forward", "date rule");
            if (row->end_of_month)
                set_spelling_or(rule, "EndOfMonth", *row->end_of_month, "N", "boolean");
            if (row->end_of_month_convention)
                set_spelling_or(rule,
                                "EndOfMonthConvention",
                                *row->end_of_month_convention,
                                "MF",
                                "business day convention");
            if (row->first_date)
                rule.set_text("FirstDate", to_ore_date(*row->first_date));
            if (row->last_date)
                rule.set_text("LastDate", to_ore_date(*row->last_date));
            if (row->remove_first_date)
                rule.set_text("RemoveFirstDate", flag_text(*row->remove_first_date));
            if (row->remove_last_date)
                rule.set_text("RemoveLastDate", flag_text(*row->remove_last_date));
            continue;
        }

        auto block = schedule.append("Dates");
        if (row->calendar)
            block.set_text("Calendar", *row->calendar);
        if (row->convention)
            set_spelling_or(block, "Convention", *row->convention, "MF", "business day convention");
        if (row->tenor)
            block.set_text("Tenor", *row->tenor);
        if (row->end_of_month)
            set_spelling_or(block, "EndOfMonth", *row->end_of_month, "N", "boolean");
        if (row->include_duplicate_dates)
            set_spelling_or(
                block, "IncludeDuplicateDates", *row->include_duplicate_dates, "N", "boolean");

        const auto dates =
            schedule_dates_of(v, owner_role, owner_number, schedule_role, row->sequence_number);
        if (dates.empty())
            continue;
        auto list = block.child("Dates");
        for (const auto* date : dates)
            list.append_value("Date", to_ore_date(date->schedule_date));
    }
    return true;
}

/// The exercise dates the option owner stated, in document order.
template <typename Data>
std::vector<std::chrono::year_month_day> exercise_dates_of(const Data& v) {
    std::vector<std::chrono::year_month_day> out;
    for (const auto* row : schedule_dates_of(v, "option", 1, "exercise_dates", 1))
        out.push_back(row->schedule_date);
    return out;
}

template <typename Data>
void write_option_block(record_writer& option, const Data& v) {
    if (v.options.empty())
        return;
    const auto& row = v.options.front();

    option.set_text("LongShort", row.long_short);
    option.set_optional_text("OptionType", row.option_type);
    option.set_optional_text("PayoffType", row.payoff_type);
    option.set_optional_text("PayoffType2", row.payoff_type_2);
    option.set_optional_text("Style", row.style);
    option.set_optional_text("NoticePeriod", row.notice_period);
    option.set_optional_text("NoticeCalendar", row.notice_calendar);
    option.set_optional_text("NoticeConvention", row.notice_convention);
    option.set_optional_text("MidCouponExercise", row.mid_coupon_exercise);
    if (row.settlement)
        set_spelling_or(option, "Settlement", *row.settlement, "Cash", "settlement type");
    if (row.settlement_method)
        set_spelling_or(
            option, "SettlementMethod", *row.settlement_method, "PhysicalOTC", "settlement method");
    option.set_optional_text("PayOffAtExpiry", row.pay_off_at_expiry);
    if (row.premium_amount)
        option.set_text("PremiumAmount", row.premium_amount->to_string());
    option.set_optional_text("PremiumCurrency", row.premium_currency);
    set_optional_date(option, "PremiumPayDate", row.premium_pay_date);
    option.set_optional_text("ExerciseFeeSettlementPeriod", row.exercise_fee_settlement_period);
    option.set_optional_text("ExerciseFeeSettlementCalendar", row.exercise_fee_settlement_calendar);
    option.set_optional_text("ExerciseFeeSettlementConvention",
                             row.exercise_fee_settlement_convention);
    if (row.automatic_exercise)
        set_spelling_or(option, "AutomaticExercise", *row.automatic_exercise, "N", "boolean");

    if (!v.option_premiums.empty()) {
        auto block = option.child("Premiums");
        for (const auto& premium : v.option_premiums) {
            auto writer = block.append("Premium");
            writer.set_decimal("Amount", premium.amount);
            writer.set_text("Currency", premium.currency);
            writer.set_text("PayDate", to_ore_date(premium.pay_date));
            if (premium.has_settlement) {
                auto settlement = writer.child("SettlementData");
                settlement.set_spelling(
                    "PayCurrency", premium.settlement_pay_currency.value_or(""), "currency");
                settlement.set_text("FXIndex", premium.settlement_fx_index.value_or(""));
                set_optional_date(settlement, "FixingDate", premium.settlement_fixing_date);
            }
        }
    }

    if (!v.option_exercise_fees.empty()) {
        auto block = option.child("ExerciseFees");
        for (const auto& fee : v.option_exercise_fees) {
            auto writer = block.append("ExerciseFee");
            writer.set_own_text(fee.amount.to_string());
            if (fee.type)
                writer.set_text("type", *fee.type);
            if (fee.start_date)
                writer.set_text("startDate", to_ore_date(*fee.start_date));
            if (fee.currency)
                writer.set_text("currency", *fee.currency);
        }
    }

    if (row.has_exercise_data) {
        auto exercise = option.child("ExerciseData");
        exercise.set_text("Date", to_ore_date(row.exercise_date));
        if (row.exercise_price)
            set_decimal_from_double(exercise, "Price", row.exercise_price->to_double());
    }

    if (row.has_payment_data) {
        auto payment = option.child("PaymentData");
        if (!v.option_payment_dates.empty()) {
            auto dates = payment.child("Dates");
            for (const auto& date : v.option_payment_dates)
                dates.append_value("Date", to_ore_date(date.payment_date));
        }
        if (row.payment_lag || row.payment_calendar || row.payment_convention) {
            auto rules = payment.append("Rules");
            rules.set_integer("Lag", std::max<std::int64_t>(row.payment_lag.value_or(0), 0));
            rules.set_text("Calendar", row.payment_calendar.value_or(""));
            set_spelling_or(rules,
                            "Convention",
                            row.payment_convention.value_or(""),
                            "MF",
                            "business day convention");
            if (row.payment_relative_to)
                set_spelling_or(
                    rules, "RelativeTo", *row.payment_relative_to, "Expiry", "pay relative to");
        }
    }

    if (row.has_settlement_data) {
        auto settlement = option.child("SettlementData");
        settlement.set_spelling(
            "PayCurrency", row.settlement_pay_currency.value_or(""), "currency");
        settlement.set_text("FXIndex", row.settlement_fx_index.value_or(""));
        set_optional_date(settlement, "FixingDate", row.settlement_fixing_date);
    }

    if (!v.option_exercise_prices.empty()) {
        std::string text;
        for (const auto& price : v.option_exercise_prices) {
            if (!text.empty())
                text += ' ';
            text += price.price.to_string();
        }
        option.set_text("ExercisePrices", text);
    }
}


/**
 * @brief Writes a whole OptionData element: the block, then the exercise dates
 * or the exercise schedule the option rows state.
 *
 * A composite with no option row still states the one member the schema
 * requires, a long position.
 */
template <typename Data>
void write_option_data(record_writer option, const Data& v) {
    if (v.options.empty())
        option.set_text("LongShort", "Long");
    else
        write_option_block(option, v);

    const auto dates = exercise_dates_of(v);
    if (!dates.empty()) {
        auto list = option.child("ExerciseDates");
        for (const auto& date : dates)
            list.append_value("ExerciseDate", to_ore_date(date));
    } else if (!schedules_of(v, "option", 1, "exercise_schedule").empty()) {
        write_stored_schedule(
            option.child("ExerciseSchedule"), v, "option", 1, "exercise_schedule");
    }
}

}

#endif
