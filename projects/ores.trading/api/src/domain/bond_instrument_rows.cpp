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
#include "ores.trading.api/domain/bond_instrument_rows.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.trading.api/domain/bond_document_ops.hpp"
#include <boost/uuid/uuid.hpp>
#include <algorithm>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

namespace ores::trading::domain {

namespace {

// The bond schedule aggregates the reader builds are hand-written string
// carriers; the repository entities now hold calendar dates. Convert at this
// boundary, one spelling each way.
std::string iso_or_empty(const std::chrono::year_month_day& d) {
    return d.ok() ? ores::platform::time::datetime::to_iso8601_date(d) : std::string{};
}

std::string iso_or_empty(const std::optional<std::chrono::year_month_day>& d) {
    return d && d->ok() ? ores::platform::time::datetime::to_iso8601_date(*d) : std::string{};
}

std::optional<std::string> iso_optional(const std::optional<std::chrono::year_month_day>& d) {
    if (!d || !d->ok())
        return std::nullopt;
    return ores::platform::time::datetime::to_iso8601_date(*d);
}

// The role the security's rows are read under: the issue-keyed tables
// already say whose rows they are, and the container still names the role.
constexpr std::string_view bond_leg_role = "bond";

/**
 * @brief The leg role a leg or one of its children belongs to.
 *
 * The trade-keyed family states the role on every row, because one table
 * holds three lists. The issue-keyed family's tables already say whose
 * rows these are, so a row there carries no role and the answer is the
 * bond.
 */
std::string_view leg_role_of(const bond_leg& row) {
    return row.leg_role;
}

std::string_view leg_role_of(const bond_issue_leg&) {
    return bond_leg_role;
}

std::string_view leg_role_of(const bond_leg_amount& row) {
    return row.leg_role;
}

std::string_view leg_role_of(const bond_issue_leg_amount&) {
    return bond_leg_role;
}

std::string_view leg_role_of(const bond_leg_rate& row) {
    return row.leg_role;
}

std::string_view leg_role_of(const bond_issue_leg_rate&) {
    return bond_leg_role;
}

std::string_view leg_role_of(const bond_leg_amortization& row) {
    return row.leg_role;
}

std::string_view leg_role_of(const bond_issue_leg_amortization&) {
    return bond_leg_role;
}

/**
 * @brief Whether a schedule row belongs to the owner a role and number name.
 *
 * The trade-keyed schedule states its owner. The issue-keyed one is owned
 * by the leg it names, so the number alone decides.
 */
bool owner_matches(const instrument_schedule& row, std::string_view owner_role, int owner_number) {
    return row.owner_role == owner_role && row.owner_number == owner_number;
}

bool owner_matches(const instrument_schedule_date& row,
                   std::string_view owner_role,
                   int owner_number) {
    return row.owner_role == owner_role && row.owner_number == owner_number;
}

bool owner_matches(const bond_issue_leg_schedule& row, std::string_view, int leg_number) {
    return row.leg_number == leg_number;
}

bool owner_matches(const bond_issue_leg_schedule_date& row, std::string_view, int leg_number) {
    return row.leg_number == leg_number;
}

/**
 * @brief The six amounts a leg can state, split by the role each plays.
 */
struct leg_amounts final {
    std::vector<bond_float_data> notional;
    std::vector<bond_float_data> rate;
    std::vector<bond_float_data> spread;
    std::vector<bond_float_data> cap;
    std::vector<bond_float_data> floor;
    std::vector<bond_float_data> gearing;
};

template <typename ScheduleRow>
bond_schedule_rules to_rules(const ScheduleRow& row) {
    bond_schedule_rules rules;
    rules.start_date = iso_or_empty(row.start_date);
    rules.end_date = iso_optional(row.end_date);
    rules.adjust_end_date_to_previous_month_end = row.adjust_end_date_to_previous_month_end;
    rules.tenor = row.tenor.value_or("");
    rules.calendar = row.calendar;
    rules.convention = row.convention.value_or("");
    rules.term_convention = row.term_convention;
    rules.rule = row.rule;
    rules.end_of_month = row.end_of_month;
    rules.end_of_month_convention = row.end_of_month_convention;
    rules.first_date = iso_optional(row.first_date);
    rules.last_date = iso_optional(row.last_date);
    rules.remove_first_date = row.remove_first_date;
    rules.remove_last_date = row.remove_last_date;
    return rules;
}

template <typename ScheduleRow, typename DateRow>
bond_schedule_data to_schedule_data(const std::vector<const ScheduleRow*>& rows,
                                    const std::vector<const DateRow*>& date_rows) {
    std::unordered_map<int, std::vector<const DateRow*>> by_sequence;
    for (const auto* date_row : date_rows)
        by_sequence[date_row->schedule_sequence_number].push_back(date_row);

    bond_schedule_data schedule;
    for (const auto* row : rows) {
        if (row->schedule_kind == "rules") {
            schedule.rules.push_back(to_rules(*row));
            continue;
        }
        bond_schedule_dates dates;
        dates.calendar = row->calendar;
        dates.convention = row->convention;
        dates.tenor = row->tenor;
        dates.end_of_month = row->end_of_month;
        dates.include_duplicate_dates = row->include_duplicate_dates;
        if (auto it = by_sequence.find(row->sequence_number); it != by_sequence.end())
            for (const auto* date_row : it->second)
                dates.dates.push_back(iso_or_empty(date_row->schedule_date));
        schedule.dates.push_back(std::move(dates));
    }
    return schedule;
}

/**
 * @brief Reads one of an owner's schedules, which its role and number name.
 */
template <typename ScheduleRow, typename DateRow>
bond_schedule_data schedule_for(const std::vector<ScheduleRow>& schedules,
                                const std::vector<DateRow>& schedule_dates,
                                std::string_view owner_role,
                                int owner_number,
                                std::string_view role) {
    std::vector<const ScheduleRow*> rows;
    for (const auto& row : schedules)
        if (row.schedule_role == role && owner_matches(row, owner_role, owner_number))
            rows.push_back(&row);

    std::vector<const DateRow*> dates;
    for (const auto& row : schedule_dates)
        if (row.schedule_role == role && owner_matches(row, owner_role, owner_number))
            dates.push_back(&row);

    return to_schedule_data(rows, dates);
}

/**
 * @brief Reads one of a trade-keyed owner's schedules.
 */
bond_schedule_data schedule_for(const bond_trade_rows& rows,
                                std::string_view owner_role,
                                int owner_number,
                                std::string_view role) {
    return schedule_for(rows.schedules, rows.schedule_dates, owner_role, owner_number, role);
}

template <typename AmountRow>
leg_amounts
collect_amounts(const std::vector<AmountRow>& rows, std::string_view leg_role, int leg_number) {
    leg_amounts amounts;
    for (const auto& row : rows) {
        if (leg_role_of(row) != leg_role || row.leg_number != leg_number)
            continue;
        bond_float_data amount{row.value, row.start_date};
        if (row.amount_role == "notional")
            amounts.notional.push_back(std::move(amount));
        else if (row.amount_role == "rate")
            amounts.rate.push_back(std::move(amount));
        else if (row.amount_role == "spread")
            amounts.spread.push_back(std::move(amount));
        else if (row.amount_role == "cap")
            amounts.cap.push_back(std::move(amount));
        else if (row.amount_role == "floor")
            amounts.floor.push_back(std::move(amount));
        else if (row.amount_role == "gearing")
            amounts.gearing.push_back(std::move(amount));
    }
    return amounts;
}

std::optional<bond_stub_interpolation>
to_interpolation(const std::optional<std::string>& short_index,
                 const std::optional<std::string>& long_index,
                 const std::optional<std::string>& rounding_type,
                 const std::optional<std::int64_t>& rounding_precision) {
    if (!short_index && !long_index && !rounding_type && !rounding_precision)
        return std::nullopt;
    bond_stub_interpolation interpolation;
    interpolation.short_index = short_index.value_or("");
    interpolation.long_index = long_index.value_or("");
    interpolation.rounding_type = rounding_type;
    interpolation.rounding_precision = rounding_precision;
    return interpolation;
}

template <typename RateRow>
bond_leg_rate_data to_rate_data(RateRow row,
                                const leg_amounts& amounts,
                                bond_schedule_data fixing_schedule,
                                bond_schedule_data reset_schedule) {
    bond_leg_rate_data rate;
    if (row.rate_kind == "floating") {
        bond_floating_leg_data floating;
        floating.index = row.index.value_or("");
        floating.is_in_arrears = row.is_in_arrears;
        floating.last_recent_period = row.last_recent_period;
        floating.last_recent_period_calendar = row.last_recent_period_calendar;
        // The column is signed and the container is not, so a negative
        // value would wrap rather than fail.
        if (row.fixing_days && *row.fixing_days >= 0)
            floating.fixing_days = static_cast<std::uint64_t>(*row.fixing_days);
        floating.lookback = row.lookback;
        floating.rate_cutoff = row.rate_cutoff;
        floating.is_averaged = row.is_averaged;
        floating.has_sub_periods = row.has_sub_periods;
        floating.include_spread = row.include_spread;
        floating.is_not_resetting_xccy = row.is_not_resetting_xccy;
        floating.spreads = amounts.spread;
        floating.caps = amounts.cap;
        floating.floors = amounts.floor;
        floating.gearings = amounts.gearing;
        floating.naked_option = row.naked_option;
        floating.local_cap_floor = row.local_cap_floor;
        floating.fixing_schedule = std::move(fixing_schedule);
        floating.reset_schedule = std::move(reset_schedule);
        floating.front_stub_interpolation = to_interpolation(row.front_stub_short_index,
                                                             row.front_stub_long_index,
                                                             row.front_stub_rounding_type,
                                                             row.front_stub_rounding_precision);
        floating.back_stub_interpolation = to_interpolation(row.back_stub_short_index,
                                                            row.back_stub_long_index,
                                                            row.back_stub_rounding_type,
                                                            row.back_stub_rounding_precision);
        floating.stub_use_original_curve = row.stub_use_original_curve;
        floating.observation_shift = row.observation_shift;
        rate.floating = std::move(floating);
        return rate;
    }
    if (row.rate_kind == "formula_based") {
        bond_formula_based_leg_data formula;
        formula.index = row.index.value_or("");
        formula.is_in_arrears = row.is_in_arrears;
        formula.fixing_days = row.fixing_days.value_or(0);
        formula.fixing_calendar = row.fixing_calendar;
        rate.formula_based = std::move(formula);
        return rate;
    }
    rate.fixed = bond_fixed_leg_data{amounts.rate};
    return rate;
}

/**
 * @brief Builds one leg from the rows that hold it and everything it states.
 *
 * The leg row may come from either leg family. Its children are read
 * from the same family, which is why the row set is a template parameter:
 * the trade-keyed family and the issue-keyed one have the same shape and
 * the shared body serves both.
 */
template <typename Rows, typename LegRow>
bond_leg_data build_leg(const Rows& rows, const LegRow& row) {
    const auto leg_role = leg_role_of(row);
    const auto leg_number = row.leg_number;

    bond_leg_data leg;
    leg.payer = row.payer;
    leg.leg_type = row.leg_type;
    leg.currency = row.currency;
    leg.payment_convention = row.payment_convention;
    leg.payment_lag = row.payment_lag;
    leg.payment_calendar = row.payment_calendar;
    leg.day_counter = row.day_counter;
    leg.last_period_day_counter = row.last_period_day_counter;
    leg.notional_payment_lag = row.notional_payment_lag;
    leg.strict_notional_dates = row.strict_notional_dates;
    leg.indexings_from_asset_leg = row.indexings_from_asset_leg;
    if (row.settlement_fx_index)
        leg.settlement = bond_settlement_data{*row.settlement_fx_index, row.settlement_fixing_date};

    const auto amounts = collect_amounts(rows.amounts, leg_role, leg_number);
    leg.notionals = amounts.notional;

    for (const auto& amortization : rows.amortizations) {
        if (leg_role_of(amortization) != leg_role || amortization.leg_number != leg_number)
            continue;
        leg.amortizations.push_back({amortization.amortization_type,
                                     amortization.value,
                                     amortization.start_date,
                                     amortization.end_date,
                                     amortization.frequency,
                                     amortization.underflow});
    }

    leg.schedule =
        schedule_for(rows.schedules, rows.schedule_dates, leg_role, leg_number, "schedule");
    leg.payment_schedule =
        schedule_for(rows.schedules, rows.schedule_dates, leg_role, leg_number, "payment_schedule");
    const auto payment_dates =
        schedule_for(rows.schedules, rows.schedule_dates, leg_role, leg_number, "payment_dates");
    for (const auto& block : payment_dates.dates)
        for (const auto& date : block.dates)
            leg.payment_dates.push_back(date);

    for (const auto& rate_row : rows.rates) {
        if (leg_role_of(rate_row) != leg_role || rate_row.leg_number != leg_number)
            continue;
        leg.rate = to_rate_data(
            rate_row,
            amounts,
            schedule_for(
                rows.schedules, rows.schedule_dates, leg_role, leg_number, "fixing_schedule"),
            schedule_for(
                rows.schedules, rows.schedule_dates, leg_role, leg_number, "reset_schedule"));
        break;
    }
    return leg;
}

void apply_leg_family(bond_instrument_data& data,
                      const bond_trade_rows& rows,
                      const issue_leg_rows& issue_rows) {
    // The security's legs come first, in the order the issue's leg list
    // holds them; they are shared by every instrument of the ISIN.
    data.bond_legs = bond_legs_of(issue_rows);

    for (const auto& row : rows.legs) {
        if (row.leg_role == "trs_funding")
            data.trs_funding_leg = build_leg(rows, row);
        else if (row.leg_role == "repo")
            data.repo_leg = build_leg(rows, row);
        else if (row.leg_role == "ascot_swap")
            data.ascot_swap_leg = build_leg(rows, row);
    }
}

std::optional<bond_schedule_data> optional_schedule(bond_schedule_data schedule) {
    if (schedule.rules.empty() && schedule.dates.empty())
        return std::nullopt;
    return schedule;
}

/**
 * @brief Builds an option settlement block, which a flag on the row engages.
 */
std::optional<bond_option_settlement>
to_option_settlement(bool engaged,
                     const std::optional<std::string>& pay_currency,
                     const std::optional<std::string>& fx_index,
                     const std::optional<std::chrono::year_month_day>& fixing_date) {
    if (!engaged)
        return std::nullopt;
    return bond_option_settlement{
        pay_currency.value_or(""), fx_index.value_or(""), iso_optional(fixing_date)};
}

/**
 * @brief Builds the payment rule block, which the row states as three columns.
 *
 * The three are required members of the rule, so a document that stated
 * the rule stated all three. A row with none of them set stated the date
 * list instead, and the two arms exclude each other.
 */
std::optional<bond_option_payment_rules> to_payment_rules(const instrument_option& row) {
    if (!row.payment_lag && !row.payment_calendar && !row.payment_convention)
        return std::nullopt;
    bond_option_payment_rules rules;
    rules.lag = static_cast<std::uint64_t>(std::max<std::int64_t>(row.payment_lag.value_or(0), 0));
    rules.calendar = row.payment_calendar.value_or("");
    rules.convention = row.payment_convention.value_or("");
    rules.relative_to = row.payment_relative_to;
    return rules;
}

void apply_option_block(bond_instrument_data& data, const bond_trade_rows& rows) {
    if (rows.option) {
        const auto& row = *rows.option;

        bond_option_data block;
        block.long_short = row.long_short;
        block.option_type = row.option_type;
        block.payoff_type = row.payoff_type;
        block.payoff_type_2 = row.payoff_type_2;
        block.style = row.style;
        block.notice_period = row.notice_period;
        block.notice_calendar = row.notice_calendar;
        block.notice_convention = row.notice_convention;
        block.mid_coupon_exercise = row.mid_coupon_exercise;
        block.settlement = row.settlement;
        block.settlement_method = row.settlement_method;
        block.pay_off_at_expiry = row.pay_off_at_expiry;
        block.premium_amount = row.premium_amount;
        block.premium_currency = row.premium_currency;
        block.premium_pay_date = iso_optional(row.premium_pay_date);

        for (const auto& premium : rows.option_premiums)
            block.premiums.push_back({premium.amount,
                                      premium.currency,
                                      iso_or_empty(premium.pay_date),
                                      to_option_settlement(premium.has_settlement,
                                                           premium.settlement_pay_currency,
                                                           premium.settlement_fx_index,
                                                           premium.settlement_fixing_date)});

        // The option's exercise price list is text in the document and rows
        // here, so the reader joins the rows back into the document's list.
        if (!rows.option_exercise_prices.empty()) {
            std::string prices;
            for (const auto& price : rows.option_exercise_prices) {
                if (!prices.empty())
                    prices += ' ';
                prices += price.price.to_string();
            }
            block.exercise_prices = std::move(prices);
        }
        for (const auto& fee : rows.option_exercise_fees)
            block.exercise_fees.push_back(
                {fee.amount, fee.type, iso_optional(fee.start_date), fee.currency});

        block.exercise_fee_settlement_period = row.exercise_fee_settlement_period;
        block.exercise_fee_settlement_calendar = row.exercise_fee_settlement_calendar;
        block.exercise_fee_settlement_convention = row.exercise_fee_settlement_convention;
        block.automatic_exercise = row.automatic_exercise;

        if (row.has_exercise_data)
            block.exercise_data =
                bond_option_exercise{iso_or_empty(row.exercise_date), row.exercise_price};

        if (row.has_payment_data) {
            bond_option_payment_data payment;
            for (const auto& date : rows.option_payment_dates)
                payment.dates.push_back(iso_or_empty(date.payment_date));
            payment.rules = to_payment_rules(row);
            block.payment_data = std::move(payment);
        }

        block.settlement_data = to_option_settlement(row.has_settlement_data,
                                                     row.settlement_pay_currency,
                                                     row.settlement_fx_index,
                                                     row.settlement_fixing_date);

        data.option_data = std::move(block);
    }

    const auto exercise_dates = schedule_for(rows, "option", 1, "exercise_dates");
    for (const auto& block : exercise_dates.dates)
        for (const auto& date : block.dates)
            data.option_exercise_dates.push_back(date);

    data.option_exercise_schedule =
        optional_schedule(schedule_for(rows, "option", 1, "exercise_schedule"));
}

void apply_strike(bond_instrument_data& data, const bond_trade_rows& rows) {
    if (!rows.strike)
        return;
    const auto& row = *rows.strike;
    data.strike_data = bond_strike_data{row.price_value,
                                        row.price_currency,
                                        row.yield_value,
                                        row.yield_compounding,
                                        row.bare_value,
                                        row.bare_currency};
}

void apply_forward(bond_instrument_data& data, const bond_trade_rows& rows) {
    if (!rows.forward)
        return;
    const auto& row = *rows.forward;
    data.forward_long_in_forward = row.long_in_forward;

    bond_forward_settlement settlement;
    settlement.forward_maturity_date = row.forward_maturity_date.value_or("");
    settlement.forward_settlement_date = row.forward_settlement_date;
    settlement.settlement = row.settlement;
    settlement.amount = row.amount;
    settlement.lock_rate = row.lock_rate;
    settlement.dv01 = row.dv01;
    settlement.lock_rate_day_counter = row.lock_rate_day_counter;
    settlement.settlement_dirty = row.settlement_dirty;
    data.forward_settlement = std::move(settlement);

    if (row.premium_amount || row.premium_date)
        data.forward_premium =
            bond_forward_premium{row.premium_amount.value_or(ores::utility::decimal::decimal{}),
                                 row.premium_date.value_or("")};
}

/**
 * @brief Copies the return-side members the fact row holds back into the container.
 *
 * The mapper leaves the payer, the price type and the initial price out
 * of the fact row it builds, so a stored row carries them only because
 * the writer put them there. The schedule is not a column and comes from
 * the shared schedule tables under the owner the swap names.
 */
void apply_trs_residue(bond_instrument_data& data, const bond_trade_rows& rows) {
    if (data.trs) {
        data.trs_payer = data.trs->payer;
        data.trs_initial_price = data.trs->initial_price;
        if (data.trs->price_type)
            data.trs_price_type = *data.trs->price_type;
    }
    data.trs_schedule = schedule_for(rows, "trs", 1, "schedule");
}

/**
 * @brief Copies the three option members the fact row holds back into the container.
 *
 * The mapper writes the option type and the strike to the fact row and
 * leaves the other three members nowhere, so a stored row carries them
 * only because the writer put them there.
 */
void apply_option_residue(bond_instrument_data& data) {
    if (!data.option)
        return;
    data.option_redemption = data.option->redemption;
    data.option_price_type = data.option->price_type;
    data.option_knocks_out = data.option->knocks_out;
}

}

namespace {

std::optional<std::chrono::year_month_day> parse_iso(const std::string& text) {
    if (text.empty())
        return std::nullopt;
    return ores::platform::time::datetime::from_iso8601_date(text);
}

std::optional<std::chrono::year_month_day> parse_iso(const std::optional<std::string>& text) {
    return text ? parse_iso(*text) : std::nullopt;
}

std::chrono::year_month_day date_of(const std::string& text) {
    return parse_iso(text).value_or(std::chrono::year_month_day{});
}

// The document states a list of numbers as one text, split on the separators
// the corpus uses.
std::vector<std::string> split_numbers(const std::string& text) {
    std::vector<std::string> out;
    std::string current;
    for (const char c : text) {
        if (c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == ',' || c == ';') {
            if (!current.empty())
                out.push_back(std::exchange(current, {}));
        } else {
            current.push_back(c);
        }
    }
    if (!current.empty())
        out.push_back(current);
    return out;
}

/*
 * The rows of one schedule of one owner. A key function states the owner on
 * each row, because the trade-keyed and the issue-keyed tables name it
 * differently and are otherwise the same shape.
 */
template <typename ScheduleRow, typename DateRow, typename Key>
void split_schedule(std::vector<ScheduleRow>& schedules,
                    std::vector<DateRow>& schedule_dates,
                    const Key& key,
                    std::string_view schedule_role,
                    const bond_schedule_data& schedule) {
    int sequence_number = 0;
    for (const auto& rule : schedule.rules) {
        ScheduleRow row{};
        key(row);
        row.schedule_role = std::string(schedule_role);
        row.sequence_number = ++sequence_number;
        row.schedule_kind = "rules";
        row.start_date = parse_iso(rule.start_date);
        row.end_date = parse_iso(rule.end_date);
        row.adjust_end_date_to_previous_month_end = rule.adjust_end_date_to_previous_month_end;
        row.tenor = rule.tenor;
        row.calendar = rule.calendar;
        row.convention = rule.convention;
        row.term_convention = rule.term_convention;
        row.rule = rule.rule;
        row.end_of_month = rule.end_of_month;
        row.end_of_month_convention = rule.end_of_month_convention;
        row.first_date = parse_iso(rule.first_date);
        row.last_date = parse_iso(rule.last_date);
        row.remove_first_date = rule.remove_first_date;
        row.remove_last_date = rule.remove_last_date;
        schedules.push_back(std::move(row));
    }
    for (const auto& dates : schedule.dates) {
        ScheduleRow row{};
        key(row);
        row.schedule_role = std::string(schedule_role);
        row.sequence_number = ++sequence_number;
        row.schedule_kind = "dates";
        row.calendar = dates.calendar;
        row.convention = dates.convention;
        row.tenor = dates.tenor;
        row.end_of_month = dates.end_of_month;
        row.include_duplicate_dates = dates.include_duplicate_dates;
        int date_number = 0;
        for (const auto& date : dates.dates) {
            DateRow date_row{};
            key(date_row);
            date_row.schedule_role = std::string(schedule_role);
            date_row.schedule_sequence_number = row.sequence_number;
            date_row.sequence_number = ++date_number;
            date_row.schedule_date = date_of(date);
            schedule_dates.push_back(std::move(date_row));
        }
        schedules.push_back(std::move(row));
    }
}

template <typename Rows, typename LegKey>
void split_amounts(Rows& rows,
                   const LegKey& key,
                   std::string_view amount_role,
                   const std::vector<bond_float_data>& amounts) {
    int sequence_number = 0;
    for (const auto& amount : amounts) {
        typename decltype(rows.amounts)::value_type row{};
        key(row);
        row.amount_role = std::string(amount_role);
        row.sequence_number = ++sequence_number;
        row.value = amount.value;
        row.start_date = amount.start_date;
        rows.amounts.push_back(std::move(row));
    }
}

template <typename RateRow>
void fill_floating_rate(RateRow& row, const bond_floating_leg_data& floating) {
    row.rate_kind = "floating";
    row.index = floating.index;
    row.is_in_arrears = floating.is_in_arrears;
    row.last_recent_period = floating.last_recent_period;
    row.last_recent_period_calendar = floating.last_recent_period_calendar;
    if (floating.fixing_days)
        row.fixing_days = static_cast<std::int64_t>(*floating.fixing_days);
    row.lookback = floating.lookback;
    row.rate_cutoff = floating.rate_cutoff;
    row.is_averaged = floating.is_averaged;
    row.has_sub_periods = floating.has_sub_periods;
    row.include_spread = floating.include_spread;
    row.is_not_resetting_xccy = floating.is_not_resetting_xccy;
    row.naked_option = floating.naked_option;
    row.local_cap_floor = floating.local_cap_floor;
    row.stub_use_original_curve = floating.stub_use_original_curve;
    row.observation_shift = floating.observation_shift;
    if (floating.front_stub_interpolation) {
        const auto& stub = *floating.front_stub_interpolation;
        row.front_stub_short_index = stub.short_index;
        row.front_stub_long_index = stub.long_index;
        row.front_stub_rounding_type = stub.rounding_type;
        row.front_stub_rounding_precision = stub.rounding_precision;
    }
    if (floating.back_stub_interpolation) {
        const auto& stub = *floating.back_stub_interpolation;
        row.back_stub_short_index = stub.short_index;
        row.back_stub_long_index = stub.long_index;
        row.back_stub_rounding_type = stub.rounding_type;
        row.back_stub_rounding_precision = stub.rounding_precision;
    }
}

/*
 * One leg's rows: the leg, its amounts, its rate, its amortizations and its
 * five schedules. A leg that carries nothing writes no row, which is what a
 * container that came from a row set rather than a document holds.
 */
template <typename Rows, typename LegKey, typename OwnerKey>
void split_leg(Rows& rows, const LegKey& key, const OwnerKey& owner, const bond_leg_data& leg) {
    if (is_empty(leg))
        return;

    typename decltype(rows.legs)::value_type leg_row{};
    key(leg_row);
    leg_row.payer = leg.payer;
    leg_row.leg_type = leg.leg_type;
    leg_row.currency = leg.currency;
    leg_row.payment_convention = leg.payment_convention;
    leg_row.payment_lag = leg.payment_lag;
    leg_row.payment_calendar = leg.payment_calendar;
    leg_row.day_counter = leg.day_counter;
    leg_row.last_period_day_counter = leg.last_period_day_counter;
    leg_row.notional_payment_lag = leg.notional_payment_lag;
    leg_row.strict_notional_dates = leg.strict_notional_dates;
    leg_row.indexings_from_asset_leg = leg.indexings_from_asset_leg;
    if (leg.settlement) {
        leg_row.settlement_fx_index = leg.settlement->fx_index;
        leg_row.settlement_fixing_date = leg.settlement->fixing_date;
    }
    rows.legs.push_back(std::move(leg_row));

    split_amounts(rows, key, "notional", leg.notionals);

    typename decltype(rows.rates)::value_type rate_row{};
    key(rate_row);
    if (leg.rate && leg.rate->floating) {
        const auto& floating = *leg.rate->floating;
        split_amounts(rows, key, "spread", floating.spreads);
        split_amounts(rows, key, "cap", floating.caps);
        split_amounts(rows, key, "floor", floating.floors);
        split_amounts(rows, key, "gearing", floating.gearings);
        fill_floating_rate(rate_row, floating);
        rows.rates.push_back(std::move(rate_row));
        split_schedule(rows.schedules,
                       rows.schedule_dates,
                       owner,
                       "fixing_schedule",
                       floating.fixing_schedule);
        split_schedule(
            rows.schedules, rows.schedule_dates, owner, "reset_schedule", floating.reset_schedule);
    } else if (leg.rate && leg.rate->formula_based) {
        const auto& formula = *leg.rate->formula_based;
        rate_row.rate_kind = "formula_based";
        rate_row.index = formula.index;
        rate_row.is_in_arrears = formula.is_in_arrears;
        rate_row.fixing_days = formula.fixing_days;
        rate_row.fixing_calendar = formula.fixing_calendar;
        rows.rates.push_back(std::move(rate_row));
    } else if (leg.rate && leg.rate->fixed) {
        split_amounts(rows, key, "rate", leg.rate->fixed->rates);
        rate_row.rate_kind = "fixed";
        rows.rates.push_back(std::move(rate_row));
    }

    int sequence_number = 0;
    for (const auto& amortization : leg.amortizations) {
        typename decltype(rows.amortizations)::value_type row{};
        key(row);
        row.sequence_number = ++sequence_number;
        row.amortization_type = amortization.type;
        row.value = amortization.value;
        row.start_date = amortization.start_date;
        row.end_date = amortization.end_date;
        row.frequency = amortization.frequency;
        row.underflow = amortization.underflow;
        rows.amortizations.push_back(std::move(row));
    }

    split_schedule(rows.schedules, rows.schedule_dates, owner, "schedule", leg.schedule);
    split_schedule(
        rows.schedules, rows.schedule_dates, owner, "payment_schedule", leg.payment_schedule);
    if (!leg.payment_dates.empty()) {
        bond_schedule_data dates;
        bond_schedule_dates block;
        block.dates = leg.payment_dates;
        dates.dates.push_back(std::move(block));
        split_schedule(rows.schedules, rows.schedule_dates, owner, "payment_dates", dates);
    }
}

void split_option_block(bond_trade_rows& rows,
                        const bond_instrument_data& data,
                        const boost::uuids::uuid& trade_id,
                        const boost::uuids::uuid& activity_id) {
    const auto stamp = [&](auto& row) {
        row.trade_id = trade_id;
        row.trade_activity_id = activity_id;
    };
    const auto owner = [&](auto& row) {
        stamp(row);
        row.owner_role = "option";
        row.owner_number = 1;
    };

    if (data.option_data) {
        const auto& block = *data.option_data;
        instrument_option row{};
        stamp(row);
        row.long_short = block.long_short;
        row.option_type = block.option_type;
        row.payoff_type = block.payoff_type;
        row.payoff_type_2 = block.payoff_type_2;
        row.style = block.style;
        row.notice_period = block.notice_period;
        row.notice_calendar = block.notice_calendar;
        row.notice_convention = block.notice_convention;
        row.mid_coupon_exercise = block.mid_coupon_exercise;
        row.settlement = block.settlement;
        row.settlement_method = block.settlement_method;
        row.pay_off_at_expiry = block.pay_off_at_expiry;
        row.premium_amount = block.premium_amount;
        row.premium_currency = block.premium_currency;
        row.premium_pay_date = parse_iso(block.premium_pay_date);
        row.exercise_fee_settlement_period = block.exercise_fee_settlement_period;
        row.exercise_fee_settlement_calendar = block.exercise_fee_settlement_calendar;
        row.exercise_fee_settlement_convention = block.exercise_fee_settlement_convention;
        row.automatic_exercise = block.automatic_exercise;
        row.has_exercise_data = block.exercise_data.has_value();
        if (block.exercise_data) {
            row.exercise_date = parse_iso(block.exercise_data->date);
            row.exercise_price = block.exercise_data->price;
        }
        row.has_payment_data = block.payment_data.has_value();
        if (block.payment_data && block.payment_data->rules) {
            const auto& rules = *block.payment_data->rules;
            row.payment_lag = static_cast<int>(rules.lag);
            row.payment_calendar = rules.calendar;
            row.payment_convention = rules.convention;
            row.payment_relative_to = rules.relative_to;
        }
        row.has_settlement_data = block.settlement_data.has_value();
        if (block.settlement_data) {
            row.settlement_pay_currency = block.settlement_data->pay_currency;
            row.settlement_fx_index = block.settlement_data->fx_index;
            row.settlement_fixing_date = parse_iso(block.settlement_data->fixing_date);
        }
        rows.option = std::move(row);

        int sequence_number = 0;
        for (const auto& premium : block.premiums) {
            instrument_option_premium child{};
            stamp(child);
            child.sequence_number = ++sequence_number;
            child.amount = premium.amount;
            child.currency = premium.currency;
            child.pay_date = date_of(premium.pay_date);
            child.has_settlement = premium.settlement.has_value();
            if (premium.settlement) {
                child.settlement_pay_currency = premium.settlement->pay_currency;
                child.settlement_fx_index = premium.settlement->fx_index;
                child.settlement_fixing_date = parse_iso(premium.settlement->fixing_date);
            }
            rows.option_premiums.push_back(std::move(child));
        }

        sequence_number = 0;
        for (const auto& fee : block.exercise_fees) {
            instrument_option_exercise_fee child{};
            stamp(child);
            child.sequence_number = ++sequence_number;
            child.amount = fee.amount;
            child.type = fee.type;
            child.start_date = parse_iso(fee.start_date);
            child.currency = fee.currency;
            rows.option_exercise_fees.push_back(std::move(child));
        }

        if (block.payment_data) {
            sequence_number = 0;
            for (const auto& date : block.payment_data->dates) {
                instrument_option_payment_date child{};
                stamp(child);
                child.sequence_number = ++sequence_number;
                child.payment_date = date_of(date);
                rows.option_payment_dates.push_back(std::move(child));
            }
        }

        // The exercise price list pairs one price with each exercise date the
        // option states, as the option block's fourth child holds it.
        if (block.exercise_prices && !data.option_exercise_dates.empty()) {
            const auto prices = split_numbers(*block.exercise_prices);
            sequence_number = 0;
            for (std::size_t i = 0; i < prices.size() && i < data.option_exercise_dates.size();
                 ++i) {
                auto parsed = ores::utility::decimal::decimal::from_string(prices[i]);
                if (!parsed)
                    continue;
                instrument_option_exercise_price child{};
                stamp(child);
                child.sequence_number = ++sequence_number;
                child.exercise_date = date_of(data.option_exercise_dates[i]);
                child.price = std::move(*parsed);
                rows.option_exercise_prices.push_back(std::move(child));
            }
        }
    }

    if (!data.option_exercise_dates.empty()) {
        bond_schedule_data schedule;
        bond_schedule_dates dates;
        dates.dates = data.option_exercise_dates;
        schedule.dates.push_back(std::move(dates));
        split_schedule(rows.schedules, rows.schedule_dates, owner, "exercise_dates", schedule);
    }
    if (data.option_exercise_schedule)
        split_schedule(rows.schedules,
                       rows.schedule_dates,
                       owner,
                       "exercise_schedule",
                       *data.option_exercise_schedule);
}

}

bond_rows disassemble_bond(const bond_instrument_data& data) {
    bond_rows result;
    auto& trade = result.trade;
    const auto trade_id = data.instrument.identity.trade_id;
    const auto activity_id = data.instrument.identity.trade_activity_id;
    const auto issue_id = data.issue.issue_id;

    int leg_number = 0;
    for (const auto& leg : data.bond_legs) {
        ++leg_number;
        const auto key = [&](auto& row) {
            row.issue_id = issue_id;
            row.leg_number = leg_number;
        };
        split_leg(result.issue, key, key, leg);
    }

    const auto trade_leg = [&](std::string_view role, const bond_leg_data& leg) {
        const auto key = [&](auto& row) {
            row.trade_id = trade_id;
            row.trade_activity_id = activity_id;
            row.leg_role = std::string(role);
            row.leg_number = 1;
        };
        const auto owner = [&](auto& row) {
            row.trade_id = trade_id;
            row.trade_activity_id = activity_id;
            row.owner_role = std::string(role);
            row.owner_number = 1;
        };
        split_leg(trade, key, owner, leg);
    };
    trade_leg("trs_funding", data.trs_funding_leg);
    trade_leg("repo", data.repo_leg);
    trade_leg("ascot_swap", data.ascot_swap_leg);

    split_option_block(trade, data, trade_id, activity_id);

    if (data.strike_data) {
        const auto& strike = *data.strike_data;
        instrument_strike row{};
        row.trade_id = trade_id;
        row.trade_activity_id = activity_id;
        row.strike_role = "single";
        row.sequence_number = 1;
        row.price_value = strike.price_value;
        row.price_currency = strike.price_currency;
        row.yield_value = strike.yield_value;
        row.yield_compounding = strike.yield_compounding;
        row.bare_value = strike.bare_value;
        row.bare_currency = strike.bare_currency;
        trade.strike = std::move(row);
    }

    if (data.forward_long_in_forward || data.forward_settlement || data.forward_premium) {
        bond_forward row{};
        row.trade_id = trade_id;
        row.trade_activity_id = activity_id;
        row.long_in_forward = data.forward_long_in_forward;
        if (data.forward_settlement) {
            const auto& settlement = *data.forward_settlement;
            row.forward_maturity_date = settlement.forward_maturity_date;
            row.forward_settlement_date = settlement.forward_settlement_date;
            row.settlement = settlement.settlement;
            row.amount = settlement.amount;
            row.lock_rate = settlement.lock_rate;
            row.dv01 = settlement.dv01;
            row.lock_rate_day_counter = settlement.lock_rate_day_counter;
            row.settlement_dirty = settlement.settlement_dirty;
        }
        if (data.forward_premium) {
            row.premium_amount = data.forward_premium->amount;
            row.premium_date = data.forward_premium->date;
        }
        trade.forward = std::move(row);
    }

    const auto trs_owner = [&](auto& row) {
        row.trade_id = trade_id;
        row.trade_activity_id = activity_id;
        row.owner_role = "trs";
        row.owner_number = 1;
    };
    split_schedule(trade.schedules, trade.schedule_dates, trs_owner, "schedule", data.trs_schedule);

    trade.underlyings = data.instrument_underlyings;

    // The product's fact row is the one its trade type names. The container
    // also states three option members and three return members beside the
    // fact row, and the row holds them.
    const auto& ttc = data.instrument.identity.trade_type_code;
    const auto stamped = [&](auto row) {
        row.trade_id = trade_id;
        row.trade_activity_id = activity_id;
        return row;
    };
    if (ttc == "BondOption" && data.option) {
        auto row = stamped(*data.option);
        row.redemption = data.option_redemption;
        row.price_type = data.option_price_type;
        row.knocks_out = data.option_knocks_out;
        trade.bond_option = std::move(row);
    } else if (ttc == "BondTRS" && data.trs) {
        auto row = stamped(*data.trs);
        row.payer = data.trs_payer;
        row.initial_price = data.trs_initial_price;
        if (!data.trs_price_type.empty())
            row.price_type = data.trs_price_type;
        trade.trs = std::move(row);
    } else if (ttc == "BondRepo" && data.repo) {
        trade.repo = stamped(*data.repo);
    } else if (ttc == "BondFuture" && data.future) {
        trade.future = stamped(*data.future);
    } else if (ttc == "Ascot" && data.ascot_row) {
        trade.ascot = stamped(*data.ascot_row);
    }
    return result;
}

void assemble_bond(bond_instrument_data& data,
                   const bond_trade_rows& trade,
                   const issue_leg_rows& issue) {
    apply_leg_family(data, trade, issue);
    apply_option_block(data, trade);
    apply_strike(data, trade);
    apply_forward(data, trade);
    if (!trade.underlyings.empty())
        data.instrument_underlyings = trade.underlyings;
    if (trade.bond_option)
        data.option = trade.bond_option;
    if (trade.trs)
        data.trs = trade.trs;
    if (trade.repo)
        data.repo = trade.repo;
    if (trade.future)
        data.future = trade.future;
    if (trade.ascot)
        data.ascot_row = trade.ascot;
    apply_option_residue(data);
    apply_trs_residue(data, trade);
}

std::vector<bond_leg_data> bond_legs_of(const issue_leg_rows& rows) {
    std::vector<bond_leg_data> legs;
    for (const auto& row : rows.legs)
        legs.push_back(build_leg(rows, row));
    return legs;
}


}
