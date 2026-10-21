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

using trading::domain::credit_basket_constituent;
using trading::domain::credit_default_swap_instrument;
using trading::domain::credit_instrument;
using trading::domain::credit_instrument_data;
using trading::domain::credit_linked_swap_instrument;
using trading::domain::index_credit_default_swap_instrument;
using trading::domain::index_credit_default_swap_option_instrument;
using trading::domain::instrument_option;
using trading::domain::instrument_option_exercise_fee;
using trading::domain::instrument_option_exercise_price;
using trading::domain::instrument_option_payment_date;
using trading::domain::instrument_option_premium;
using trading::domain::instrument_schedule;
using trading::domain::instrument_schedule_date;
using trading::domain::instrument_strike;
using trading::domain::risk_participation_agreement_instrument;
using trading::domain::swap_leg;
using trading::domain::swap_leg_amount;
using trading::domain::swap_leg_rate;
using trading::domain::synthetic_cdo_instrument;

using xml::record_reader;
using xml::record_writer;

// ---------------------------------------------------------------------------
// The vocabulary both directions share
// ---------------------------------------------------------------------------

/// The actor and reason every imported row states.
constexpr std::string_view actor = "ores";
constexpr std::string_view change_reason = "system.external_data_import";
constexpr std::string_view change_commentary = "Imported from ORE XML";

/**
 * @brief The trade types the family states, which are the ones the old
 * dispatcher took.
 */
constexpr bool is_credit_trade_type(std::string_view type) {
    return type == "CreditDefaultSwap" || type == "IndexCreditDefaultSwap" ||
           type == "IndexCreditDefaultSwapOption" || type == "CreditLinkedSwap" ||
           type == "SyntheticCDO" || type == "RiskParticipationAgreement";
}

void stamp(swap_leg& row) {
    row.audit.modified_by = std::string(actor);
    row.audit.performed_by = std::string(actor);
    row.audit.change_reason_code = std::string(change_reason);
    row.audit.change_commentary = std::string(change_commentary);
}

template <typename Row>
void stamp_row(Row& row) {
    row.modified_by = std::string(actor);
    row.performed_by = std::string(actor);
    row.change_reason_code = std::string(change_reason);
    row.change_commentary = std::string(change_commentary);
}

auto make_header(std::string_view trade_type_code) -> credit_instrument {
    credit_instrument r;
    r.identity.trade_type_code = std::string(trade_type_code);
    r.audit.modified_by = std::string(actor);
    r.audit.performed_by = std::string(actor);
    r.audit.change_reason_code = std::string(change_reason);
    r.audit.change_commentary = std::string(change_commentary);
    return r;
}

// ---------------------------------------------------------------------------
// Reading: the values, dates and spellings a row states
// ---------------------------------------------------------------------------

auto to_domain_date(const std::string& text) -> std::chrono::year_month_day {
    if (text.empty())
        return {};
    return ores::platform::time::datetime::from_iso8601_date(text);
}

auto to_optional_domain_date(const std::string& text)
    -> std::optional<std::chrono::year_month_day> {
    if (text.empty())
        return std::nullopt;
    return ores::platform::time::datetime::from_iso8601_date(text);
}

auto to_ore_date(const std::chrono::year_month_day& d) -> std::string {
    return d.ok() ? ores::platform::time::datetime::to_iso8601_date(d) : std::string{};
}

auto to_ore_date(const std::optional<std::chrono::year_month_day>& d) -> std::string {
    return d ? ores::platform::time::datetime::to_iso8601_date(*d) : std::string{};
}

auto text_of(const record_reader& r, std::string_view name) -> std::string {
    if (const auto value = r.text(name))
        return *value;
    return {};
}

auto spelling_of(const record_reader& r, std::string_view name) -> std::string {
    if (const auto value = r.spelling(name))
        return *value;
    return {};
}

// A boolean member the schema types as text keeps the spelling the document
// wrote, so the reading accepts every spelling the binder accepts for a flag.
auto is_flag_on(std::string_view text) -> bool {
    return text == "Y" || text == "YES" || text == "TRUE" || text == "True" || text == "true" ||
           text == "1";
}

auto flag_text(bool value) -> std::string_view {
    return value ? "true" : "false";
}

/// The exact decimal an xs:simpleContent numeric leaf carries.
auto decimal_text(const record_reader& r) -> decimal {
    return r.own_decimal().value_or(decimal{});
}

auto optional_decimal_of(const record_reader& r, std::string_view name) -> std::optional<decimal> {
    const auto value = decimal_as_double(r, name);
    if (!value)
        return std::nullopt;
    return decimal_from_double(*value);
}

/// A number the document spelled as text, such as a CDO's upfront fee.
auto decimal_from_text(const std::string& text) -> std::optional<decimal> {
    if (text.empty())
        return std::nullopt;
    if (auto parsed = decimal::from_string(text))
        return std::move(*parsed);
    return std::nullopt;
}

auto optional_int_of(const record_reader& r, std::string_view name) -> std::optional<int> {
    if (const auto value = r.integer(name))
        return static_cast<int>(*value);
    return std::nullopt;
}

/// The first schedule rule's tenor, which is the leg's payment frequency.
auto first_tenor(const std::optional<record_reader>& schedule) -> std::string {
    if (!schedule)
        return {};
    const auto rules = schedule->children("Rules");
    if (rules.empty())
        return {};
    return text_of(rules.front(), "Tenor");
}

/// The header's dates are the first schedule rule of the leg: its start and end.
void map_leg_dates(const record_reader& leg, credit_instrument& header) {
    const auto schedule = leg.child("ScheduleData");
    if (!schedule)
        return;
    const auto rules = schedule->children("Rules");
    if (rules.empty())
        return;
    const auto& rule = rules.front();
    header.start_date = to_optional_domain_date(text_of(rule, "StartDate"));
    if (rule.has("EndDate"))
        header.maturity_date = to_optional_domain_date(text_of(rule, "EndDate"));
}

// The accrual flags and the protection payment time are the same three
// elements on every swap-like product.
template <typename Fact>
void read_accrual(Fact& fact, const record_reader& d) {
    fact.settles_accrual = d.flag("SettlesAccrual");
    fact.rebates_accrual = d.flag("RebatesAccrual");
    fact.pays_at_default_time = d.flag("PaysAtDefaultTime");
    fact.protection_payment_time = d.text("ProtectionPaymentTime");
}

// ---------------------------------------------------------------------------
// Reading: the shared children
// ---------------------------------------------------------------------------

void stamp_schedule_rule(instrument_schedule& row, const record_reader& rule) {
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

void stamp_schedule_dates(instrument_schedule& row, const record_reader& block) {
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
// becomes a schedule row, and a date block's dates become rows of their own.
// The ordinals run across both arms, in document order.
void read_schedule(credit_instrument_data& result,
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

void read_premiums(credit_instrument_data& result, const record_reader& option) {
    const auto block = option.child("Premiums");
    if (!block)
        return;
    int sequence_number = 0;
    for (const auto& premium : block->children("Premium")) {
        instrument_option_premium row;
        row.sequence_number = ++sequence_number;
        row.amount = decimal_from_text(text_of(premium, "Amount")).value_or(decimal{});
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

void read_exercise_fees(credit_instrument_data& result, const record_reader& option) {
    const auto block = option.child("ExerciseFees");
    if (!block)
        return;
    int sequence_number = 0;
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

void read_payment_dates(credit_instrument_data& result, const record_reader& option) {
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

// The exercise price list pairs one price with each exercise date the group
// states, in order.
auto split_numbers(const std::string& text) -> std::vector<std::string> {
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

void read_exercise_prices(credit_instrument_data& result,
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

void read_option_block(credit_instrument_data& result, const record_reader& od) {
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
                row.payment_lag = static_cast<int>(*lag);
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
    const auto list = group ? group->values("ExerciseDate") : std::vector<std::string>{};
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

// ---------------------------------------------------------------------------
// Reading: the legs, their economics and the basket
// ---------------------------------------------------------------------------

swap_leg read_leg(const record_reader& leg, int leg_number, std::string_view group) {
    swap_leg row;
    row.identity.leg_number = leg_number;
    row.payer = is_flag_on(text_of(leg, "Payer"));
    row.leg_type_code = spelling_of(leg, "LegType");
    row.currency = text_of(leg, "Currency");
    row.day_count_fraction_code = spelling_of(leg, "DayCounter");
    row.business_day_convention_code = spelling_of(leg, "PaymentConvention");
    row.payment_frequency_code = tenor_to_payment_frequency(first_tenor(leg.child("ScheduleData")));
    if (const auto floating = leg.child("FloatingLegData"))
        row.floating_index_code = text_of(*floating, "Index");
    row.leg_group = std::string(group);
    stamp(row);
    return row;
}

std::vector<swap_leg_amount> read_leg_amounts(const record_reader& leg, int leg_number) {
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

    // The document states its notional either as a plain list or as an
    // amortisation schedule, and a cashflow leg states its amounts as dated
    // payments. Each arm lands in the same column.
    if (const auto notionals = leg.child("Notionals")) {
        for (const auto& notional : notionals->children("Notional"))
            append(decimal_text(notional), text_of(notional, "startDate"));
    } else if (const auto cashflow_data = leg.child("CashflowData")) {
        if (const auto cashflow = cashflow_data->child("Cashflow"))
            for (const auto& amount : cashflow->children("Amount"))
                append(decimal_text(amount), text_of(amount, "date"));
    } else if (const auto amortizations = leg.child("Amortizations")) {
        for (const auto& amortization : amortizations->children("AmortizationData")) {
            const auto value = amortization.has("Value") ?
                                   optional_decimal_of(amortization, "Value").value_or(decimal{}) :
                                   decimal{};
            append(value, text_of(amortization, "StartDate"));
        }
    }
    return amounts;
}

std::vector<swap_leg_rate> read_leg_rates(const record_reader& leg, int leg_number) {
    std::vector<swap_leg_rate> rates;
    const auto append = [&](std::string_view role, const decimal& value) {
        swap_leg_rate row;
        row.leg_number = leg_number;
        row.rate_role = std::string(role);
        row.sequence_number = static_cast<int>(rates.size()) + 1;
        row.value = value;
        stamp_row(row);
        rates.push_back(std::move(row));
    };

    if (const auto fixed = leg.child("FixedLegData")) {
        if (const auto list = fixed->child("Rates"))
            for (const auto& rate : list->children("Rate"))
                append("fixed", decimal_text(rate));
    }
    if (const auto floating = leg.child("FloatingLegData")) {
        if (const auto list = floating->child("Spreads"))
            for (const auto& spread : list->children("Spread"))
                append("spread", decimal_text(spread));
    }
    return rates;
}

void append_leg(credit_instrument_data& result, const record_reader& leg, std::string_view group) {
    const int leg_number = static_cast<int>(result.legs.size()) + 1;
    result.legs.push_back(read_leg(leg, leg_number, group));
    for (auto& amount : read_leg_amounts(leg, leg_number))
        result.leg_amounts.push_back(std::move(amount));
    for (auto& rate : read_leg_rates(leg, leg_number))
        result.leg_rates.push_back(std::move(rate));
}

/// The legs of one of the four payment groups a credit linked swap states.
void append_payment_group(credit_instrument_data& result,
                          const record_reader& payments,
                          std::string_view group) {
    for (const auto& leg : payments.children("LegData"))
        append_leg(result, leg, group);
}

void append_basket(credit_instrument_data& result, const record_reader& basket) {
    int sequence = 0;
    for (const auto& name : basket.children("Name")) {
        credit_basket_constituent row;
        row.sequence_number = ++sequence;
        row.issuer_id = text_of(name, "IssuerId");
        if (name.has("Qualifier"))
            row.qualifier = text_of(name, "Qualifier");
        if (name.has("CreditCurveId"))
            row.credit_curve_id = text_of(name, "CreditCurveId");
        row.notional = name.decimal("Notional");
        row.weight = decimal_as_double(name, "Weight");
        if (name.has("Currency"))
            row.currency = spelling_of(name, "Currency");
        stamp_row(row);
        result.constituents.push_back(std::move(row));
    }
}

// ---------------------------------------------------------------------------
// Reading: one function per product
// ---------------------------------------------------------------------------

/// The fact fields an index swap states, which its option also wraps.
auto read_index_swap(const record_reader& d) -> index_credit_default_swap_instrument {
    index_credit_default_swap_instrument fact;
    fact.credit_curve_id = text_of(d, "CreditCurveId");
    fact.issuer_id = d.text("IssuerId");
    read_accrual(fact, d);
    fact.protection_start_date = to_optional_domain_date(text_of(d, "ProtectionStart"));
    fact.upfront_date = to_optional_domain_date(text_of(d, "UpfrontDate"));
    fact.upfront_fee = d.decimal("UpfrontFee");
    fact.trade_date = to_optional_domain_date(text_of(d, "TradeDate"));
    fact.cash_settlement_days = optional_int_of(d, "CashSettlementDays");
    return fact;
}

auto read_cds(const record_reader& trade) -> credit_instrument_data {
    credit_instrument_data result;
    result.instrument = make_header("CreditDefaultSwap");

    credit_default_swap_instrument fact;
    if (const auto d = trade.child("CreditDefaultSwapData")) {
        if (const auto curve = d->text("CreditCurveId"))
            fact.credit_curve_id = *curve;
        fact.issuer_id = d->text("IssuerId");
        fact.reference_obligation = d->text("ReferenceObligation");
        read_accrual(fact, *d);
        fact.protection_start_date = to_optional_domain_date(text_of(*d, "ProtectionStart"));
        fact.upfront_date = to_optional_domain_date(text_of(*d, "UpfrontDate"));
        fact.upfront_fee = d->decimal("UpfrontFee");
        fact.recovery_rate = decimal_as_double(*d, "FixedRecoveryRate");
        fact.trade_date = to_optional_domain_date(text_of(*d, "TradeDate"));
        fact.cash_settlement_days = optional_int_of(*d, "CashSettlementDays");
        result.instrument.reference_entity =
            fact.issuer_id ? *fact.issuer_id : fact.credit_curve_id.value_or("");
        if (const auto leg = d->child("LegData")) {
            map_leg_dates(*leg, result.instrument);
            append_leg(result, *leg, "premium");
        }
    }
    stamp_row(fact);
    result.credit_default_swap_instruments.push_back(std::move(fact));
    return result;
}

auto read_index_cds(const record_reader& trade) -> credit_instrument_data {
    credit_instrument_data result;
    result.instrument = make_header("IndexCreditDefaultSwap");

    index_credit_default_swap_instrument fact;
    if (const auto d = trade.child("IndexCreditDefaultSwapData")) {
        fact = read_index_swap(*d);
        result.instrument.reference_entity = fact.credit_curve_id;
        if (const auto leg = d->child("LegData")) {
            map_leg_dates(*leg, result.instrument);
            append_leg(result, *leg, "premium");
        }
        if (const auto basket = d->child("BasketData"))
            append_basket(result, *basket);
    }
    stamp_row(fact);
    result.index_credit_default_swap_instruments.push_back(std::move(fact));
    return result;
}

auto read_index_cds_option(const record_reader& trade) -> credit_instrument_data {
    credit_instrument_data result;
    result.instrument = make_header("IndexCreditDefaultSwapOption");

    index_credit_default_swap_option_instrument fact;
    if (const auto od = trade.child("IndexCreditDefaultSwapOptionData")) {
        fact.knock_out = od->flag("KnockOut");
        fact.index_term = od->text("IndexTerm");
        if (od->has("StrikeType"))
            fact.strike_type = spelling_of(*od, "StrikeType");
        fact.trade_date = to_optional_domain_date(text_of(*od, "TradeDate"));
        fact.front_end_protection_start_date =
            to_optional_domain_date(text_of(*od, "FrontEndProtectionStartDate"));

        if (const auto d = od->child("IndexCreditDefaultSwapData")) {
            result.wrapped_index_swap = read_index_swap(*d);
            stamp_row(*result.wrapped_index_swap);
            result.instrument.reference_entity = result.wrapped_index_swap->credit_curve_id;
            if (const auto leg = d->child("LegData")) {
                map_leg_dates(*leg, result.instrument);
                append_leg(result, *leg, "premium");
            }
            if (const auto basket = d->child("BasketData"))
                append_basket(result, *basket);
        }

        // The strike is the option's one shared strike row, and its role says
        // so because the same table holds a cap/floor's arms.
        if (const auto strike = od->decimal("Strike")) {
            instrument_strike row;
            row.strike_role = "single";
            row.sequence_number = 1;
            row.bare_value = *strike;
            stamp_row(row);
            result.strikes.push_back(std::move(row));
        }

        if (const auto option = od->child("OptionData"))
            read_option_block(result, *option);
    }
    stamp_row(fact);
    result.index_credit_default_swap_option_instruments.push_back(std::move(fact));
    return result;
}

auto read_credit_linked_swap(const record_reader& trade) -> credit_instrument_data {
    credit_instrument_data result;
    result.instrument = make_header("CreditLinkedSwap");

    credit_linked_swap_instrument fact;
    if (const auto d = trade.child("CreditLinkedSwapData")) {
        fact.credit_curve_id = text_of(*d, "CreditCurveId");
        fact.settles_accrual = d->flag("SettlesAccrual");
        fact.recovery_rate = decimal_as_double(*d, "FixedRecoveryRate");
        fact.default_payment_time = d->text("DefaultPaymentTime");
        result.instrument.reference_entity = fact.credit_curve_id;

        // The header's dates are those of the first contingent payment leg.
        if (const auto contingent = d->child("ContingentPayments")) {
            const auto legs = contingent->children("LegData");
            if (!legs.empty())
                map_leg_dates(legs.front(), result.instrument);
        }
        if (const auto payments = d->child("IndependentPayments"))
            append_payment_group(result, *payments, "independent");
        if (const auto payments = d->child("ContingentPayments"))
            append_payment_group(result, *payments, "contingent");
        if (const auto payments = d->child("DefaultPayments"))
            append_payment_group(result, *payments, "default");
        if (const auto payments = d->child("RecoveryPayments"))
            append_payment_group(result, *payments, "recovery");
    }
    stamp_row(fact);
    result.credit_linked_swap_instruments.push_back(std::move(fact));
    return result;
}

auto read_synthetic_cdo(const record_reader& trade) -> credit_instrument_data {
    credit_instrument_data result;
    result.instrument = make_header("SyntheticCDO");

    synthetic_cdo_instrument fact;
    if (const auto d = trade.child("CdoData")) {
        fact.qualifier = text_of(*d, "Qualifier");
        fact.protection_start_date = to_domain_date(text_of(*d, "ProtectionStart"));
        fact.upfront_date = to_optional_domain_date(text_of(*d, "UpfrontDate"));
        fact.upfront_fee = decimal_from_text(text_of(*d, "UpfrontFee"));
        read_accrual(fact, *d);
        fact.attachment_point = decimal_as_double(*d, "AttachmentPoint").value_or(0.0);
        fact.detachment_point = decimal_as_double(*d, "DetachmentPoint").value_or(0.0);
        fact.recovery_rate = decimal_as_double(*d, "FixedRecoveryRate");
        result.instrument.reference_entity = fact.qualifier;
        result.instrument.start_date = to_optional_domain_date(text_of(*d, "ProtectionStart"));
        if (const auto leg = d->child("LegData")) {
            map_leg_dates(*leg, result.instrument);
            append_leg(result, *leg, "premium");
        }
        if (const auto basket = d->child("BasketData"))
            append_basket(result, *basket);
    }
    stamp_row(fact);
    result.synthetic_cdo_instruments.push_back(std::move(fact));
    return result;
}

auto read_rpa(const record_reader& trade) -> credit_instrument_data {
    credit_instrument_data result;
    result.instrument = make_header("RiskParticipationAgreement");

    risk_participation_agreement_instrument fact;
    if (const auto d = trade.child("RiskParticipationAgreementData")) {
        fact.participation_rate = decimal_as_double(*d, "ParticipationRate").value_or(0.0);
        fact.credit_curve_id = text_of(*d, "CreditCurveId");
        if (const auto issuer = d->text("IssuerId"))
            fact.issuer_id = *issuer;
        if (d->has("SettlesAccrual"))
            fact.settles_accrual = is_flag_on(text_of(*d, "SettlesAccrual"));
        fact.recovery_rate = decimal_as_double(*d, "FixedRecoveryRate");
        result.instrument.reference_entity =
            fact.issuer_id ? *fact.issuer_id : fact.credit_curve_id;
        result.instrument.start_date = to_optional_domain_date(text_of(*d, "ProtectionStart"));
        result.instrument.maturity_date = to_optional_domain_date(text_of(*d, "ProtectionEnd"));

        // A leg keeps its own schedule when it states one, because the header's
        // dates are the protection period and not the leg's.
        const auto append_with_schedule = [&result](const record_reader& leg,
                                                    std::string_view group) {
            const int leg_number = static_cast<int>(result.legs.size()) + 1;
            append_leg(result, leg, group);
            if (const auto schedule = leg.child("ScheduleData"))
                read_schedule(result, *schedule, "swap", leg_number, "schedule");
        };
        if (const auto protection_fee = d->child("ProtectionFee"))
            for (const auto& leg : protection_fee->children("LegData"))
                append_with_schedule(leg, "protection_fee");

        if (const auto underlying = d->child("Underlying")) {
            for (const auto& leg : underlying->children("LegData"))
                append_with_schedule(leg, "underlying");
            if (underlying->has("NakedOption"))
                fact.naked_option = is_flag_on(text_of(*underlying, "NakedOption"));

            // The agreement has no option of its own, so the option rows are
            // the underlying's.
            if (const auto option = underlying->child("OptionData"))
                read_option_block(result, *option);
        }
    }
    stamp_row(fact);
    result.risk_participation_agreement_instruments.push_back(std::move(fact));
    return result;
}

// ---------------------------------------------------------------------------
// Writing: the spellings a table refuses
// ---------------------------------------------------------------------------

auto accepts_spelling(const record_writer& w,
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
void set_spelling_or(record_writer& w,
                     std::string_view name,
                     const std::string& value,
                     std::string_view fallback,
                     std::string_view what) {
    w.set_spelling(name, accepts_spelling(w, name, value) ? value : fallback, what);
}

void set_optional_date(record_writer& w,
                       std::string_view name,
                       const std::optional<std::chrono::year_month_day>& value) {
    if (value)
        w.set_text(name, to_ore_date(*value));
}

void set_optional_days(record_writer& w, std::string_view name, const std::optional<int>& value) {
    if (value)
        w.set_integer(name, *value);
}

void set_optional_decimal_from_double(record_writer& w,
                                      std::string_view name,
                                      const std::optional<double>& value) {
    if (value)
        set_decimal_from_double(w, name, *value);
}

// ---------------------------------------------------------------------------
// Writing: the shared children
// ---------------------------------------------------------------------------

std::vector<const instrument_schedule*> schedules_of(const credit_instrument_data& v,
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

std::vector<const instrument_schedule_date*> schedule_dates_of(const credit_instrument_data& v,
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

void write_schedule_rules(record_writer& schedule,
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

/// The stored schedule rows of one owner, written back as the document's own
/// rules and date lists, and whether they stated a schedule at all.
bool write_stored_schedule(record_writer schedule,
                           const credit_instrument_data& v,
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
std::vector<std::chrono::year_month_day> exercise_dates_of(const credit_instrument_data& v) {
    std::vector<std::chrono::year_month_day> out;
    for (const auto* row : schedule_dates_of(v, "option", 1, "exercise_dates", 1))
        out.push_back(row->schedule_date);
    return out;
}

void write_option_block(record_writer option, const credit_instrument_data& v) {
    if (v.options.empty()) {
        option.set_text("LongShort", "Long");
    } else {
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
            set_spelling_or(option,
                            "SettlementMethod",
                            *row.settlement_method,
                            "PhysicalOTC",
                            "settlement method");
        option.set_optional_text("PayOffAtExpiry", row.pay_off_at_expiry);
        if (row.premium_amount)
            option.set_text("PremiumAmount", row.premium_amount->to_string());
        option.set_optional_text("PremiumCurrency", row.premium_currency);
        set_optional_date(option, "PremiumPayDate", row.premium_pay_date);
        option.set_optional_text("ExerciseFeeSettlementPeriod", row.exercise_fee_settlement_period);
        option.set_optional_text("ExerciseFeeSettlementCalendar",
                                 row.exercise_fee_settlement_calendar);
        option.set_optional_text("ExerciseFeeSettlementConvention",
                                 row.exercise_fee_settlement_convention);
        if (row.automatic_exercise)
            set_spelling_or(option, "AutomaticExercise", *row.automatic_exercise, "N", "boolean");

        if (!v.option_premiums.empty()) {
            auto block = option.child("Premiums");
            for (const auto& premium : v.option_premiums) {
                auto writer = block.append("Premium");
                set_decimal_from_double(writer, "Amount", premium.amount.to_double());
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

// ---------------------------------------------------------------------------
// Writing: the legs, their economics and the basket
// ---------------------------------------------------------------------------

std::vector<swap_leg_amount> amounts_for_leg(const credit_instrument_data& v, int leg_number) {
    std::vector<swap_leg_amount> out;
    for (const auto& row : v.leg_amounts)
        if (row.leg_number == leg_number)
            out.push_back(row);
    return out;
}

std::vector<swap_leg_rate> rates_for_leg(const credit_instrument_data& v, int leg_number) {
    std::vector<swap_leg_rate> out;
    for (const auto& row : v.leg_rates)
        if (row.leg_number == leg_number)
            out.push_back(row);
    return out;
}

/// The legs one group of a trade states, in the order the document stated them.
std::vector<const swap_leg*> legs_in_group(const credit_instrument_data& v,
                                           std::string_view group) {
    std::vector<const swap_leg*> legs;
    for (const auto& leg : v.legs)
        if (leg.leg_group && *leg.leg_group == group)
            legs.push_back(&leg);
    std::stable_sort(legs.begin(), legs.end(), [](const auto* a, const auto* b) {
        return a->identity.leg_number < b->identity.leg_number;
    });
    return legs;
}

void write_leg(record_writer& leg,
               const credit_instrument_data& v,
               const swap_leg& row,
               const std::vector<swap_leg_amount>& amounts,
               const std::vector<swap_leg_rate>& rates) {
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

    // The stored schedule is the document's own; a leg that states none gets
    // the period the header spans, which is what the old export wrote.
    auto schedule = leg.child("ScheduleData");
    if (!write_stored_schedule(schedule, v, "swap", row.identity.leg_number, "schedule"))
        write_schedule_rules(schedule,
                             v.instrument.start_date,
                             v.instrument.maturity_date,
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
        const auto spreads = std::count_if(rates.begin(), rates.end(), [](const auto& rate) {
            return rate.rate_role == "spread";
        });
        if (spreads != 0) {
            auto list = floating.child("Spreads");
            for (const auto& rate : rates)
                if (rate.rate_role == "spread")
                    list.append("Spread").set_own_text(rate.value.to_string());
        }
    }
}

/// The whole of one leg, at the element the caller names.
void append_leg_writer(record_writer& parent,
                       std::string_view name,
                       const credit_instrument_data& v,
                       const swap_leg& row) {
    auto leg = parent.append(name);
    write_leg(leg,
              v,
              row,
              amounts_for_leg(v, row.identity.leg_number),
              rates_for_leg(v, row.identity.leg_number));
}

/// The first leg of a group, at the single element the product states it in.
void write_first_leg(record_writer& parent,
                     std::string_view name,
                     const credit_instrument_data& v,
                     std::string_view group) {
    const auto legs = legs_in_group(v, group);
    if (legs.empty())
        return;
    auto leg = parent.child(name);
    write_leg(leg,
              v,
              *legs.front(),
              amounts_for_leg(v, legs.front()->identity.leg_number),
              rates_for_leg(v, legs.front()->identity.leg_number));
}

/// Every leg of one payment group, at the repeated element it sits in.
void write_payment_group(record_writer& parent,
                         std::string_view name,
                         const credit_instrument_data& v,
                         std::string_view group) {
    const auto legs = legs_in_group(v, group);
    if (legs.empty())
        return;
    auto payments = parent.child(name);
    for (const auto* row : legs)
        append_leg_writer(payments, "LegData", v, *row);
}

void write_basket(record_writer basket, const credit_instrument_data& v) {
    std::vector<const credit_basket_constituent*> ordered;
    for (const auto& row : v.constituents)
        ordered.push_back(&row);
    std::stable_sort(ordered.begin(), ordered.end(), [](const auto* a, const auto* b) {
        return a->sequence_number < b->sequence_number;
    });

    for (const auto* row : ordered) {
        auto name = basket.append("Name");
        name.set_text("IssuerId", row->issuer_id);
        if (row->qualifier)
            name.set_text("Qualifier", *row->qualifier);
        if (row->credit_curve_id)
            name.set_text("CreditCurveId", *row->credit_curve_id);
        if (row->notional)
            name.set_decimal("Notional", *row->notional);
        if (row->weight)
            set_decimal_from_double(name, "Weight", *row->weight);
        if (row->currency)
            set_spelling_or(name, "Currency", *row->currency, "USD", "currency");
    }
}

// ---------------------------------------------------------------------------
// Writing: one function per product
// ---------------------------------------------------------------------------

template <typename Fact>
auto fact_of(const credit_instrument_data& data) -> const Fact& {
    static const Fact empty{};
    const auto& rows = fact_rows<Fact>(data);
    return rows.empty() ? empty : rows.front();
}

void write_accrual(record_writer& d,
                   const std::optional<bool>& settles_accrual,
                   const std::optional<bool>& rebates_accrual,
                   const std::optional<bool>& pays_at_default_time,
                   const std::optional<std::string>& protection_payment_time) {
    d.set_optional_flag("SettlesAccrual", settles_accrual);
    d.set_optional_flag("RebatesAccrual", rebates_accrual);
    d.set_optional_flag("PaysAtDefaultTime", pays_at_default_time);
    d.set_optional_text("ProtectionPaymentTime", protection_payment_time);
}

void write_cds_data(record_writer& d,
                    const credit_default_swap_instrument& fact,
                    const credit_instrument_data& v) {
    if (fact.credit_curve_id)
        d.set_text("CreditCurveId", *fact.credit_curve_id);
    d.set_optional_text("IssuerId", fact.issuer_id);
    d.set_optional_text("ReferenceObligation", fact.reference_obligation);
    write_accrual(d,
                  fact.settles_accrual,
                  fact.rebates_accrual,
                  fact.pays_at_default_time,
                  fact.protection_payment_time);
    set_optional_date(d, "ProtectionStart", fact.protection_start_date);
    set_optional_date(d, "UpfrontDate", fact.upfront_date);
    d.set_optional_decimal("UpfrontFee", fact.upfront_fee);
    set_optional_decimal_from_double(d, "FixedRecoveryRate", fact.recovery_rate);
    set_optional_date(d, "TradeDate", fact.trade_date);
    set_optional_days(d, "CashSettlementDays", fact.cash_settlement_days);
    write_first_leg(d, "LegData", v, "premium");
}

void write_index_swap_data(record_writer& d,
                           const index_credit_default_swap_instrument& fact,
                           const credit_instrument_data& v) {
    d.set_text("CreditCurveId", fact.credit_curve_id);
    d.set_optional_text("IssuerId", fact.issuer_id);
    write_accrual(d,
                  fact.settles_accrual,
                  fact.rebates_accrual,
                  fact.pays_at_default_time,
                  fact.protection_payment_time);
    set_optional_date(d, "ProtectionStart", fact.protection_start_date);
    set_optional_date(d, "UpfrontDate", fact.upfront_date);
    d.set_optional_decimal("UpfrontFee", fact.upfront_fee);
    set_optional_date(d, "TradeDate", fact.trade_date);
    set_optional_days(d, "CashSettlementDays", fact.cash_settlement_days);
    write_first_leg(d, "LegData", v, "premium");
    if (!v.constituents.empty())
        write_basket(d.child("BasketData"), v);
}

void write_cds(record_writer& trade, const credit_instrument_data& v) {
    auto data = trade.child("CreditDefaultSwapData");
    write_cds_data(data, fact_of<credit_default_swap_instrument>(v), v);
}

void write_index_cds(record_writer& trade, const credit_instrument_data& v) {
    auto data = trade.child("IndexCreditDefaultSwapData");
    write_index_swap_data(data, fact_of<index_credit_default_swap_instrument>(v), v);
}

void write_index_cds_option(record_writer& trade, const credit_instrument_data& v) {
    const auto& fact = fact_of<index_credit_default_swap_option_instrument>(v);
    auto data = trade.child("IndexCreditDefaultSwapOptionData");
    data.set_optional_flag("KnockOut", fact.knock_out);
    data.set_optional_text("IndexTerm", fact.index_term);
    if (fact.strike_type)
        data.set_spelling(
            "StrikeType", *fact.strike_type == "Spread" ? "Spread" : "Price", "strike type");
    set_optional_date(data, "TradeDate", fact.trade_date);
    set_optional_date(data, "FrontEndProtectionStartDate", fact.front_end_protection_start_date);

    // The importer writes the wrapped swap's row beside the option's, so a
    // missing row means a partial write. The old export left the curve empty
    // rather than dropping the required element.
    auto swap_data = data.child("IndexCreditDefaultSwapData");
    const auto& wrapped = v.wrapped_index_swap;
    write_index_swap_data(
        swap_data, wrapped ? *wrapped : index_credit_default_swap_instrument{}, v);

    // An index option states one strike with no arm, so the row it reads is the
    // one whose role says so rather than whichever row came first.
    const auto strike = std::find_if(v.strikes.begin(), v.strikes.end(), [](const auto& row) {
        return row.strike_role == "single";
    });
    if (strike != v.strikes.end() && strike->bare_value)
        set_decimal_from_double(data, "Strike", strike->bare_value->to_double());

    write_option_block(data.child("OptionData"), v);
}

void write_credit_linked_swap(record_writer& trade, const credit_instrument_data& v) {
    const auto& fact = fact_of<credit_linked_swap_instrument>(v);
    auto data = trade.child("CreditLinkedSwapData");
    data.set_text("CreditCurveId", fact.credit_curve_id);
    data.set_optional_flag("SettlesAccrual", fact.settles_accrual);
    set_optional_decimal_from_double(data, "FixedRecoveryRate", fact.recovery_rate);
    data.set_optional_text("DefaultPaymentTime", fact.default_payment_time);
    write_payment_group(data, "IndependentPayments", v, "independent");
    write_payment_group(data, "ContingentPayments", v, "contingent");
    write_payment_group(data, "DefaultPayments", v, "default");
    write_payment_group(data, "RecoveryPayments", v, "recovery");
}

void write_synthetic_cdo(record_writer& trade, const credit_instrument_data& v) {
    const auto& fact = fact_of<synthetic_cdo_instrument>(v);
    auto data = trade.child("CdoData");
    data.set_text("Qualifier", fact.qualifier);
    if (fact.protection_start_date.ok())
        data.set_text("ProtectionStart", to_ore_date(fact.protection_start_date));
    set_optional_date(data, "UpfrontDate", fact.upfront_date);
    if (fact.upfront_fee)
        data.set_text("UpfrontFee", fact.upfront_fee->to_string());
    write_accrual(data,
                  fact.settles_accrual,
                  fact.rebates_accrual,
                  fact.pays_at_default_time,
                  fact.protection_payment_time);
    set_decimal_from_double(data, "AttachmentPoint", fact.attachment_point);
    set_decimal_from_double(data, "DetachmentPoint", fact.detachment_point);
    set_optional_decimal_from_double(data, "FixedRecoveryRate", fact.recovery_rate);
    write_first_leg(data, "LegData", v, "premium");
    if (!v.constituents.empty())
        write_basket(data.child("BasketData"), v);
}

void write_rpa(record_writer& trade, const credit_instrument_data& v) {
    const auto& fact = fact_of<risk_participation_agreement_instrument>(v);
    auto data = trade.child("RiskParticipationAgreementData");
    set_decimal_from_double(data, "ParticipationRate", fact.participation_rate);
    data.set_text("CreditCurveId", fact.credit_curve_id);
    if (fact.issuer_id)
        data.set_text("IssuerId", *fact.issuer_id);
    if (fact.settles_accrual)
        data.set_text("SettlesAccrual", flag_text(*fact.settles_accrual));
    if (v.instrument.start_date)
        data.set_text("ProtectionStart", to_ore_date(*v.instrument.start_date));
    if (v.instrument.maturity_date)
        data.set_text("ProtectionEnd", to_ore_date(*v.instrument.maturity_date));
    set_optional_decimal_from_double(data, "FixedRecoveryRate", fact.recovery_rate);

    auto protection_fee = data.child("ProtectionFee");
    for (const auto* row : legs_in_group(v, "protection_fee"))
        append_leg_writer(protection_fee, "LegData", v, *row);

    auto underlying = data.child("Underlying");
    for (const auto* row : legs_in_group(v, "underlying"))
        append_leg_writer(underlying, "LegData", v, *row);
    if (fact.naked_option)
        underlying.set_text("NakedOption", flag_text(*fact.naked_option));
    if (!v.options.empty())
        write_option_block(underlying.child("OptionData"), v);
}

}

auto read_credit_instrument(xml::record_reader trade, std::string_view trade_type)
    -> std::optional<trading::domain::trade_instrument> {
    if (!is_credit_trade_type(trade_type))
        return std::nullopt;

    if (trade_type == "CreditDefaultSwap")
        return trading::domain::trade_instrument{read_cds(trade)};
    if (trade_type == "IndexCreditDefaultSwap")
        return trading::domain::trade_instrument{read_index_cds(trade)};
    if (trade_type == "IndexCreditDefaultSwapOption")
        return trading::domain::trade_instrument{read_index_cds_option(trade)};
    if (trade_type == "CreditLinkedSwap")
        return trading::domain::trade_instrument{read_credit_linked_swap(trade)};
    if (trade_type == "SyntheticCDO")
        return trading::domain::trade_instrument{read_synthetic_cdo(trade)};
    return trading::domain::trade_instrument{read_rpa(trade)};
}

auto write_credit_instrument(xml::record_writer& trade,
                             std::string_view trade_type,
                             const trading::domain::trade_instrument& instrument,
                             const portfolio_reference_curves&) -> bool {
    const auto* v = std::get_if<credit_instrument_data>(&instrument);
    if (v == nullptr || !is_credit_trade_type(trade_type))
        return false;

    if (trade_type == "CreditDefaultSwap")
        write_cds(trade, *v);
    else if (trade_type == "IndexCreditDefaultSwap")
        write_index_cds(trade, *v);
    else if (trade_type == "IndexCreditDefaultSwapOption")
        write_index_cds_option(trade, *v);
    else if (trade_type == "CreditLinkedSwap")
        write_credit_linked_swap(trade, *v);
    else if (trade_type == "SyntheticCDO")
        write_synthetic_cdo(trade, *v);
    else
        write_rpa(trade, *v);
    return true;
}

}
