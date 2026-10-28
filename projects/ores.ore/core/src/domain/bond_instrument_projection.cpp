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
#include "ores.ore.core/domain/portfolio_instrument_projection.hpp"
#include "ores.ore.core/xml/binding.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <algorithm>
#include <charconv>
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

using trading::domain::ascot;
using trading::domain::bond_amortization_data;
using trading::domain::bond_fixed_leg_data;
using trading::domain::bond_float_data;
using trading::domain::bond_floating_leg_data;
using trading::domain::bond_formula_based_leg_data;
using trading::domain::bond_forward_premium;
using trading::domain::bond_forward_settlement;
using trading::domain::bond_future;
using trading::domain::bond_instrument_data;
using trading::domain::bond_issue;
using trading::domain::bond_issue_call_date;
using trading::domain::bond_issue_conversion_target;
using trading::domain::bond_leg_data;
using trading::domain::bond_leg_rate_data;
using trading::domain::bond_option;
using trading::domain::bond_option_data;
using trading::domain::bond_option_exercise;
using trading::domain::bond_option_exercise_fee;
using trading::domain::bond_option_payment_data;
using trading::domain::bond_option_payment_rules;
using trading::domain::bond_option_premium;
using trading::domain::bond_option_settlement;
using trading::domain::bond_repo;
using trading::domain::bond_schedule_data;
using trading::domain::bond_schedule_dates;
using trading::domain::bond_schedule_rules;
using trading::domain::bond_settlement_data;
using trading::domain::bond_strike_data;
using trading::domain::bond_stub_interpolation;
using trading::domain::bond_trs;

using xml::record_reader;
using xml::record_writer;

// ---------------------------------------------------------------------------
// The vocabulary both directions share
// ---------------------------------------------------------------------------

/// The actor and reason every imported row states.
constexpr std::string_view actor = "ores";
constexpr std::string_view change_reason = "system.external_data_import";
constexpr std::string_view change_commentary = "Imported from ORE XML";

/// The trade types the family states, which are the ones the old dispatcher took.
constexpr bool is_bond_trade_type(std::string_view type) {
    return type == "Bond" || type == "ForwardBond" || type == "CallableBond" ||
           type == "ConvertibleBond" || type == "BondOption" || type == "BondTRS" ||
           type == "BondRepo" || type == "BondFuture" || type == "Ascot";
}

template <typename Row>
void stamp_row(Row& row) {
    row.modified_by = std::string(actor);
    row.performed_by = std::string(actor);
    row.change_reason_code = std::string(change_reason);
    row.change_commentary = std::string(change_commentary);
}

auto make_base(std::string_view trade_type_code) -> bond_instrument_data {
    bond_instrument_data r;
    r.instrument.identity.trade_type_code = std::string(trade_type_code);
    r.instrument.audit.modified_by = std::string(actor);
    r.instrument.audit.performed_by = std::string(actor);
    r.instrument.audit.change_reason_code = std::string(change_reason);
    r.instrument.audit.change_commentary = std::string(change_commentary);
    stamp_row(r.issue);
    return r;
}

/// Mints the issue row a mapped trade states, which no lookup stands behind.
void mint_issue(bond_instrument_data& result) {
    const auto issue_id = boost::uuids::random_generator()();
    result.instrument.issue_id = issue_id;
    result.issue.issue_id = issue_id;
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

auto bond_instrument_projection_text_of(const record_reader& r,
                                        std::string_view name) -> std::string {
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

auto bond_instrument_projection_decimal_of(const record_reader& r,
                                           std::string_view name) -> decimal {
    return decimal_from_double(decimal_as_double(r, name).value_or(0.0));
}

/// A number the document spelled as text, such as a bond's notional.
auto decimal_from_text(const std::string& text) -> std::optional<decimal> {
    if (text.empty())
        return std::nullopt;
    if (auto parsed = decimal::from_string(text))
        return std::move(*parsed);
    return std::nullopt;
}

// The schema spells the settlement day count as text while the column holds an
// integer. A value the parser cannot read leaves the column at its default.
auto count_of(const std::string& text, int fallback) -> int {
    try {
        return std::stoi(text);
    } catch (const std::exception&) {
        return fallback;
    }
}

// The schema states a strike as text and a contract notional as text, and the
// schema requires a decimal spelling there, so the shortest text that reads
// back as the same number is what goes out.
auto format_number(double value) -> std::string {
    char buffer[32];
    const auto written = std::to_chars(buffer, buffer + sizeof(buffer), value);
    if (written.ec != std::errc{})
        return {};
    return std::string(buffer, written.ptr);
}

// ---------------------------------------------------------------------------
// Reading: the schedules
// ---------------------------------------------------------------------------

auto read_number(const record_reader& r) -> bond_float_data {
    bond_float_data row;
    row.value = decimal_text(r);
    if (r.has("startDate"))
        row.start_date = bond_instrument_projection_text_of(r, "startDate");
    return row;
}

auto read_schedule(const record_reader& sd) -> bond_schedule_data {
    bond_schedule_data result;
    for (const auto& r : sd.children("Rules")) {
        bond_schedule_rules row;
        row.start_date = bond_instrument_projection_text_of(r, "StartDate");
        if (r.has("EndDate"))
            row.end_date = bond_instrument_projection_text_of(r, "EndDate");
        if (r.has("AdjustEndDateToPreviousMonthEnd"))
            row.adjust_end_date_to_previous_month_end =
                spelling_of(r, "AdjustEndDateToPreviousMonthEnd");
        row.tenor = bond_instrument_projection_text_of(r, "Tenor");
        if (r.has("Calendar"))
            row.calendar = bond_instrument_projection_text_of(r, "Calendar");
        row.convention = spelling_of(r, "Convention");
        if (r.has("TermConvention"))
            row.term_convention = spelling_of(r, "TermConvention");
        if (r.has("Rule"))
            row.rule = spelling_of(r, "Rule");
        if (r.has("EndOfMonth"))
            row.end_of_month = spelling_of(r, "EndOfMonth");
        if (r.has("EndOfMonthConvention"))
            row.end_of_month_convention = spelling_of(r, "EndOfMonthConvention");
        if (r.has("FirstDate"))
            row.first_date = bond_instrument_projection_text_of(r, "FirstDate");
        if (r.has("LastDate"))
            row.last_date = bond_instrument_projection_text_of(r, "LastDate");
        if (r.has("RemoveFirstDate"))
            row.remove_first_date =
                is_flag_on(bond_instrument_projection_text_of(r, "RemoveFirstDate"));
        if (r.has("RemoveLastDate"))
            row.remove_last_date =
                is_flag_on(bond_instrument_projection_text_of(r, "RemoveLastDate"));
        result.rules.push_back(std::move(row));
    }
    for (const auto& block : sd.children("Dates")) {
        bond_schedule_dates row;
        if (block.has("Calendar"))
            row.calendar = bond_instrument_projection_text_of(block, "Calendar");
        if (block.has("Convention"))
            row.convention = spelling_of(block, "Convention");
        if (block.has("Tenor"))
            row.tenor = bond_instrument_projection_text_of(block, "Tenor");
        if (block.has("EndOfMonth"))
            row.end_of_month = spelling_of(block, "EndOfMonth");
        if (block.has("IncludeDuplicateDates"))
            row.include_duplicate_dates = spelling_of(block, "IncludeDuplicateDates");
        if (const auto dates = block.child("Dates"))
            row.dates = dates->values("Date");
        result.dates.push_back(std::move(row));
    }
    return result;
}

auto read_amortization(const record_reader& a) -> bond_amortization_data {
    bond_amortization_data row;
    row.type = spelling_of(a, "Type");
    if (a.has("Value"))
        row.value = bond_instrument_projection_decimal_of(a, "Value");
    if (a.has("StartDate"))
        row.start_date = bond_instrument_projection_text_of(a, "StartDate");
    if (a.has("EndDate"))
        row.end_date = bond_instrument_projection_text_of(a, "EndDate");
    if (a.has("Frequency"))
        row.frequency = bond_instrument_projection_text_of(a, "Frequency");
    if (a.has("Underflow"))
        row.underflow = is_flag_on(bond_instrument_projection_text_of(a, "Underflow"));
    return row;
}

auto read_stub(const record_reader& s) -> bond_stub_interpolation {
    bond_stub_interpolation row;
    row.short_index = bond_instrument_projection_text_of(s, "ShortIndex");
    row.long_index = bond_instrument_projection_text_of(s, "LongIndex");
    if (s.has("RoundingType"))
        row.rounding_type = spelling_of(s, "RoundingType");
    if (s.has("RoundingPrecision"))
        row.rounding_precision = s.integer("RoundingPrecision");
    return row;
}

auto read_settlement(const record_reader& s) -> bond_settlement_data {
    bond_settlement_data row;
    row.fx_index = bond_instrument_projection_text_of(s, "FXIndex");
    if (s.has("FixingDate"))
        row.fixing_date = bond_instrument_projection_text_of(s, "FixingDate");
    return row;
}

auto read_floating_leg(const record_reader& f) -> bond_floating_leg_data {
    bond_floating_leg_data row;
    row.index = bond_instrument_projection_text_of(f, "Index");
    if (f.has("IsInArrears"))
        row.is_in_arrears = is_flag_on(bond_instrument_projection_text_of(f, "IsInArrears"));
    if (f.has("LastRecentPeriod"))
        row.last_recent_period = bond_instrument_projection_text_of(f, "LastRecentPeriod");
    if (f.has("LastRecentPeriodCalendar"))
        row.last_recent_period_calendar =
            bond_instrument_projection_text_of(f, "LastRecentPeriodCalendar");
    if (f.has("FixingDays")) {
        const auto value = f.integer("FixingDays").value_or(0);
        if (value >= 0)
            row.fixing_days = static_cast<std::uint64_t>(value);
    }
    if (f.has("Lookback"))
        row.lookback = bond_instrument_projection_text_of(f, "Lookback");
    if (f.has("RateCutoff"))
        row.rate_cutoff = f.integer("RateCutoff");
    if (f.has("IsAveraged"))
        row.is_averaged = is_flag_on(bond_instrument_projection_text_of(f, "IsAveraged"));
    if (f.has("HasSubPeriods"))
        row.has_sub_periods = is_flag_on(bond_instrument_projection_text_of(f, "HasSubPeriods"));
    if (f.has("IncludeSpread"))
        row.include_spread = is_flag_on(bond_instrument_projection_text_of(f, "IncludeSpread"));
    if (f.has("IsNotResettingXCCY"))
        row.is_not_resetting_xccy =
            is_flag_on(bond_instrument_projection_text_of(f, "IsNotResettingXCCY"));
    if (const auto spreads = f.child("Spreads"))
        for (const auto& s : spreads->children("Spread"))
            row.spreads.push_back(read_number(s));
    if (const auto caps = f.child("Caps"))
        for (const auto& c : caps->children("Cap"))
            row.caps.push_back(read_number(c));
    if (const auto floors = f.child("Floors"))
        for (const auto& c : floors->children("Floor"))
            row.floors.push_back(read_number(c));
    if (const auto gearings = f.child("Gearings"))
        for (const auto& g : gearings->children("Gearing"))
            row.gearings.push_back(read_number(g));
    if (f.has("NakedOption"))
        row.naked_option = is_flag_on(bond_instrument_projection_text_of(f, "NakedOption"));
    if (f.has("LocalCapFloor"))
        row.local_cap_floor = is_flag_on(bond_instrument_projection_text_of(f, "LocalCapFloor"));
    if (const auto schedule = f.child("FixingSchedule"))
        row.fixing_schedule = read_schedule(*schedule);
    if (const auto schedule = f.child("ResetSchedule"))
        row.reset_schedule = read_schedule(*schedule);
    if (const auto stub = f.child("FrontStubInterpolation"))
        row.front_stub_interpolation = read_stub(*stub);
    if (const auto stub = f.child("BackStubInterpolation"))
        row.back_stub_interpolation = read_stub(*stub);
    if (f.has("StubUseOriginalCurve"))
        row.stub_use_original_curve =
            is_flag_on(bond_instrument_projection_text_of(f, "StubUseOriginalCurve"));
    if (f.has("ObservationShift"))
        row.observation_shift =
            is_flag_on(bond_instrument_projection_text_of(f, "ObservationShift"));
    return row;
}

auto read_formula_leg(const record_reader& f) -> bond_formula_based_leg_data {
    bond_formula_based_leg_data row;
    row.index = bond_instrument_projection_text_of(f, "Index");
    if (f.has("IsInArrears"))
        row.is_in_arrears = is_flag_on(bond_instrument_projection_text_of(f, "IsInArrears"));
    row.fixing_days = f.integer("FixingDays").value_or(0);
    if (f.has("FixingCalendar"))
        row.fixing_calendar = bond_instrument_projection_text_of(f, "FixingCalendar");
    return row;
}

auto read_rate_group(const record_reader& leg) -> std::optional<bond_leg_rate_data> {
    bond_leg_rate_data row;
    bool stated = false;
    if (const auto fixed = leg.child("FixedLegData")) {
        bond_fixed_leg_data rates;
        if (const auto list = fixed->child("Rates"))
            for (const auto& r : list->children("Rate"))
                rates.rates.push_back(read_number(r));
        row.fixed = std::move(rates);
        stated = true;
    }
    if (const auto floating = leg.child("FloatingLegData")) {
        row.floating = read_floating_leg(*floating);
        stated = true;
    }
    if (const auto formula = leg.child("FormulaBasedLegData")) {
        row.formula_based = read_formula_leg(*formula);
        stated = true;
    }
    if (!stated)
        return std::nullopt;
    return row;
}

auto read_leg(const record_reader& ld) -> bond_leg_data {
    bond_leg_data row;
    row.payer = is_flag_on(bond_instrument_projection_text_of(ld, "Payer"));
    row.leg_type = spelling_of(ld, "LegType");
    if (ld.has("Currency"))
        row.currency = bond_instrument_projection_text_of(ld, "Currency");
    if (ld.has("PaymentConvention"))
        row.payment_convention = spelling_of(ld, "PaymentConvention");
    if (ld.has("PaymentLag"))
        row.payment_lag = bond_instrument_projection_text_of(ld, "PaymentLag");
    if (ld.has("PaymentCalendar"))
        row.payment_calendar = bond_instrument_projection_text_of(ld, "PaymentCalendar");
    if (ld.has("DayCounter"))
        row.day_counter = spelling_of(ld, "DayCounter");
    if (ld.has("LastPeriodDayCounter"))
        row.last_period_day_counter = spelling_of(ld, "LastPeriodDayCounter");
    if (ld.has("NotionalPaymentLag"))
        row.notional_payment_lag = ld.integer("NotionalPaymentLag");
    if (ld.has("StrictNotionalDates"))
        row.strict_notional_dates =
            is_flag_on(bond_instrument_projection_text_of(ld, "StrictNotionalDates"));
    if (const auto schedule = ld.child("ScheduleData"))
        row.schedule = read_schedule(*schedule);
    if (const auto amortizations = ld.child("Amortizations"))
        for (const auto& a : amortizations->children("AmortizationData"))
            row.amortizations.push_back(read_amortization(a));
    if (const auto notionals = ld.child("Notionals"))
        for (const auto& n : notionals->children("Notional"))
            row.notionals.push_back(read_number(n));
    if (const auto dates = ld.child("PaymentDates"))
        row.payment_dates = dates->values("PaymentDate");
    if (const auto indexings = ld.child("Indexings")) {
        if (indexings->has("FromAssetLeg"))
            row.indexings_from_asset_leg =
                is_flag_on(bond_instrument_projection_text_of(*indexings, "FromAssetLeg"));
    }
    row.rate = read_rate_group(ld);
    if (const auto schedule = ld.child("PaymentSchedule"))
        row.payment_schedule = read_schedule(*schedule);
    if (const auto settlement = ld.child("SettlementData"))
        row.settlement = read_settlement(*settlement);
    return row;
}

// ---------------------------------------------------------------------------
// Reading: the option block and the strike
// ---------------------------------------------------------------------------

auto read_option_settlement(const record_reader& s) -> bond_option_settlement {
    bond_option_settlement row;
    row.pay_currency = spelling_of(s, "PayCurrency");
    row.fx_index = bond_instrument_projection_text_of(s, "FXIndex");
    if (s.has("FixingDate"))
        row.fixing_date = bond_instrument_projection_text_of(s, "FixingDate");
    return row;
}

auto read_option_premium(const record_reader& p) -> bond_option_premium {
    bond_option_premium row;
    row.amount = bond_instrument_projection_decimal_of(p, "Amount");
    row.currency = bond_instrument_projection_text_of(p, "Currency");
    row.pay_date = bond_instrument_projection_text_of(p, "PayDate");
    if (const auto settlement = p.child("SettlementData"))
        row.settlement = read_option_settlement(*settlement);
    return row;
}

auto read_option_exercise_fee(const record_reader& f) -> bond_option_exercise_fee {
    bond_option_exercise_fee row;
    row.amount = decimal_text(f);
    if (f.has("type"))
        row.type = bond_instrument_projection_text_of(f, "type");
    if (f.has("startDate"))
        row.start_date = bond_instrument_projection_text_of(f, "startDate");
    if (f.has("currency"))
        row.currency = bond_instrument_projection_text_of(f, "currency");
    return row;
}

auto read_option_exercise(const record_reader& e) -> bond_option_exercise {
    bond_option_exercise row;
    row.date = bond_instrument_projection_text_of(e, "Date");
    if (e.has("Price"))
        row.price = bond_instrument_projection_decimal_of(e, "Price");
    return row;
}

auto read_option_payment_data(const record_reader& p) -> bond_option_payment_data {
    bond_option_payment_data row;
    if (const auto dates = p.child("Dates"))
        row.dates = dates->values("Date");
    if (const auto rules = p.child("Rules")) {
        bond_option_payment_rules r;
        r.lag = static_cast<std::uint64_t>(
            std::max<std::int64_t>(rules->integer("Lag").value_or(0), 0));
        r.calendar = bond_instrument_projection_text_of(*rules, "Calendar");
        r.convention = spelling_of(*rules, "Convention");
        if (rules->has("RelativeTo"))
            r.relative_to = spelling_of(*rules, "RelativeTo");
        row.rules = std::move(r);
    }
    return row;
}

auto read_option_data(const record_reader& od) -> bond_option_data {
    bond_option_data row;
    row.long_short = bond_instrument_projection_text_of(od, "LongShort");
    if (od.has("OptionType"))
        row.option_type = bond_instrument_projection_text_of(od, "OptionType");
    if (od.has("PayoffType"))
        row.payoff_type = bond_instrument_projection_text_of(od, "PayoffType");
    if (od.has("PayoffType2"))
        row.payoff_type_2 = bond_instrument_projection_text_of(od, "PayoffType2");
    if (od.has("Style"))
        row.style = bond_instrument_projection_text_of(od, "Style");
    if (od.has("NoticePeriod"))
        row.notice_period = bond_instrument_projection_text_of(od, "NoticePeriod");
    if (od.has("NoticeCalendar"))
        row.notice_calendar = bond_instrument_projection_text_of(od, "NoticeCalendar");
    if (od.has("NoticeConvention"))
        row.notice_convention = bond_instrument_projection_text_of(od, "NoticeConvention");
    if (od.has("MidCouponExercise"))
        row.mid_coupon_exercise = bond_instrument_projection_text_of(od, "MidCouponExercise");
    if (od.has("Settlement"))
        row.settlement = spelling_of(od, "Settlement");
    if (od.has("SettlementMethod"))
        row.settlement_method = spelling_of(od, "SettlementMethod");
    if (od.has("PayOffAtExpiry"))
        row.pay_off_at_expiry = bond_instrument_projection_text_of(od, "PayOffAtExpiry");
    if (od.has("PremiumAmount"))
        row.premium_amount =
            decimal_from_text(bond_instrument_projection_text_of(od, "PremiumAmount"));
    if (od.has("PremiumCurrency"))
        row.premium_currency = bond_instrument_projection_text_of(od, "PremiumCurrency");
    if (od.has("PremiumPayDate"))
        row.premium_pay_date = bond_instrument_projection_text_of(od, "PremiumPayDate");
    if (const auto premiums = od.child("Premiums"))
        for (const auto& p : premiums->children("Premium"))
            row.premiums.push_back(read_option_premium(p));
    if (od.has("ExercisePrices"))
        row.exercise_prices = bond_instrument_projection_text_of(od, "ExercisePrices");
    if (const auto fees = od.child("ExerciseFees"))
        for (const auto& f : fees->children("ExerciseFee"))
            row.exercise_fees.push_back(read_option_exercise_fee(f));
    if (od.has("ExerciseFeeSettlementPeriod"))
        row.exercise_fee_settlement_period =
            bond_instrument_projection_text_of(od, "ExerciseFeeSettlementPeriod");
    if (od.has("ExerciseFeeSettlementCalendar"))
        row.exercise_fee_settlement_calendar =
            bond_instrument_projection_text_of(od, "ExerciseFeeSettlementCalendar");
    if (od.has("ExerciseFeeSettlementConvention"))
        row.exercise_fee_settlement_convention =
            bond_instrument_projection_text_of(od, "ExerciseFeeSettlementConvention");
    if (od.has("AutomaticExercise"))
        row.automatic_exercise = spelling_of(od, "AutomaticExercise");
    if (const auto exercise = od.child("ExerciseData"))
        row.exercise_data = read_option_exercise(*exercise);
    if (const auto payment = od.child("PaymentData"))
        row.payment_data = read_option_payment_data(*payment);
    if (const auto settlement = od.child("SettlementData"))
        row.settlement_data = read_option_settlement(*settlement);
    return row;
}

auto read_strike_data(const record_reader& sd) -> bond_strike_data {
    bond_strike_data row;
    if (const auto price = sd.child("StrikePrice")) {
        row.price_value = bond_instrument_projection_decimal_of(*price, "Value");
        if (price->has("Currency"))
            row.price_currency = bond_instrument_projection_text_of(*price, "Currency");
    }
    if (const auto yield = sd.child("StrikeYield")) {
        row.yield_value = bond_instrument_projection_decimal_of(*yield, "Yield");
        if (yield->has("Compounding"))
            row.yield_compounding = spelling_of(*yield, "Compounding");
    }
    if (sd.has("Value"))
        row.bare_value = bond_instrument_projection_decimal_of(sd, "Value");
    if (sd.has("Currency"))
        row.bare_currency = bond_instrument_projection_text_of(sd, "Currency");
    return row;
}

void read_exercise_dates(const record_reader& od, bond_instrument_data& result) {
    if (const auto dates = od.child("ExerciseDates"))
        result.option_exercise_dates = dates->values("ExerciseDate");
    if (const auto schedule = od.child("ExerciseSchedule"))
        result.option_exercise_schedule = read_schedule(*schedule);
}

// ---------------------------------------------------------------------------
// Reading: the bond terms, its issue children and its legs
// ---------------------------------------------------------------------------

void read_conversion_targets(const record_reader& data,
                             boost::uuids::uuid issue_id,
                             std::vector<bond_issue_conversion_target>& targets) {
    const auto conversion = data.child("ConversionData");
    if (!conversion)
        return;
    const auto ratios = conversion->child("ConversionRatios");
    if (!ratios)
        return;

    std::string underlying_id;
    if (const auto underlying = conversion->child("Underlying"))
        underlying_id = bond_instrument_projection_text_of(*underlying, "Name");

    int sequence = 0;
    for (const auto& ratio : ratios->children("ConversionRatio")) {
        const auto value = decimal_text(ratio).to_double();
        if (value <= 0.0)
            continue;
        bond_issue_conversion_target row;
        row.issue_id = issue_id;
        row.sequence_number = ++sequence;
        row.underlying_id = underlying_id;
        row.conversion_ratio = value;
        stamp_row(row);
        targets.push_back(std::move(row));
    }
}

void read_call_dates(const record_reader& data,
                     boost::uuids::uuid issue_id,
                     std::vector<bond_issue_call_date>& dates) {
    const auto call = data.child("CallData");
    if (!call)
        return;
    const auto schedule = call->child("ScheduleData");
    if (!schedule)
        return;
    int sequence = 0;
    for (const auto& block : schedule->children("Dates")) {
        const auto list = block.child("Dates");
        if (!list)
            continue;
        for (const auto& date : list->values("Date")) {
            bond_issue_call_date row;
            row.issue_id = issue_id;
            row.sequence_number = ++sequence;
            row.call_date = to_domain_date(date);
            stamp_row(row);
            dates.push_back(std::move(row));
        }
    }
}

void read_bond_terms(const record_reader& bd, bond_instrument_data& result) {
    auto& issue = result.issue;
    issue.security_id = bond_instrument_projection_text_of(bd, "SecurityId");
    if (bd.has("IssuerId"))
        issue.issuer = bond_instrument_projection_text_of(bd, "IssuerId");
    if (bd.has("IssueDate"))
        issue.issue_date =
            to_optional_domain_date(bond_instrument_projection_text_of(bd, "IssueDate"));
    if (bd.has("SettlementDays"))
        issue.settlement_days =
            count_of(bond_instrument_projection_text_of(bd, "SettlementDays"), 0);
    if (bd.has("Calendar"))
        issue.calendar = bond_instrument_projection_text_of(bd, "Calendar");
    if (bd.has("CreditCurveId"))
        issue.credit_curve_id = bond_instrument_projection_text_of(bd, "CreditCurveId");
    if (bd.has("ReferenceCurveId"))
        issue.reference_curve_id = bond_instrument_projection_text_of(bd, "ReferenceCurveId");
    if (bd.has("IncomeCurveId"))
        issue.income_curve_id = bond_instrument_projection_text_of(bd, "IncomeCurveId");
    if (bd.has("CreditGroup"))
        issue.credit_group = bond_instrument_projection_text_of(bd, "CreditGroup");
    if (bd.has("VolatilityCurveId"))
        issue.volatility_curve_id = bond_instrument_projection_text_of(bd, "VolatilityCurveId");
    if (bd.has("PriceQuoteMethod"))
        issue.price_quote_method = bond_instrument_projection_text_of(bd, "PriceQuoteMethod");
    if (bd.has("PriceQuoteBaseValue"))
        issue.price_quote_base_value =
            decimal_from_text(bond_instrument_projection_text_of(bd, "PriceQuoteBaseValue"));
    if (bd.has("SubType"))
        issue.sub_type = bond_instrument_projection_text_of(bd, "SubType");
    if (bd.has("PriceType"))
        issue.price_type = spelling_of(bd, "PriceType");
    if (bd.has("Payer"))
        issue.payer = bond_instrument_projection_text_of(bd, "Payer");
    if (bd.has("CreditRisk"))
        issue.credit_risk = spelling_of(bd, "CreditRisk");
    if (bd.has("BondNotional"))
        result.instrument.notional =
            decimal_from_text(bond_instrument_projection_text_of(bd, "BondNotional"));

    for (const auto& ld : bd.children("LegData"))
        result.bond_legs.push_back(read_leg(ld));

    // The face value is the first leg's first notional, which is the only
    // place ORE states it: the issue carries no coupon terms.
    if (!result.bond_legs.empty() && !result.bond_legs.front().notionals.empty())
        issue.face_value = result.bond_legs.front().notionals.front().value;
}

// ---------------------------------------------------------------------------
// Reading: one function per product
// ---------------------------------------------------------------------------

auto read_bond(const record_reader& trade) -> bond_instrument_data {
    auto result = make_base("Bond");
    if (const auto data = trade.child("BondData"))
        read_bond_terms(*data, result);
    mint_issue(result);
    return result;
}

auto read_forward_bond(const record_reader& trade) -> bond_instrument_data {
    auto result = make_base("ForwardBond");
    const auto data = trade.child("ForwardBondData");
    if (data) {
        if (const auto bd = data->child("BondData"))
            read_bond_terms(*bd, result);
        result.forward_long_in_forward = bond_instrument_projection_text_of(*data, "LongInForward");

        bond_forward_settlement settlement;
        if (const auto s = data->child("SettlementData")) {
            settlement.forward_maturity_date =
                bond_instrument_projection_text_of(*s, "ForwardMaturityDate");
            if (s->has("ForwardSettlementDate"))
                settlement.forward_settlement_date =
                    bond_instrument_projection_text_of(*s, "ForwardSettlementDate");
            if (s->has("Settlement"))
                settlement.settlement = bond_instrument_projection_text_of(*s, "Settlement");
            if (s->has("Amount"))
                settlement.amount = s->decimal("Amount");
            if (s->has("LockRate"))
                settlement.lock_rate = s->decimal("LockRate");
            if (s->has("dv01"))
                settlement.dv01 = s->decimal("dv01");
            if (s->has("LockRateDayCounter"))
                settlement.lock_rate_day_counter =
                    bond_instrument_projection_text_of(*s, "LockRateDayCounter");
            if (s->has("SettlementDirty"))
                settlement.settlement_dirty =
                    bond_instrument_projection_text_of(*s, "SettlementDirty");
        }
        result.forward_settlement = std::move(settlement);

        if (const auto p = data->child("PremiumData")) {
            bond_forward_premium premium;
            premium.amount = decimal_from_text(bond_instrument_projection_text_of(*p, "Amount"))
                                 .value_or(decimal{});
            premium.date = bond_instrument_projection_text_of(*p, "Date");
            result.forward_premium = std::move(premium);
        }
    }
    mint_issue(result);
    return result;
}

auto read_callable_bond(const record_reader& trade) -> bond_instrument_data {
    auto result = make_base("CallableBond");
    if (const auto data = trade.child("CallableBondData")) {
        if (const auto bd = data->child("BondData"))
            read_bond_terms(*bd, result);
        mint_issue(result);
        read_call_dates(*data, result.issue.issue_id, result.call_dates);
        return result;
    }
    mint_issue(result);
    return result;
}

auto read_convertible_bond(const record_reader& trade) -> bond_instrument_data {
    auto result = make_base("ConvertibleBond");
    if (const auto data = trade.child("ConvertibleBondData")) {
        if (const auto bd = data->child("BondData"))
            read_bond_terms(*bd, result);
        mint_issue(result);
        read_conversion_targets(*data, result.issue.issue_id, result.conversion_targets);
        return result;
    }
    mint_issue(result);
    return result;
}

auto read_bond_option(const record_reader& trade) -> bond_instrument_data {
    auto result = make_base("BondOption");
    const auto data = trade.child("BondOptionData");
    if (!data) {
        mint_issue(result);
        return result;
    }

    if (const auto bd = data->child("BondData"))
        read_bond_terms(*bd, result);
    mint_issue(result);

    bond_option row;
    if (const auto od = data->child("OptionData"))
        row.option_type = bond_instrument_projection_text_of(*od, "OptionType");
    if (data->has("Strike"))
        row.option_strike = decimal_from_text(bond_instrument_projection_text_of(*data, "Strike"))
                                .value_or(decimal{});
    stamp_row(row);
    result.option = std::move(row);

    if (const auto od = data->child("OptionData")) {
        read_exercise_dates(*od, result);
        result.option_data = read_option_data(*od);
    }
    if (const auto strike = data->child("StrikeData"))
        result.strike_data = read_strike_data(*strike);
    if (data->has("Redemption"))
        result.option_redemption = bond_instrument_projection_text_of(*data, "Redemption");
    if (data->has("PriceType"))
        result.option_price_type = bond_instrument_projection_text_of(*data, "PriceType");
    if (data->has("KnocksOut"))
        result.option_knocks_out = spelling_of(*data, "KnocksOut");
    return result;
}

auto read_bond_trs(const record_reader& trade) -> bond_instrument_data {
    auto result = make_base("BondTRS");
    const auto data = trade.child("BondTRSData");
    if (!data) {
        mint_issue(result);
        return result;
    }

    if (const auto bd = data->child("BondData"))
        read_bond_terms(*bd, result);
    mint_issue(result);

    const auto funding = data->child("FundingData");
    const auto funding_leg = funding ? funding->child("LegData") : std::nullopt;

    bond_trs trs;
    // An ORE BondTRS is a total return swap by construction: no field of the
    // schema selects a return type, and PriceType names a price-quoting
    // convention rather than a return type.
    trs.return_type = "Total";
    if (funding_leg) {
        if (const auto floating = funding_leg->child("FloatingLegData")) {
            trs.funding_leg_type = "Floating";
            trs.funding_index = bond_instrument_projection_text_of(*floating, "Index");
        } else if (const auto fixed = funding_leg->child("FixedLegData")) {
            trs.funding_leg_type = "Fixed";
            if (const auto rates = fixed->child("Rates")) {
                const auto list = rates->children("Rate");
                if (!list.empty())
                    trs.funding_rate = decimal_text(list.front());
            }
        }
    }
    stamp_row(trs);
    result.trs = std::move(trs);
    if (funding_leg)
        result.trs_funding_leg = read_leg(*funding_leg);

    if (const auto total_return = data->child("TotalReturnData")) {
        result.trs_price_type = bond_instrument_projection_text_of(*total_return, "PriceType");
        result.trs_payer = bond_instrument_projection_text_of(*total_return, "Payer");
        if (total_return->has("InitialPrice"))
            result.trs_initial_price = total_return->decimal("InitialPrice");
        if (const auto schedule = total_return->child("ScheduleData"))
            result.trs_schedule = read_schedule(*schedule);
    }
    return result;
}

auto read_bond_repo(const record_reader& trade) -> bond_instrument_data {
    auto result = make_base("BondRepo");
    const auto data = trade.child("BondRepoData");
    if (!data) {
        mint_issue(result);
        return result;
    }

    if (const auto bd = data->child("BondData"))
        read_bond_terms(*bd, result);
    mint_issue(result);

    const auto repo = data->child("RepoData");
    const auto repo_leg = repo ? repo->child("LegData") : std::nullopt;

    bond_repo row;
    row.repo_type = "Fixed";
    if (repo_leg && spelling_of(*repo_leg, "LegType") == "Floating")
        row.repo_type = "Floating";
    if (repo_leg) {
        if (const auto fixed = repo_leg->child("FixedLegData")) {
            if (const auto rates = fixed->child("Rates")) {
                const auto list = rates->children("Rate");
                if (!list.empty())
                    row.repo_rate = decimal_text(list.front());
            }
        }
        if (!row.repo_rate) {
            if (const auto floating = repo_leg->child("FloatingLegData"))
                row.repo_index = bond_instrument_projection_text_of(*floating, "Index");
        }
    }
    stamp_row(row);
    result.repo = std::move(row);
    if (repo_leg)
        result.repo_leg = read_leg(*repo_leg);
    return result;
}

auto read_bond_future(const record_reader& trade) -> bond_instrument_data {
    auto result = make_base("BondFuture");
    // bondFutureData holds no bond terms, so the issue row the instrument points
    // at is minted empty and no lookup can match it.
    mint_issue(result);
    const auto data = trade.child("BondFutureData");
    if (!data)
        return result;

    bond_future row;
    row.contract_name = bond_instrument_projection_text_of(*data, "ContractName");
    row.contract_notional =
        decimal_from_text(bond_instrument_projection_text_of(*data, "ContractNotional"))
            .value_or(decimal{});
    row.long_short = bond_instrument_projection_text_of(*data, "LongShort");
    if (data->has("ApplyConversionFactor"))
        row.apply_conversion_factor =
            is_flag_on(bond_instrument_projection_text_of(*data, "ApplyConversionFactor"));
    if (data->has("UseFuturePrice"))
        row.use_future_price =
            is_flag_on(bond_instrument_projection_text_of(*data, "UseFuturePrice"));
    stamp_row(row);
    result.future = std::move(row);
    return result;
}

auto read_ascot(const record_reader& trade) -> bond_instrument_data {
    auto result = make_base("Ascot");
    const auto data = trade.child("AscotData");
    if (!data) {
        mint_issue(result);
        return result;
    }

    const auto convertible = data->child("ConvertibleBondData");
    if (convertible) {
        if (const auto bd = convertible->child("BondData"))
            read_bond_terms(*bd, result);
    }
    mint_issue(result);
    if (convertible)
        read_conversion_targets(*convertible, result.issue.issue_id, result.conversion_targets);

    ascot row;
    if (const auto od = data->child("OptionData"))
        row.ascot_option_type = bond_instrument_projection_text_of(*od, "OptionType");
    stamp_row(row);
    result.ascot_row = std::move(row);

    if (const auto od = data->child("OptionData")) {
        read_exercise_dates(*od, result);
        result.option_data = read_option_data(*od);
    }
    if (const auto swap = data->child("ReferenceSwapData")) {
        if (const auto leg = swap->child("LegData"))
            result.ascot_swap_leg = read_leg(*leg);
    }
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

// ---------------------------------------------------------------------------
// Writing: the schedules
// ---------------------------------------------------------------------------

void write_number(record_writer w, const bond_float_data& row) {
    w.set_own_text(row.value.to_string());
    if (row.start_date)
        w.set_text("startDate", *row.start_date);
}

void write_schedule(record_writer schedule, const bond_schedule_data& sd) {
    for (const auto& row : sd.rules) {
        auto r = schedule.append("Rules");
        r.set_text("StartDate", row.start_date);
        if (row.end_date)
            r.set_text("EndDate", *row.end_date);
        if (row.adjust_end_date_to_previous_month_end)
            set_spelling_or(r,
                            "AdjustEndDateToPreviousMonthEnd",
                            *row.adjust_end_date_to_previous_month_end,
                            "N",
                            "boolean");
        r.set_text("Tenor", row.tenor);
        if (row.calendar)
            r.set_text("Calendar", *row.calendar);
        set_spelling_or(r, "Convention", row.convention, "F", "business day convention");
        if (row.term_convention)
            set_spelling_or(
                r, "TermConvention", *row.term_convention, "F", "business day convention");
        if (row.rule)
            set_spelling_or(r, "Rule", *row.rule, "Backward", "date rule");
        if (row.end_of_month)
            set_spelling_or(r, "EndOfMonth", *row.end_of_month, "N", "boolean");
        if (row.end_of_month_convention)
            set_spelling_or(r,
                            "EndOfMonthConvention",
                            *row.end_of_month_convention,
                            "F",
                            "business day convention");
        if (row.first_date)
            r.set_text("FirstDate", *row.first_date);
        if (row.last_date)
            r.set_text("LastDate", *row.last_date);
        if (row.remove_first_date)
            r.set_text("RemoveFirstDate", flag_text(*row.remove_first_date));
        if (row.remove_last_date)
            r.set_text("RemoveLastDate", flag_text(*row.remove_last_date));
    }
    for (const auto& row : sd.dates) {
        if (row.dates.empty())
            continue;
        auto block = schedule.append("Dates");
        if (row.calendar)
            block.set_text("Calendar", *row.calendar);
        if (row.convention)
            set_spelling_or(block, "Convention", *row.convention, "F", "business day convention");
        block.set_optional_text("Tenor", row.tenor);
        if (row.end_of_month)
            set_spelling_or(block, "EndOfMonth", *row.end_of_month, "N", "boolean");
        if (row.include_duplicate_dates)
            set_spelling_or(
                block, "IncludeDuplicateDates", *row.include_duplicate_dates, "N", "boolean");
        auto dates = block.child("Dates");
        for (const auto& date : row.dates)
            dates.append_value("Date", date);
    }
}

// ---------------------------------------------------------------------------
// Writing: the legs and their economics
// ---------------------------------------------------------------------------

void write_amortization(record_writer w, const bond_amortization_data& row) {
    set_spelling_or(w, "Type", row.type, "FixedAmount", "amortization type");
    if (row.value)
        w.set_decimal("Value", *row.value);
    if (row.start_date)
        w.set_text("StartDate", *row.start_date);
    if (row.end_date)
        w.set_text("EndDate", *row.end_date);
    if (row.frequency)
        w.set_text("Frequency", *row.frequency);
    if (row.underflow)
        w.set_text("Underflow", flag_text(*row.underflow));
}

void write_stub(record_writer w, const bond_stub_interpolation& row) {
    w.set_text("ShortIndex", row.short_index);
    w.set_text("LongIndex", row.long_index);
    if (row.rounding_type)
        set_spelling_or(w, "RoundingType", *row.rounding_type, "Closest", "rounding type");
    if (row.rounding_precision)
        w.set_integer("RoundingPrecision", *row.rounding_precision);
}

void write_floating_leg(record_writer w, const bond_floating_leg_data& row) {
    w.set_text("Index", row.index);
    if (row.is_in_arrears)
        w.set_text("IsInArrears", flag_text(*row.is_in_arrears));
    if (row.last_recent_period)
        w.set_text("LastRecentPeriod", *row.last_recent_period);
    if (row.last_recent_period_calendar)
        w.set_text("LastRecentPeriodCalendar", *row.last_recent_period_calendar);
    if (row.fixing_days)
        w.set_integer("FixingDays", static_cast<std::int64_t>(*row.fixing_days));
    if (row.lookback)
        w.set_text("Lookback", *row.lookback);
    if (row.rate_cutoff)
        w.set_integer("RateCutoff", *row.rate_cutoff);
    if (row.is_averaged)
        w.set_text("IsAveraged", flag_text(*row.is_averaged));
    if (row.has_sub_periods)
        w.set_text("HasSubPeriods", flag_text(*row.has_sub_periods));
    if (row.include_spread)
        w.set_text("IncludeSpread", flag_text(*row.include_spread));
    if (row.is_not_resetting_xccy)
        w.set_text("IsNotResettingXCCY", flag_text(*row.is_not_resetting_xccy));
    if (!row.spreads.empty()) {
        auto list = w.child("Spreads");
        for (const auto& spread : row.spreads)
            write_number(list.append("Spread"), spread);
    }
    if (!row.caps.empty()) {
        auto list = w.child("Caps");
        for (const auto& cap : row.caps)
            write_number(list.append("Cap"), cap);
    }
    if (!row.floors.empty()) {
        auto list = w.child("Floors");
        for (const auto& floor : row.floors)
            write_number(list.append("Floor"), floor);
    }
    if (!row.gearings.empty()) {
        auto list = w.child("Gearings");
        for (const auto& gearing : row.gearings)
            write_number(list.append("Gearing"), gearing);
    }
    if (row.naked_option)
        w.set_text("NakedOption", flag_text(*row.naked_option));
    if (row.local_cap_floor)
        w.set_text("LocalCapFloor", flag_text(*row.local_cap_floor));
    if (!row.fixing_schedule.rules.empty() || !row.fixing_schedule.dates.empty())
        write_schedule(w.child("FixingSchedule"), row.fixing_schedule);
    if (!row.reset_schedule.rules.empty() || !row.reset_schedule.dates.empty())
        write_schedule(w.child("ResetSchedule"), row.reset_schedule);
    if (row.front_stub_interpolation)
        write_stub(w.child("FrontStubInterpolation"), *row.front_stub_interpolation);
    if (row.back_stub_interpolation)
        write_stub(w.child("BackStubInterpolation"), *row.back_stub_interpolation);
    if (row.stub_use_original_curve)
        w.set_text("StubUseOriginalCurve", flag_text(*row.stub_use_original_curve));
    if (row.observation_shift)
        w.set_text("ObservationShift", flag_text(*row.observation_shift));
}

void write_formula_leg(record_writer w, const bond_formula_based_leg_data& row) {
    w.set_text("Index", row.index);
    if (row.is_in_arrears)
        w.set_text("IsInArrears", flag_text(*row.is_in_arrears));
    w.set_integer("FixingDays", row.fixing_days);
    if (row.fixing_calendar)
        w.set_text("FixingCalendar", *row.fixing_calendar);
}

void write_rate_group(record_writer leg, const bond_leg_rate_data& row) {
    if (row.fixed) {
        auto fixed = leg.child("FixedLegData");
        auto list = fixed.child("Rates");
        for (const auto& rate : row.fixed->rates)
            write_number(list.append("Rate"), rate);
    }
    if (row.floating)
        write_floating_leg(leg.child("FloatingLegData"), *row.floating);
    if (row.formula_based)
        write_formula_leg(leg.child("FormulaBasedLegData"), *row.formula_based);
}

void write_leg(record_writer leg, const bond_leg_data& row) {
    leg.set_text("Payer", flag_text(row.payer.value_or(false)));
    set_spelling_or(leg, "LegType", row.leg_type.value_or(""), "Fixed", "leg type");
    if (row.currency)
        leg.set_text("Currency", *row.currency);
    if (row.payment_convention)
        set_spelling_or(
            leg, "PaymentConvention", *row.payment_convention, "F", "business day convention");
    if (row.payment_lag)
        leg.set_text("PaymentLag", *row.payment_lag);
    if (row.payment_calendar)
        leg.set_text("PaymentCalendar", *row.payment_calendar);
    if (row.day_counter)
        set_spelling_or(leg, "DayCounter", *row.day_counter, "A360", "day counter");
    if (row.last_period_day_counter)
        set_spelling_or(
            leg, "LastPeriodDayCounter", *row.last_period_day_counter, "A360", "day counter");
    if (row.notional_payment_lag)
        leg.set_integer("NotionalPaymentLag", *row.notional_payment_lag);
    if (row.strict_notional_dates)
        leg.set_text("StrictNotionalDates", flag_text(*row.strict_notional_dates));
    if (!row.schedule.rules.empty() || !row.schedule.dates.empty())
        write_schedule(leg.child("ScheduleData"), row.schedule);
    if (!row.amortizations.empty()) {
        auto group = leg.child("Amortizations");
        for (const auto& amortization : row.amortizations)
            write_amortization(group.append("AmortizationData"), amortization);
    }
    if (!row.notionals.empty()) {
        auto group = leg.child("Notionals");
        for (const auto& notional : row.notionals)
            write_number(group.append("Notional"), notional);
    }
    if (!row.payment_dates.empty()) {
        auto group = leg.child("PaymentDates");
        for (const auto& date : row.payment_dates)
            group.append_value("PaymentDate", date);
    }
    if (row.indexings_from_asset_leg)
        leg.child("Indexings").set_text("FromAssetLeg", flag_text(*row.indexings_from_asset_leg));
    if (row.rate)
        write_rate_group(leg, *row.rate);
    if (!row.payment_schedule.rules.empty() || !row.payment_schedule.dates.empty())
        write_schedule(leg.child("PaymentSchedule"), row.payment_schedule);
    if (row.settlement) {
        auto settlement = leg.child("SettlementData");
        settlement.set_text("FXIndex", row.settlement->fx_index);
        if (row.settlement->fixing_date)
            settlement.set_text("FixingDate", *row.settlement->fixing_date);
    }
}

// ---------------------------------------------------------------------------
// Writing: the option block and the strike
// ---------------------------------------------------------------------------

void write_option_settlement(record_writer w, const bond_option_settlement& row) {
    set_spelling_or(w, "PayCurrency", row.pay_currency, "USD", "currency");
    w.set_text("FXIndex", row.fx_index);
    if (row.fixing_date)
        w.set_text("FixingDate", *row.fixing_date);
}

void write_option_data(record_writer od, const bond_option_data& row) {
    od.set_text("LongShort", row.long_short);
    od.set_optional_text("OptionType", row.option_type);
    od.set_optional_text("PayoffType", row.payoff_type);
    od.set_optional_text("PayoffType2", row.payoff_type_2);
    od.set_optional_text("Style", row.style);
    od.set_optional_text("NoticePeriod", row.notice_period);
    od.set_optional_text("NoticeCalendar", row.notice_calendar);
    od.set_optional_text("NoticeConvention", row.notice_convention);
    od.set_optional_text("MidCouponExercise", row.mid_coupon_exercise);
    if (row.settlement)
        set_spelling_or(od, "Settlement", *row.settlement, "Cash", "settlement type");
    if (row.settlement_method)
        set_spelling_or(
            od, "SettlementMethod", *row.settlement_method, "PhysicalOTC", "settlement method");
    od.set_optional_text("PayOffAtExpiry", row.pay_off_at_expiry);
    if (row.premium_amount)
        od.set_text("PremiumAmount", row.premium_amount->to_string());
    od.set_optional_text("PremiumCurrency", row.premium_currency);
    od.set_optional_text("PremiumPayDate", row.premium_pay_date);
    if (!row.premiums.empty()) {
        auto premiums = od.child("Premiums");
        for (const auto& premium : row.premiums) {
            auto w = premiums.append("Premium");
            w.set_decimal("Amount", premium.amount);
            w.set_text("Currency", premium.currency);
            w.set_text("PayDate", premium.pay_date);
            if (premium.settlement)
                write_option_settlement(w.child("SettlementData"), *premium.settlement);
        }
    }
    od.set_optional_text("ExercisePrices", row.exercise_prices);
    if (!row.exercise_fees.empty()) {
        auto fees = od.child("ExerciseFees");
        for (const auto& fee : row.exercise_fees) {
            auto w = fees.append("ExerciseFee");
            w.set_own_text(fee.amount.to_string());
            if (fee.type)
                w.set_text("type", *fee.type);
            if (fee.start_date)
                w.set_text("startDate", *fee.start_date);
            if (fee.currency)
                w.set_text("currency", *fee.currency);
        }
    }
    od.set_optional_text("ExerciseFeeSettlementPeriod", row.exercise_fee_settlement_period);
    od.set_optional_text("ExerciseFeeSettlementCalendar", row.exercise_fee_settlement_calendar);
    od.set_optional_text("ExerciseFeeSettlementConvention", row.exercise_fee_settlement_convention);
    if (row.automatic_exercise)
        set_spelling_or(od, "AutomaticExercise", *row.automatic_exercise, "N", "boolean");
    if (row.exercise_data) {
        auto exercise = od.child("ExerciseData");
        exercise.set_text("Date", row.exercise_data->date);
        if (row.exercise_data->price)
            exercise.set_decimal("Price", *row.exercise_data->price);
    }
    if (row.payment_data) {
        auto payment = od.child("PaymentData");
        if (!row.payment_data->dates.empty()) {
            auto dates = payment.child("Dates");
            for (const auto& date : row.payment_data->dates)
                dates.append_value("Date", date);
        }
        if (row.payment_data->rules) {
            const auto& rules = *row.payment_data->rules;
            auto w = payment.child("Rules");
            w.set_integer("Lag", static_cast<std::int64_t>(rules.lag));
            w.set_text("Calendar", rules.calendar);
            set_spelling_or(w, "Convention", rules.convention, "MF", "business day convention");
            if (rules.relative_to)
                set_spelling_or(w, "RelativeTo", *rules.relative_to, "Expiry", "pay relative to");
        }
    }
    if (row.settlement_data)
        write_option_settlement(od.child("SettlementData"), *row.settlement_data);
}

void write_strike_data(record_writer sd, const bond_strike_data& row) {
    if (row.price_value || row.price_currency) {
        auto price = sd.child("StrikePrice");
        if (row.price_value)
            price.set_decimal("Value", *row.price_value);
        if (row.price_currency)
            price.set_text("Currency", *row.price_currency);
    }
    if (row.yield_value || row.yield_compounding) {
        auto yield = sd.child("StrikeYield");
        if (row.yield_value)
            yield.set_decimal("Yield", *row.yield_value);
        if (row.yield_compounding)
            set_spelling_or(
                yield, "Compounding", *row.yield_compounding, "Compounded", "compounding");
    }
    if (row.bare_value)
        sd.set_decimal("Value", *row.bare_value);
    if (row.bare_currency)
        sd.set_text("Currency", *row.bare_currency);
}

void write_exercise_dates(record_writer option,
                          const std::vector<std::string>& dates,
                          const std::optional<bond_schedule_data>& schedule) {
    if (!dates.empty()) {
        auto list = option.child("ExerciseDates");
        for (const auto& date : dates)
            list.append_value("ExerciseDate", date);
    } else if (schedule) {
        write_schedule(option.child("ExerciseSchedule"), *schedule);
    }
}

// ---------------------------------------------------------------------------
// Writing: the bond terms, its issue children and its legs
// ---------------------------------------------------------------------------

void write_conversion_targets(const std::vector<bond_issue_conversion_target>& targets,
                              record_writer conversion) {
    if (targets.empty())
        return;
    auto ratios = conversion.child("ConversionRatios");
    for (const auto& row : targets)
        ratios.append("ConversionRatio")
            .set_own_text(decimal_from_double(row.conversion_ratio).to_string());
    if (!targets.front().underlying_id.empty())
        conversion.child("Underlying").set_text("Name", targets.front().underlying_id);
}

void write_call_dates(const std::vector<bond_issue_call_date>& dates, record_writer call) {
    if (dates.empty())
        return;
    auto schedule = call.child("ScheduleData");
    auto block = schedule.append("Dates");
    auto list = block.child("Dates");
    for (const auto& row : dates)
        list.append_value("Date", to_ore_date(row.call_date));

    // The schema requires the call block's styles, prices, price types and
    // include-accrual lists beside its schedule, and no row holds any of them.
    // The emitter refuses a required list with no entry, so one default entry
    // stands in for each list and the document binds again.
    call.child("Styles").append("Style").set_own_text("");
    call.child("Prices").append("Price").set_own_text("0");
    call.child("PriceTypes").append("PriceType").set_own_text("");
    call.child("IncludeAccruals").append("IncludeAccrual").set_own_text("false");
}

void write_bond_terms(record_writer bd,
                      const bond_instrument_data& data,
                      const std::optional<std::string>& reference_curve) {
    const auto& issue = data.issue;
    bd.set_text("SecurityId", issue.security_id);
    if (!issue.issuer.empty())
        bd.set_text("IssuerId", issue.issuer);
    if (issue.issue_date)
        bd.set_text("IssueDate", to_ore_date(*issue.issue_date));
    if (issue.settlement_days != 0)
        bd.set_text("SettlementDays", std::to_string(issue.settlement_days));
    bd.set_optional_text("Calendar", issue.calendar);
    bd.set_optional_text("CreditCurveId", issue.credit_curve_id);
    bd.set_optional_text("ReferenceCurveId", reference_curve);
    bd.set_optional_text("IncomeCurveId", issue.income_curve_id);
    bd.set_optional_text("CreditGroup", issue.credit_group);
    bd.set_optional_text("VolatilityCurveId", issue.volatility_curve_id);
    bd.set_optional_text("PriceQuoteMethod", issue.price_quote_method);
    if (issue.price_quote_base_value)
        bd.set_text("PriceQuoteBaseValue", issue.price_quote_base_value->to_string());
    bd.set_optional_text("SubType", issue.sub_type);
    bd.set_optional_text("Payer", issue.payer);
    if (issue.price_type && accepts_spelling(bd, "PriceType", *issue.price_type))
        bd.set_spelling("PriceType", *issue.price_type, "price type");
    if (issue.credit_risk)
        set_spelling_or(bd, "CreditRisk", *issue.credit_risk, "N", "boolean");
    if (data.instrument.notional)
        bd.set_text("BondNotional", data.instrument.notional->to_string());

    // The legs are emitted when the document held any, or when the issue states
    // a face value and carries none: the issue row's face value repeats the
    // first leg's notional and stands in for a missing leg.
    if (!data.bond_legs.empty() || (issue.face_value && !issue.face_value->is_zero())) {
        const std::size_t leg_count = data.bond_legs.empty() ? 1 : data.bond_legs.size();
        for (std::size_t i = 0; i < leg_count; ++i) {
            const bond_leg_data absent;
            const bond_leg_data& source = data.bond_legs.empty() ? absent : data.bond_legs[i];
            auto leg = bd.append("LegData");
            write_leg(leg, source);

            if (i == 0 && source.notionals.empty() && issue.face_value &&
                !issue.face_value->is_zero()) {
                bond_float_data notional;
                notional.value = *issue.face_value;
                write_number(leg.child("Notionals").append("Notional"), notional);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Writing: one function per product
// ---------------------------------------------------------------------------

/// The ORE curve name the run's mapping gives the issue's market series.
auto reference_curve_of(const bond_instrument_data& data,
                        const portfolio_reference_curves& reference_curves)
    -> std::optional<std::string> {
    auto name = data.issue.reference_curve_id;
    if (reference_curves && name && !name->empty())
        name = reference_curves(*name);
    return name;
}

void write_bond(record_writer trade,
                const bond_instrument_data& data,
                const std::optional<std::string>& reference_curve) {
    write_bond_terms(trade.child("BondData"), data, reference_curve);
}

void write_forward_bond(record_writer trade,
                        const bond_instrument_data& data,
                        const std::optional<std::string>& reference_curve) {
    auto d = trade.child("ForwardBondData");
    write_bond_terms(d.child("BondData"), data, reference_curve);
    if (data.forward_long_in_forward)
        d.set_text("LongInForward", *data.forward_long_in_forward);
    if (data.forward_settlement) {
        const auto& row = *data.forward_settlement;
        auto s = d.child("SettlementData");
        s.set_text("ForwardMaturityDate", row.forward_maturity_date);
        if (row.forward_settlement_date)
            s.set_text("ForwardSettlementDate", *row.forward_settlement_date);
        if (row.settlement)
            s.set_text("Settlement", *row.settlement);
        s.set_optional_decimal("Amount", row.amount);
        s.set_optional_decimal("LockRate", row.lock_rate);
        s.set_optional_decimal("dv01", row.dv01);
        if (row.lock_rate_day_counter)
            s.set_text("LockRateDayCounter", *row.lock_rate_day_counter);
        if (row.settlement_dirty)
            s.set_text("SettlementDirty", *row.settlement_dirty);
    }
    if (data.forward_premium) {
        auto p = d.child("PremiumData");
        p.set_text("Amount", data.forward_premium->amount.to_string());
        p.set_text("Date", data.forward_premium->date);
    }
}

void write_callable_bond(record_writer trade,
                         const bond_instrument_data& data,
                         const std::optional<std::string>& reference_curve) {
    auto d = trade.child("CallableBondData");
    write_bond_terms(d.child("BondData"), data, reference_curve);
    if (!data.call_dates.empty())
        write_call_dates(data.call_dates, d.child("CallData"));
}

void write_convertible_bond(record_writer trade,
                            const bond_instrument_data& data,
                            const std::optional<std::string>& reference_curve) {
    auto d = trade.child("ConvertibleBondData");
    write_bond_terms(d.child("BondData"), data, reference_curve);
    if (!data.conversion_targets.empty())
        write_conversion_targets(data.conversion_targets, d.child("ConversionData"));
}

void write_bond_option(record_writer trade,
                       const bond_instrument_data& data,
                       const std::optional<std::string>& reference_curve) {
    auto d = trade.child("BondOptionData");
    write_bond_terms(d.child("BondData"), data, reference_curve);
    auto option = d.child("OptionData");
    if (data.option_data)
        write_option_data(option, *data.option_data);
    else {
        // The option block is required, and its direction is required within
        // it. The row set holds neither, and the old export stated the
        // direction empty.
        option.set_text("LongShort", "");
        if (data.option && !data.option->option_type.empty())
            option.set_text("OptionType", data.option->option_type);
    }
    if (data.option && !data.option->option_strike.is_zero() && !data.strike_data)
        d.set_text("Strike", format_number(data.option->option_strike.to_double()));
    if (data.strike_data)
        write_strike_data(d.child("StrikeData"), *data.strike_data);
    if (data.option_redemption)
        d.set_text("Redemption", *data.option_redemption);
    if (data.option_price_type)
        d.set_text("PriceType", *data.option_price_type);
    if (data.option_knocks_out)
        set_spelling_or(d, "KnocksOut", *data.option_knocks_out, "N", "boolean");
    write_exercise_dates(option, data.option_exercise_dates, data.option_exercise_schedule);
}

void write_bond_trs(record_writer trade,
                    const bond_instrument_data& data,
                    const std::optional<std::string>& reference_curve) {
    auto d = trade.child("BondTRSData");
    write_bond_terms(d.child("BondData"), data, reference_curve);

    auto total_return = d.child("TotalReturnData");
    if (!data.trs_price_type.empty())
        total_return.set_text("PriceType", data.trs_price_type);

    auto funding_leg = d.child("FundingData").child("LegData");
    write_leg(funding_leg, data.trs_funding_leg);
    // The leg type comes from the container, which holds the document's own
    // statement. The row's funding type stands in only when the payload holds
    // no leg of its own.
    const bool leg_type_from_row = !data.trs_funding_leg.leg_type;
    const bool fixed =
        !data.trs || data.trs->funding_leg_type.empty() || data.trs->funding_leg_type == "Fixed";
    if (fixed) {
        if (leg_type_from_row)
            funding_leg.set_spelling("LegType", "Fixed", "leg type");
        if (data.trs && data.trs->funding_rate && !data.trs->funding_rate->is_zero()) {
            auto rates = funding_leg.child("FixedLegData").child("Rates");
            bond_float_data rate;
            rate.value = *data.trs->funding_rate;
            write_number(rates.append("Rate"), rate);
        }
    } else if (data.trs) {
        if (leg_type_from_row)
            funding_leg.set_spelling("LegType", "Floating", "leg type");
        auto floating = funding_leg.child("FloatingLegData");
        if (data.trs->funding_index)
            floating.set_text("Index", *data.trs->funding_index);
    }

    if (data.trs_payer)
        total_return.set_text("Payer", *data.trs_payer);
    if (data.trs_initial_price)
        total_return.set_decimal("InitialPrice", *data.trs_initial_price);
    write_schedule(total_return.child("ScheduleData"), data.trs_schedule);
}

void write_bond_repo(record_writer trade,
                     const bond_instrument_data& data,
                     const std::optional<std::string>& reference_curve) {
    auto d = trade.child("BondRepoData");
    write_bond_terms(d.child("BondData"), data, reference_curve);

    auto leg = d.child("RepoData").child("LegData");
    write_leg(leg, data.repo_leg);
    const bool floating = data.repo && data.repo->repo_type == "Floating";
    if (!data.repo_leg.leg_type)
        leg.set_spelling("LegType", floating ? "Floating" : "Fixed", "leg type");
    if (floating) {
        auto floating_leg = leg.child("FloatingLegData");
        if (data.repo->repo_index)
            floating_leg.set_text("Index", *data.repo->repo_index);
    } else if (data.repo && data.repo->repo_rate && !data.repo->repo_rate->is_zero()) {
        auto rates = leg.child("FixedLegData").child("Rates");
        bond_float_data rate;
        rate.value = *data.repo->repo_rate;
        write_number(rates.append("Rate"), rate);
    }
}

void write_bond_future(record_writer trade,
                       const bond_instrument_data& data,
                       const std::optional<std::string>&) {
    auto d = trade.child("BondFutureData");
    if (!data.future)
        return;
    const auto& row = *data.future;
    d.set_text("ContractName", row.contract_name);
    d.set_text("ContractNotional", format_number(row.contract_notional.to_double()));
    d.set_text("LongShort", row.long_short);
    // An absent member stays absent and a stated one is written, so a document
    // that states false keeps it.
    if (row.apply_conversion_factor)
        d.set_text("ApplyConversionFactor", flag_text(*row.apply_conversion_factor));
    if (row.use_future_price)
        d.set_text("UseFuturePrice", flag_text(*row.use_future_price));
}

void write_ascot(record_writer trade,
                 const bond_instrument_data& data,
                 const std::optional<std::string>& reference_curve) {
    auto d = trade.child("AscotData");
    auto convertible = d.child("ConvertibleBondData");
    write_bond_terms(convertible.child("BondData"), data, reference_curve);
    if (!data.conversion_targets.empty())
        write_conversion_targets(data.conversion_targets, convertible.child("ConversionData"));
    auto option = d.child("OptionData");
    if (data.option_data)
        write_option_data(option, *data.option_data);
    else {
        option.set_text("LongShort", "");
        if (data.ascot_row && !data.ascot_row->ascot_option_type.empty())
            option.set_text("OptionType", data.ascot_row->ascot_option_type);
    }
    write_exercise_dates(option, data.option_exercise_dates, data.option_exercise_schedule);
    write_leg(d.child("ReferenceSwapData").child("LegData"), data.ascot_swap_leg);
}

}

auto read_bond_instrument(xml::record_reader trade, std::string_view trade_type)
    -> std::optional<trading::domain::trade_instrument> {
    if (!is_bond_trade_type(trade_type))
        return std::nullopt;

    if (trade_type == "Bond")
        return trading::domain::trade_instrument{read_bond(trade)};
    if (trade_type == "ForwardBond")
        return trading::domain::trade_instrument{read_forward_bond(trade)};
    if (trade_type == "CallableBond")
        return trading::domain::trade_instrument{read_callable_bond(trade)};
    if (trade_type == "ConvertibleBond")
        return trading::domain::trade_instrument{read_convertible_bond(trade)};
    if (trade_type == "BondOption")
        return trading::domain::trade_instrument{read_bond_option(trade)};
    if (trade_type == "BondTRS")
        return trading::domain::trade_instrument{read_bond_trs(trade)};
    if (trade_type == "BondRepo")
        return trading::domain::trade_instrument{read_bond_repo(trade)};
    if (trade_type == "BondFuture")
        return trading::domain::trade_instrument{read_bond_future(trade)};
    return trading::domain::trade_instrument{read_ascot(trade)};
}

auto write_bond_instrument(xml::record_writer& trade,
                           std::string_view trade_type,
                           const trading::domain::trade_instrument& instrument,
                           const portfolio_reference_curves& reference_curves) -> bool {
    const auto* data = std::get_if<bond_instrument_data>(&instrument);
    if (data == nullptr || !is_bond_trade_type(trade_type))
        return false;

    const auto reference_curve = reference_curve_of(*data, reference_curves);
    if (trade_type == "Bond")
        write_bond(trade, *data, reference_curve);
    else if (trade_type == "ForwardBond")
        write_forward_bond(trade, *data, reference_curve);
    else if (trade_type == "CallableBond")
        write_callable_bond(trade, *data, reference_curve);
    else if (trade_type == "ConvertibleBond")
        write_convertible_bond(trade, *data, reference_curve);
    else if (trade_type == "BondOption")
        write_bond_option(trade, *data, reference_curve);
    else if (trade_type == "BondTRS")
        write_bond_trs(trade, *data, reference_curve);
    else if (trade_type == "BondRepo")
        write_bond_repo(trade, *data, reference_curve);
    else if (trade_type == "BondFuture")
        write_bond_future(trade, *data, reference_curve);
    else
        write_ascot(trade, *data, reference_curve);
    return true;
}

}
