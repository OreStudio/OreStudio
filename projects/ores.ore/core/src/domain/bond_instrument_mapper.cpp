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
#include "ores.ore.core/domain/bond_instrument_mapper.hpp"
#include <boost/uuid/random_generator.hpp>
#include <charconv>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace ores::ore::domain {

using namespace ores::logging;
using ores::trading::domain::ascot;
using ores::trading::domain::bond_amortization_data;
using ores::trading::domain::bond_fixed_leg_data;
using ores::trading::domain::bond_float_data;
using ores::trading::domain::bond_floating_leg_data;
using ores::trading::domain::bond_formula_based_leg_data;
using ores::trading::domain::bond_forward_premium;
using ores::trading::domain::bond_forward_settlement;
using ores::trading::domain::bond_future;
using ores::trading::domain::bond_instrument_data;
using ores::trading::domain::bond_issue;
using ores::trading::domain::bond_issue_call_date;
using ores::trading::domain::bond_issue_conversion_target;
using ores::trading::domain::bond_leg_data;
using ores::trading::domain::bond_leg_rate_data;
using ores::trading::domain::bond_option;
using ores::trading::domain::bond_option_data;
using ores::trading::domain::bond_option_exercise;
using ores::trading::domain::bond_option_exercise_fee;
using ores::trading::domain::bond_option_payment_data;
using ores::trading::domain::bond_option_payment_rules;
using ores::trading::domain::bond_option_premium;
using ores::trading::domain::bond_option_settlement;
using ores::trading::domain::bond_repo;
using ores::trading::domain::bond_schedule_data;
using ores::trading::domain::bond_schedule_dates;
using ores::trading::domain::bond_schedule_rules;
using ores::trading::domain::bond_settlement_data;
using ores::trading::domain::bond_strike_data;
using ores::trading::domain::bond_stub_interpolation;
using ores::trading::domain::bond_trs;

namespace {

std::string first_tenor(const xsd::optional<scheduleData>& sd) {
    if (!sd || sd->Rules.empty())
        return {};
    return std::string(sd->Rules.front().Tenor);
}

// The instrument header, the issue and the fact rows each carry the audit
// columns; every row of a mapped trade shares the import provenance.
template <typename T>
void stamp_audit(T& row) {
    row.modified_by = "ores";
    row.performed_by = "ores";
    row.change_reason_code = "system.external_data_import";
    row.change_commentary = "Imported from ORE XML";
}

bond_instrument_data make_base(const std::string& trade_type_code) {
    bond_instrument_data result;
    result.instrument.identity.trade_type_code = trade_type_code;
    stamp_audit(result.instrument.audit);
    stamp_audit(result.issue);
    return result;
}

// A lookup hit adopts the row the security already has; the document's
// own terms are the fallback, and a miss mints one issue row.
void resolve_issue(bond_instrument_data& result, const bond_issue_lookup& lookup) {
    if (lookup && !result.issue.security_id.empty()) {
        if (auto found = lookup(result.issue.security_id)) {
            result.issue = *found;
            result.instrument.issue_id = result.issue.issue_id;
            return;
        }
    }
    const auto issue_id = boost::uuids::random_generator()();
    result.instrument.issue_id = issue_id;
    result.issue.issue_id = issue_id;
}

// The generated domain exports to_string for every enumeration but no
// parse. Scanning the spellings back through to_string keeps one source
// of truth: a spelling the schema adds is parsed with no table here to
// keep aligned with the generated one.
template <typename Enum>
Enum parse_code(const std::string& text, int count, Enum fallback) {
    for (int i = 0; i < count; ++i) {
        const auto candidate = static_cast<Enum>(i);
        if (to_string(candidate) == text)
            return candidate;
    }
    return fallback;
}

constexpr int amortization_type_count = 5;
constexpr int bool__count = 13;
constexpr int business_day_convention_count = 27;
constexpr int compounding_count = 5;
constexpr int date_rule_count = 16;
constexpr int currency_code_count = 191;
constexpr int day_counter_count = 71;
constexpr int leg_type_count = 18;
constexpr int option_pay_relative_to_count = 2;
constexpr int rounding_type_count = 5;
constexpr int settlement_method_count = 4;
constexpr int settlement_type_count = 2;

// The class carries its own logger for its members. The helpers below are
// free functions, so they reach the same channel through this one.
inline std::string_view logger_name = "ores.ore.domain.bond_instrument_mapper";

auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

// The schema states the future's price and lag fields as strings. A
// value the parser cannot read leaves the column at its default rather
// than failing the whole import.
double number_of(const std::string& text, double fallback) {
    try {
        return std::stod(text);
    } catch (const std::exception&) {
        BOOST_LOG_SEV(lg(), warn) << "Unreadable number '" << text << "', using " << fallback;
        return fallback;
    }
}

int count_of(const std::string& text, int fallback) {
    try {
        return std::stoi(text);
    } catch (const std::exception&) {
        BOOST_LOG_SEV(lg(), warn) << "Unreadable count '" << text << "', using " << fallback;
        return fallback;
    }
}

// A generated wrapper derives from the string type it carries, and a
// std::string does not convert to a derived type, so the reverse
// direction writes through the base reference.
template <typename Field>
void set_text(Field& field, const std::string& value) {
    static_cast<std::string&>(field) = value;
}

template <typename Field>
void set_optional_text(xsd::optional<Field>& field, const std::string& value) {
    if (value.empty())
        return;
    Field wrapper;
    static_cast<std::string&>(wrapper) = value;
    field = std::move(wrapper);
}

// Emits on presence rather than on content: an element the document
// states empty is a different document from one it omits.
template <typename Field>
void set_present_text(xsd::optional<Field>& field, const std::optional<std::string>& value) {
    if (!value)
        return;
    Field wrapper;
    static_cast<std::string&>(wrapper) = *value;
    field = std::move(wrapper);
}

// std::to_string writes six fixed decimals, so a value below 1e-6 comes
// back as zero and anything past the sixth decimal is gone. The schema
// states these as xs:float, and the documents write decimals the reader
// has already rounded to fit, so the reverse direction writes the
// shortest text that still reads back as the same number.
std::string format_number(double value) {
    char buffer[32];
    const auto written = std::to_chars(buffer, buffer + sizeof(buffer), value);
    if (written.ec != std::errc{})
        return {};
    return std::string(buffer, written.ptr);
}

bond_schedule_data map_schedule(const scheduleData& sd) {
    bond_schedule_data result;
    for (const auto& r : sd.Rules) {
        bond_schedule_rules row;
        row.start_date = r.StartDate;
        if (r.EndDate)
            row.end_date = *r.EndDate;
        if (r.AdjustEndDateToPreviousMonthEnd)
            row.adjust_end_date_to_previous_month_end = to_string(*r.AdjustEndDateToPreviousMonthEnd);
        row.tenor = r.Tenor;
        if (r.Calendar)
            row.calendar = *r.Calendar;
        row.convention = to_string(r.Convention);
        if (r.TermConvention)
            row.term_convention = to_string(*r.TermConvention);
        if (r.Rule)
            row.rule = to_string(*r.Rule);
        if (r.EndOfMonth)
            row.end_of_month = to_string(*r.EndOfMonth);
        if (r.EndOfMonthConvention)
            row.end_of_month_convention = to_string(*r.EndOfMonthConvention);
        if (r.FirstDate)
            row.first_date = *r.FirstDate;
        if (r.LastDate)
            row.last_date = *r.LastDate;
        if (r.RemoveFirstDate)
            row.remove_first_date = *r.RemoveFirstDate;
        if (r.RemoveLastDate)
            row.remove_last_date = *r.RemoveLastDate;
        result.rules.push_back(std::move(row));
    }
    for (const auto& d : sd.Dates) {
        bond_schedule_dates row;
        if (d.Calendar)
            row.calendar = *d.Calendar;
        if (d.Convention)
            row.convention = to_string(*d.Convention);
        if (d.Tenor)
            row.tenor = *d.Tenor;
        if (d.EndOfMonth)
            row.end_of_month = to_string(*d.EndOfMonth);
        if (d.IncludeDuplicateDates)
            row.include_duplicate_dates = to_string(*d.IncludeDuplicateDates);
        for (const auto& date : d.Dates.Date)
            row.dates.push_back(date);
        result.dates.push_back(std::move(row));
    }
    return result;
}

// Text members emit on presence, so an element the document states
// empty is re-emitted rather than dropped. The generated bool type is
// written the same way: the container holds one of its spellings, the
// empty one included, so the spelling the document chose comes back.
scheduleData reverse_schedule(const bond_schedule_data& sd) {
    scheduleData result;
    for (const auto& row : sd.rules) {
        scheduleData_Rules_t r;
        r.StartDate = row.start_date;
        if (row.end_date)
            r.EndDate = *row.end_date;
        if (row.adjust_end_date_to_previous_month_end)
            r.AdjustEndDateToPreviousMonthEnd =
                parse_code(*row.adjust_end_date_to_previous_month_end, bool__count, bool_::N);
        set_text(r.Tenor, row.tenor);
        if (row.calendar)
            r.Calendar = *row.calendar;
        r.Convention =
            parse_code(row.convention, business_day_convention_count, businessDayConvention::F);
        if (row.term_convention)
            r.TermConvention = parse_code(*row.term_convention,
                                          business_day_convention_count,
                                          businessDayConvention::F);
        if (row.rule)
            r.Rule = parse_code(*row.rule, date_rule_count, dateRule::Backward);
        if (row.end_of_month)
            r.EndOfMonth = parse_code(*row.end_of_month, bool__count, bool_::N);
        if (row.end_of_month_convention)
            r.EndOfMonthConvention = parse_code(*row.end_of_month_convention,
                                                business_day_convention_count,
                                                businessDayConvention::F);
        if (row.first_date)
            r.FirstDate = *row.first_date;
        if (row.last_date)
            r.LastDate = *row.last_date;
        if (row.remove_first_date)
            r.RemoveFirstDate = *row.remove_first_date;
        if (row.remove_last_date)
            r.RemoveLastDate = *row.remove_last_date;
        result.Rules.push_back(std::move(r));
    }
    for (const auto& row : sd.dates) {
        scheduleData_Dates_t d;
        if (row.calendar)
            d.Calendar = *row.calendar;
        if (row.convention)
            d.Convention = parse_code(*row.convention,
                                      business_day_convention_count,
                                      businessDayConvention::F);
        set_present_text(d.Tenor, row.tenor);
        if (row.end_of_month)
            d.EndOfMonth = parse_code(*row.end_of_month, bool__count, bool_::N);
        if (row.include_duplicate_dates)
            d.IncludeDuplicateDates =
                parse_code(*row.include_duplicate_dates, bool__count, bool_::N);
        for (const auto& date : row.dates)
            d.Dates.Date.push_back(date);
        result.Dates.push_back(std::move(d));
    }
    return result;
}

// The fixed rates, the spreads, the caps, the floors, the gearings and the
// notionals all carry one number and an optional start date, and the
// generated types differ only in the name of the wrapper.
template <typename Row>
bond_float_data map_number(const Row& row) {
    bond_float_data result;
    result.value = static_cast<double>(row);
    if (row.startDate)
        result.start_date = std::string(*row.startDate);
    return result;
}

template <typename Row>
Row reverse_number(const bond_float_data& row) {
    Row result;
    static_cast<float&>(result) = static_cast<float>(row.value);
    if (row.start_date)
        result.startDate = *row.start_date;
    return result;
}

template <typename Row>
xsd::vector<Row> reverse_numbers(const std::vector<bond_float_data>& rows) {
    xsd::vector<Row> result;
    for (const auto& row : rows)
        result.push_back(reverse_number<Row>(row));
    return result;
}

bond_amortization_data map_amortization(const amortizationData& a) {
    bond_amortization_data result;
    result.type = to_string(a.Type);
    if (a.Value)
        result.value = static_cast<double>(*a.Value);
    if (a.StartDate)
        result.start_date = std::string(*a.StartDate);
    if (a.EndDate)
        result.end_date = std::string(*a.EndDate);
    if (a.Frequency)
        result.frequency = std::string(*a.Frequency);
    if (a.Underflow)
        result.underflow = *a.Underflow;
    return result;
}

amortizationData reverse_amortization(const bond_amortization_data& row) {
    amortizationData result;
    result.Type =
        parse_code(row.type, amortization_type_count, amortizationType::FixedAmount);
    if (row.value)
        result.Value = static_cast<float>(*row.value);
    set_present_text(result.StartDate, row.start_date);
    set_present_text(result.EndDate, row.end_date);
    set_present_text(result.Frequency, row.frequency);
    if (row.underflow)
        result.Underflow = *row.underflow;
    return result;
}

bond_stub_interpolation map_stub(const stubInterpolation& s) {
    bond_stub_interpolation result;
    result.short_index = std::string(s.ShortIndex);
    result.long_index = std::string(s.LongIndex);
    if (s.RoundingType)
        result.rounding_type = to_string(*s.RoundingType);
    if (s.RoundingPrecision)
        result.rounding_precision = *s.RoundingPrecision;
    return result;
}

stubInterpolation reverse_stub(const bond_stub_interpolation& s) {
    stubInterpolation result;
    static_cast<std::string&>(result.ShortIndex) = s.short_index;
    static_cast<std::string&>(result.LongIndex) = s.long_index;
    if (s.rounding_type)
        result.RoundingType =
            parse_code(*s.rounding_type, rounding_type_count, roundingType::Closest);
    if (s.rounding_precision)
        result.RoundingPrecision = *s.rounding_precision;
    return result;
}

bond_forward_settlement map_forward_settlement(const settlementData& s) {
    bond_forward_settlement result;
    result.forward_maturity_date = std::string(s.ForwardMaturityDate);
    if (s.ForwardSettlementDate)
        result.forward_settlement_date = std::string(*s.ForwardSettlementDate);
    if (s.Settlement)
        result.settlement = std::string(*s.Settlement);
    if (s.Amount)
        result.amount = static_cast<double>(*s.Amount);
    if (s.LockRate)
        result.lock_rate = static_cast<double>(*s.LockRate);
    if (s.dv01)
        result.dv01 = static_cast<double>(*s.dv01);
    if (s.LockRateDayCounter)
        result.lock_rate_day_counter = std::string(*s.LockRateDayCounter);
    if (s.SettlementDirty)
        result.settlement_dirty = std::string(*s.SettlementDirty);
    return result;
}

settlementData reverse_forward_settlement(const bond_forward_settlement& row) {
    settlementData result;
    set_text(result.ForwardMaturityDate, row.forward_maturity_date);
    set_present_text(result.ForwardSettlementDate, row.forward_settlement_date);
    set_present_text(result.Settlement, row.settlement);
    if (row.amount)
        result.Amount = static_cast<float>(*row.amount);
    if (row.lock_rate)
        result.LockRate = static_cast<float>(*row.lock_rate);
    if (row.dv01)
        result.dv01 = static_cast<float>(*row.dv01);
    set_present_text(result.LockRateDayCounter, row.lock_rate_day_counter);
    set_present_text(result.SettlementDirty, row.settlement_dirty);
    return result;
}

bond_forward_premium map_forward_premium(const forwardBondData_PremiumData_t& p) {
    bond_forward_premium result;
    result.amount = std::string(p.Amount);
    result.date = std::string(p.Date);
    return result;
}

forwardBondData_PremiumData_t reverse_forward_premium(const bond_forward_premium& row) {
    forwardBondData_PremiumData_t result;
    set_text(result.Amount, row.amount);
    set_text(result.Date, row.date);
    return result;
}

bond_settlement_data map_settlement(const legData_SettlementData_t& s) {
    bond_settlement_data result;
    result.fx_index = std::string(s.FXIndex);
    if (s.FixingDate)
        result.fixing_date = std::string(*s.FixingDate);
    return result;
}

legData_SettlementData_t reverse_settlement(const bond_settlement_data& s) {
    legData_SettlementData_t result;
    static_cast<std::string&>(result.FXIndex) = s.fx_index;
    set_present_text(result.FixingDate, s.fixing_date);
    return result;
}

bond_floating_leg_data map_floating_leg(const _FloatingLegData_t& f) {
    bond_floating_leg_data result;
    result.index = std::string(f.Index);
    if (f.IsInArrears)
        result.is_in_arrears = *f.IsInArrears;
    if (f.LastRecentPeriod)
        result.last_recent_period = std::string(*f.LastRecentPeriod);
    if (f.LastRecentPeriodCalendar)
        result.last_recent_period_calendar = std::string(*f.LastRecentPeriodCalendar);
    if (f.FixingDays)
        result.fixing_days = *f.FixingDays;
    if (f.Lookback)
        result.lookback = std::string(*f.Lookback);
    if (f.RateCutoff)
        result.rate_cutoff = *f.RateCutoff;
    if (f.IsAveraged)
        result.is_averaged = *f.IsAveraged;
    if (f.HasSubPeriods)
        result.has_sub_periods = *f.HasSubPeriods;
    if (f.IncludeSpread)
        result.include_spread = *f.IncludeSpread;
    if (f.IsNotResettingXCCY)
        result.is_not_resetting_xccy = *f.IsNotResettingXCCY;
    if (f.Spreads)
        for (const auto& s : f.Spreads->Spread)
            result.spreads.push_back(map_number(s));
    if (f.Caps)
        for (const auto& c : f.Caps->Cap)
            result.caps.push_back(map_number(c));
    if (f.Floors)
        for (const auto& c : f.Floors->Floor)
            result.floors.push_back(map_number(c));
    if (f.Gearings)
        for (const auto& g : f.Gearings->Gearing)
            result.gearings.push_back(map_number(g));
    if (f.NakedOption)
        result.naked_option = *f.NakedOption;
    if (f.LocalCapFloor)
        result.local_cap_floor = *f.LocalCapFloor;
    if (f.FixingSchedule)
        result.fixing_schedule = map_schedule(*f.FixingSchedule);
    if (f.ResetSchedule)
        result.reset_schedule = map_schedule(*f.ResetSchedule);
    if (f.FrontStubInterpolation)
        result.front_stub_interpolation = map_stub(*f.FrontStubInterpolation);
    if (f.BackStubInterpolation)
        result.back_stub_interpolation = map_stub(*f.BackStubInterpolation);
    if (f.StubUseOriginalCurve)
        result.stub_use_original_curve = *f.StubUseOriginalCurve;
    if (f.ObservationShift)
        result.observation_shift = *f.ObservationShift;
    return result;
}

_FloatingLegData_t reverse_floating_leg(const bond_floating_leg_data& leg) {
    _FloatingLegData_t result;
    static_cast<std::string&>(result.Index) = leg.index;
    if (leg.is_in_arrears)
        result.IsInArrears = *leg.is_in_arrears;
    set_present_text(result.LastRecentPeriod, leg.last_recent_period);
    set_present_text(result.LastRecentPeriodCalendar, leg.last_recent_period_calendar);
    if (leg.fixing_days)
        result.FixingDays = *leg.fixing_days;
    set_present_text(result.Lookback, leg.lookback);
    if (leg.rate_cutoff)
        result.RateCutoff = *leg.rate_cutoff;
    if (leg.is_averaged)
        result.IsAveraged = *leg.is_averaged;
    if (leg.has_sub_periods)
        result.HasSubPeriods = *leg.has_sub_periods;
    if (leg.include_spread)
        result.IncludeSpread = *leg.include_spread;
    if (leg.is_not_resetting_xccy)
        result.IsNotResettingXCCY = *leg.is_not_resetting_xccy;
    if (!leg.spreads.empty()) {
        spreads group;
        group.Spread = reverse_numbers<floatWithAttribute>(leg.spreads);
        result.Spreads = std::move(group);
    }
    if (!leg.caps.empty()) {
        caps group;
        group.Cap = reverse_numbers<floatWithAttribute>(leg.caps);
        result.Caps = std::move(group);
    }
    if (!leg.floors.empty()) {
        floors group;
        group.Floor = reverse_numbers<floatWithAttribute>(leg.floors);
        result.Floors = std::move(group);
    }
    if (!leg.gearings.empty()) {
        gearings group;
        group.Gearing = reverse_numbers<floatWithAttribute>(leg.gearings);
        result.Gearings = std::move(group);
    }
    if (leg.naked_option)
        result.NakedOption = *leg.naked_option;
    if (leg.local_cap_floor)
        result.LocalCapFloor = *leg.local_cap_floor;
    if (!leg.fixing_schedule.rules.empty() || !leg.fixing_schedule.dates.empty())
        result.FixingSchedule = reverse_schedule(leg.fixing_schedule);
    if (!leg.reset_schedule.rules.empty() || !leg.reset_schedule.dates.empty())
        result.ResetSchedule = reverse_schedule(leg.reset_schedule);
    if (leg.front_stub_interpolation)
        result.FrontStubInterpolation = reverse_stub(*leg.front_stub_interpolation);
    if (leg.back_stub_interpolation)
        result.BackStubInterpolation = reverse_stub(*leg.back_stub_interpolation);
    if (leg.stub_use_original_curve)
        result.StubUseOriginalCurve = *leg.stub_use_original_curve;
    if (leg.observation_shift)
        result.ObservationShift = *leg.observation_shift;
    return result;
}

bond_fixed_leg_data map_fixed_leg(const _FixedLegData_t& f) {
    bond_fixed_leg_data result;
    for (const auto& r : f.Rates.Rate)
        result.rates.push_back(map_number(r));
    return result;
}

_FixedLegData_t reverse_fixed_leg(const bond_fixed_leg_data& leg) {
    _FixedLegData_t result;
    result.Rates.Rate = reverse_numbers<_FixedLegData_t_Rates_t_Rate_t>(leg.rates);
    return result;
}

bond_formula_based_leg_data map_formula_leg(const _FormulaBasedLegData_t& f) {
    bond_formula_based_leg_data result;
    result.index = std::string(f.Index);
    if (f.IsInArrears)
        result.is_in_arrears = *f.IsInArrears;
    result.fixing_days = f.FixingDays;
    if (f.FixingCalendar)
        result.fixing_calendar = std::string(*f.FixingCalendar);
    return result;
}

_FormulaBasedLegData_t reverse_formula_leg(const bond_formula_based_leg_data& leg) {
    _FormulaBasedLegData_t result;
    static_cast<std::string&>(result.Index) = leg.index;
    if (leg.is_in_arrears)
        result.IsInArrears = *leg.is_in_arrears;
    result.FixingDays = leg.fixing_days;
    set_present_text(result.FixingCalendar, leg.fixing_calendar);
    return result;
}

// legDataType is a substitution group of eighteen members. Three of them
// have a container here; a document that states one of the other fifteen
// leaves the group present with no member set, and the reverse mapper then
// writes an empty legDataType element.
bond_leg_rate_data map_rate_group(const legDataType_group_t& g) {
    bond_leg_rate_data result;
    if (g.FixedLegData)
        result.fixed = map_fixed_leg(*g.FixedLegData);
    if (g.FloatingLegData)
        result.floating = map_floating_leg(*g.FloatingLegData);
    if (g.FormulaBasedLegData)
        result.formula_based = map_formula_leg(*g.FormulaBasedLegData);
    return result;
}

legDataType_group_t reverse_rate_group(const bond_leg_rate_data& r) {
    legDataType_group_t result;
    if (r.fixed)
        result.FixedLegData = reverse_fixed_leg(*r.fixed);
    if (r.floating)
        result.FloatingLegData = reverse_floating_leg(*r.floating);
    if (r.formula_based)
        result.FormulaBasedLegData = reverse_formula_leg(*r.formula_based);
    return result;
}

bond_leg_data map_leg(const legData& ld) {
    bond_leg_data result;
    result.payer = ld.Payer;
    result.leg_type = to_string(ld.LegType);
    if (ld.Currency)
        result.currency = std::string(*ld.Currency);
    if (ld.PaymentConvention)
        result.payment_convention = to_string(*ld.PaymentConvention);
    if (ld.PaymentLag)
        result.payment_lag = std::string(*ld.PaymentLag);
    if (ld.PaymentCalendar)
        result.payment_calendar = std::string(*ld.PaymentCalendar);
    if (ld.DayCounter)
        result.day_counter = to_string(*ld.DayCounter);
    if (ld.LastPeriodDayCounter)
        result.last_period_day_counter = to_string(*ld.LastPeriodDayCounter);
    if (ld.NotionalPaymentLag)
        result.notional_payment_lag = *ld.NotionalPaymentLag;
    if (ld.StrictNotionalDates)
        result.strict_notional_dates = *ld.StrictNotionalDates;
    if (ld.ScheduleData)
        result.schedule = map_schedule(*ld.ScheduleData);
    if (ld.Amortizations)
        for (const auto& a : ld.Amortizations->AmortizationData)
            result.amortizations.push_back(map_amortization(a));
    if (ld.Notionals)
        for (const auto& n : ld.Notionals->Notional)
            result.notionals.push_back(map_number(n));
    if (ld.PaymentDates)
        for (const auto& d : ld.PaymentDates->PaymentDate)
            result.payment_dates.push_back(d);
    if (ld.Indexings && ld.Indexings->FromAssetLeg)
        result.indexings_from_asset_leg = *ld.Indexings->FromAssetLeg;
    if (ld.legDataType)
        result.rate = map_rate_group(*ld.legDataType);
    if (ld.PaymentSchedule)
        result.payment_schedule = map_schedule(*ld.PaymentSchedule);
    if (ld.SettlementData)
        result.settlement = map_settlement(*ld.SettlementData);
    return result;
}

// Every scalar the container holds is written back, so a leg the
// document stated comes back whole. The caller supplies the two members
// the issue row also mirrors when the leg is silent, which is the case
// for a payload built from a row set.
void reverse_leg(const bond_leg_data& leg, legData& ld) {
    ld.Payer = leg.payer.value_or(false);
    ld.LegType = parse_code(leg.leg_type.value_or(std::string()), leg_type_count, legType::Fixed);
    if (leg.currency)
        ld.Currency = *leg.currency;
    if (leg.payment_convention)
        ld.PaymentConvention = parse_code(
            *leg.payment_convention, business_day_convention_count, businessDayConvention::F);
    if (leg.payment_lag)
        ld.PaymentLag = *leg.payment_lag;
    if (leg.payment_calendar) {
        legData_PaymentCalendar_t calendar;
        static_cast<std::string&>(calendar) = *leg.payment_calendar;
        ld.PaymentCalendar = std::move(calendar);
    }
    if (leg.day_counter)
        ld.DayCounter = parse_code(*leg.day_counter, day_counter_count, dayCounter::A360);
    if (leg.last_period_day_counter)
        ld.LastPeriodDayCounter =
            parse_code(*leg.last_period_day_counter, day_counter_count, dayCounter::A360);
    if (leg.notional_payment_lag)
        ld.NotionalPaymentLag = *leg.notional_payment_lag;
    if (leg.strict_notional_dates)
        ld.StrictNotionalDates = *leg.strict_notional_dates;
    if (!leg.schedule.rules.empty() || !leg.schedule.dates.empty())
        ld.ScheduleData = reverse_schedule(leg.schedule);
    if (!leg.amortizations.empty()) {
        legData_Amortizations_t group;
        for (const auto& row : leg.amortizations)
            group.AmortizationData.push_back(reverse_amortization(row));
        ld.Amortizations = std::move(group);
    }
    if (!leg.notionals.empty()) {
        legData_Notionals_t group;
        group.Notional = reverse_numbers<legData_Notionals_t_Notional_t>(leg.notionals);
        ld.Notionals = std::move(group);
    }
    if (!leg.payment_dates.empty()) {
        legData_PaymentDates_t group;
        for (const auto& d : leg.payment_dates)
            group.PaymentDate.push_back(d);
        ld.PaymentDates = std::move(group);
    }
    if (leg.indexings_from_asset_leg) {
        legData_Indexings_t group;
        group.FromAssetLeg = *leg.indexings_from_asset_leg;
        ld.Indexings = std::move(group);
    }
    if (leg.rate)
        ld.legDataType = reverse_rate_group(*leg.rate);
    if (!leg.payment_schedule.rules.empty() || !leg.payment_schedule.dates.empty())
        ld.PaymentSchedule = reverse_schedule(leg.payment_schedule);
    if (leg.settlement)
        ld.SettlementData = reverse_settlement(*leg.settlement);
}

// The schema spells the settlement block twice, once under the option
// and once under each premium, and the two copies carry the same three
// members under different wrapper names.
template <typename Settlement>
bond_option_settlement map_option_settlement(const Settlement& s) {
    bond_option_settlement result;
    result.pay_currency = to_string(s.PayCurrency);
    result.fx_index = s.FXIndex;
    if (s.FixingDate)
        result.fixing_date = std::string(*s.FixingDate);
    return result;
}

template <typename Settlement>
Settlement reverse_option_settlement(const bond_option_settlement& row) {
    Settlement result;
    result.PayCurrency = parse_code(row.pay_currency, currency_code_count, currencyCode::USD);
    set_text(result.FXIndex, row.fx_index);
    set_present_text(result.FixingDate, row.fixing_date);
    return result;
}

bond_option_premium map_option_premium(const premiumData_Premium_t& p) {
    bond_option_premium result;
    result.amount = static_cast<double>(p.Amount);
    result.currency = p.Currency;
    result.pay_date = p.PayDate;
    if (p.SettlementData)
        result.settlement = map_option_settlement(*p.SettlementData);
    return result;
}

premiumData_Premium_t reverse_option_premium(const bond_option_premium& row) {
    premiumData_Premium_t result;
    result.Amount = static_cast<float>(row.amount);
    set_text(result.Currency, row.currency);
    set_text(result.PayDate, row.pay_date);
    if (row.settlement)
        result.SettlementData =
            reverse_option_settlement<premiumData_Premium_t_SettlementData_t>(*row.settlement);
    return result;
}

bond_option_exercise_fee map_option_exercise_fee(const optionData_ExerciseFees_t_ExerciseFee_t& f) {
    bond_option_exercise_fee result;
    result.amount = static_cast<double>(f);
    if (f.type)
        result.type = std::string(*f.type);
    if (f.startDate)
        result.start_date = std::string(*f.startDate);
    if (f.currency)
        result.currency = std::string(*f.currency);
    return result;
}

optionData_ExerciseFees_t_ExerciseFee_t
reverse_option_exercise_fee(const bond_option_exercise_fee& row) {
    optionData_ExerciseFees_t_ExerciseFee_t result;
    static_cast<float&>(result) = static_cast<float>(row.amount);
    if (row.type)
        result.type = *row.type;
    if (row.start_date)
        result.startDate = *row.start_date;
    if (row.currency)
        result.currency = *row.currency;
    return result;
}

bond_option_exercise map_option_exercise(const optionExerciseData& e) {
    bond_option_exercise result;
    result.date = e.Date;
    if (e.Price)
        result.price = *e.Price;
    return result;
}

optionExerciseData reverse_option_exercise(const bond_option_exercise& row) {
    optionExerciseData result;
    set_text(result.Date, row.date);
    if (row.price)
        result.Price = *row.price;
    return result;
}

bond_option_payment_rules map_option_payment_rules(const optionPaymentData_Rules_t& r) {
    bond_option_payment_rules result;
    result.lag = r.Lag;
    result.calendar = r.Calendar;
    result.convention = to_string(r.Convention);
    if (r.RelativeTo)
        result.relative_to = to_string(*r.RelativeTo);
    return result;
}

optionPaymentData_Rules_t reverse_option_payment_rules(const bond_option_payment_rules& row) {
    optionPaymentData_Rules_t result;
    result.Lag = row.lag;
    set_text(result.Calendar, row.calendar);
    result.Convention =
        parse_code(row.convention, business_day_convention_count, businessDayConvention::F);
    if (row.relative_to)
        result.RelativeTo = parse_code(*row.relative_to,
                                       option_pay_relative_to_count,
                                       optionPayRelativeTo::Expiry);
    return result;
}

bond_option_payment_data map_option_payment_data(const optionPaymentData& p) {
    bond_option_payment_data result;
    if (p.Dates)
        for (const auto& date : p.Dates->Date)
            result.dates.push_back(date);
    if (p.Rules)
        result.rules = map_option_payment_rules(*p.Rules);
    return result;
}

optionPaymentData reverse_option_payment_data(const bond_option_payment_data& row) {
    optionPaymentData result;
    if (!row.dates.empty()) {
        optionPaymentData_Dates_t dates;
        for (const auto& date : row.dates)
            dates.Date.push_back(date);
        result.Dates = std::move(dates);
    }
    if (row.rules)
        result.Rules = reverse_option_payment_rules(*row.rules);
    return result;
}

bond_option_data map_option_data(const optionData& od) {
    bond_option_data result;
    result.long_short = od.LongShort;
    if (od.OptionType)
        result.option_type = std::string(*od.OptionType);
    if (od.PayoffType)
        result.payoff_type = std::string(*od.PayoffType);
    if (od.PayoffType2)
        result.payoff_type_2 = std::string(*od.PayoffType2);
    if (od.Style)
        result.style = std::string(*od.Style);
    if (od.NoticePeriod)
        result.notice_period = std::string(*od.NoticePeriod);
    if (od.NoticeCalendar)
        result.notice_calendar = std::string(*od.NoticeCalendar);
    if (od.NoticeConvention)
        result.notice_convention = std::string(*od.NoticeConvention);
    if (od.MidCouponExercise)
        result.mid_coupon_exercise = std::string(*od.MidCouponExercise);
    if (od.Settlement)
        result.settlement = to_string(*od.Settlement);
    if (od.SettlementMethod)
        result.settlement_method = to_string(*od.SettlementMethod);
    if (od.PayOffAtExpiry)
        result.pay_off_at_expiry = std::string(*od.PayOffAtExpiry);
    if (od.PremiumAmount)
        result.premium_amount = std::string(*od.PremiumAmount);
    if (od.PremiumCurrency)
        result.premium_currency = std::string(*od.PremiumCurrency);
    if (od.PremiumPayDate)
        result.premium_pay_date = std::string(*od.PremiumPayDate);
    if (od.Premiums)
        for (const auto& p : od.Premiums->Premium)
            result.premiums.push_back(map_option_premium(p));
    if (od.ExercisePrices)
        result.exercise_prices = std::string(*od.ExercisePrices);
    if (od.ExerciseFees)
        for (const auto& f : od.ExerciseFees->ExerciseFee)
            result.exercise_fees.push_back(map_option_exercise_fee(f));
    if (od.ExerciseFeeSettlementPeriod)
        result.exercise_fee_settlement_period = std::string(*od.ExerciseFeeSettlementPeriod);
    if (od.ExerciseFeeSettlementCalendar)
        result.exercise_fee_settlement_calendar = std::string(*od.ExerciseFeeSettlementCalendar);
    if (od.ExerciseFeeSettlementConvention)
        result.exercise_fee_settlement_convention =
            std::string(*od.ExerciseFeeSettlementConvention);
    if (od.AutomaticExercise)
        result.automatic_exercise = to_string(*od.AutomaticExercise);
    if (od.ExerciseData)
        result.exercise_data = map_option_exercise(*od.ExerciseData);
    if (od.PaymentData)
        result.payment_data = map_option_payment_data(*od.PaymentData);
    if (od.SettlementData)
        result.settlement_data = map_option_settlement(*od.SettlementData);
    return result;
}

void reverse_option_data(const bond_option_data& row, optionData& od) {
    set_text(od.LongShort, row.long_short);
    set_present_text(od.OptionType, row.option_type);
    set_present_text(od.PayoffType, row.payoff_type);
    set_present_text(od.PayoffType2, row.payoff_type_2);
    set_present_text(od.Style, row.style);
    set_present_text(od.NoticePeriod, row.notice_period);
    set_present_text(od.NoticeCalendar, row.notice_calendar);
    set_present_text(od.NoticeConvention, row.notice_convention);
    set_present_text(od.MidCouponExercise, row.mid_coupon_exercise);
    if (row.settlement)
        od.Settlement = parse_code(*row.settlement, settlement_type_count, settlementType::Cash);
    if (row.settlement_method)
        od.SettlementMethod = parse_code(*row.settlement_method,
                                         settlement_method_count,
                                         settlementMethod::PhysicalOTC);
    set_present_text(od.PayOffAtExpiry, row.pay_off_at_expiry);
    set_present_text(od.PremiumAmount, row.premium_amount);
    set_present_text(od.PremiumCurrency, row.premium_currency);
    set_present_text(od.PremiumPayDate, row.premium_pay_date);
    if (!row.premiums.empty()) {
        premiumData premiums;
        for (const auto& p : row.premiums)
            premiums.Premium.push_back(reverse_option_premium(p));
        od.Premiums = std::move(premiums);
    }
    set_present_text(od.ExercisePrices, row.exercise_prices);
    if (!row.exercise_fees.empty()) {
        optionData_ExerciseFees_t fees;
        for (const auto& f : row.exercise_fees)
            fees.ExerciseFee.push_back(reverse_option_exercise_fee(f));
        od.ExerciseFees = std::move(fees);
    }
    set_present_text(od.ExerciseFeeSettlementPeriod, row.exercise_fee_settlement_period);
    set_present_text(od.ExerciseFeeSettlementCalendar, row.exercise_fee_settlement_calendar);
    set_present_text(od.ExerciseFeeSettlementConvention, row.exercise_fee_settlement_convention);
    if (row.automatic_exercise)
        od.AutomaticExercise =
            parse_code(*row.automatic_exercise, bool__count, bool_::N);
    if (row.exercise_data)
        od.ExerciseData = reverse_option_exercise(*row.exercise_data);
    if (row.payment_data)
        od.PaymentData = reverse_option_payment_data(*row.payment_data);
    if (row.settlement_data)
        od.SettlementData =
            reverse_option_settlement<optionData_SettlementData_t>(*row.settlement_data);
}

bond_strike_data map_strike_data(const _StrikeData_t& sd) {
    bond_strike_data result;
    if (sd.StrikePrice) {
        result.price_value = static_cast<double>(sd.StrikePrice->Value);
        if (sd.StrikePrice->Currency)
            result.price_currency = std::string(*sd.StrikePrice->Currency);
    }
    if (sd.StrikeYield) {
        result.yield_value = static_cast<double>(sd.StrikeYield->Yield);
        if (sd.StrikeYield->Compounding)
            result.yield_compounding = to_string(*sd.StrikeYield->Compounding);
    }
    if (sd.Value)
        result.bare_value = static_cast<double>(*sd.Value);
    if (sd.Currency)
        result.bare_currency = std::string(*sd.Currency);
    return result;
}

_StrikeData_t reverse_strike_data(const bond_strike_data& row) {
    _StrikeData_t result;
    if (row.price_value || row.price_currency) {
        strikePriceData price;
        if (row.price_value)
            price.Value = static_cast<float>(*row.price_value);
        if (row.price_currency)
            price.Currency = *row.price_currency;
        result.StrikePrice = std::move(price);
    }
    if (row.yield_value || row.yield_compounding) {
        strikeYieldData yield;
        if (row.yield_value)
            yield.Yield = static_cast<float>(*row.yield_value);
        if (row.yield_compounding)
            yield.Compounding =
                parse_code(*row.yield_compounding, compounding_count, compounding::Compounded);
        result.StrikeYield = std::move(yield);
    }
    if (row.bare_value)
        result.Value = static_cast<float>(*row.bare_value);
    if (row.bare_currency)
        result.Currency = *row.bare_currency;
    return result;
}

void map_exercise_dates(const optionData& od, std::vector<std::string>& dates) {
    if (od.exerciseDatesGroup && od.exerciseDatesGroup->ExerciseDates)
        for (const auto& date : od.exerciseDatesGroup->ExerciseDates->ExerciseDate)
            dates.push_back(date);
}

std::optional<bond_schedule_data> map_exercise_schedule(const optionData& od) {
    if (od.exerciseDatesGroup && od.exerciseDatesGroup->ExerciseSchedule)
        return map_schedule(*od.exerciseDatesGroup->ExerciseSchedule);
    return {};
}

// The group is a choice between a date list and a schedule, so one writer
// covers both alternatives.
void reverse_exercise_dates(const std::vector<std::string>& dates,
                            const std::optional<bond_schedule_data>& schedule,
                            optionData& od) {
    if (!dates.empty()) {
        _ExerciseDates_t exd;
        for (const auto& value : dates)
            exd.ExerciseDate.push_back(value);
        exerciseDatesGroup_group_t eg;
        eg.ExerciseDates = std::move(exd);
        od.exerciseDatesGroup = std::move(eg);
    } else if (schedule) {
        exerciseDatesGroup_group_t eg;
        eg.ExerciseSchedule = reverse_schedule(*schedule);
        od.exerciseDatesGroup = std::move(eg);
    }
}

} // namespace

void bond_instrument_mapper::map_bond_data(const bondData& bd, bond_instrument_data& data) {
    auto& issue = data.issue;
    issue.security_id = std::string(bd.SecurityId);
    if (bd.IssuerId)
        issue.issuer = std::string(*bd.IssuerId);
    if (bd.IssueDate)
        issue.issue_date = std::string(*bd.IssueDate);
    if (bd.SettlementDays)
        issue.settlement_days = count_of(*bd.SettlementDays, 0);

    if (bd.Calendar)
        issue.calendar = std::string(*bd.Calendar);
    if (bd.CreditCurveId)
        issue.credit_curve_id = std::string(*bd.CreditCurveId);
    if (bd.ReferenceCurveId)
        issue.reference_curve_id = std::string(*bd.ReferenceCurveId);
    if (bd.IncomeCurveId)
        issue.income_curve_id = std::string(*bd.IncomeCurveId);
    if (bd.BondNotional)
        issue.bond_notional = std::string(*bd.BondNotional);

    // Every leg the document states is carried, in document order.
    for (const auto& ld : bd.LegData)
        data.bond_legs.push_back(map_leg(ld));

    // The issue row mirrors the first leg: one row cannot hold two
    // coupons, and the row is the fallback for a payload that came from a
    // row set rather than from the document's own statement.
    if (!bd.LegData.empty()) {
        const auto& ld = bd.LegData.front();
        if (ld.Currency)
            issue.currency = std::string(*ld.Currency);
        if (ld.Notionals && !ld.Notionals->Notional.empty())
            issue.face_value = static_cast<double>(ld.Notionals->Notional.front());
        if (ld.DayCounter)
            issue.day_count_code = to_string(*ld.DayCounter);
        issue.coupon_frequency_code = first_tenor(ld.ScheduleData);

        if (ld.legDataType && ld.legDataType->FixedLegData &&
            !ld.legDataType->FixedLegData->Rates.Rate.empty())
            issue.coupon_rate =
                static_cast<double>(ld.legDataType->FixedLegData->Rates.Rate.front());

    }
}

bondData bond_instrument_mapper::reverse_bond_data(const bond_instrument_data& data) {
    const auto& issue = data.issue;
    bondData bd;

    static_cast<std::string&>(bd.SecurityId) = issue.security_id;
    if (!issue.issuer.empty()) {
        bondData_IssuerId_t id;
        static_cast<std::string&>(id) = issue.issuer;
        bd.IssuerId = std::move(id);
    }
    if (!issue.issue_date.empty()) {
        bondData_IssueDate_t d;
        static_cast<std::string&>(d) = issue.issue_date;
        bd.IssueDate = std::move(d);
    }
    if (issue.settlement_days != 0) {
        bondData_SettlementDays_t sd;
        static_cast<std::string&>(sd) = std::to_string(issue.settlement_days);
        bd.SettlementDays = std::move(sd);
    }

    set_present_text(bd.Calendar, issue.calendar);
    set_present_text(bd.CreditCurveId, issue.credit_curve_id);
    set_present_text(bd.ReferenceCurveId, issue.reference_curve_id);
    set_present_text(bd.IncomeCurveId, issue.income_curve_id);
    set_present_text(bd.BondNotional, issue.bond_notional);

    // The legs are emitted when the document held any, whether or not the
    // columns that mirror the first hold a value: Currency and Notionals
    // are optional in the schema, so a leg the document carries with
    // neither still has to come back out.
    if (!data.bond_legs.empty() || !issue.currency.empty() || issue.face_value != 0.0) {
        const std::size_t leg_count = data.bond_legs.empty() ? 1 : data.bond_legs.size();
        for (std::size_t i = 0; i < leg_count; ++i) {
            const bond_leg_data absent;
            const bond_leg_data& source = data.bond_legs.empty() ? absent : data.bond_legs[i];
            legData ld;
            reverse_leg(source, ld);

            // The issue row mirrors the first leg's currency, day counter,
            // notional, rate and schedule, so it supplies them only when the
            // container came from a row set. A document's own statement is
            // already on the leg, and a later leg has no row at all.
            if (i == 0) {
                if (!ld.Currency && !issue.currency.empty())
                    ld.Currency = issue.currency;
                if (!ld.DayCounter && !issue.day_count_code.empty())
                    ld.DayCounter = parse_code(issue.day_count_code,
                                               day_counter_count,
                                               dayCounter::A360);

                if (!ld.Notionals && issue.face_value != 0.0) {
                    legData_Notionals_t n;
                    legData_Notionals_t_Notional_t nv;
                    static_cast<float&>(nv) = static_cast<float>(issue.face_value);
                    n.Notional.push_back(nv);
                    ld.Notionals = std::move(n);
                }

                // A container that came from a document carries the leg
                // whole, so this rebuild runs only for a row set with no
                // remainder: the issue terms are then the whole of what is
                // known. The schedule's end date is not among them, because
                // the issue row does not hold it; it lives in the schedule
                // table and is read from there.
                if (!ld.ScheduleData && !issue.coupon_frequency_code.empty()) {
                    scheduleData_Rules_t rule;
                    static_cast<std::string&>(rule.Tenor) = issue.coupon_frequency_code;
                    if (!issue.issue_date.empty())
                        rule.StartDate = issue.issue_date;
                    scheduleData sched;
                    sched.Rules.push_back(std::move(rule));
                    ld.ScheduleData = std::move(sched);
                }

                if (!ld.legDataType && issue.coupon_rate != 0.0) {
                    _FixedLegData_t fld;
                    _FixedLegData_t_Rates_t_Rate_t rate;
                    static_cast<float&>(rate) = static_cast<float>(issue.coupon_rate);
                    fld.Rates.Rate.push_back(rate);
                    legDataType_group_t ldt;
                    ldt.FixedLegData = std::move(fld);
                    ld.legDataType = std::move(ldt);
                }
            }

            bd.LegData.push_back(std::move(ld));
        }
    }

    return bd;
}

// The call and conversion structures exist in two forms, and only one
// of them has a row shape. An explicit date list or ratio list maps one
// row per entry; a rule-based schedule stays in the document, because
// expanding a tenor needs a calendar and the parent story's schedule
// tables hold the rules rather than the expanded dates.
void bond_instrument_mapper::map_call_dates(
    const callableBondCallData& call_data,
    boost::uuids::uuid issue_id,
    std::vector<bond_issue_call_date>& dates) {
    int sequence = 0;
    for (const auto& block : call_data.ScheduleData.Dates)
        for (const auto& d : block.Dates.Date) {
            bond_issue_call_date row;
            row.issue_id = issue_id;
            row.sequence_number = ++sequence;
            row.call_date = static_cast<const std::string&>(d);
            stamp_audit(row);
            dates.push_back(std::move(row));
        }
}

void bond_instrument_mapper::reverse_call_dates(
    const std::vector<bond_issue_call_date>& dates,
    callableBondCallData& call_data) {
    if (dates.empty())
        return;
    scheduleData_Dates_t block;
    for (const auto& row : dates) {
        domain::date d;
        static_cast<std::string&>(d) = row.call_date;
        block.Dates.Date.push_back(std::move(d));
    }
    call_data.ScheduleData.Dates.push_back(std::move(block));
}

void bond_instrument_mapper::map_conversion_targets(
    const cbConversionData& conversion_data,
    boost::uuids::uuid issue_id,
    std::vector<bond_issue_conversion_target>& targets) {
    if (!conversion_data.ConversionRatios)
        return;
    const std::string underlying_id =
        conversion_data.Underlying ? std::string(conversion_data.Underlying->Name) : std::string();
    int sequence = 0;
    for (const auto& ratio : conversion_data.ConversionRatios->ConversionRatio) {
        const auto value = static_cast<double>(ratio);
        if (value <= 0.0)
            continue;
        bond_issue_conversion_target row;
        row.issue_id = issue_id;
        row.sequence_number = ++sequence;
        row.underlying_id = underlying_id;
        row.conversion_ratio = value;
        stamp_audit(row);
        targets.push_back(std::move(row));
    }
}

void bond_instrument_mapper::reverse_conversion_targets(
    const std::vector<bond_issue_conversion_target>& targets,
    cbConversionData& conversion_data) {
    if (targets.empty())
        return;
    cbConversionData_ConversionRatios_t ratios;
    for (const auto& row : targets) {
        cbConversionData_ConversionRatios_t_ConversionRatio_t ratio;
        static_cast<float&>(ratio) = static_cast<float>(row.conversion_ratio);
        ratios.ConversionRatio.push_back(std::move(ratio));
    }
    conversion_data.ConversionRatios = std::move(ratios);
    if (!targets.front().underlying_id.empty()) {
        underlying u;
        static_cast<std::string&>(u.Name) = targets.front().underlying_id;
        conversion_data.Underlying = std::move(u);
    }
}

bond_instrument_data bond_instrument_mapper::forward_bond(const trade& t,
                                                          const bond_issue_lookup& lookup) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping Bond: " << std::string(t.id);
    bond_instrument_data result = make_base("Bond");
    if (t.BondData)
        map_bond_data(*t.BondData, result);
    resolve_issue(result, lookup);
    return result;
}

bond_instrument_data bond_instrument_mapper::forward_forward_bond(
    const trade& t, const bond_issue_lookup& lookup) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping ForwardBond: " << std::string(t.id);
    bond_instrument_data result = make_base("ForwardBond");
    if (t.ForwardBondData) {
        const auto& d = *t.ForwardBondData;
        map_bond_data(d.BondData, result);
        result.forward_long_in_forward = std::string(d.LongInForward);
        result.forward_settlement = map_forward_settlement(d.SettlementData);
        if (d.PremiumData)
            result.forward_premium = map_forward_premium(*d.PremiumData);
    }
    resolve_issue(result, lookup);
    return result;
}

bond_instrument_data bond_instrument_mapper::forward_callable_bond(
    const trade& t, const bond_issue_lookup& lookup) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping CallableBond: " << std::string(t.id);
    bond_instrument_data result = make_base("CallableBond");
    if (t.CallableBondData) {
        const auto& d = *t.CallableBondData;
        map_bond_data(d.BondData, result);
        resolve_issue(result, lookup);
        if (d.CallData)
            map_call_dates(*d.CallData, result.issue.issue_id, result.call_dates);
        return result;
    }
    resolve_issue(result, lookup);
    return result;
}

bond_instrument_data bond_instrument_mapper::forward_convertible_bond(
    const trade& t, const bond_issue_lookup& lookup) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping ConvertibleBond: " << std::string(t.id);
    bond_instrument_data result = make_base("ConvertibleBond");
    if (t.ConvertibleBondData) {
        const auto& d = *t.ConvertibleBondData;
        map_bond_data(d.BondData, result);
        resolve_issue(result, lookup);
        if (d.ConversionData)
            map_conversion_targets(
                *d.ConversionData, result.issue.issue_id, result.conversion_targets);
        return result;
    }
    resolve_issue(result, lookup);
    return result;
}

bond_instrument_data bond_instrument_mapper::forward_bond_option(
    const trade& t, const bond_issue_lookup& lookup) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping BondOption: " << std::string(t.id);
    bond_instrument_data result = make_base("BondOption");
    if (!t.BondOptionData) {
        resolve_issue(result, lookup);
        return result;
    }
    const auto& d = *t.BondOptionData;

    map_bond_data(d.BondData, result);
    resolve_issue(result, lookup);

    bond_option option;
    if (d.OptionData.OptionType)
        option.option_type = std::string(*d.OptionData.OptionType);
    if (d.strikeGroup.Strike)
        option.option_strike = number_of(*d.strikeGroup.Strike, 0.0);
    stamp_audit(option);
    result.option = option;

    map_exercise_dates(d.OptionData, result.option_exercise_dates);
    result.option_exercise_schedule = map_exercise_schedule(d.OptionData);
    result.option_data = map_option_data(d.OptionData);
    if (d.strikeGroup.StrikeData)
        result.strike_data = map_strike_data(*d.strikeGroup.StrikeData);
    if (d.Redemption)
        result.option_redemption = std::string(*d.Redemption);
    if (d.PriceType)
        result.option_price_type = std::string(*d.PriceType);
    if (d.KnocksOut)
        result.option_knocks_out = to_string(*d.KnocksOut);

    return result;
}

bond_instrument_data bond_instrument_mapper::forward_bond_trs(const trade& t,
                                                              const bond_issue_lookup& lookup) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping BondTRS: " << std::string(t.id);
    bond_instrument_data result = make_base("BondTRS");
    if (!t.BondTRSData) {
        resolve_issue(result, lookup);
        return result;
    }
    const auto& d = *t.BondTRSData;

    map_bond_data(d.BondData, result);
    resolve_issue(result, lookup);

    bond_trs trs;
    // An ORE BondTRS is a total return swap by construction: no field of
    // the schema selects a return type, and PriceType names a
    // price-quoting convention rather than a return type. TotalReturn is
    // the model default the column check admits, with the ORE User Guide
    // as the evidence.
    trs.return_type = "TotalReturn";
    const auto& ld = d.FundingData.LegData;
    if (ld.legDataType) {
        if (ld.legDataType->FloatingLegData) {
            trs.funding_leg_type = "Floating";
            trs.funding_index = std::string(ld.legDataType->FloatingLegData->Index);
        } else if (ld.legDataType->FixedLegData) {
            trs.funding_leg_type = "Fixed";
            if (!ld.legDataType->FixedLegData->Rates.Rate.empty())
                trs.funding_rate =
                    static_cast<double>(ld.legDataType->FixedLegData->Rates.Rate.front());
        }
    }
    stamp_audit(trs);
    result.trs = trs;
    result.trs_funding_leg = map_leg(ld);
    result.trs_price_type = d.TotalReturnData.PriceType;
    result.trs_payer = std::string(d.TotalReturnData.Payer);
    if (d.TotalReturnData.InitialPrice)
        result.trs_initial_price = static_cast<double>(*d.TotalReturnData.InitialPrice);
    result.trs_schedule = map_schedule(d.TotalReturnData.ScheduleData);
    return result;
}

bond_instrument_data bond_instrument_mapper::forward_bond_repo(
    const trade& t, const bond_issue_lookup& lookup) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping BondRepo: " << std::string(t.id);
    bond_instrument_data result = make_base("BondRepo");
    if (!t.BondRepoData) {
        resolve_issue(result, lookup);
        return result;
    }
    const auto& d = *t.BondRepoData;

    map_bond_data(d.BondData, result);
    resolve_issue(result, lookup);

    bond_repo repo;
    repo.repo_type = (d.RepoData.LegData.LegType == legType::Floating) ? "Floating" : "Fixed";
    if (d.RepoData.LegData.legDataType) {
        const auto& rl = d.RepoData.LegData.legDataType;
        if (rl->FixedLegData && !rl->FixedLegData->Rates.Rate.empty())
            repo.repo_rate = static_cast<double>(rl->FixedLegData->Rates.Rate.front());
        else if (rl->FloatingLegData)
            repo.repo_index = std::string(rl->FloatingLegData->Index);
    }
    stamp_audit(repo);
    result.repo = repo;
    result.repo_leg = map_leg(d.RepoData.LegData);
    return result;
}

bond_instrument_data bond_instrument_mapper::forward_bond_future(
    const trade& t, const bond_issue_lookup& lookup) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping BondFuture: " << std::string(t.id);
    bond_instrument_data result = make_base("BondFuture");
    // A future carries no bond terms: the schema's bondFutureData holds
    // no BondData, so the issue row that the instrument's NOT NULL
    // issue_id points at is minted empty and no lookup can match it.
    resolve_issue(result, lookup);
    if (!t.BondFutureData)
        return result;
    const auto& d = *t.BondFutureData;

    bond_future future;
    future.contract_name = d.ContractName;
    future.contract_notional = number_of(d.ContractNotional, 0.0);
    future.long_short = d.LongShort;
    // v17 moved the contract's own terms out of the trade and into the
    // BondFutureReferenceData datum keyed by ContractName: currency,
    // contract month, deliverable grade, settlement and its basis,
    // expiry basis, the two lags, root date, last trading and delivery
    // dates, and the delivery basket. They are properties of the
    // contract rather than of a trade on it, which is why they moved.
    // Nothing reads them here because the reference data document is
    // not yet parsed; the fields stay unset rather than being invented
    // from the trade. FairPrice is not in v17 at all.
    stamp_audit(future);
    result.future = future;
    return result;
}

bond_instrument_data bond_instrument_mapper::forward_ascot(const trade& t,
                                                           const bond_issue_lookup& lookup) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping Ascot: " << std::string(t.id);
    bond_instrument_data result = make_base("Ascot");
    if (!t.AscotData) {
        resolve_issue(result, lookup);
        return result;
    }
    const auto& d = *t.AscotData;

    map_bond_data(d.ConvertibleBondData.BondData, result);
    resolve_issue(result, lookup);
    if (d.ConvertibleBondData.ConversionData)
        map_conversion_targets(
            *d.ConvertibleBondData.ConversionData, result.issue.issue_id, result.conversion_targets);

    ascot row;
    if (d.OptionData.OptionType)
        row.ascot_option_type = *d.OptionData.OptionType;
    stamp_audit(row);
    result.ascot_row = row;

    map_exercise_dates(d.OptionData, result.option_exercise_dates);
    result.option_exercise_schedule = map_exercise_schedule(d.OptionData);
    result.option_data = map_option_data(d.OptionData);
    result.ascot_swap_leg = map_leg(d.ReferenceSwapData.LegData);
    return result;
}

trade bond_instrument_mapper::reverse_bond(const bond_instrument_data& data) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping Bond";
    trade t;
    t.TradeType = oreTradeType::Bond;
    t.BondData = reverse_bond_data(data);
    return t;
}

trade bond_instrument_mapper::reverse_forward_bond(const bond_instrument_data& data) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping ForwardBond";
    trade t;
    t.TradeType = oreTradeType::ForwardBond;
    forwardBondData fbd;
    fbd.BondData = reverse_bond_data(data);
    if (data.forward_long_in_forward)
        set_text(fbd.LongInForward, *data.forward_long_in_forward);
    if (data.forward_settlement)
        fbd.SettlementData = reverse_forward_settlement(*data.forward_settlement);
    if (data.forward_premium)
        fbd.PremiumData = reverse_forward_premium(*data.forward_premium);
    t.ForwardBondData = std::move(fbd);
    return t;
}

trade bond_instrument_mapper::reverse_callable_bond(const bond_instrument_data& data) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping CallableBond";
    trade t;
    t.TradeType = oreTradeType::CallableBond;
    callableBondData cbd;
    cbd.BondData = reverse_bond_data(data);
    if (!data.call_dates.empty()) {
        callableBondCallData call_data;
        reverse_call_dates(data.call_dates, call_data);
        cbd.CallData = std::move(call_data);
    }
    t.CallableBondData = std::move(cbd);
    return t;
}

trade bond_instrument_mapper::reverse_convertible_bond(const bond_instrument_data& data) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping ConvertibleBond";
    trade t;
    t.TradeType = oreTradeType::ConvertibleBond;
    convertibleBondData cvbd;
    cvbd.BondData = reverse_bond_data(data);
    if (!data.conversion_targets.empty()) {
        cbConversionData conversion_data;
        reverse_conversion_targets(data.conversion_targets, conversion_data);
        cvbd.ConversionData = std::move(conversion_data);
    }
    t.ConvertibleBondData = std::move(cvbd);
    return t;
}

trade bond_instrument_mapper::reverse_bond_option(const bond_instrument_data& data) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping BondOption";
    trade t;
    t.TradeType = oreTradeType::BondOption;
    bondOptionData d;
    d.BondData = reverse_bond_data(data);
    if (data.option_data)
        reverse_option_data(*data.option_data, d.OptionData);
    if (data.option) {
        // The container holds the document's own statement, so the row is
        // the fallback for a payload built from a row set.
        if (!data.option_data && !data.option->option_type.empty()) {
            optionData_OptionType_t ot;
            static_cast<std::string&>(ot) = data.option->option_type;
            d.OptionData.OptionType = std::move(ot);
        }
        if (data.option->option_strike != 0.0 && !data.strike_data) {
            _Strike_t s;
            static_cast<std::string&>(s) = format_number(data.option->option_strike);
            d.strikeGroup.Strike = std::move(s);
        }
    }
    if (data.strike_data)
        d.strikeGroup.StrikeData = reverse_strike_data(*data.strike_data);
    if (data.option_redemption)
        set_present_text(d.Redemption, data.option_redemption);
    if (data.option_price_type)
        set_present_text(d.PriceType, data.option_price_type);
    if (data.option_knocks_out)
        d.KnocksOut = parse_code(*data.option_knocks_out, bool__count, bool_::N);
    reverse_exercise_dates(
        data.option_exercise_dates, data.option_exercise_schedule, d.OptionData);
    t.BondOptionData = std::move(d);
    return t;
}

trade bond_instrument_mapper::reverse_bond_trs(const bond_instrument_data& data) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping BondTRS";
    trade t;
    t.TradeType = oreTradeType::BondTRS;
    bondTRSData d;
    d.BondData = reverse_bond_data(data);
    if (!data.trs_price_type.empty()) {
        totalReturnData_PriceType_t pt;
        static_cast<std::string&>(pt) = data.trs_price_type;
        d.TotalReturnData.PriceType = std::move(pt);
    }
    reverse_leg(data.trs_funding_leg, d.FundingData.LegData);
    // The leg type comes from the container, which holds the document's
    // own statement. The row's funding type stands in only when the
    // payload was built from a row set and holds no leg.
    const bool leg_type_from_row = !data.trs_funding_leg.leg_type;
    const bool fixed =
        !data.trs || data.trs->funding_leg_type.empty() || data.trs->funding_leg_type == "Fixed";
    if (fixed) {
        if (leg_type_from_row)
            d.FundingData.LegData.LegType = legType::Fixed;
        if (data.trs && data.trs->funding_rate != 0.0) {
            _FixedLegData_t fld;
            _FixedLegData_t_Rates_t_Rate_t rate;
            static_cast<float&>(rate) = static_cast<float>(data.trs->funding_rate);
            fld.Rates.Rate.push_back(rate);
            legDataType_group_t ldt;
            ldt.FixedLegData = std::move(fld);
            d.FundingData.LegData.legDataType = std::move(ldt);
        }
    } else if (data.trs) {
        if (leg_type_from_row)
            d.FundingData.LegData.LegType = legType::Floating;
        _FloatingLegData_t fld;
        static_cast<std::string&>(fld.Index) = data.trs->funding_index;
        legDataType_group_t ldt;
        ldt.FloatingLegData = std::move(fld);
        d.FundingData.LegData.legDataType = std::move(ldt);
    }
    if (data.trs_payer)
        set_text(d.TotalReturnData.Payer, *data.trs_payer);
    if (data.trs_initial_price)
        d.TotalReturnData.InitialPrice = static_cast<float>(*data.trs_initial_price);
    d.TotalReturnData.ScheduleData = reverse_schedule(data.trs_schedule);
    t.BondTRSData = std::move(d);
    return t;
}

trade bond_instrument_mapper::reverse_bond_repo(const bond_instrument_data& data) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping BondRepo";
    trade t;
    t.TradeType = oreTradeType::BondRepo;
    bondRepoData d;
    d.BondData = reverse_bond_data(data);
    reverse_leg(data.repo_leg, d.RepoData.LegData);
    const bool floating = data.repo && data.repo->repo_type == "Floating";
    if (!data.repo_leg.leg_type)
        d.RepoData.LegData.LegType = floating ? legType::Floating : legType::Fixed;
    if (floating) {
        _FloatingLegData_t fld;
        static_cast<std::string&>(fld.Index) = data.repo->repo_index;
        legDataType_group_t ldt;
        ldt.FloatingLegData = std::move(fld);
        d.RepoData.LegData.legDataType = std::move(ldt);
    } else if (data.repo && data.repo->repo_rate != 0.0) {
        _FixedLegData_t fld;
        _FixedLegData_t_Rates_t_Rate_t rate;
        static_cast<float&>(rate) = static_cast<float>(data.repo->repo_rate);
        fld.Rates.Rate.push_back(rate);
        legDataType_group_t ldt;
        ldt.FixedLegData = std::move(fld);
        d.RepoData.LegData.legDataType = std::move(ldt);
    }
    t.BondRepoData = std::move(d);
    return t;
}

trade bond_instrument_mapper::reverse_bond_future(const bond_instrument_data& data) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping BondFuture";
    trade t;
    t.TradeType = oreTradeType::BondFuture;
    bondFutureData d;
    if (data.future) {
        const auto& f = *data.future;
        set_text(d.ContractName, f.contract_name);
        set_text(d.ContractNotional, format_number(f.contract_notional));
        set_text(d.LongShort, f.long_short);
        // The contract's own terms belong to the BondFutureReferenceData
        // datum in v17, not to the trade, so there is nowhere here to
        // write currency, contract month, deliverable grade, settlement
        // and its basis, expiry basis, the lags, root date, or the last
        // trading and delivery dates. Writing them would produce a
        // document the schema rejects. They are exported once the
        // reference data document is emitted alongside the portfolio.
    }
    t.BondFutureData = std::move(d);
    return t;
}

trade bond_instrument_mapper::reverse_ascot(const bond_instrument_data& data) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping Ascot";
    trade t;
    t.TradeType = oreTradeType::Ascot;
    ascotData d;
    d.ConvertibleBondData.BondData = reverse_bond_data(data);
    if (!data.conversion_targets.empty()) {
        cbConversionData conversion_data;
        reverse_conversion_targets(data.conversion_targets, conversion_data);
        d.ConvertibleBondData.ConversionData = std::move(conversion_data);
    }
    if (data.option_data)
        reverse_option_data(*data.option_data, d.OptionData);
    else if (data.ascot_row && !data.ascot_row->ascot_option_type.empty()) {
        optionData_OptionType_t ot;
        static_cast<std::string&>(ot) = data.ascot_row->ascot_option_type;
        d.OptionData.OptionType = std::move(ot);
    }
    reverse_exercise_dates(
        data.option_exercise_dates, data.option_exercise_schedule, d.OptionData);
    reverse_leg(data.ascot_swap_leg, d.ReferenceSwapData.LegData);
    t.AscotData = std::move(d);
    return t;
}

}
