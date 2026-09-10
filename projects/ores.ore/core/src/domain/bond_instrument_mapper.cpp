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
#include <stdexcept>
#include <utility>
#include <vector>

namespace ores::ore::domain {

using namespace ores::logging;
using ores::trading::domain::ascot;
using ores::trading::domain::bond_future;
using ores::trading::domain::bond_instrument_data;
using ores::trading::domain::bond_issue;
using ores::trading::domain::bond_issue_call_date;
using ores::trading::domain::bond_issue_conversion_target;
using ores::trading::domain::bond_leg_data;
using ores::trading::domain::bond_option;
using ores::trading::domain::bond_repo;
using ores::trading::domain::bond_schedule_data;
using ores::trading::domain::bond_schedule_dates;
using ores::trading::domain::bond_schedule_rules;
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

constexpr int business_day_convention_count = 27;
constexpr int date_rule_count = 16;
constexpr int currency_code_count = 191;
constexpr int day_counter_count = 71;
constexpr int leg_type_count = 18;

// The schema states the future's price and lag fields as strings. A
// value the parser cannot read leaves the column at its default rather
// than failing the whole import.
double number_of(const std::string& text, double fallback) {
    try {
        return std::stod(text);
    } catch (const std::exception&) {
        return fallback;
    }
}

int count_of(const std::string& text, int fallback) {
    try {
        return std::stoi(text);
    } catch (const std::exception&) {
        return fallback;
    }
}

// The schema states a boolean as a string in several spellings. This is
// the same set the generated bool_ enumeration carries.
bool flag_of(const std::string& text) {
    return text == "Y" || text == "YES" || text == "TRUE" || text == "True" || text == "true" ||
           text == "1";
}

// An empty string is the on spelling. The bool type enumerates thirteen
// spellings and the corpus writes the empty one for the on state: EndOfMonth
// is the only element any document types as bool, and the documents write it
// empty 3,442 times, false 103 times and true 12 times.
bool flag_of(const bool_& value) {
    const std::string text = to_string(value);
    return text.empty() || flag_of(text);
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

bond_schedule_data map_schedule(const scheduleData& sd) {
    bond_schedule_data result;
    for (const auto& r : sd.Rules) {
        bond_schedule_rules row;
        row.start_date = r.StartDate;
        if (r.EndDate)
            row.end_date = *r.EndDate;
        if (r.AdjustEndDateToPreviousMonthEnd)
            row.adjust_end_date_to_previous_month_end = flag_of(*r.AdjustEndDateToPreviousMonthEnd);
        row.tenor = r.Tenor;
        if (r.Calendar)
            row.calendar = *r.Calendar;
        row.convention = to_string(r.Convention);
        if (r.TermConvention)
            row.term_convention = to_string(*r.TermConvention);
        if (r.Rule)
            row.rule = to_string(*r.Rule);
        if (r.EndOfMonth)
            row.end_of_month = flag_of(*r.EndOfMonth);
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
            row.end_of_month = flag_of(*d.EndOfMonth);
        if (d.IncludeDuplicateDates)
            row.include_duplicate_dates = flag_of(*d.IncludeDuplicateDates);
        for (const auto& date : d.Dates.Date)
            row.dates.push_back(date);
        result.dates.push_back(std::move(row));
    }
    return result;
}

// Text members emit on presence, so an element the document states
// empty is re-emitted rather than dropped. A flag has no empty
// spelling to hand back, so it emits the canonical Y or N.
scheduleData reverse_schedule(const bond_schedule_data& sd) {
    scheduleData result;
    for (const auto& row : sd.rules) {
        scheduleData_Rules_t r;
        r.StartDate = row.start_date;
        if (row.end_date)
            r.EndDate = *row.end_date;
        if (row.adjust_end_date_to_previous_month_end)
            r.AdjustEndDateToPreviousMonthEnd =
                *row.adjust_end_date_to_previous_month_end ? bool_::Y : bool_::N;
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
            r.EndOfMonth = *row.end_of_month ? bool_::Y : bool_::N;
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
            d.EndOfMonth = *row.end_of_month ? bool_::Y : bool_::N;
        if (row.include_duplicate_dates)
            d.IncludeDuplicateDates = *row.include_duplicate_dates ? bool_::Y : bool_::N;
        for (const auto& date : row.dates)
            d.Dates.Date.push_back(date);
        result.Dates.push_back(std::move(d));
    }
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
}

void map_exercise_dates(const optionData& od, std::vector<std::string>& dates) {
    if (od.exerciseDatesGroup && od.exerciseDatesGroup->ExerciseDates)
        for (const auto& date : od.exerciseDatesGroup->ExerciseDates->ExerciseDate)
            dates.push_back(date);
}

void reverse_exercise_dates(const std::vector<std::string>& dates, optionData& od) {
    if (dates.empty())
        return;
    _ExerciseDates_t exd;
    for (const auto& value : dates)
        exd.ExerciseDate.push_back(value);
    exerciseDatesGroup_group_t eg;
    eg.ExerciseDates = std::move(exd);
    od.exerciseDatesGroup = std::move(eg);
}

} // namespace

void bond_instrument_mapper::map_bond_data(const bondData& bd, bond_instrument_data& data) {
    auto& issue = data.issue;
    issue.security_id = std::string(bd.SecurityId);
    if (bd.IssuerId)
        issue.issuer = std::string(*bd.IssuerId);
    if (bd.IssueDate)
        issue.issue_date = std::string(*bd.IssueDate);
    if (bd.SettlementDays) {
        const std::string settlement_days_str(*bd.SettlementDays);
        if (!settlement_days_str.empty())
            issue.settlement_days = std::stoi(settlement_days_str);
    }

    if (!bd.LegData.empty()) {
        const auto& ld = bd.LegData.front();
        data.bond_leg = map_leg(ld);
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

        if (ld.ScheduleData && !ld.ScheduleData->Rules.empty()) {
            const auto& rule = ld.ScheduleData->Rules.front();
            if (rule.EndDate)
                issue.maturity_date = std::string(*rule.EndDate);
        }
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

    // The leg is emitted when the document held one, whether or not the
    // columns that mirror it hold a value: Currency and Notionals are
    // optional in the schema, so a leg the document carries with neither
    // still has to come back out.
    if (!data.bond_leg.is_empty() || !issue.currency.empty() || issue.face_value != 0.0) {
        legData ld;
        reverse_leg(data.bond_leg, ld);

        // The issue row mirrors the leg's currency and day counter, so it
        // supplies them only when the container came from a row set. A
        // document's own statement is already on the leg.
        if (!ld.Currency && !issue.currency.empty())
            ld.Currency = issue.currency;
        if (!ld.DayCounter && !issue.day_count_code.empty())
            ld.DayCounter = parse_code(issue.day_count_code,
                                       day_counter_count,
                                       dayCounter::A360);

        if (issue.face_value != 0.0) {
            legData_Notionals_t n;
            legData_Notionals_t_Notional_t nv;
            static_cast<float&>(nv) = static_cast<float>(issue.face_value);
            n.Notional.push_back(nv);
            ld.Notionals = std::move(n);
        }

        // A container that came from a document carries the leg whole, so
        // this rebuild runs only for a row set with no remainder: the issue
        // terms are then the whole of what is known, and the issue date is
        // the closest stand-in the row holds for a schedule start.
        if (!ld.ScheduleData &&
            (!issue.maturity_date.empty() || !issue.coupon_frequency_code.empty())) {
            scheduleData_Rules_t rule;
            if (!issue.maturity_date.empty()) {
                domain::date d;
                static_cast<std::string&>(d) = issue.maturity_date;
                rule.EndDate = xsd::optional<domain::date>(d);
            }
            if (!issue.coupon_frequency_code.empty())
                static_cast<std::string&>(rule.Tenor) = issue.coupon_frequency_code;
            if (!issue.issue_date.empty())
                rule.StartDate = issue.issue_date;
            scheduleData sched;
            sched.Rules.push_back(std::move(rule));
            ld.ScheduleData = std::move(sched);
        }

        if (issue.coupon_rate != 0.0) {
            _FixedLegData_t fld;
            _FixedLegData_t_Rates_t_Rate_t rate;
            static_cast<float&>(rate) = static_cast<float>(issue.coupon_rate);
            fld.Rates.Rate.push_back(rate);
            legDataType_group_t ldt;
            ldt.FixedLegData = std::move(fld);
            ld.legDataType = std::move(ldt);
        }

        bd.LegData.push_back(std::move(ld));
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
    if (t.ForwardBondData)
        map_bond_data(t.ForwardBondData->BondData, result);
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
    if (d.strikeGroup.Strike) {
        const std::string s(*d.strikeGroup.Strike);
        if (!s.empty())
            option.option_strike = std::stod(s);
    }
    stamp_audit(option);
    result.option = option;

    map_exercise_dates(d.OptionData, result.option_exercise_dates);

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
    if (d.Currency)
        future.currency = to_string(*d.Currency);
    if (d.ContractMonth)
        future.contract_month = *d.ContractMonth;
    if (d.DeliverableGrade)
        future.deliverable_grade = *d.DeliverableGrade;
    if (d.FairPrice)
        future.fair_price = number_of(*d.FairPrice, 0.0);
    if (d.Settlement)
        future.settlement = *d.Settlement;
    if (d.SettlementDirty)
        future.settlement_dirty = flag_of(*d.SettlementDirty);
    if (d.RootDate)
        future.root_date = *d.RootDate;
    if (d.ExpiryBasis)
        future.expiry_basis = *d.ExpiryBasis;
    if (d.SettlementBasis)
        future.settlement_basis = *d.SettlementBasis;
    if (d.ExpiryLag)
        future.expiry_lag = count_of(*d.ExpiryLag, 0);
    if (d.SettlementLag)
        future.settlement_lag = count_of(*d.SettlementLag, 0);
    if (d.LastTradingDate)
        future.last_trading_date = *d.LastTradingDate;
    if (d.LastDeliveryDate)
        future.last_delivery_date = *d.LastDeliveryDate;
    if (d.DeliveryBasket)
        for (const auto& id : d.DeliveryBasket->Id)
            result.future_delivery_basket.push_back(id);
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
    result.ascot = row;

    map_exercise_dates(d.OptionData, result.option_exercise_dates);
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
    if (data.option) {
        if (!data.option->option_type.empty()) {
            optionData_OptionType_t ot;
            static_cast<std::string&>(ot) = data.option->option_type;
            d.OptionData.OptionType = std::move(ot);
        }
        if (data.option->option_strike != 0.0) {
            _Strike_t s;
            static_cast<std::string&>(s) = std::to_string(data.option->option_strike);
            d.strikeGroup.Strike = std::move(s);
        }
    }
    reverse_exercise_dates(data.option_exercise_dates, d.OptionData);
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
        set_text(d.ContractNotional, std::to_string(f.contract_notional));
        set_text(d.LongShort, f.long_short);
        if (!f.currency.empty())
            d.Currency = parse_code(f.currency, currency_code_count, currencyCode::USD);
        set_optional_text(d.ContractMonth, f.contract_month);
        set_optional_text(d.DeliverableGrade, f.deliverable_grade);
        if (f.fair_price != 0.0)
            set_optional_text(d.FairPrice, std::to_string(f.fair_price));
        set_optional_text(d.Settlement, f.settlement);
        if (f.settlement_dirty)
            set_optional_text(d.SettlementDirty, "true");
        set_optional_text(d.RootDate, f.root_date);
        set_optional_text(d.ExpiryBasis, f.expiry_basis);
        set_optional_text(d.SettlementBasis, f.settlement_basis);
        if (f.expiry_lag != 0)
            set_optional_text(d.ExpiryLag, std::to_string(f.expiry_lag));
        if (f.settlement_lag != 0)
            set_optional_text(d.SettlementLag, std::to_string(f.settlement_lag));
        set_optional_text(d.LastTradingDate, f.last_trading_date);
        set_optional_text(d.LastDeliveryDate, f.last_delivery_date);
    }
    if (!data.future_delivery_basket.empty()) {
        deliveryBasket basket;
        for (const auto& id : data.future_delivery_basket) {
            deliveryBasket_Id_t entry;
            static_cast<std::string&>(entry) = id;
            basket.Id.push_back(std::move(entry));
        }
        d.DeliveryBasket = std::move(basket);
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
    if (data.ascot && !data.ascot->ascot_option_type.empty()) {
        optionData_OptionType_t ot;
        static_cast<std::string&>(ot) = data.ascot->ascot_option_type;
        d.OptionData.OptionType = std::move(ot);
    }
    reverse_exercise_dates(data.option_exercise_dates, d.OptionData);
    reverse_leg(data.ascot_swap_leg, d.ReferenceSwapData.LegData);
    t.AscotData = std::move(d);
    return t;
}

}
