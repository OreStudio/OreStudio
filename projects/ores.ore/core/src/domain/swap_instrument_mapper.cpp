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
#include "ores.ore.core/domain/swap_instrument_mapper.hpp"
#include "ores.ore.core/domain/payment_frequency_conversion.hpp"
#include "ores.platform/time/datetime.hpp"
#include <algorithm>
#include <chrono>
#include <cstdint>
#include <map>
#include <optional>
#include <string>
#include <vector>

namespace ores::ore::domain {

using namespace ores::logging;
using ores::trading::domain::fra_instrument;
using ores::trading::domain::vanilla_swap_instrument;
using ores::trading::domain::cap_floor_instrument;
using ores::trading::domain::swaption_instrument;
using ores::trading::domain::balance_guaranteed_swap_instrument;
using ores::trading::domain::callable_swap_instrument;
using ores::trading::domain::knock_out_swap_instrument;
using ores::trading::domain::inflation_swap_instrument;
using ores::trading::domain::swap_leg;

// ---------------------------------------------------------------------------
// Helpers: forward direction (ORE XSD → string)
// ---------------------------------------------------------------------------

namespace {

// The trading domain holds a date as std::chrono::year_month_day; the ORE XML
// holds its ISO-8601 spelling. An absent ORE date maps to a default
// (invalid) calendar date, which renders back to an empty string.
std::chrono::year_month_day to_domain_date(const std::string& s) {
    if (s.empty())
        return {};
    return ores::platform::time::datetime::from_iso8601_date(s);
}

std::string to_ore_date(const std::chrono::year_month_day& d) {
    return d.ok() ? ores::platform::time::datetime::to_iso8601_date(d) : std::string{};
}

std::string to_ore_date(const std::optional<std::chrono::year_month_day>& d) {
    return d ? ores::platform::time::datetime::to_iso8601_date(*d) : std::string{};
}

std::string day_counter_string(const xsd::optional<dayCounter>& dc) {
    if (!dc)
        return {};
    return to_string(*dc);
}

std::string bdc_string(const xsd::optional<businessDayConvention>& bdc) {
    if (!bdc)
        return {};
    return to_string(*bdc);
}

std::string first_tenor(const xsd::optional<scheduleData>& sd) {
    if (!sd)
        return {};
    if (!sd->Rules.empty())
        return std::string(sd->Rules.front().Tenor);
    return {};
}

std::chrono::year_month_day start_date_from_schedule(const xsd::optional<scheduleData>& sd) {
    if (!sd)
        return {};
    if (!sd->Rules.empty())
        return to_domain_date(std::string(sd->Rules.front().StartDate));
    // A schedule may carry an explicit date list instead of a rule. Its first
    // entry is the effective date, and a date-typed start_date cannot hold
    // the empty string the rule-only path leaves behind.
    if (!sd->Dates.empty() && !sd->Dates.front().Dates.Date.empty())
        return to_domain_date(std::string(sd->Dates.front().Dates.Date.front()));
    return {};
}

std::chrono::year_month_day end_date_from_schedule(const xsd::optional<scheduleData>& sd) {
    if (!sd)
        return {};
    if (!sd->Rules.empty() && sd->Rules.front().EndDate)
        return to_domain_date(std::string(*sd->Rules.front().EndDate));
    // Symmetric with start_date_from_schedule: an explicit date list carries
    // the maturity as its last entry, and a date-typed maturity_date cannot
    // hold the empty string the rule-only path leaves behind.
    if (!sd->Dates.empty() && !sd->Dates.front().Dates.Date.empty())
        return to_domain_date(std::string(sd->Dates.front().Dates.Date.back()));
    return {};
}

// CapFloor leg uses non-optional scheduleData
std::chrono::year_month_day start_date_from_schedule(const scheduleData& sd) {
    if (!sd.Rules.empty())
        return to_domain_date(std::string(sd.Rules.front().StartDate));
    return {};
}

std::chrono::year_month_day end_date_from_schedule(const scheduleData& sd) {
    if (!sd.Rules.empty() && sd.Rules.front().EndDate)
        return to_domain_date(std::string(*sd.Rules.front().EndDate));
    return {};
}

std::string first_tenor(const scheduleData& sd) {
    if (!sd.Rules.empty())
        return std::string(sd.Rules.front().Tenor);
    return {};
}

// ---------------------------------------------------------------------------
// Helpers: reverse direction (string → ORE XSD)
// ---------------------------------------------------------------------------

scheduleData make_schedule(const std::optional<std::chrono::year_month_day>& start,
                           const std::optional<std::chrono::year_month_day>& end,
                           const std::string& tenor) {
    scheduleData sd;
    scheduleData_Rules_t r;
    r.StartDate = to_ore_date(start);
    if (end) {
        domain::date d;
        static_cast<std::string&>(d) = to_ore_date(end);
        r.EndDate = xsd::optional<domain::date>(d);
    }
    static_cast<std::string&>(r.Tenor) = tenor;
    r.Convention = businessDayConvention::MF;
    sd.Rules.push_back(std::move(r));
    return sd;
}

std::optional<legType> leg_type_from_string(const std::string& s) {
    if (s == "Fixed")
        return legType::Fixed;
    if (s == "Floating")
        return legType::Floating;
    if (s == "CPI")
        return legType::CPI;
    if (s == "YY")
        return legType::YY;
    if (s == "CMS")
        return legType::CMS;
    if (s == "CMB")
        return legType::CMB;
    if (s == "DigitalCMS")
        return legType::DigitalCMS;
    if (s == "CMSSpread")
        return legType::CMSSpread;
    if (s == "DigitalCMSSpread")
        return legType::DigitalCMSSpread;
    if (s == "Cashflow")
        return legType::Cashflow;
    if (s == "Equity")
        return legType::Equity;
    if (s == "FormulaBased")
        return legType::FormulaBased;
    if (s == "ZeroCouponFixed")
        return legType::ZeroCouponFixed;
    if (s == "CommodityFixed")
        return legType::CommodityFixed;
    if (s == "CommodityFloating")
        return legType::CommodityFloating;
    if (s == "EquityMargin")
        return legType::EquityMargin;
    if (s == "DurationAdjustedCMS")
        return legType::DurationAdjustedCMS;
    return std::nullopt;
}

/**
 * @brief Parse an ISO 4217 currency code string to the currencyCode enum.
 *
 * Covers every enumerator in domain::currencyCode including crypto codes
 * (BTC, ETH, XBT, etc.). Throws std::runtime_error for any unrecognised
 * string — a silent default would generate ORE XML that fails XSD validation.
 */
currencyCode parse_currency_code(const std::string& s) {
    static const std::map<std::string, currencyCode> map = {
        {"AED", currencyCode::AED},
        {"AFN", currencyCode::AFN},
        {"ALL", currencyCode::ALL},
        {"AMD", currencyCode::AMD},
        {"ANG", currencyCode::ANG},
        {"AOA", currencyCode::AOA},
        {"ARS", currencyCode::ARS},
        {"AUD", currencyCode::AUD},
        {"AWG", currencyCode::AWG},
        {"AZN", currencyCode::AZN},
        {"BAM", currencyCode::BAM},
        {"BBD", currencyCode::BBD},
        {"BDT", currencyCode::BDT},
        {"BGN", currencyCode::BGN},
        {"BHD", currencyCode::BHD},
        {"BIF", currencyCode::BIF},
        {"BMD", currencyCode::BMD},
        {"BND", currencyCode::BND},
        {"BOB", currencyCode::BOB},
        {"BOV", currencyCode::BOV},
        {"BRL", currencyCode::BRL},
        {"BSD", currencyCode::BSD},
        {"BTN", currencyCode::BTN},
        {"BWP", currencyCode::BWP},
        {"BYN", currencyCode::BYN},
        {"BZD", currencyCode::BZD},
        {"CAD", currencyCode::CAD},
        {"CDF", currencyCode::CDF},
        {"CHE", currencyCode::CHE},
        {"CHF", currencyCode::CHF},
        {"CHW", currencyCode::CHW},
        {"CLF", currencyCode::CLF},
        {"CLP", currencyCode::CLP},
        {"CNH", currencyCode::CNH},
        {"CNT", currencyCode::CNT},
        {"CNY", currencyCode::CNY},
        {"COP", currencyCode::COP},
        {"COU", currencyCode::COU},
        {"CRC", currencyCode::CRC},
        {"CUC", currencyCode::CUC},
        {"CUP", currencyCode::CUP},
        {"CVE", currencyCode::CVE},
        {"CYP", currencyCode::CYP},
        {"CZK", currencyCode::CZK},
        {"DJF", currencyCode::DJF},
        {"DKK", currencyCode::DKK},
        {"DOP", currencyCode::DOP},
        {"DZD", currencyCode::DZD},
        {"EGP", currencyCode::EGP},
        {"ERN", currencyCode::ERN},
        {"ETB", currencyCode::ETB},
        {"EUR", currencyCode::EUR},
        {"FJD", currencyCode::FJD},
        {"FKP", currencyCode::FKP},
        {"GBP", currencyCode::GBP},
        {"GEL", currencyCode::GEL},
        {"GGP", currencyCode::GGP},
        {"GHS", currencyCode::GHS},
        {"GIP", currencyCode::GIP},
        {"GMD", currencyCode::GMD},
        {"GNF", currencyCode::GNF},
        {"GTQ", currencyCode::GTQ},
        {"GYD", currencyCode::GYD},
        {"HKD", currencyCode::HKD},
        {"HNL", currencyCode::HNL},
        {"HRK", currencyCode::HRK},
        {"HTG", currencyCode::HTG},
        {"HUF", currencyCode::HUF},
        {"IDR", currencyCode::IDR},
        {"ILS", currencyCode::ILS},
        {"IMP", currencyCode::IMP},
        {"INR", currencyCode::INR},
        {"IQD", currencyCode::IQD},
        {"IRR", currencyCode::IRR},
        {"ISK", currencyCode::ISK},
        {"JEP", currencyCode::JEP},
        {"JMD", currencyCode::JMD},
        {"JOD", currencyCode::JOD},
        {"JPY", currencyCode::JPY},
        {"KES", currencyCode::KES},
        {"KGS", currencyCode::KGS},
        {"KHR", currencyCode::KHR},
        {"KID", currencyCode::KID},
        {"KMF", currencyCode::KMF},
        {"KPW", currencyCode::KPW},
        {"KRW", currencyCode::KRW},
        {"KWD", currencyCode::KWD},
        {"KYD", currencyCode::KYD},
        {"KZT", currencyCode::KZT},
        {"LAK", currencyCode::LAK},
        {"LBP", currencyCode::LBP},
        {"LKR", currencyCode::LKR},
        {"LRD", currencyCode::LRD},
        {"LSL", currencyCode::LSL},
        {"LTL", currencyCode::LTL},
        {"LVL", currencyCode::LVL},
        {"LYD", currencyCode::LYD},
        {"MAD", currencyCode::MAD},
        {"MDL", currencyCode::MDL},
        {"MGA", currencyCode::MGA},
        {"MKD", currencyCode::MKD},
        {"MMK", currencyCode::MMK},
        {"MNT", currencyCode::MNT},
        {"MOP", currencyCode::MOP},
        {"MRU", currencyCode::MRU},
        {"MUR", currencyCode::MUR},
        {"MVR", currencyCode::MVR},
        {"MWK", currencyCode::MWK},
        {"MXN", currencyCode::MXN},
        {"MXV", currencyCode::MXV},
        {"MYR", currencyCode::MYR},
        {"MZN", currencyCode::MZN},
        {"NAD", currencyCode::NAD},
        {"NGN", currencyCode::NGN},
        {"NIO", currencyCode::NIO},
        {"NOK", currencyCode::NOK},
        {"NPR", currencyCode::NPR},
        {"NZD", currencyCode::NZD},
        {"OMR", currencyCode::OMR},
        {"PAB", currencyCode::PAB},
        {"PEN", currencyCode::PEN},
        {"PGK", currencyCode::PGK},
        {"PHP", currencyCode::PHP},
        {"PKR", currencyCode::PKR},
        {"PLN", currencyCode::PLN},
        {"PYG", currencyCode::PYG},
        {"QAR", currencyCode::QAR},
        {"RON", currencyCode::RON},
        {"RSD", currencyCode::RSD},
        {"RUB", currencyCode::RUB},
        {"RWF", currencyCode::RWF},
        {"SAR", currencyCode::SAR},
        {"SBD", currencyCode::SBD},
        {"SCR", currencyCode::SCR},
        {"SDG", currencyCode::SDG},
        {"SEK", currencyCode::SEK},
        {"SGD", currencyCode::SGD},
        {"SHP", currencyCode::SHP},
        {"SLL", currencyCode::SLL},
        {"SKK", currencyCode::SKK},
        {"SOS", currencyCode::SOS},
        {"SRD", currencyCode::SRD},
        {"SSP", currencyCode::SSP},
        {"STN", currencyCode::STN},
        {"SVC", currencyCode::SVC},
        {"SYP", currencyCode::SYP},
        {"SZL", currencyCode::SZL},
        {"THB", currencyCode::THB},
        {"TJS", currencyCode::TJS},
        {"TMT", currencyCode::TMT},
        {"TND", currencyCode::TND},
        {"TOP", currencyCode::TOP},
        {"TRY", currencyCode::TRY},
        {"TTD", currencyCode::TTD},
        {"TWD", currencyCode::TWD},
        {"TZS", currencyCode::TZS},
        {"UAH", currencyCode::UAH},
        {"UGX", currencyCode::UGX},
        {"USD", currencyCode::USD},
        {"USN", currencyCode::USN},
        {"UYI", currencyCode::UYI},
        {"UYU", currencyCode::UYU},
        {"UYW", currencyCode::UYW},
        {"UZS", currencyCode::UZS},
        {"VES", currencyCode::VES},
        {"VND", currencyCode::VND},
        {"VUV", currencyCode::VUV},
        {"WST", currencyCode::WST},
        {"XAF", currencyCode::XAF},
        {"XAG", currencyCode::XAG},
        {"XAU", currencyCode::XAU},
        {"XBT", currencyCode::XBT},
        {"XCD", currencyCode::XCD},
        {"XOF", currencyCode::XOF},
        {"XPD", currencyCode::XPD},
        {"XPF", currencyCode::XPF},
        {"XPT", currencyCode::XPT},
        {"XRP", currencyCode::XRP},
        {"XSU", currencyCode::XSU},
        {"XUA", currencyCode::XUA},
        {"YER", currencyCode::YER},
        {"ZAR", currencyCode::ZAR},
        {"ZMW", currencyCode::ZMW},
        {"ZWL", currencyCode::ZWL},
        // Crypto codes recognised by ORE
        {"BTC", currencyCode::BTC},
        {"ETH", currencyCode::ETH},
        {"ETC", currencyCode::ETC},
        {"BCH", currencyCode::BCH},
        {"LTC", currencyCode::LTC},
        // ORE internal synthetic codes
        {"ZUR", currencyCode::ZUR},
        {"ZUG", currencyCode::ZUG},
    };
    const auto it = map.find(s);
    if (it == map.end())
        throw std::runtime_error("parse_currency_code: unrecognised currency code '" + s +
                                 "' — cannot produce valid ORE XML");
    return it->second;
}

/**
 * One notional child of a leg, with the provenance every imported row carries.
 */
trading::domain::swap_leg_amount make_leg_amount(int leg_number,
                                                 int sequence_number,
                                                 const ores::utility::decimal::decimal& amount) {
    trading::domain::swap_leg_amount a;
    a.leg_number = leg_number;
    a.sequence_number = sequence_number;
    a.amount = amount;
    a.modified_by = "ores";
    a.performed_by = "ores";
    a.change_reason_code = "system.external_data_import";
    a.change_commentary = "Imported from ORE XML";
    return a;
}

/**
 * One rate or spread child of a leg, with the same provenance.
 */
trading::domain::swap_leg_rate
make_leg_rate(int leg_number, const std::string& rate_role, int sequence_number, double value) {
    trading::domain::swap_leg_rate r;
    r.leg_number = leg_number;
    r.rate_role = rate_role;
    r.sequence_number = sequence_number;
    r.value = value;
    r.modified_by = "ores";
    r.performed_by = "ores";
    r.change_reason_code = "system.external_data_import";
    r.change_commentary = "Imported from ORE XML";
    return r;
}

/**
 * The notional children of one leg, in the order the document stated them.
 */
std::vector<trading::domain::swap_leg_amount>
amounts_for_leg(const std::vector<trading::domain::swap_leg_amount>& all, int leg_number) {
    std::vector<trading::domain::swap_leg_amount> out;
    for (const auto& a : all)
        if (a.leg_number == leg_number)
            out.push_back(a);
    return out;
}

/**
 * The rate and spread children of one leg, in the order the document stated
 * them.
 */
std::vector<trading::domain::swap_leg_rate>
rates_for_leg(const std::vector<trading::domain::swap_leg_rate>& all, int leg_number) {
    std::vector<trading::domain::swap_leg_rate> out;
    for (const auto& r : all)
        if (r.leg_number == leg_number)
            out.push_back(r);
    return out;
}

// ---------------------------------------------------------------------------
// Helpers: the shared children (strikes, schedules, the option block)
// ---------------------------------------------------------------------------

// A shared child row carries the same import provenance as the family's own.
template <typename Row>
void stamp_child_audit(Row& row) {
    row.modified_by = "ores";
    row.performed_by = "ores";
    row.change_reason_code = "system.external_data_import";
    row.change_commentary = "Imported from ORE XML";
}

std::optional<std::chrono::year_month_day> to_optional_domain_date(const std::string& s) {
    if (s.empty())
        return std::nullopt;
    return ores::platform::time::datetime::from_iso8601_date(s);
}

// The schema states amounts and prices as text; a value the parser cannot
// read leaves the column absent rather than failing the whole import.
std::optional<ores::utility::decimal::decimal> to_optional_decimal(const std::string& text) {
    if (text.empty())
        return std::nullopt;
    auto parsed = ores::utility::decimal::decimal::from_string(text);
    if (!parsed)
        return std::nullopt;
    return std::move(*parsed);
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

constexpr int bool__count = 13;
constexpr int business_day_convention_count = 27;
constexpr int date_rule_count = 16;
constexpr int option_pay_relative_to_count = 2;
constexpr int settlement_method_count = 4;
constexpr int settlement_type_count = 2;

template <typename Field>
void set_text(Field& field, const std::string& value) {
    static_cast<std::string&>(field) = value;
}

template <typename Field>
void set_present_text(xsd::optional<Field>& field, const std::optional<std::string>& value) {
    if (!value)
        return;
    Field wrapper;
    static_cast<std::string&>(wrapper) = *value;
    field = std::move(wrapper);
}

template <typename Field>
void set_present_decimal(xsd::optional<Field>& field,
                         const std::optional<ores::utility::decimal::decimal>& value) {
    if (!value)
        return;
    Field wrapper;
    static_cast<std::string&>(wrapper) = value->to_string();
    field = std::move(wrapper);
}

domain::date make_date(const std::chrono::year_month_day& d) {
    domain::date out;
    static_cast<std::string&>(out) = to_ore_date(d);
    return out;
}

// The schema's exercise price list is text; the tokens are the document's
// own spelling of the numbers, split on the separators the corpus uses.
std::vector<std::string> split_numbers(const std::string& text) {
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

void fill_schedule_rule(trading::domain::instrument_schedule& row,
                        const scheduleData_Rules_t& r,
                        const std::string& owner_role,
                        int owner_number,
                        const std::string& schedule_role,
                        int sequence_number) {
    row.owner_role = owner_role;
    row.owner_number = owner_number;
    row.schedule_role = schedule_role;
    row.sequence_number = sequence_number;
    row.schedule_kind = "rules";
    row.start_date = to_domain_date(std::string(r.StartDate));
    if (r.EndDate)
        row.end_date = to_optional_domain_date(std::string(*r.EndDate));
    if (r.AdjustEndDateToPreviousMonthEnd)
        row.adjust_end_date_to_previous_month_end = to_string(*r.AdjustEndDateToPreviousMonthEnd);
    row.tenor = std::string(r.Tenor);
    if (r.Calendar)
        row.calendar = std::string(*r.Calendar);
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
        row.first_date = to_optional_domain_date(std::string(*r.FirstDate));
    if (r.LastDate)
        row.last_date = to_optional_domain_date(std::string(*r.LastDate));
    if (r.RemoveFirstDate)
        row.remove_first_date = *r.RemoveFirstDate;
    if (r.RemoveLastDate)
        row.remove_last_date = *r.RemoveLastDate;
    stamp_child_audit(row);
}

void fill_schedule_dates(trading::domain::instrument_schedule& row,
                         const scheduleData_Dates_t& d,
                         const std::string& owner_role,
                         int owner_number,
                         const std::string& schedule_role,
                         int sequence_number) {
    row.owner_role = owner_role;
    row.owner_number = owner_number;
    row.schedule_role = schedule_role;
    row.sequence_number = sequence_number;
    row.schedule_kind = "dates";
    if (d.Calendar)
        row.calendar = std::string(*d.Calendar);
    if (d.Convention)
        row.convention = to_string(*d.Convention);
    if (d.Tenor)
        row.tenor = std::string(*d.Tenor);
    if (d.EndOfMonth)
        row.end_of_month = to_string(*d.EndOfMonth);
    if (d.IncludeDuplicateDates)
        row.include_duplicate_dates = to_string(*d.IncludeDuplicateDates);
    stamp_child_audit(row);
}

// One schedule of one owner, as rows: each rule block and each date block
// becomes a schedule row and a date block's dates become rows of their own.
// The ordinals run across both arms, in document order.
void append_schedule(trading::domain::swap_instrument_data& result,
                     const scheduleData& sd,
                     const std::string& owner_role,
                     int owner_number,
                     const std::string& schedule_role) {
    int sequence_number = 0;
    for (const auto& rule : sd.Rules) {
        trading::domain::instrument_schedule row;
        fill_schedule_rule(row, rule, owner_role, owner_number, schedule_role, ++sequence_number);
        result.schedules.push_back(std::move(row));
    }
    for (const auto& block : sd.Dates) {
        const int parent_sequence = ++sequence_number;
        trading::domain::instrument_schedule row;
        fill_schedule_dates(row, block, owner_role, owner_number, schedule_role, parent_sequence);
        result.schedules.push_back(std::move(row));
        int date_number = 0;
        for (const auto& date : block.Dates.Date) {
            trading::domain::instrument_schedule_date date_row;
            date_row.owner_role = owner_role;
            date_row.owner_number = owner_number;
            date_row.schedule_role = schedule_role;
            date_row.schedule_sequence_number = parent_sequence;
            date_row.sequence_number = ++date_number;
            date_row.schedule_date = to_domain_date(std::string(date));
            stamp_child_audit(date_row);
            result.schedule_dates.push_back(std::move(date_row));
        }
    }
}

// The shared strike table holds one row per trade, keyed by the trade
// alone, while the schema states a cap/floor's strikes as two lists. The
// first cap, or the first floor when no cap is stated, is the row.
void append_capfloor_strike(trading::domain::swap_instrument_data& result, const capFloorData& cf) {
    std::optional<double> value;
    if (cf.Caps && !cf.Caps->Cap.empty())
        value = static_cast<double>(cf.Caps->Cap.front());
    else if (cf.Floors && !cf.Floors->Floor.empty())
        value = static_cast<double>(cf.Floors->Floor.front());
    if (!value)
        return;

    trading::domain::instrument_strike row;
    row.bare_value = ores::utility::decimal::decimal::from_double(*value).value();
    stamp_child_audit(row);
    result.strikes.push_back(std::move(row));
}

/**
 * One of an owner's schedules, rebuilt from the rows that hold it.
 */
std::optional<scheduleData>
schedule_for(const std::vector<trading::domain::instrument_schedule>& schedules,
             const std::vector<trading::domain::instrument_schedule_date>& schedule_dates,
             const std::string& owner_role,
             int owner_number,
             const std::string& schedule_role) {
    std::vector<const trading::domain::instrument_schedule*> rows;
    for (const auto& row : schedules)
        if (row.owner_role == owner_role && row.owner_number == owner_number &&
            row.schedule_role == schedule_role)
            rows.push_back(&row);
    if (rows.empty())
        return std::nullopt;
    std::sort(rows.begin(), rows.end(), [](const auto* a, const auto* b) {
        return a->sequence_number < b->sequence_number;
    });

    scheduleData sd;
    for (const auto* row : rows) {
        if (row->schedule_kind == "rules") {
            scheduleData_Rules_t r;
            static_cast<std::string&>(r.StartDate) = to_ore_date(row->start_date);
            if (row->end_date) {
                domain::date d;
                static_cast<std::string&>(d) = to_ore_date(*row->end_date);
                r.EndDate = xsd::optional<domain::date>(d);
            }
            if (row->adjust_end_date_to_previous_month_end)
                r.AdjustEndDateToPreviousMonthEnd =
                    parse_code(*row->adjust_end_date_to_previous_month_end, bool__count, bool_::N);
            static_cast<std::string&>(r.Tenor) = row->tenor.value_or("");
            if (row->calendar)
                r.Calendar = *row->calendar;
            if (row->convention)
                r.Convention = parse_code(
                    *row->convention, business_day_convention_count, businessDayConvention::MF);
            if (row->term_convention)
                r.TermConvention = parse_code(*row->term_convention,
                                              business_day_convention_count,
                                              businessDayConvention::MF);
            if (row->rule)
                r.Rule = parse_code(*row->rule, date_rule_count, dateRule::Forward);
            if (row->end_of_month)
                r.EndOfMonth = parse_code(*row->end_of_month, bool__count, bool_::N);
            if (row->end_of_month_convention)
                r.EndOfMonthConvention = parse_code(*row->end_of_month_convention,
                                                    business_day_convention_count,
                                                    businessDayConvention::MF);
            if (row->first_date) {
                domain::date d;
                static_cast<std::string&>(d) = to_ore_date(*row->first_date);
                r.FirstDate = xsd::optional<domain::date>(d);
            }
            if (row->last_date) {
                domain::date d;
                static_cast<std::string&>(d) = to_ore_date(*row->last_date);
                r.LastDate = xsd::optional<domain::date>(d);
            }
            if (row->remove_first_date)
                r.RemoveFirstDate = *row->remove_first_date;
            if (row->remove_last_date)
                r.RemoveLastDate = *row->remove_last_date;
            sd.Rules.push_back(std::move(r));
            continue;
        }

        scheduleData_Dates_t d;
        if (row->calendar)
            d.Calendar = *row->calendar;
        if (row->convention)
            d.Convention = parse_code(
                *row->convention, business_day_convention_count, businessDayConvention::MF);
        if (row->tenor) {
            scheduleData_Dates_t_Tenor_t tenor;
            static_cast<std::string&>(tenor) = *row->tenor;
            d.Tenor = std::move(tenor);
        }
        if (row->end_of_month)
            d.EndOfMonth = parse_code(*row->end_of_month, bool__count, bool_::N);
        if (row->include_duplicate_dates)
            d.IncludeDuplicateDates =
                parse_code(*row->include_duplicate_dates, bool__count, bool_::N);
        for (const auto& date_row : schedule_dates)
            if (date_row.owner_role == owner_role && date_row.owner_number == owner_number &&
                date_row.schedule_role == schedule_role &&
                date_row.schedule_sequence_number == row->sequence_number)
                d.Dates.Date.push_back(make_date(date_row.schedule_date));
        sd.Dates.push_back(std::move(d));
    }
    return sd;
}

/**
 * The exercise dates the option owner stated, in document order.
 */
std::vector<std::chrono::year_month_day>
exercise_dates_of(const std::vector<trading::domain::instrument_schedule_date>& schedule_dates) {
    std::vector<const trading::domain::instrument_schedule_date*> rows;
    for (const auto& row : schedule_dates)
        if (row.owner_role == "option" && row.owner_number == 1 &&
            row.schedule_role == "exercise_dates")
            rows.push_back(&row);
    std::sort(rows.begin(), rows.end(), [](const auto* a, const auto* b) {
        return a->sequence_number < b->sequence_number;
    });
    std::vector<std::chrono::year_month_day> out;
    for (const auto* row : rows)
        out.push_back(row->schedule_date);
    return out;
}

/**
 * The option block, its children and its exercise-date schedule, as rows.
 */
void append_option_block(trading::domain::swap_instrument_data& result, const optionData& od) {
    trading::domain::instrument_option option;
    option.long_short = od.LongShort;
    if (od.OptionType)
        option.option_type = std::string(*od.OptionType);
    if (od.PayoffType)
        option.payoff_type = std::string(*od.PayoffType);
    if (od.PayoffType2)
        option.payoff_type_2 = std::string(*od.PayoffType2);
    if (od.Style)
        option.style = std::string(*od.Style);
    if (od.NoticePeriod)
        option.notice_period = std::string(*od.NoticePeriod);
    if (od.NoticeCalendar)
        option.notice_calendar = std::string(*od.NoticeCalendar);
    if (od.NoticeConvention)
        option.notice_convention = std::string(*od.NoticeConvention);
    if (od.MidCouponExercise)
        option.mid_coupon_exercise = std::string(*od.MidCouponExercise);
    if (od.Settlement)
        option.settlement = to_string(*od.Settlement);
    if (od.SettlementMethod)
        option.settlement_method = to_string(*od.SettlementMethod);
    if (od.PayOffAtExpiry)
        option.pay_off_at_expiry = std::string(*od.PayOffAtExpiry);
    if (od.PremiumAmount)
        option.premium_amount = to_optional_decimal(std::string(*od.PremiumAmount));
    if (od.PremiumCurrency)
        option.premium_currency = std::string(*od.PremiumCurrency);
    if (od.PremiumPayDate)
        option.premium_pay_date = to_optional_domain_date(std::string(*od.PremiumPayDate));
    if (od.ExerciseFeeSettlementPeriod)
        option.exercise_fee_settlement_period = std::string(*od.ExerciseFeeSettlementPeriod);
    if (od.ExerciseFeeSettlementCalendar)
        option.exercise_fee_settlement_calendar = std::string(*od.ExerciseFeeSettlementCalendar);
    if (od.ExerciseFeeSettlementConvention)
        option.exercise_fee_settlement_convention =
            std::string(*od.ExerciseFeeSettlementConvention);
    if (od.AutomaticExercise)
        option.automatic_exercise = to_string(*od.AutomaticExercise);

    option.has_exercise_data = static_cast<bool>(od.ExerciseData);
    if (od.ExerciseData) {
        option.exercise_date = to_domain_date(std::string(od.ExerciseData->Date));
        if (od.ExerciseData->Price)
            option.exercise_price =
                ores::utility::decimal::decimal::from_double(*od.ExerciseData->Price).value();
    }

    option.has_payment_data = static_cast<bool>(od.PaymentData);
    if (od.PaymentData && od.PaymentData->Rules) {
        const auto& rules = *od.PaymentData->Rules;
        option.payment_lag = static_cast<std::int64_t>(rules.Lag);
        option.payment_calendar = std::string(rules.Calendar);
        option.payment_convention = to_string(rules.Convention);
        if (rules.RelativeTo)
            option.payment_relative_to = to_string(*rules.RelativeTo);
    }

    option.has_settlement_data = static_cast<bool>(od.SettlementData);
    if (od.SettlementData) {
        option.settlement_pay_currency = to_string(od.SettlementData->PayCurrency);
        option.settlement_fx_index = std::string(od.SettlementData->FXIndex);
        if (od.SettlementData->FixingDate)
            option.settlement_fixing_date =
                to_optional_domain_date(std::string(*od.SettlementData->FixingDate));
    }
    stamp_child_audit(option);
    result.options.push_back(std::move(option));

    int sequence_number = 0;
    if (od.Premiums)
        for (const auto& p : od.Premiums->Premium) {
            trading::domain::instrument_option_premium premium;
            premium.sequence_number = ++sequence_number;
            premium.amount = ores::utility::decimal::decimal::from_double(p.Amount).value();
            premium.currency = std::string(p.Currency);
            premium.pay_date = to_domain_date(std::string(p.PayDate));
            premium.has_settlement = static_cast<bool>(p.SettlementData);
            if (p.SettlementData) {
                premium.settlement_pay_currency = to_string(p.SettlementData->PayCurrency);
                premium.settlement_fx_index = std::string(p.SettlementData->FXIndex);
                if (p.SettlementData->FixingDate)
                    premium.settlement_fixing_date =
                        to_optional_domain_date(std::string(*p.SettlementData->FixingDate));
            }
            stamp_child_audit(premium);
            result.option_premiums.push_back(std::move(premium));
        }

    sequence_number = 0;
    if (od.ExerciseFees)
        for (const auto& f : od.ExerciseFees->ExerciseFee) {
            trading::domain::instrument_option_exercise_fee fee;
            fee.sequence_number = ++sequence_number;
            fee.amount = ores::utility::decimal::decimal::from_double(f).value();
            if (f.type)
                fee.type = std::string(*f.type);
            if (f.startDate)
                fee.start_date = to_optional_domain_date(std::string(*f.startDate));
            if (f.currency)
                fee.currency = std::string(*f.currency);
            stamp_child_audit(fee);
            result.option_exercise_fees.push_back(std::move(fee));
        }

    int payment_number = 0;
    if (od.PaymentData && od.PaymentData->Dates)
        for (const auto& date : od.PaymentData->Dates->Date) {
            trading::domain::instrument_option_payment_date payment;
            payment.sequence_number = ++payment_number;
            payment.payment_date = to_domain_date(std::string(date));
            stamp_child_audit(payment);
            result.option_payment_dates.push_back(std::move(payment));
        }

    // The exercise price list pairs one price with each exercise date the
    // group states, in order. The two lists are stored apart, so a price
    // with no date beside it has no row.
    const auto exercise_dates =
        od.exerciseDatesGroup && od.exerciseDatesGroup->ExerciseDates ?
            [&] {
                std::vector<std::chrono::year_month_day> out;
                for (const auto& date : od.exerciseDatesGroup->ExerciseDates->ExerciseDate)
                    out.push_back(to_domain_date(std::string(date)));
                return out;
            }() :
            std::vector<std::chrono::year_month_day>{};
    if (od.ExercisePrices) {
        const auto prices = split_numbers(std::string(*od.ExercisePrices));
        for (std::size_t i = 0; i < prices.size() && i < exercise_dates.size(); ++i) {
            auto parsed = ores::utility::decimal::decimal::from_string(prices[i]);
            if (!parsed)
                continue;
            trading::domain::instrument_option_exercise_price price;
            price.sequence_number = static_cast<int>(i) + 1;
            price.exercise_date = exercise_dates[i];
            price.price = std::move(*parsed);
            stamp_child_audit(price);
            result.option_exercise_prices.push_back(std::move(price));
        }
    }

    // The exercise dates live in the shared schedule tables under the
    // option owner, whether the document stated a list or a schedule.
    if (od.exerciseDatesGroup) {
        if (od.exerciseDatesGroup->ExerciseDates) {
            trading::domain::instrument_schedule row;
            row.owner_role = "option";
            row.owner_number = 1;
            row.schedule_role = "exercise_dates";
            row.sequence_number = 1;
            row.schedule_kind = "dates";
            stamp_child_audit(row);
            result.schedules.push_back(std::move(row));
            int date_number = 0;
            for (const auto& date : od.exerciseDatesGroup->ExerciseDates->ExerciseDate) {
                trading::domain::instrument_schedule_date date_row;
                date_row.owner_role = "option";
                date_row.owner_number = 1;
                date_row.schedule_role = "exercise_dates";
                date_row.schedule_sequence_number = 1;
                date_row.sequence_number = ++date_number;
                date_row.schedule_date = to_domain_date(std::string(date));
                stamp_child_audit(date_row);
                result.schedule_dates.push_back(std::move(date_row));
            }
        } else if (od.exerciseDatesGroup->ExerciseSchedule) {
            append_schedule(
                result, *od.exerciseDatesGroup->ExerciseSchedule, "option", 1, "exercise_schedule");
        }
    }
}

/**
 * The option block and its children, written back into the document's
 * option element. The exercise-date group is set by the caller, because it
 * is a choice between a date list and a schedule.
 */
void reverse_option_block(
    const std::vector<trading::domain::instrument_option>& options,
    const std::vector<trading::domain::instrument_option_premium>& premiums,
    const std::vector<trading::domain::instrument_option_exercise_fee>& fees,
    const std::vector<trading::domain::instrument_option_payment_date>& payment_dates,
    const std::vector<trading::domain::instrument_option_exercise_price>& exercise_prices,
    optionData& od) {
    if (options.empty())
        return;
    const auto& row = options.front();

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
        od.SettlementMethod = parse_code(
            *row.settlement_method, settlement_method_count, settlementMethod::PhysicalOTC);
    set_present_text(od.PayOffAtExpiry, row.pay_off_at_expiry);
    set_present_decimal(od.PremiumAmount, row.premium_amount);
    set_present_text(od.PremiumCurrency, row.premium_currency);
    if (row.premium_pay_date)
        set_present_text(od.PremiumPayDate, to_ore_date(*row.premium_pay_date));
    set_present_text(od.ExerciseFeeSettlementPeriod, row.exercise_fee_settlement_period);
    set_present_text(od.ExerciseFeeSettlementCalendar, row.exercise_fee_settlement_calendar);
    set_present_text(od.ExerciseFeeSettlementConvention, row.exercise_fee_settlement_convention);
    if (row.automatic_exercise)
        od.AutomaticExercise = parse_code(*row.automatic_exercise, bool__count, bool_::N);

    if (!premiums.empty()) {
        premiumData block;
        for (const auto& p : premiums) {
            premiumData_Premium_t v;
            v.Amount = static_cast<float>(p.amount.to_double());
            set_text(v.Currency, p.currency);
            set_text(v.PayDate, to_ore_date(p.pay_date));
            if (p.has_settlement) {
                premiumData_Premium_t_SettlementData_t s;
                s.PayCurrency = parse_currency_code(p.settlement_pay_currency.value_or(""));
                set_text(s.FXIndex, p.settlement_fx_index.value_or(""));
                if (p.settlement_fixing_date)
                    set_present_text(s.FixingDate, to_ore_date(*p.settlement_fixing_date));
                v.SettlementData = std::move(s);
            }
            block.Premium.push_back(std::move(v));
        }
        od.Premiums = std::move(block);
    }

    if (!fees.empty()) {
        optionData_ExerciseFees_t block;
        for (const auto& f : fees) {
            optionData_ExerciseFees_t_ExerciseFee_t v;
            static_cast<float&>(v) = static_cast<float>(f.amount.to_double());
            if (f.type)
                v.type = *f.type;
            if (f.start_date)
                v.startDate = to_ore_date(*f.start_date);
            if (f.currency)
                v.currency = *f.currency;
            block.ExerciseFee.push_back(std::move(v));
        }
        od.ExerciseFees = std::move(block);
    }

    if (row.has_exercise_data) {
        optionExerciseData e;
        set_text(e.Date, to_ore_date(row.exercise_date));
        if (row.exercise_price)
            e.Price = row.exercise_price->to_double();
        od.ExerciseData = std::move(e);
    }

    if (row.has_payment_data) {
        optionPaymentData payment;
        if (!payment_dates.empty()) {
            optionPaymentData_Dates_t dates;
            for (const auto& date : payment_dates)
                dates.Date.push_back(make_date(date.payment_date));
            payment.Dates = std::move(dates);
        }
        if (row.payment_lag || row.payment_calendar || row.payment_convention) {
            optionPaymentData_Rules_t rules;
            rules.Lag =
                static_cast<std::uint64_t>(std::max<std::int64_t>(row.payment_lag.value_or(0), 0));
            set_text(rules.Calendar, row.payment_calendar.value_or(""));
            rules.Convention = parse_code(row.payment_convention.value_or(""),
                                          business_day_convention_count,
                                          businessDayConvention::MF);
            if (row.payment_relative_to)
                rules.RelativeTo = parse_code(*row.payment_relative_to,
                                              option_pay_relative_to_count,
                                              optionPayRelativeTo::Expiry);
            payment.Rules = std::move(rules);
        }
        od.PaymentData = std::move(payment);
    }

    if (row.has_settlement_data) {
        optionData_SettlementData_t s;
        s.PayCurrency = parse_currency_code(row.settlement_pay_currency.value_or(""));
        set_text(s.FXIndex, row.settlement_fx_index.value_or(""));
        if (row.settlement_fixing_date)
            set_present_text(s.FixingDate, to_ore_date(*row.settlement_fixing_date));
        od.SettlementData = std::move(s);
    }

    if (!exercise_prices.empty()) {
        std::string text;
        for (const auto& p : exercise_prices) {
            if (!text.empty())
                text += ' ';
            text += p.price.to_string();
        }
        set_present_text(od.ExercisePrices, text);
    }
}

} // namespace

// ---------------------------------------------------------------------------
// Forward mapping: legData (swap) → swap_leg
// ---------------------------------------------------------------------------

void swap_instrument_mapper::append_leg(trading::domain::swap_instrument_data& result,
                                        const legData& ld,
                                        int leg_number) {
    result.legs.push_back(map_leg(ld, leg_number));
    for (auto& amount : map_leg_amounts(ld, leg_number))
        result.leg_amounts.push_back(std::move(amount));
    for (auto& rate : map_leg_rates(ld, leg_number))
        result.leg_rates.push_back(std::move(rate));
}

swap_leg swap_instrument_mapper::map_leg(const legData& ld, int leg_number) {
    swap_leg sl;
    auto& id = sl.identity;
    auto& tm = sl;
    auto& au = sl.audit;
    id.leg_number = leg_number;
    tm.payer = ld.Payer;
    tm.leg_type_code = to_string(ld.LegType);

    if (ld.Currency)
        tm.currency = std::string(*ld.Currency);
    tm.day_count_fraction_code = day_counter_string(ld.DayCounter);
    tm.business_day_convention_code = bdc_string(ld.PaymentConvention);
    tm.payment_frequency_code = tenor_to_payment_frequency(first_tenor(ld.ScheduleData));

    if (ld.legDataType) {
        const auto& ldt = *ld.legDataType;
        if (ldt.FloatingLegData)
            tm.floating_index_code = std::string(ldt.FloatingLegData->Index);
    }

    au.modified_by = "ores";
    au.performed_by = "ores";
    au.change_reason_code = "system.external_data_import";
    au.change_commentary = "Imported from ORE XML";
    return sl;
}

std::vector<ores::trading::domain::swap_leg_amount>
swap_instrument_mapper::map_leg_amounts(const legData& ld, int leg_number) {
    std::vector<ores::trading::domain::swap_leg_amount> amounts;

    const auto append = [&](double value, const std::string& start_date) {
        auto a = make_leg_amount(leg_number,
                                 static_cast<int>(amounts.size()) + 1,
                                 ores::utility::decimal::decimal::from_double(value).value());
        if (!start_date.empty())
            a.start_date = to_domain_date(start_date);
        amounts.push_back(std::move(a));
    };

    // The document states its notional either as a plain list, each entry with
    // an optional start date, or as an amortisation schedule. The schedule is
    // the richer arm and states its own end date and frequency, which this row
    // does not carry yet, so its value and start date are read and the
    // remainder is recorded as a follow-up.
    if (ld.Notionals) {
        for (const auto& n : ld.Notionals->Notional) {
            const std::string from = n.startDate ? std::string(*n.startDate) : std::string{};
            append(static_cast<double>(static_cast<float>(n)), from);
        }
    } else if (ld.Amortizations) {
        for (const auto& a : ld.Amortizations->AmortizationData) {
            const std::string from = a.StartDate ? std::string(*a.StartDate) : std::string{};
            append(a.Value ? static_cast<double>(*a.Value) : 0.0, from);
        }
    }

    return amounts;
}

std::vector<ores::trading::domain::swap_leg_rate>
swap_instrument_mapper::map_leg_rates(const legData& ld, int leg_number) {
    std::vector<ores::trading::domain::swap_leg_rate> rates;

    const auto append = [&](const std::string& role, double value) {
        rates.push_back(make_leg_rate(leg_number, role, static_cast<int>(rates.size()) + 1, value));
    };

    if (ld.legDataType) {
        const auto& ldt = *ld.legDataType;
        if (ldt.FixedLegData) {
            for (const auto& rate : ldt.FixedLegData->Rates.Rate)
                append("fixed", static_cast<double>(rate));
        }
        if (ldt.FloatingLegData && ldt.FloatingLegData->Spreads) {
            for (const auto& spread : ldt.FloatingLegData->Spreads->Spread)
                append("spread", static_cast<double>(spread));
        }
    }

    return rates;
}

// ---------------------------------------------------------------------------
// Forward: Swap / CrossCurrencySwap
// ---------------------------------------------------------------------------

trading::domain::swap_instrument_data swap_instrument_mapper::forward_swap(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping swap: " << std::string(t.id);

    const swapData* sd = nullptr;
    // The schema states a cross-currency swap in its own element of the same
    // type as a plain swap, so which one the document used is the product
    // type and has to be kept: the two are different products, not two
    // spellings of one.
    bool cross_currency = false;

    if (t.SwapData) {
        sd = &*t.SwapData;
    } else if (t.CrossCurrencySwapData) {
        sd = &*t.CrossCurrencySwapData;
        cross_currency = true;
    }

    trading::domain::rate_instrument header;
    header.identity.trade_type_code = cross_currency ? "CrossCurrencySwap" : "Swap";
    header.audit.modified_by = "ores";
    header.audit.performed_by = "ores";
    header.audit.change_reason_code = "system.external_data_import";
    header.audit.change_commentary = "Imported from ORE XML";

    trading::domain::swap_instrument_data result;
    result.header = std::move(header);
    result.facts = vanilla_swap_instrument{};

    if (!sd)
        return result;

    if (!sd->LegData.empty()) {
        result.header.start_date = start_date_from_schedule(sd->LegData.front().ScheduleData);
        result.header.maturity_date = end_date_from_schedule(sd->LegData.front().ScheduleData);
    }

    int leg_num = 1;
    for (const auto& ld : sd->LegData)
        append_leg(result, ld, leg_num++);

    return result;
}

// ---------------------------------------------------------------------------
// Forward: KnockOutSwap
// ---------------------------------------------------------------------------

trading::domain::swap_instrument_data
swap_instrument_mapper::forward_knock_out_swap(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping KnockOutSwap: " << std::string(t.id);

    trading::domain::rate_instrument header;
    header.identity.trade_type_code = "KnockOutSwap";
    header.audit.modified_by = "ores";
    header.audit.performed_by = "ores";
    header.audit.change_reason_code = "system.external_data_import";
    header.audit.change_commentary = "Imported from ORE XML";

    trading::domain::swap_instrument_data result;
    result.header = std::move(header);
    result.facts = knock_out_swap_instrument{};

    if (!t.KnockOutSwapData)
        return result;
    const auto& sd = *t.KnockOutSwapData;

    auto& ki = std::get<knock_out_swap_instrument>(result.facts);

    ki.barrier_type = to_string(sd.BarrierData.Type);
    ki.barrier_start_date = to_domain_date(std::string(sd.BarrierStartDate));
    if (!sd.BarrierData.Levels.Level.empty())
        ki.barrier_level = static_cast<double>(sd.BarrierData.Levels.Level.front());

    if (!sd.LegData.empty()) {
        result.header.start_date = start_date_from_schedule(sd.LegData.front().ScheduleData);
        result.header.maturity_date = end_date_from_schedule(sd.LegData.front().ScheduleData);
    }

    int leg_num = 1;
    for (const auto& ld : sd.LegData)
        append_leg(result, ld, leg_num++);

    return result;
}

// ---------------------------------------------------------------------------
// Forward: InflationSwap
// ---------------------------------------------------------------------------

trading::domain::swap_instrument_data
swap_instrument_mapper::forward_inflation_swap(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping InflationSwap: " << std::string(t.id);

    trading::domain::rate_instrument header;
    header.identity.trade_type_code = "InflationSwap";
    header.audit.modified_by = "ores";
    header.audit.performed_by = "ores";
    header.audit.change_reason_code = "system.external_data_import";
    header.audit.change_commentary = "Imported from ORE XML";

    trading::domain::swap_instrument_data result;
    result.header = std::move(header);
    result.facts = inflation_swap_instrument{};

    if (!t.InflationSwapData)
        return result;
    const auto& sd = *t.InflationSwapData;

    if (!sd.LegData.empty()) {
        result.header.start_date = start_date_from_schedule(sd.LegData.front().ScheduleData);
        result.header.maturity_date = end_date_from_schedule(sd.LegData.front().ScheduleData);
    }

    int leg_num = 1;
    for (const auto& ld : sd.LegData)
        append_leg(result, ld, leg_num++);

    return result;
}

// ---------------------------------------------------------------------------
// Forward: ForwardRateAgreement
// ---------------------------------------------------------------------------

trading::domain::swap_instrument_data swap_instrument_mapper::forward_fra(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FRA: " << std::string(t.id);

    trading::domain::rate_instrument header;
    header.identity.trade_type_code = "ForwardRateAgreement";
    header.audit.modified_by = "ores";
    header.audit.performed_by = "ores";
    header.audit.change_reason_code = "system.external_data_import";
    header.audit.change_commentary = "Imported from ORE XML";

    trading::domain::swap_instrument_data result;
    result.header = std::move(header);
    result.facts = fra_instrument{};

    if (!t.ForwardRateAgreementData)
        return result;

    const auto& fra = *t.ForwardRateAgreementData;
    auto& fi = std::get<fra_instrument>(result.facts);

    result.header.start_date = to_domain_date(std::string(fra.StartDate));
    result.header.maturity_date = to_domain_date(std::string(fra.EndDate));
    fi.currency = to_string(fra.Currency);
    fi.notional =
        ores::utility::decimal::decimal::from_double(static_cast<double>(fra.Notional)).value();
    fi.rate_index = std::string(fra.Index);
    fi.strike = static_cast<double>(fra.Strike);
    fi.long_short = "Long";

    swap_leg sl;
    sl.identity.leg_number = 1;
    sl.leg_type_code = "Fixed";
    sl.currency = to_string(fra.Currency);
    sl.floating_index_code = std::string(fra.Index);
    sl.audit.modified_by = "ores";
    sl.audit.performed_by = "ores";
    sl.audit.change_reason_code = "system.external_data_import";
    sl.audit.change_commentary = "Imported from ORE XML";
    const int fra_leg_number = sl.identity.leg_number;
    result.legs.push_back(std::move(sl));

    result.leg_amounts.push_back(make_leg_amount(
        fra_leg_number,
        1,
        ores::utility::decimal::decimal::from_double(static_cast<double>(fra.Notional)).value()));
    result.leg_rates.push_back(
        make_leg_rate(fra_leg_number, "fixed", 1, static_cast<double>(fra.Strike)));

    return result;
}

// ---------------------------------------------------------------------------
// Forward: CapFloor
// ---------------------------------------------------------------------------

trading::domain::swap_instrument_data swap_instrument_mapper::forward_capfloor(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping capfloor: " << std::string(t.id);

    trading::domain::rate_instrument header;
    header.identity.trade_type_code = "CapFloor";
    header.audit.modified_by = "ores";
    header.audit.performed_by = "ores";
    header.audit.change_reason_code = "system.external_data_import";
    header.audit.change_commentary = "Imported from ORE XML";

    trading::domain::swap_instrument_data result;
    result.header = std::move(header);
    result.facts = cap_floor_instrument{};

    if (!t.CapFloorData)
        return result;

    const auto& cf = *t.CapFloorData;

    result.header.start_date = start_date_from_schedule(cf.LegData.ScheduleData);
    result.header.maturity_date = end_date_from_schedule(cf.LegData.ScheduleData);

    swap_leg sl;
    sl.identity.leg_number = 1;
    sl.leg_type_code = to_string(cf.LegData.LegType);
    sl.currency = to_string(cf.LegData.Currency);
    sl.day_count_fraction_code = to_string(cf.LegData.DayCounter);
    sl.business_day_convention_code = bdc_string(cf.LegData.PaymentConvention);
    sl.payment_frequency_code = tenor_to_payment_frequency(first_tenor(cf.LegData.ScheduleData));

    if (cf.LegData.legDataType.FloatingLegData)
        sl.floating_index_code = std::string(cf.LegData.legDataType.FloatingLegData->Index);

    sl.audit.modified_by = "ores";
    sl.audit.performed_by = "ores";
    sl.audit.change_reason_code = "system.external_data_import";
    sl.audit.change_commentary = "Imported from ORE XML";
    const int cap_floor_leg_number = sl.identity.leg_number;
    result.legs.push_back(std::move(sl));

    int cap_floor_amount_number = 0;
    for (const auto& notional : cf.LegData.Notionals.Notional) {
        result.leg_amounts.push_back(make_leg_amount(
            cap_floor_leg_number,
            ++cap_floor_amount_number,
            ores::utility::decimal::decimal::from_double(static_cast<double>(notional)).value()));
    }
    if (cf.LegData.legDataType.FixedLegData) {
        int rate_number = 0;
        for (const auto& rate : cf.LegData.legDataType.FixedLegData->Rates.Rate) {
            result.leg_rates.push_back(make_leg_rate(
                cap_floor_leg_number, "fixed", ++rate_number, static_cast<double>(rate)));
        }
    }
    if (cf.LegData.legDataType.FloatingLegData && cf.LegData.legDataType.FloatingLegData->Spreads) {
        int spread_number = 0;
        for (const auto& spread : cf.LegData.legDataType.FloatingLegData->Spreads->Spread) {
            result.leg_rates.push_back(make_leg_rate(
                cap_floor_leg_number, "spread", ++spread_number, static_cast<double>(spread)));
        }
    }

    // The cap/floor's strikes and the schedule of its underlying swap leg
    // are shared children, keyed to the same trade.
    append_capfloor_strike(result, cf);
    append_schedule(result, cf.LegData.ScheduleData, "swap", cap_floor_leg_number, "schedule");

    return result;
}

// ---------------------------------------------------------------------------
// Reverse: swap_leg → legData
// ---------------------------------------------------------------------------

legData swap_instrument_mapper::reverse_leg(
    const std::optional<std::chrono::year_month_day>& start_date,
    const std::optional<std::chrono::year_month_day>& maturity_date,
    const swap_leg& sl,
    const std::vector<ores::trading::domain::swap_leg_amount>& amounts,
    const std::vector<ores::trading::domain::swap_leg_rate>& rates) {
    legData ld;

    const auto& tm = sl;
    const auto leg_type = leg_type_from_string(tm.leg_type_code);
    if (!leg_type)
        throw std::runtime_error("reverse_leg: unrecognised leg type '" + tm.leg_type_code +
                                 "' — cannot produce valid ORE XML");
    ld.LegType = *leg_type;
    ld.Payer = tm.payer.value_or(false);
    ld.Currency = tm.currency;

    if (!amounts.empty()) {
        legData_Notionals_t n;
        for (const auto& a : amounts) {
            legData_Notionals_t_Notional_t v;
            static_cast<float&>(v) = static_cast<float>(a.amount.to_double());
            if (a.start_date)
                v.startDate = to_ore_date(*a.start_date);
            n.Notional.push_back(v);
        }
        ld.Notionals = std::move(n);
    }

    ld.ScheduleData = make_schedule(
        start_date, maturity_date, payment_frequency_to_tenor(tm.payment_frequency_code));

    legDataType_group_t ldt;
    const auto has_role = [&](const char* role) {
        return std::any_of(
            rates.begin(), rates.end(), [&](const ores::trading::domain::swap_leg_rate& r) {
                return r.rate_role == role;
            });
    };
    if (ld.LegType == legType::Fixed && has_role("fixed")) {
        _FixedLegData_t fld;
        for (const auto& r : rates) {
            if (r.rate_role != "fixed")
                continue;
            _FixedLegData_t_Rates_t_Rate_t rate;
            static_cast<float&>(rate) = static_cast<float>(r.value);
            fld.Rates.Rate.push_back(rate);
        }
        ldt.FixedLegData = std::move(fld);
    } else if (ld.LegType == legType::Floating) {
        _FloatingLegData_t fld;
        fld.Index = tm.floating_index_code;
        spreads sp;
        for (const auto& r : rates) {
            if (r.rate_role != "spread")
                continue;
            floatWithAttribute sv;
            static_cast<float&>(sv) = static_cast<float>(r.value);
            sp.Spread.push_back(sv);
        }
        if (!sp.Spread.empty())
            fld.Spreads = std::move(sp);
        ldt.FloatingLegData = std::move(fld);
    }
    ld.legDataType = std::move(ldt);

    return ld;
}

legData_Notionals_t swap_instrument_mapper::make_notionals(double notional) {
    legData_Notionals_t n;
    legData_Notionals_t_Notional_t v;
    static_cast<float&>(v) = static_cast<float>(notional);
    n.Notional.push_back(v);
    return n;
}

// ---------------------------------------------------------------------------
// Reverse: Swap
// ---------------------------------------------------------------------------

trade swap_instrument_mapper::reverse_swap(
    const trading::domain::rate_instrument& header,
    [[maybe_unused]] const vanilla_swap_instrument& instr,
    const std::vector<swap_leg>& legs,
    const std::vector<ores::trading::domain::swap_leg_amount>& amounts,
    const std::vector<ores::trading::domain::swap_leg_rate>& rates) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping swap";

    // The product type the row states decides the element, because the
    // schema gives a plain swap and a cross-currency swap an element each,
    // and writing both as SwapData turns the second into the first.
    const bool cross_currency = header.identity.trade_type_code == "CrossCurrencySwap";

    trade t;
    t.TradeType = cross_currency ? oreTradeType::CrossCurrencySwap : oreTradeType::Swap;

    swapData sd;
    for (const auto& sl : legs)
        sd.LegData.push_back(reverse_leg(header.start_date,
                                         header.maturity_date,
                                         sl,
                                         amounts_for_leg(amounts, sl.identity.leg_number),
                                         rates_for_leg(rates, sl.identity.leg_number)));

    if (cross_currency)
        t.CrossCurrencySwapData = std::move(sd);
    else
        t.SwapData = std::move(sd);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: KnockOutSwap
// ---------------------------------------------------------------------------

namespace {

// The ORE barrierType set, read back from the code the table holds.
barrierType barrier_type_from_string(const std::string& code) {
    if (code == "UpAndOut")
        return barrierType::UpAndOut;
    if (code == "UpAndIn")
        return barrierType::UpAndIn;
    if (code == "DownAndIn")
        return barrierType::DownAndIn;
    if (code == "KnockIn")
        return barrierType::KnockIn;
    if (code == "KnockOut")
        return barrierType::KnockOut;
    if (code == "CumulatedProfitCap")
        return barrierType::CumulatedProfitCap;
    if (code == "CumulatedProfitCapPoints")
        return barrierType::CumulatedProfitCapPoints;
    if (code == "FixingCap")
        return barrierType::FixingCap;
    if (code == "FixingFloor")
        return barrierType::FixingFloor;
    return barrierType::DownAndOut;
}

}

trade swap_instrument_mapper::reverse_knock_out_swap(
    const trading::domain::rate_instrument& header,
    const knock_out_swap_instrument& instr,
    const std::vector<swap_leg>& legs,
    const std::vector<ores::trading::domain::swap_leg_amount>& amounts,
    const std::vector<ores::trading::domain::swap_leg_rate>& rates) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping KnockOutSwap";

    trade t;
    t.TradeType = oreTradeType::KnockOutSwap;

    knockOutSwapData d;
    d.BarrierData.Type = barrier_type_from_string(instr.barrier_type);
    static_cast<std::string&>(d.BarrierStartDate) = to_ore_date(instr.barrier_start_date);
    d.BarrierData.Levels.Level.push_back(static_cast<float>(instr.barrier_level));
    for (const auto& sl : legs)
        d.LegData.push_back(reverse_leg(header.start_date,
                                        header.maturity_date,
                                        sl,
                                        amounts_for_leg(amounts, sl.identity.leg_number),
                                        rates_for_leg(rates, sl.identity.leg_number)));

    t.KnockOutSwapData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: ForwardRateAgreement
// ---------------------------------------------------------------------------

trade swap_instrument_mapper::reverse_fra(
    const trading::domain::rate_instrument& header,
    const fra_instrument& instr,
    const std::vector<swap_leg>& legs,
    const std::vector<ores::trading::domain::swap_leg_amount>& amounts,
    const std::vector<ores::trading::domain::swap_leg_rate>& rates) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FRA";

    trade t;
    t.TradeType = oreTradeType::ForwardRateAgreement;

    forwardRateAgreementData fra;
    fra.StartDate = to_ore_date(header.start_date);
    fra.EndDate = to_ore_date(header.maturity_date);
    fra.Currency = parse_currency_code(instr.currency);
    fra.Notional = static_cast<float>(instr.notional.to_double());

    if (!legs.empty()) {
        const auto& tm = legs.front();
        static_cast<std::string&>(fra.Index) = tm.floating_index_code;
        const auto fixed = std::find_if(
            rates.begin(), rates.end(), [](const auto& r) { return r.rate_role == "fixed"; });
        if (fixed != rates.end())
            fra.Strike = static_cast<float>(fixed->value);
        fra.LongShort = longShort::Long;
    }
    // The leg's own notional is the one the document stated on the leg; the
    // instrument's copy is the fallback for a row written before the children.
    if (!amounts.empty())
        fra.Notional = static_cast<float>(amounts.front().amount.to_double());

    t.ForwardRateAgreementData = std::move(fra);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: CapFloor
// ---------------------------------------------------------------------------

trade swap_instrument_mapper::reverse_capfloor(
    const trading::domain::rate_instrument& header,
    [[maybe_unused]] const cap_floor_instrument& instr,
    const std::vector<swap_leg>& legs,
    const std::vector<ores::trading::domain::swap_leg_amount>& amounts,
    const std::vector<ores::trading::domain::swap_leg_rate>& rates,
    const std::vector<ores::trading::domain::instrument_strike>& strikes,
    const std::vector<ores::trading::domain::instrument_schedule>& schedules,
    const std::vector<ores::trading::domain::instrument_schedule_date>& schedule_dates) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping capfloor";

    trade t;
    t.TradeType = oreTradeType::CapFloor;

    capFloorData cf;
    cf.LongShort = longShort::Long;

    if (!legs.empty()) {
        const auto& sl = legs.front();

        const auto& tm = sl;
        cf.LegData.LegType = leg_type_from_string(tm.leg_type_code).value_or(legType::Floating);

        cf.LegData.Currency = parse_currency_code(tm.currency);

        cf.LegData.DayCounter = dayCounter::ACT_365;
        // The stored schedule is the document's own; a row written before
        // the schedule children existed falls back to the header's dates.
        if (const auto stored = schedule_for(schedules, schedule_dates, "swap", 1, "schedule"))
            cf.LegData.ScheduleData = *stored;
        else
            cf.LegData.ScheduleData =
                make_schedule(header.start_date,
                              header.maturity_date,
                              payment_frequency_to_tenor(tm.payment_frequency_code));
        if (!amounts.empty()) {
            for (const auto& amount : amounts) {
                legData_capfloor_Notionals_t_Notional_t nv;
                static_cast<float&>(nv) = static_cast<float>(amount.amount.to_double());
                cf.LegData.Notionals.Notional.push_back(nv);
            }
        }

        if (cf.LegData.LegType == legType::Floating) {
            _FloatingLegData_t fld;
            fld.Index = tm.floating_index_code;
            spreads sp;
            for (const auto& rate : rates) {
                if (rate.rate_role != "spread")
                    continue;
                floatWithAttribute sv;
                static_cast<float&>(sv) = static_cast<float>(rate.value);
                sp.Spread.push_back(sv);
            }
            if (!sp.Spread.empty())
                fld.Spreads = std::move(sp);
            cf.LegData.legDataType.FloatingLegData = std::move(fld);
        }
    }

    // The shared strike table holds one row per trade; the document states
    // the strike as a cap. A floor has no separate column to return to.
    if (!strikes.empty() && strikes.front().bare_value) {
        capFloorData_Caps_t caps;
        capFloorData_Caps_t_Cap_t cap;
        static_cast<float&>(cap) = static_cast<float>(strikes.front().bare_value->to_double());
        caps.Cap.push_back(cap);
        cf.Caps = std::move(caps);
    }

    t.CapFloorData = std::move(cf);
    return t;
}

// ---------------------------------------------------------------------------
// Forward: Swaption
// ---------------------------------------------------------------------------

trading::domain::swap_instrument_data swap_instrument_mapper::forward_swaption(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping Swaption: " << std::string(t.id);

    trading::domain::rate_instrument header;
    header.identity.trade_type_code = "Swaption";
    header.audit.modified_by = "ores";
    header.audit.performed_by = "ores";
    header.audit.change_reason_code = "system.external_data_import";
    header.audit.change_commentary = "Imported from ORE XML";

    trading::domain::swap_instrument_data result;
    result.header = std::move(header);
    result.facts = swaption_instrument{};

    if (!t.SwaptionData)
        return result;
    const auto& sd = *t.SwaptionData;

    auto& si = std::get<swaption_instrument>(result.facts);

    if (sd.OptionData) {
        const auto& od = *sd.OptionData;
        if (od.Style)
            si.exercise_type = std::string(*od.Style);
        if (od.exerciseDatesGroup && od.exerciseDatesGroup->ExerciseDates &&
            !od.exerciseDatesGroup->ExerciseDates->ExerciseDate.empty())
            si.expiry_date = to_domain_date(
                std::string(od.exerciseDatesGroup->ExerciseDates->ExerciseDate.front()));
        append_option_block(result, od);
    }

    int leg_num = 1;
    for (const auto& ld : sd.LegData)
        append_leg(result, ld, leg_num++);

    if (!result.legs.empty()) {
        if (!result.header.maturity_date) {
            const auto maturity = end_date_from_schedule(sd.LegData.front().ScheduleData);
            if (maturity.ok())
                result.header.maturity_date = maturity;
        }
        if (!result.header.start_date) {
            const auto start = start_date_from_schedule(sd.LegData.front().ScheduleData);
            if (start.ok())
                result.header.start_date = start;
        }
    }

    return result;
}

// ---------------------------------------------------------------------------
// Reverse: Swaption
// ---------------------------------------------------------------------------

trade swap_instrument_mapper::reverse_swaption(
    const trading::domain::rate_instrument& header,
    const swaption_instrument& instr,
    const std::vector<swap_leg>& legs,
    const std::vector<ores::trading::domain::swap_leg_amount>& amounts,
    const std::vector<ores::trading::domain::swap_leg_rate>& rates,
    const std::vector<ores::trading::domain::instrument_schedule>& schedules,
    const std::vector<ores::trading::domain::instrument_schedule_date>& schedule_dates,
    const std::vector<ores::trading::domain::instrument_option>& options,
    const std::vector<ores::trading::domain::instrument_option_premium>& option_premiums,
    const std::vector<ores::trading::domain::instrument_option_exercise_fee>& option_exercise_fees,
    const std::vector<ores::trading::domain::instrument_option_payment_date>& option_payment_dates,
    const std::vector<ores::trading::domain::instrument_option_exercise_price>&
        option_exercise_prices) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping Swaption";

    trade t;
    t.TradeType = oreTradeType::Swaption;

    swaptionData sd;

    optionData od;
    if (!options.empty()) {
        reverse_option_block(options,
                             option_premiums,
                             option_exercise_fees,
                             option_payment_dates,
                             option_exercise_prices,
                             od);
    } else {
        // A row written before the option child existed carries only the
        // fact row's style and expiry, which are the fallback here.
        static_cast<std::string&>(od.LongShort) = "Long";
        if (!instr.exercise_type.empty()) {
            optionData_Style_t style;
            static_cast<std::string&>(style) = instr.exercise_type;
            od.Style = std::move(style);
        }
        if (instr.expiry_date.ok()) {
            _ExerciseDates_t ed;
            ed.ExerciseDate.push_back(make_date(instr.expiry_date));
            exerciseDatesGroup_group_t edg;
            edg.ExerciseDates = std::move(ed);
            od.exerciseDatesGroup = std::move(edg);
        }
    }

    const auto exercise_dates = exercise_dates_of(schedule_dates);
    if (!exercise_dates.empty()) {
        _ExerciseDates_t ed;
        for (const auto& date : exercise_dates)
            ed.ExerciseDate.push_back(make_date(date));
        exerciseDatesGroup_group_t edg;
        edg.ExerciseDates = std::move(ed);
        od.exerciseDatesGroup = std::move(edg);
    } else if (const auto schedule =
                   schedule_for(schedules, schedule_dates, "option", 1, "exercise_schedule")) {
        exerciseDatesGroup_group_t edg;
        edg.ExerciseSchedule = *schedule;
        od.exerciseDatesGroup = std::move(edg);
    }
    sd.OptionData = std::move(od);

    for (const auto& sl : legs)
        sd.LegData.push_back(reverse_leg(header.start_date,
                                         header.maturity_date,
                                         sl,
                                         amounts_for_leg(amounts, sl.identity.leg_number),
                                         rates_for_leg(rates, sl.identity.leg_number)));

    t.SwaptionData = std::move(sd);
    return t;
}

// ---------------------------------------------------------------------------
// Forward: CallableSwap
// ---------------------------------------------------------------------------

trading::domain::swap_instrument_data
swap_instrument_mapper::forward_callable_swap(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping CallableSwap: " << std::string(t.id);

    trading::domain::rate_instrument header;
    header.identity.trade_type_code = "CallableSwap";
    header.audit.modified_by = "ores";
    header.audit.performed_by = "ores";
    header.audit.change_reason_code = "system.external_data_import";
    header.audit.change_commentary = "Imported from ORE XML";

    trading::domain::swap_instrument_data result;
    result.header = std::move(header);
    result.facts = callable_swap_instrument{};

    if (!t.CallableSwapData)
        return result;
    const auto& cd = *t.CallableSwapData;

    if (cd.OptionData && cd.OptionData->exerciseDatesGroup &&
        cd.OptionData->exerciseDatesGroup->ExerciseDates) {
        int sequence_number = 1;
        for (const auto& d : cd.OptionData->exerciseDatesGroup->ExerciseDates->ExerciseDate) {
            trading::domain::callable_swap_call_date call_date;
            call_date.sequence_number = sequence_number++;
            call_date.call_date = to_domain_date(std::string(d));
            call_date.modified_by = "ores";
            call_date.performed_by = "ores";
            call_date.change_reason_code = "system.external_data_import";
            call_date.change_commentary = "Imported from ORE XML";
            result.call_dates.push_back(std::move(call_date));
        }
    }

    int leg_num = 1;
    for (const auto& ld : cd.LegData)
        append_leg(result, ld, leg_num++);

    if (!result.legs.empty()) {
        result.header.start_date = start_date_from_schedule(cd.LegData.front().ScheduleData);
        result.header.maturity_date = end_date_from_schedule(cd.LegData.front().ScheduleData);
    }

    return result;
}

// ---------------------------------------------------------------------------
// Reverse: CallableSwap
// ---------------------------------------------------------------------------

trade swap_instrument_mapper::reverse_callable_swap(
    const trading::domain::rate_instrument& header,
    [[maybe_unused]] const callable_swap_instrument& instr,
    const std::vector<swap_leg>& legs,
    const std::vector<ores::trading::domain::swap_leg_amount>& amounts,
    const std::vector<ores::trading::domain::swap_leg_rate>& rates,
    const std::vector<trading::domain::callable_swap_call_date>& call_dates) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping CallableSwap";

    trade t;
    t.TradeType = oreTradeType::CallableSwap;

    callableSwapData cd;

    if (!call_dates.empty()) {
        optionData od;
        static_cast<std::string&>(od.LongShort) = "Long";
        _ExerciseDates_t ed;
        for (const auto& call_date : call_dates) {
            domain::date d;
            static_cast<std::string&>(d) =
                ores::platform::time::datetime::to_iso8601_date(call_date.call_date);
            ed.ExerciseDate.push_back(d);
        }
        exerciseDatesGroup_group_t edg;
        edg.ExerciseDates = std::move(ed);
        od.exerciseDatesGroup = std::move(edg);
        cd.OptionData = std::move(od);
    }

    for (const auto& sl : legs)
        cd.LegData.push_back(reverse_leg(header.start_date,
                                         header.maturity_date,
                                         sl,
                                         amounts_for_leg(amounts, sl.identity.leg_number),
                                         rates_for_leg(rates, sl.identity.leg_number)));

    t.CallableSwapData = std::move(cd);
    return t;
}

// ---------------------------------------------------------------------------
// Forward: FlexiSwap (leg economics only)
// ---------------------------------------------------------------------------

trading::domain::swap_instrument_data swap_instrument_mapper::forward_flexi_swap(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FlexiSwap: " << std::string(t.id);

    trading::domain::rate_instrument header;
    header.identity.trade_type_code = "FlexiSwap";
    header.audit.modified_by = "ores";
    header.audit.performed_by = "ores";
    header.audit.change_reason_code = "system.external_data_import";
    header.audit.change_commentary = "Imported from ORE XML";

    trading::domain::swap_instrument_data result;
    result.header = std::move(header);
    result.facts = vanilla_swap_instrument{};

    if (!t.FlexiSwapData)
        return result;
    const auto& fd = *t.FlexiSwapData;

    int leg_num = 1;
    for (const auto& ld : fd.LegData)
        append_leg(result, ld, leg_num++);

    if (!result.legs.empty()) {
        result.header.start_date = start_date_from_schedule(fd.LegData.front().ScheduleData);
        result.header.maturity_date = end_date_from_schedule(fd.LegData.front().ScheduleData);
    }

    return result;
}

// ---------------------------------------------------------------------------
// Forward: BalanceGuaranteedSwap (leg economics only)
// ---------------------------------------------------------------------------

trading::domain::swap_instrument_data
swap_instrument_mapper::forward_balance_guaranteed_swap(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping BalanceGuaranteedSwap: " << std::string(t.id);

    trading::domain::rate_instrument header;
    header.identity.trade_type_code = "BalanceGuaranteedSwap";
    header.audit.modified_by = "ores";
    header.audit.performed_by = "ores";
    header.audit.change_reason_code = "system.external_data_import";
    header.audit.change_commentary = "Imported from ORE XML";

    trading::domain::swap_instrument_data result;
    result.header = std::move(header);
    result.facts = balance_guaranteed_swap_instrument{};

    if (!t.BalanceGuaranteedSwapData)
        return result;
    const auto& bd = *t.BalanceGuaranteedSwapData;

    int leg_num = 1;
    for (const auto& ld : bd.LegData)
        append_leg(result, ld, leg_num++);

    if (!result.legs.empty()) {
        result.header.start_date = start_date_from_schedule(bd.LegData.front().ScheduleData);
        result.header.maturity_date = end_date_from_schedule(bd.LegData.front().ScheduleData);
    }

    return result;
}

} // namespace ores::ore::domain
