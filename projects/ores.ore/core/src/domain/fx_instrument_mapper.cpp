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
#include "ores.ore.core/domain/fx_instrument_mapper.hpp"
#include "ores.ore.core/domain/ore_boundary_decimal.hpp"
#include "ores.platform/time/datetime.hpp"
#include <chrono>
#include <iomanip>
#include <limits>
#include <map>
#include <optional>
#include <sstream>
#include <stdexcept>

namespace ores::ore::domain {

using namespace ores::logging;
using ores::trading::domain::fx_forward_instrument;
using ores::trading::domain::fx_vanilla_option_instrument;
using ores::trading::domain::fx_barrier_option_instrument;
using ores::trading::domain::fx_digital_option_instrument;
using ores::trading::domain::fx_asian_forward_instrument;
using ores::trading::domain::fx_accumulator_instrument;
using ores::trading::domain::fx_variance_swap_instrument;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

namespace {

// The trading domain holds a date as std::chrono::year_month_day, while the
// ORE XML holds its ISO-8601 spelling. An absent or empty ORE date maps to a
// default-constructed (invalid) calendar date, which renders back to an empty
// string, so a missing date round-trips as missing.
std::chrono::year_month_day to_domain_date(const std::string& s) {
    if (s.empty())
        return {};
    return ores::platform::time::datetime::from_iso8601_date(s);
}

std::optional<std::chrono::year_month_day> to_optional_domain_date(const std::string& s) {
    if (s.empty())
        return std::nullopt;
    return ores::platform::time::datetime::from_iso8601_date(s);
}

std::string to_ore_date(const std::chrono::year_month_day& d) {
    return d.ok() ? ores::platform::time::datetime::to_iso8601_date(d) : std::string{};
}

std::string to_ore_date(const std::optional<std::chrono::year_month_day>& d) {
    return d ? ores::platform::time::datetime::to_iso8601_date(*d) : std::string{};
}

std::string json_escape(const std::string& s) {
    std::string out;
    out.reserve(s.size());
    for (char c : s) {
        if (c == '"')
            out += "\\\"";
        else if (c == '\\')
            out += "\\\\";
        else
            out += c;
    }
    return out;
}

// A level renders with every digit a float carries, so reading it back yields
// the same float the document stated.
std::string to_json_number(float v) {
    std::ostringstream os;
    os << std::setprecision(std::numeric_limits<float>::max_digits10) << v;
    return os.str();
}

// A range bound holds only the levels the document states, so each entry is a
// JSON object carrying exactly those fields.
std::string range_bound_to_json(const rangeBound& rb) {
    std::vector<std::string> fields;
    if (rb.startDate)
        fields.push_back("\"StartDate\":\"" + json_escape(std::string(*rb.startDate)) + "\"");
    if (rb.RangeFrom)
        fields.push_back("\"RangeFrom\":" + to_json_number(*rb.RangeFrom));
    if (rb.RangeTo)
        fields.push_back("\"RangeTo\":" + to_json_number(*rb.RangeTo));
    if (rb.Leverage)
        fields.push_back("\"Leverage\":" + to_json_number(*rb.Leverage));
    if (rb.Strike)
        fields.push_back("\"Strike\":" + to_json_number(*rb.Strike));
    if (rb.StrikeAdjustment)
        fields.push_back("\"StrikeAdjustment\":" + to_json_number(*rb.StrikeAdjustment));

    std::string json = "{";
    for (std::size_t i = 0; i < fields.size(); ++i) {
        if (i != 0)
            json += ",";
        json += fields[i];
    }
    json += "}";
    return json;
}

std::string range_bounds_to_json(const xsd::vector<rangeBound>& bounds) {
    std::string json = "[";
    for (std::size_t i = 0; i < bounds.size(); ++i) {
        if (i != 0)
            json += ",";
        json += range_bound_to_json(bounds[i]);
    }
    json += "]";
    return json;
}

std::vector<rangeBound> parse_json_range_bounds(const std::string& json) {
    std::vector<rangeBound> result;
    std::size_t pos = 0;
    while (true) {
        const auto open = json.find('{', pos);
        if (open == std::string::npos)
            break;
        const auto close = json.find('}', open);
        if (close == std::string::npos)
            break;
        const auto body = json.substr(open + 1, close - open - 1);
        rangeBound rb;
        std::size_t p = 0;
        while (p < body.size()) {
            const auto key_open = body.find('"', p);
            if (key_open == std::string::npos)
                break;
            const auto key_close = body.find('"', key_open + 1);
            if (key_close == std::string::npos)
                break;
            const auto key = body.substr(key_open + 1, key_close - key_open - 1);
            const auto colon = body.find(':', key_close);
            if (colon == std::string::npos)
                break;
            if (key == "StartDate") {
                const auto value_open = body.find('"', colon);
                const auto value_close = value_open == std::string::npos ?
                                             std::string::npos :
                                             body.find('"', value_open + 1);
                if (value_close == std::string::npos)
                    break;
                xsd::string s;
                static_cast<std::string&>(s) =
                    body.substr(value_open + 1, value_close - value_open - 1);
                rb.startDate = std::move(s);
                p = value_close + 1;
                continue;
            }
            const auto value_end = body.find(',', colon);
            const auto value = body.substr(colon + 1,
                                           value_end == std::string::npos ? std::string::npos :
                                                                            value_end - colon - 1);
            const auto number = std::stof(value);
            if (key == "RangeFrom")
                rb.RangeFrom = number;
            else if (key == "RangeTo")
                rb.RangeTo = number;
            else if (key == "Leverage")
                rb.Leverage = number;
            else if (key == "Strike")
                rb.Strike = number;
            else if (key == "StrikeAdjustment")
                rb.StrikeAdjustment = number;
            p = value_end == std::string::npos ? body.size() : value_end + 1;
        }
        result.push_back(std::move(rb));
        pos = close + 1;
    }
    return result;
}

constexpr const char* k_modified_by = "ores";
constexpr const char* k_performed_by = "ores";
constexpr const char* k_change_reason = "system.external_data_import";
constexpr const char* k_change_comment = "Imported from ORE XML";

void set_audit(auto& r) {
    r.modified_by = k_modified_by;
    r.performed_by = k_performed_by;
    r.change_reason_code = k_change_reason;
    r.change_commentary = k_change_comment;
}

currencyCode parse_currency_code(const std::string& s) {
    static const std::map<std::string, currencyCode> map = {
        {"AED", currencyCode::AED}, {"AFN", currencyCode::AFN}, {"ALL", currencyCode::ALL},
        {"AMD", currencyCode::AMD}, {"ANG", currencyCode::ANG}, {"AOA", currencyCode::AOA},
        {"ARS", currencyCode::ARS}, {"AUD", currencyCode::AUD}, {"AWG", currencyCode::AWG},
        {"AZN", currencyCode::AZN}, {"BAM", currencyCode::BAM}, {"BBD", currencyCode::BBD},
        {"BDT", currencyCode::BDT}, {"BGN", currencyCode::BGN}, {"BHD", currencyCode::BHD},
        {"BIF", currencyCode::BIF}, {"BMD", currencyCode::BMD}, {"BND", currencyCode::BND},
        {"BOB", currencyCode::BOB}, {"BOV", currencyCode::BOV}, {"BRL", currencyCode::BRL},
        {"BSD", currencyCode::BSD}, {"BTN", currencyCode::BTN}, {"BWP", currencyCode::BWP},
        {"BYN", currencyCode::BYN}, {"BZD", currencyCode::BZD}, {"CAD", currencyCode::CAD},
        {"CDF", currencyCode::CDF}, {"CHE", currencyCode::CHE}, {"CHF", currencyCode::CHF},
        {"CHW", currencyCode::CHW}, {"CLF", currencyCode::CLF}, {"CLP", currencyCode::CLP},
        {"CNH", currencyCode::CNH}, {"CNT", currencyCode::CNT}, {"CNY", currencyCode::CNY},
        {"COP", currencyCode::COP}, {"COU", currencyCode::COU}, {"CRC", currencyCode::CRC},
        {"CUC", currencyCode::CUC}, {"CUP", currencyCode::CUP}, {"CVE", currencyCode::CVE},
        {"CYP", currencyCode::CYP}, {"CZK", currencyCode::CZK}, {"DJF", currencyCode::DJF},
        {"DKK", currencyCode::DKK}, {"DOP", currencyCode::DOP}, {"DZD", currencyCode::DZD},
        {"EGP", currencyCode::EGP}, {"ERN", currencyCode::ERN}, {"ETB", currencyCode::ETB},
        {"EUR", currencyCode::EUR}, {"FJD", currencyCode::FJD}, {"FKP", currencyCode::FKP},
        {"GBP", currencyCode::GBP}, {"GEL", currencyCode::GEL}, {"GGP", currencyCode::GGP},
        {"GHS", currencyCode::GHS}, {"GIP", currencyCode::GIP}, {"GMD", currencyCode::GMD},
        {"GNF", currencyCode::GNF}, {"GTQ", currencyCode::GTQ}, {"GYD", currencyCode::GYD},
        {"HKD", currencyCode::HKD}, {"HNL", currencyCode::HNL}, {"HRK", currencyCode::HRK},
        {"HTG", currencyCode::HTG}, {"HUF", currencyCode::HUF}, {"IDR", currencyCode::IDR},
        {"ILS", currencyCode::ILS}, {"IMP", currencyCode::IMP}, {"INR", currencyCode::INR},
        {"IQD", currencyCode::IQD}, {"IRR", currencyCode::IRR}, {"ISK", currencyCode::ISK},
        {"JEP", currencyCode::JEP}, {"JMD", currencyCode::JMD}, {"JOD", currencyCode::JOD},
        {"JPY", currencyCode::JPY}, {"KES", currencyCode::KES}, {"KGS", currencyCode::KGS},
        {"KHR", currencyCode::KHR}, {"KID", currencyCode::KID}, {"KMF", currencyCode::KMF},
        {"KPW", currencyCode::KPW}, {"KRW", currencyCode::KRW}, {"KWD", currencyCode::KWD},
        {"KYD", currencyCode::KYD}, {"KZT", currencyCode::KZT}, {"LAK", currencyCode::LAK},
        {"LBP", currencyCode::LBP}, {"LKR", currencyCode::LKR}, {"LRD", currencyCode::LRD},
        {"LSL", currencyCode::LSL}, {"LTL", currencyCode::LTL}, {"LVL", currencyCode::LVL},
        {"LYD", currencyCode::LYD}, {"MAD", currencyCode::MAD}, {"MDL", currencyCode::MDL},
        {"MGA", currencyCode::MGA}, {"MKD", currencyCode::MKD}, {"MMK", currencyCode::MMK},
        {"MNT", currencyCode::MNT}, {"MOP", currencyCode::MOP}, {"MRU", currencyCode::MRU},
        {"MUR", currencyCode::MUR}, {"MVR", currencyCode::MVR}, {"MWK", currencyCode::MWK},
        {"MXN", currencyCode::MXN}, {"MXV", currencyCode::MXV}, {"MYR", currencyCode::MYR},
        {"MZN", currencyCode::MZN}, {"NAD", currencyCode::NAD}, {"NGN", currencyCode::NGN},
        {"NIO", currencyCode::NIO}, {"NOK", currencyCode::NOK}, {"NPR", currencyCode::NPR},
        {"NZD", currencyCode::NZD}, {"OMR", currencyCode::OMR}, {"PAB", currencyCode::PAB},
        {"PEN", currencyCode::PEN}, {"PGK", currencyCode::PGK}, {"PHP", currencyCode::PHP},
        {"PKR", currencyCode::PKR}, {"PLN", currencyCode::PLN}, {"PYG", currencyCode::PYG},
        {"QAR", currencyCode::QAR}, {"RON", currencyCode::RON}, {"RSD", currencyCode::RSD},
        {"RUB", currencyCode::RUB}, {"RWF", currencyCode::RWF}, {"SAR", currencyCode::SAR},
        {"SBD", currencyCode::SBD}, {"SCR", currencyCode::SCR}, {"SDG", currencyCode::SDG},
        {"SEK", currencyCode::SEK}, {"SGD", currencyCode::SGD}, {"SHP", currencyCode::SHP},
        {"SLL", currencyCode::SLL}, {"SKK", currencyCode::SKK}, {"SOS", currencyCode::SOS},
        {"SRD", currencyCode::SRD}, {"SSP", currencyCode::SSP}, {"STN", currencyCode::STN},
        {"SVC", currencyCode::SVC}, {"SYP", currencyCode::SYP}, {"SZL", currencyCode::SZL},
        {"THB", currencyCode::THB}, {"TJS", currencyCode::TJS}, {"TMT", currencyCode::TMT},
        {"TND", currencyCode::TND}, {"TOP", currencyCode::TOP}, {"TRY", currencyCode::TRY},
        {"TTD", currencyCode::TTD}, {"TWD", currencyCode::TWD}, {"TZS", currencyCode::TZS},
        {"UAH", currencyCode::UAH}, {"UGX", currencyCode::UGX}, {"USD", currencyCode::USD},
        {"USN", currencyCode::USN}, {"UYI", currencyCode::UYI}, {"UYU", currencyCode::UYU},
        {"UYW", currencyCode::UYW}, {"UZS", currencyCode::UZS}, {"VES", currencyCode::VES},
        {"VND", currencyCode::VND}, {"VUV", currencyCode::VUV}, {"WST", currencyCode::WST},
        {"XAF", currencyCode::XAF}, {"XAG", currencyCode::XAG}, {"XAU", currencyCode::XAU},
        {"XCD", currencyCode::XCD}, {"XOF", currencyCode::XOF}, {"XPD", currencyCode::XPD},
        {"XPF", currencyCode::XPF}, {"XPT", currencyCode::XPT}, {"XSU", currencyCode::XSU},
        {"XUA", currencyCode::XUA}, {"YER", currencyCode::YER}, {"ZAR", currencyCode::ZAR},
        {"ZMW", currencyCode::ZMW}, {"ZWL", currencyCode::ZWL}, {"BTC", currencyCode::BTC},
        {"ETH", currencyCode::ETH}, {"XBT", currencyCode::XBT}, {"ETC", currencyCode::ETC},
        {"BCH", currencyCode::BCH}, {"XRP", currencyCode::XRP}, {"LTC", currencyCode::LTC},
        {"ZUR", currencyCode::ZUR}, {"ZUG", currencyCode::ZUG},
    };
    const auto it = map.find(s);
    if (it == map.end())
        throw std::runtime_error("parse_currency_code: unrecognised currency code '" + s +
                                 "' — cannot produce valid ORE XML");
    return it->second;
}

std::chrono::year_month_day expiry_date_from_single(const optionData& od) {
    if (!od.exerciseDatesGroup)
        return {};
    if (!od.exerciseDatesGroup->ExerciseDates)
        return {};
    if (od.exerciseDatesGroup->ExerciseDates->ExerciseDate.empty())
        return {};
    return ores::platform::time::datetime::from_iso8601_date(
        std::string(od.exerciseDatesGroup->ExerciseDates->ExerciseDate.front()));
}

std::string option_type_from_single(const optionData& od) {
    if (od.OptionType)
        return std::string(*od.OptionType);
    return {};
}

// BarrierData carries one level for a single barrier and two for a
// double barrier, lower first.
template <typename Lower, typename Upper>
void fill_barrier(const barrierData& bd, std::string& barrier_type, Lower& lower, Upper& upper) {
    barrier_type = to_string(bd.Type);
    const auto& levels = bd.Levels.Level;
    if (!levels.empty())
        lower = exact_decimal(levels.front());
    if (levels.size() > 1)
        upper = exact_decimal(levels[1]);
}

void validate_long_short(const std::string& v) {
    if (v != "Long" && v != "Short")
        throw std::invalid_argument("Unrecognised LongShort value: '" + v + "'");
}

void validate_option_type(const std::string& v) {
    if (v != "Call" && v != "Put")
        throw std::invalid_argument("Unrecognised OptionType value: '" + v + "'");
}

// Build an OptionData element from an option_type and its expiry date.
optionData make_option_entry(const std::string& option_type,
                             const std::chrono::year_month_day& expiry_date,
                             const std::string& long_short = "Long") {
    validate_long_short(long_short);
    optionData od;
    static_cast<std::string&>(od.LongShort) = long_short;
    if (!option_type.empty()) {
        validate_option_type(option_type);
        optionData_OptionType_t ot;
        static_cast<std::string&>(ot) = option_type;
        od.OptionType = std::move(ot);
    }
    if (expiry_date.ok()) {
        _ExerciseDates_t ed;
        date dt;
        static_cast<std::string&>(dt) =
            ores::platform::time::datetime::to_iso8601_date(expiry_date);
        ed.ExerciseDate.push_back(dt);
        exerciseDatesGroup_group_t edg;
        edg.ExerciseDates = std::move(ed);
        od.exerciseDatesGroup = std::move(edg);
    }
    return od;
}

underlyingTypes_group_t make_underlying_type_name(const std::string& name) {
    underlyingTypes_group_t u;
    _Name_t n;
    static_cast<std::string&>(n) = name;
    u.Name = std::move(n);
    return u;
}

} // namespace

// ---------------------------------------------------------------------------
// Private helper: make_barrier
// ---------------------------------------------------------------------------

barrierData fx_instrument_mapper::make_barrier(const std::string& type,
                                               const ores::utility::decimal::decimal& level) {
    static const std::map<std::string, barrierType> bmap = {
        {"UpAndOut", barrierType::UpAndOut},
        {"UpAndIn", barrierType::UpAndIn},
        {"DownAndOut", barrierType::DownAndOut},
        {"DownAndIn", barrierType::DownAndIn},
        {"KnockIn", barrierType::KnockIn},
        {"KnockOut", barrierType::KnockOut},
        {"CumulatedProfitCap", barrierType::CumulatedProfitCap},
        {"CumulatedProfitCapPoints", barrierType::CumulatedProfitCapPoints},
        {"FixingCap", barrierType::FixingCap},
        {"FixingFloor", barrierType::FixingFloor},
    };
    barrierData b;
    const auto it = bmap.find(type);
    if (it == bmap.end())
        throw std::runtime_error("make_barrier: unrecognized barrier type '" + type + "'");
    b.Type = it->second;
    push_money(b.Levels.Level, level);
    return b;
}

// The reverse of fill_barrier. Barrier types with a plain double lower
// level use zero to mean "no lower barrier", so a zero lower lets the
// upper level start the group on its own.
barrierData fx_instrument_mapper::make_barrier_group(
    const std::string& type,
    const std::optional<ores::utility::decimal::decimal>& lower,
    const std::optional<ores::utility::decimal::decimal>& upper) {
    const bool has_lower = lower.has_value() && *lower != ores::utility::decimal::decimal{};
    if (!has_lower && !upper.has_value())
        return {};
    barrierData b = make_barrier(type, has_lower ? *lower : *upper);
    if (has_lower && upper.has_value())
        push_money(b.Levels.Level, *upper);
    return b;
}

// ===========================================================================
// Forward: FxForward
// ===========================================================================

trading::domain::fx_forward_instrument fx_instrument_mapper::forward_fx_forward(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxForward: " << std::string(t.id);
    fx_forward_instrument r;
    r.identity.trade_type_code = "FxForward";
    set_audit(r.audit);
    if (!t.FxForwardData)
        return r;
    const auto& fwd = *t.FxForwardData;

    r.value_date = to_domain_date(std::string(fwd.ValueDate));
    r.bought_currency = to_string(fwd.BoughtCurrency);
    r.bought_amount = exact_decimal(fwd.BoughtAmount);
    r.sold_currency = to_string(fwd.SoldCurrency);
    r.sold_amount = exact_decimal(fwd.SoldAmount);
    if (fwd.Settlement)
        r.settlement = to_string(*fwd.Settlement);

    return r;
}

// ===========================================================================
// Forward: FxSwap
// ===========================================================================

trading::domain::fx_forward_instrument fx_instrument_mapper::forward_fx_swap(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxSwap: " << std::string(t.id);
    fx_forward_instrument r;
    r.identity.trade_type_code = "FxSwap";
    set_audit(r.audit);
    if (!t.FxSwapData)
        return r;
    const auto& sw = *t.FxSwapData;

    // Near leg only; far leg is a documented coverage gap.
    r.value_date = to_domain_date(std::string(sw.NearDate));
    r.bought_currency = to_string(sw.NearBoughtCurrency);
    r.bought_amount = exact_decimal(sw.NearBoughtAmount);
    r.sold_currency = to_string(sw.NearSoldCurrency);
    r.sold_amount = exact_decimal(sw.NearSoldAmount);
    if (sw.Settlement)
        r.settlement = to_string(*sw.Settlement);

    return r;
}

// ===========================================================================
// Forward: FxOption
// ===========================================================================

trading::domain::fx_vanilla_option_instrument
fx_instrument_mapper::forward_fx_option(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxOption: " << std::string(t.id);
    fx_vanilla_option_instrument r;
    r.identity.trade_type_code = "FxOption";
    set_audit(r.audit);
    if (!t.FxOptionData)
        return r;
    const auto& opt = *t.FxOptionData;

    r.bought_currency = to_string(opt.BoughtCurrency);
    r.bought_amount = exact_decimal(opt.BoughtAmount);
    r.sold_currency = to_string(opt.SoldCurrency);
    if (opt.SoldAmount)
        r.sold_amount = exact_decimal(*opt.SoldAmount);

    const auto& od = opt.OptionData;
    if (od.OptionType)
        r.option_type = std::string(*od.OptionType);
    if (od.Settlement)
        r.settlement = to_string(*od.Settlement);
    r.exercise_style = "European"; // default; American not detected here
    r.expiry_date = expiry_date_from_single(od);

    return r;
}

// ===========================================================================
// Forward: FxBarrierOption
// ===========================================================================

trading::domain::fx_barrier_option_instrument
fx_instrument_mapper::forward_fx_barrier_option(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxBarrierOption: " << std::string(t.id);
    fx_barrier_option_instrument r;
    r.identity.trade_type_code = "FxBarrierOption";
    set_audit(r.audit);
    if (!t.FxBarrierOptionData)
        return r;
    const auto& d = *t.FxBarrierOptionData;

    r.bought_currency = to_string(d.BoughtCurrency);
    r.bought_amount = exact_decimal(d.BoughtAmount);
    r.sold_currency = to_string(d.SoldCurrency);
    r.sold_amount = exact_decimal(d.SoldAmount);
    r.option_type = option_type_from_single(d.OptionData);
    r.expiry_date = expiry_date_from_single(d.OptionData);
    fill_barrier(d.BarrierData, r.barrier_type, r.lower_barrier, r.upper_barrier);
    return r;
}

// ===========================================================================
// Forward: FxDoubleBarrierOption
// ===========================================================================

trading::domain::fx_barrier_option_instrument
fx_instrument_mapper::forward_fx_double_barrier_option(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxDoubleBarrierOption: " << std::string(t.id);
    fx_barrier_option_instrument r;
    r.identity.trade_type_code = "FxDoubleBarrierOption";
    set_audit(r.audit);
    if (!t.FxDoubleBarrierOptionData)
        return r;
    const auto& d = *t.FxDoubleBarrierOptionData;

    r.bought_currency = to_string(d.BoughtCurrency);
    r.bought_amount = exact_decimal(d.BoughtAmount);
    r.sold_currency = to_string(d.SoldCurrency);
    r.sold_amount = exact_decimal(d.SoldAmount);
    r.option_type = option_type_from_single(d.OptionData);
    r.expiry_date = expiry_date_from_single(d.OptionData);
    fill_barrier(d.BarrierData, r.barrier_type, r.lower_barrier, r.upper_barrier);
    return r;
}

// ===========================================================================
// Forward: FxEuropeanBarrierOption
// ===========================================================================

trading::domain::fx_barrier_option_instrument
fx_instrument_mapper::forward_fx_european_barrier_option(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxEuropeanBarrierOption: " << std::string(t.id);
    fx_barrier_option_instrument r;
    r.identity.trade_type_code = "FxEuropeanBarrierOption";
    set_audit(r.audit);
    if (!t.FxEuropeanBarrierOptionData)
        return r;
    const auto& d = *t.FxEuropeanBarrierOptionData;

    r.bought_currency = to_string(d.BoughtCurrency);
    r.bought_amount = exact_decimal(d.BoughtAmount);
    r.sold_currency = to_string(d.SoldCurrency);
    r.sold_amount = exact_decimal(d.SoldAmount);
    r.option_type = option_type_from_single(d.OptionData);
    r.expiry_date = expiry_date_from_single(d.OptionData);
    fill_barrier(d.BarrierData, r.barrier_type, r.lower_barrier, r.upper_barrier);
    return r;
}

// ===========================================================================
// Forward: FxKIKOBarrierOption
// ===========================================================================

trading::domain::fx_barrier_option_instrument
fx_instrument_mapper::forward_fx_kiko_barrier_option(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxKIKOBarrierOption: " << std::string(t.id);
    fx_barrier_option_instrument r;
    r.identity.trade_type_code = "FxKIKOBarrierOption";
    set_audit(r.audit);
    if (!t.FxKIKOBarrierOptionData)
        return r;
    const auto& d = *t.FxKIKOBarrierOptionData;

    r.bought_currency = to_string(d.BoughtCurrency);
    r.bought_amount = exact_decimal(d.BoughtAmount);
    r.sold_currency = to_string(d.SoldCurrency);
    r.sold_amount = exact_decimal(d.SoldAmount);
    if (d.OptionData.OptionType)
        r.option_type = std::string(*d.OptionData.OptionType);
    r.expiry_date = expiry_date_from_single(d.OptionData);

    if (!d.Barriers.BarrierData.empty()) {
        r.barrier_type = to_string(d.Barriers.BarrierData.front().Type);
        if (!d.Barriers.BarrierData.front().Levels.Level.empty())
            r.lower_barrier = exact_decimal(d.Barriers.BarrierData.front().Levels.Level.front());
        if (d.Barriers.BarrierData.size() > 1 && !d.Barriers.BarrierData[1].Levels.Level.empty())
            r.upper_barrier = exact_decimal(d.Barriers.BarrierData[1].Levels.Level.front());
    }
    return r;
}

// ===========================================================================
// Forward: FxGenericBarrierOption
// ===========================================================================

trading::domain::fx_barrier_option_instrument
fx_instrument_mapper::forward_fx_generic_barrier_option(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxGenericBarrierOption: " << std::string(t.id);
    fx_barrier_option_instrument r;
    r.identity.trade_type_code = "FxGenericBarrierOption";
    set_audit(r.audit);
    if (!t.FxGenericBarrierOptionData)
        return r;
    const auto& d = *t.FxGenericBarrierOptionData;

    if (d.underlyingTypes.Name)
        r.underlying_code = std::string(*d.underlyingTypes.Name);
    else if (d.underlyingTypes.Underlying)
        r.underlying_code = std::string(d.underlyingTypes.Underlying->Name);

    r.bought_currency = to_string(d.PayCurrency);
    if (d.OptionData.OptionType)
        r.option_type = std::string(*d.OptionData.OptionType);
    r.expiry_date = expiry_date_from_single(d.OptionData);

    if (!d.Barriers.BarrierData.empty()) {
        r.barrier_type = to_string(d.Barriers.BarrierData.front().Type);
        if (!d.Barriers.BarrierData.front().Levels.Level.empty())
            r.lower_barrier = exact_decimal(d.Barriers.BarrierData.front().Levels.Level.front());
        if (d.Barriers.BarrierData.size() > 1 && !d.Barriers.BarrierData[1].Levels.Level.empty())
            r.upper_barrier = exact_decimal(d.Barriers.BarrierData[1].Levels.Level.front());
    }
    return r;
}

// ===========================================================================
// Forward: FxDigitalOption
// ===========================================================================

trading::domain::fx_digital_option_instrument
fx_instrument_mapper::forward_fx_digital_option(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxDigitalOption: " << std::string(t.id);
    fx_digital_option_instrument r;
    r.identity.trade_type_code = "FxDigitalOption";
    r.long_short = "Long";
    set_audit(r.audit);
    if (!t.FxDigitalOptionData)
        return r;
    const auto& d = *t.FxDigitalOptionData;

    r.foreign_currency = to_string(d.ForeignCurrency);
    r.domestic_currency = to_string(d.DomesticCurrency);
    r.payoff_currency = to_string(d.ForeignCurrency); // defaults to foreign
    r.strike = exact_decimal(d.Strike);
    r.payoff_amount = exact_decimal(d.PayoffAmount);
    r.option_type = option_type_from_single(d.OptionData);
    r.expiry_date = expiry_date_from_single(d.OptionData);
    return r;
}

// ===========================================================================
// Forward: FxDigitalBarrierOption
// ===========================================================================

trading::domain::fx_digital_option_instrument
fx_instrument_mapper::forward_fx_digital_barrier_option(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxDigitalBarrierOption: " << std::string(t.id);
    fx_digital_option_instrument r;
    r.identity.trade_type_code = "FxDigitalBarrierOption";
    r.long_short = "Long";
    set_audit(r.audit);
    if (!t.FxDigitalBarrierOptionData)
        return r;
    const auto& d = *t.FxDigitalBarrierOptionData;

    r.foreign_currency = to_string(d.ForeignCurrency);
    r.domestic_currency = to_string(d.DomesticCurrency);
    r.payoff_currency = to_string(d.ForeignCurrency);
    r.strike = exact_decimal(d.Strike);
    r.payoff_amount = exact_decimal(d.PayoffAmount);
    r.option_type = option_type_from_single(d.OptionData);
    r.expiry_date = expiry_date_from_single(d.OptionData);

    if (!d.BarrierData.Levels.Level.empty()) {
        r.barrier_type = to_string(d.BarrierData.Type);
        r.lower_barrier = exact_decimal(d.BarrierData.Levels.Level.front());
    }
    return r;
}

// ===========================================================================
// Forward: FxTouchOption / FxDoubleTouchOption
// ===========================================================================

trading::domain::fx_digital_option_instrument
fx_instrument_mapper::forward_fx_touch_option(const trade& t) {
    const std::string type_str = to_string(t.TradeType);
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping " << type_str << ": " << std::string(t.id);
    fx_digital_option_instrument r;
    r.identity.trade_type_code = type_str;
    r.long_short = "Long";
    set_audit(r.audit);

    const fxTouchOptionData* dp = nullptr;
    if (t.FxTouchOptionData)
        dp = &(*t.FxTouchOptionData);
    else if (t.FxDoubleTouchOptionData)
        dp = &(*t.FxDoubleTouchOptionData);
    if (!dp)
        return r;
    const auto& d = *dp;

    r.foreign_currency = to_string(d.ForeignCurrency);
    r.domestic_currency = to_string(d.DomesticCurrency);
    r.payoff_currency = to_string(d.PayoffCurrency);
    r.payoff_amount = exact_decimal(d.PayoffAmount);
    r.expiry_date = expiry_date_from_single(d.OptionData);
    fill_barrier(d.BarrierData, r.barrier_type, r.lower_barrier, r.upper_barrier);
    return r;
}

// ===========================================================================
// Forward: FxVarianceSwap
// ===========================================================================

trading::domain::fx_variance_swap_instrument
fx_instrument_mapper::forward_fx_variance_swap(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxVarianceSwap: " << std::string(t.id);
    fx_variance_swap_instrument r;
    r.identity.trade_type_code = "FxVarianceSwap";
    r.long_short = "Long";
    set_audit(r.audit);
    if (!t.FxVarianceSwapData)
        return r;
    const auto& d = *t.FxVarianceSwapData;

    if (d.underlyingTypes.Name)
        r.underlying_code = std::string(*d.underlyingTypes.Name);
    else if (d.underlyingTypes.Underlying)
        r.underlying_code = std::string(d.underlyingTypes.Underlying->Name);

    r.start_date = to_domain_date(std::string(d.StartDate));
    r.end_date = to_domain_date(std::string(d.EndDate));
    r.strike = static_cast<double>(d.Strike);
    r.notional = exact_decimal(d.Notional);
    r.currency = to_string(d.Currency);
    r.moment_type = "Variance"; // default; MomentType element not always present
    return r;
}

// ===========================================================================
// Forward: FxAverageForward
// ===========================================================================

trading::domain::fx_asian_forward_instrument
fx_instrument_mapper::forward_fx_average_forward(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxAverageForward: " << std::string(t.id);
    fx_asian_forward_instrument r;
    r.identity.trade_type_code = "FxAverageForward";
    set_audit(r.audit);
    if (!t.FxAverageForwardData)
        return r;
    const auto& d = *t.FxAverageForwardData;

    r.payment_date = to_optional_domain_date(std::string(d.PaymentDate));
    r.reference_currency = to_string(d.ReferenceCurrency);
    r.reference_notional = exact_decimal(d.ReferenceNotional);
    r.settlement_currency = to_string(d.SettlementCurrency);
    r.settlement_notional = exact_decimal(d.SettlementNotional);
    r.fx_index = std::string(d.FXIndex);
    r.long_short = "Long";
    return r;
}

// ===========================================================================
// Forward: FxAccumulator
// ===========================================================================

trading::domain::fx_accumulator_instrument
fx_instrument_mapper::forward_fx_accumulator(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxAccumulator: " << std::string(t.id);
    fx_accumulator_instrument r;
    r.identity.trade_type_code = "FxAccumulator";
    r.long_short = "Long";
    set_audit(r.audit);
    if (!t.FxAccumulatorData)
        return r;
    const auto& d = *t.FxAccumulatorData;

    r.currency = to_string(d.Currency);
    r.underlying_code = std::string(d.Underlying.Name);
    r.fixing_amount = exact_decimal(d.FixingAmount);
    if (d.Strike)
        r.strike = exact_decimal(*d.Strike);
    if (d.StartDate)
        r.start_date = to_domain_date(std::string(*d.StartDate));

    if (d.Barriers) {
        for (const auto& bd : d.Barriers->BarrierData) {
            const auto btype = to_string(bd.Type);
            if ((btype == "DownAndOut" || btype == "UpAndOut") && !bd.Levels.Level.empty()) {
                r.knock_out_barrier = exact_decimal(bd.Levels.Level.front());
                break;
            }
        }
    }
    // The schema requires at least one range bound, so the list is what makes
    // the exported accumulator readable.
    if (!d.RangeBounds.RangeBound.empty())
        r.range_bounds_json = range_bounds_to_json(d.RangeBounds.RangeBound);
    return r;
}

// ===========================================================================
// Forward: FxTaRF
// ===========================================================================

trading::domain::fx_asian_forward_instrument fx_instrument_mapper::forward_fx_tarf(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxTaRF: " << std::string(t.id);
    fx_asian_forward_instrument r;
    r.identity.trade_type_code = "FxTaRF";
    set_audit(r.audit);
    if (!t.FxTaRFData)
        return r;
    const auto& d = *t.FxTaRFData;

    r.currency = to_string(d.Currency);
    r.fx_index = std::string(d.Underlying.Name);
    r.fixing_amount = exact_decimal(d.FixingAmount);
    r.long_short = std::string(d.OptionData.LongShort);
    if (d.Strike)
        r.strike = exact_decimal(*d.Strike);

    // Target amount (profit cap) if present
    for (const auto& bd : d.Barriers.BarrierData) {
        const auto btype = to_string(bd.Type);
        if ((btype == "CumulatedProfitCap" || btype == "FixingCap") && !bd.Levels.Level.empty()) {
            r.target_amount = exact_decimal(bd.Levels.Level.front());
            break;
        }
    }
    return r;
}

// ===========================================================================
// Reverse: FxForward
// ===========================================================================

trade fx_instrument_mapper::reverse_fx_forward(const fx_forward_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FxForward";
    trade t;
    t.TradeType = oreTradeType::FxForward;

    fxForwardData fwd;
    static_cast<std::string&>(fwd.ValueDate) = to_ore_date(instr.value_date);
    fwd.BoughtCurrency = parse_currency_code(instr.bought_currency);
    set_exact(fwd.BoughtAmount, instr.bought_amount);
    fwd.SoldCurrency = parse_currency_code(instr.sold_currency);
    set_exact(fwd.SoldAmount, instr.sold_amount);

    t.FxForwardData = std::move(fwd);
    return t;
}

// ===========================================================================
// Reverse: FxSwap
// ===========================================================================

trade fx_instrument_mapper::reverse_fx_swap(const fx_forward_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FxSwap";
    trade t;
    t.TradeType = oreTradeType::FxSwap;

    fxSwapData sw;
    static_cast<std::string&>(sw.NearDate) = to_ore_date(instr.value_date);
    sw.NearBoughtCurrency = parse_currency_code(instr.bought_currency);
    set_exact(sw.NearBoughtAmount, instr.bought_amount);
    sw.NearSoldCurrency = parse_currency_code(instr.sold_currency);
    set_exact(sw.NearSoldAmount, instr.sold_amount);
    // Far leg: not captured in forward mapping — round-trip as empty date/zero amounts.
    static_cast<std::string&>(sw.FarDate) = "";
    static_cast<float&>(sw.FarBoughtAmount) = 0.0f;
    static_cast<float&>(sw.FarSoldAmount) = 0.0f;

    t.FxSwapData = std::move(sw);
    return t;
}

// ===========================================================================
// Reverse: FxOption
// ===========================================================================

trade fx_instrument_mapper::reverse_fx_option(const fx_vanilla_option_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FxOption";
    trade t;
    t.TradeType = oreTradeType::FxOption;

    fxOptionData opt;
    opt.BoughtCurrency = parse_currency_code(instr.bought_currency);
    set_exact(opt.BoughtAmount, instr.bought_amount);
    opt.SoldCurrency = parse_currency_code(instr.sold_currency);
    if (!instr.sold_amount.is_zero())
        set_money(opt.SoldAmount, instr.sold_amount);

    opt.OptionData = make_option_entry(instr.option_type, instr.expiry_date);
    if (!instr.settlement.empty()) {
        if (instr.settlement == "Physical")
            opt.OptionData.Settlement = settlementType::Physical;
        else if (instr.settlement == "Cash")
            opt.OptionData.Settlement = settlementType::Cash;
        else
            throw std::invalid_argument("Unrecognised settlement type: " + instr.settlement);
    }

    t.FxOptionData = std::move(opt);
    return t;
}

// ===========================================================================
// Reverse: FxBarrierOption
// ===========================================================================

trade fx_instrument_mapper::reverse_fx_barrier_option(const fx_barrier_option_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FxBarrierOption";
    trade t;
    t.TradeType = oreTradeType::FxBarrierOption;

    fxBarrierOptionData d;
    d.BoughtCurrency = parse_currency_code(instr.bought_currency);
    set_exact(d.BoughtAmount, instr.bought_amount);
    d.SoldCurrency = parse_currency_code(instr.sold_currency);
    set_exact(d.SoldAmount, instr.sold_amount);
    d.OptionData = make_option_entry(instr.option_type, instr.expiry_date);

    if (!instr.barrier_type.empty())
        d.BarrierData =
            make_barrier_group(instr.barrier_type, instr.lower_barrier, instr.upper_barrier);

    t.FxBarrierOptionData = std::move(d);
    return t;
}

// ===========================================================================
// Reverse: FxDoubleBarrierOption
// ===========================================================================

trade fx_instrument_mapper::reverse_fx_double_barrier_option(
    const fx_barrier_option_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FxDoubleBarrierOption";
    trade t;
    t.TradeType = oreTradeType::FxDoubleBarrierOption;
    fxBarrierOptionData d;
    d.BoughtCurrency = parse_currency_code(instr.bought_currency);
    set_exact(d.BoughtAmount, instr.bought_amount);
    d.SoldCurrency = parse_currency_code(instr.sold_currency);
    set_exact(d.SoldAmount, instr.sold_amount);
    d.OptionData = make_option_entry(instr.option_type, instr.expiry_date);
    if (!instr.barrier_type.empty())
        d.BarrierData =
            make_barrier_group(instr.barrier_type, instr.lower_barrier, instr.upper_barrier);
    t.FxDoubleBarrierOptionData = std::move(d);
    return t;
}

// ===========================================================================
// Reverse: FxEuropeanBarrierOption
// ===========================================================================

trade fx_instrument_mapper::reverse_fx_european_barrier_option(
    const fx_barrier_option_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FxEuropeanBarrierOption";
    trade t;
    t.TradeType = oreTradeType::FxEuropeanBarrierOption;
    fxBarrierOptionData d;
    d.BoughtCurrency = parse_currency_code(instr.bought_currency);
    set_exact(d.BoughtAmount, instr.bought_amount);
    d.SoldCurrency = parse_currency_code(instr.sold_currency);
    set_exact(d.SoldAmount, instr.sold_amount);
    d.OptionData = make_option_entry(instr.option_type, instr.expiry_date);
    if (!instr.barrier_type.empty())
        d.BarrierData =
            make_barrier_group(instr.barrier_type, instr.lower_barrier, instr.upper_barrier);
    t.FxEuropeanBarrierOptionData = std::move(d);
    return t;
}

// ===========================================================================
// Reverse: FxKIKOBarrierOption
// ===========================================================================

trade fx_instrument_mapper::reverse_fx_kiko_barrier_option(
    const fx_barrier_option_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FxKIKOBarrierOption";
    trade t;
    t.TradeType = oreTradeType::FxKIKOBarrierOption;
    fxKIKOBarrierOptionData d;
    d.BoughtCurrency = parse_currency_code(instr.bought_currency);
    set_exact(d.BoughtAmount, instr.bought_amount);
    d.SoldCurrency = parse_currency_code(instr.sold_currency);
    set_exact(d.SoldAmount, instr.sold_amount);
    d.OptionData = make_option_entry(instr.option_type, instr.expiry_date);
    if (!instr.barrier_type.empty() && instr.lower_barrier != ores::utility::decimal::decimal{})
        d.Barriers.BarrierData.push_back(make_barrier(instr.barrier_type, instr.lower_barrier));
    if (instr.upper_barrier.has_value())
        d.Barriers.BarrierData.push_back(make_barrier(instr.barrier_type, *instr.upper_barrier));
    t.FxKIKOBarrierOptionData = std::move(d);
    return t;
}

// ===========================================================================
// Reverse: FxGenericBarrierOption
// ===========================================================================

trade fx_instrument_mapper::reverse_fx_generic_barrier_option(
    const fx_barrier_option_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FxGenericBarrierOption";
    trade t;
    t.TradeType = oreTradeType::FxGenericBarrierOption;

    genericBarrierOptionData d;
    if (!instr.bought_currency.empty())
        d.PayCurrency = parse_currency_code(instr.bought_currency);
    d.underlyingTypes = make_underlying_type_name(instr.underlying_code);
    d.OptionData = make_option_entry(instr.option_type, instr.expiry_date);

    if (!instr.barrier_type.empty() && instr.lower_barrier != ores::utility::decimal::decimal{})
        d.Barriers.BarrierData.push_back(make_barrier(instr.barrier_type, instr.lower_barrier));
    if (instr.upper_barrier.has_value())
        d.Barriers.BarrierData.push_back(make_barrier(instr.barrier_type, *instr.upper_barrier));

    t.FxGenericBarrierOptionData = std::move(d);
    return t;
}

// ===========================================================================
// Reverse: FxDigitalOption
// ===========================================================================

trade fx_instrument_mapper::reverse_fx_digital_option(const fx_digital_option_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FxDigitalOption";
    trade t;
    t.TradeType = oreTradeType::FxDigitalOption;

    fxDigitalOptionData d;
    d.ForeignCurrency = parse_currency_code(instr.foreign_currency);
    d.DomesticCurrency = parse_currency_code(instr.domestic_currency);
    set_exact(d.Strike, instr.strike.value_or(ores::utility::decimal::decimal{}));
    set_exact(d.PayoffAmount, instr.payoff_amount);
    d.OptionData = make_option_entry(instr.option_type, instr.expiry_date, instr.long_short);

    t.FxDigitalOptionData = std::move(d);
    return t;
}

// ===========================================================================
// Reverse: FxDigitalBarrierOption
// ===========================================================================

trade fx_instrument_mapper::reverse_fx_digital_barrier_option(
    const fx_digital_option_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FxDigitalBarrierOption";
    trade t;
    t.TradeType = oreTradeType::FxDigitalBarrierOption;

    fxDigitalBarrierOptionData d;
    d.ForeignCurrency = parse_currency_code(instr.foreign_currency);
    d.DomesticCurrency = parse_currency_code(instr.domestic_currency);
    set_exact(d.Strike, instr.strike.value_or(ores::utility::decimal::decimal{}));
    set_exact(d.PayoffAmount, instr.payoff_amount);
    d.OptionData = make_option_entry(instr.option_type, instr.expiry_date, instr.long_short);

    if (!instr.barrier_type.empty())
        d.BarrierData =
            make_barrier_group(instr.barrier_type, instr.lower_barrier, instr.upper_barrier);

    t.FxDigitalBarrierOptionData = std::move(d);
    return t;
}

// ===========================================================================
// Reverse: FxTouchOption / FxDoubleTouchOption
// ===========================================================================

trade fx_instrument_mapper::reverse_fx_touch_option(const fx_digital_option_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping " << instr.identity.trade_type_code;
    trade t;
    t.TradeType = (instr.identity.trade_type_code == "FxDoubleTouchOption") ?
                      oreTradeType::FxDoubleTouchOption :
                      oreTradeType::FxTouchOption;

    fxTouchOptionData d;
    if (!instr.foreign_currency.empty())
        d.ForeignCurrency = parse_currency_code(instr.foreign_currency);
    if (!instr.domestic_currency.empty())
        d.DomesticCurrency = parse_currency_code(instr.domestic_currency);
    if (!instr.payoff_currency.empty())
        d.PayoffCurrency = parse_currency_code(instr.payoff_currency);
    set_exact(d.PayoffAmount, instr.payoff_amount);
    d.OptionData = make_option_entry("", instr.expiry_date, instr.long_short);

    if (!instr.barrier_type.empty())
        d.BarrierData =
            make_barrier_group(instr.barrier_type, instr.lower_barrier, instr.upper_barrier);

    if (instr.identity.trade_type_code == "FxDoubleTouchOption")
        t.FxDoubleTouchOptionData = std::move(d);
    else
        t.FxTouchOptionData = std::move(d);
    return t;
}

// ===========================================================================
// Reverse: FxVarianceSwap
// ===========================================================================

trade fx_instrument_mapper::reverse_fx_variance_swap(const fx_variance_swap_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FxVarianceSwap";
    trade t;
    t.TradeType = oreTradeType::FxVarianceSwap;

    varianceSwapData d;
    static_cast<std::string&>(d.StartDate) = to_ore_date(instr.start_date);
    static_cast<std::string&>(d.EndDate) = to_ore_date(instr.end_date);
    if (!instr.currency.empty())
        d.Currency = parse_currency_code(instr.currency);
    d.underlyingTypes = make_underlying_type_name(instr.underlying_code);
    validate_long_short(instr.long_short);
    static_cast<std::string&>(d.LongShort) = instr.long_short;
    d.Strike = static_cast<float>(instr.strike);
    set_exact(d.Notional, instr.notional);
    static_cast<std::string&>(d.Calendar) = "TARGET";

    t.FxVarianceSwapData = std::move(d);
    return t;
}

// ===========================================================================
// Reverse: FxAverageForward
// ===========================================================================

trade fx_instrument_mapper::reverse_fx_average_forward(const fx_asian_forward_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FxAverageForward";
    trade t;
    t.TradeType = oreTradeType::FxAverageForward;

    fxAverageForwardData d;
    static_cast<std::string&>(d.PaymentDate) = to_ore_date(instr.payment_date);
    if (!instr.reference_currency.empty())
        d.ReferenceCurrency = parse_currency_code(instr.reference_currency);
    set_exact(d.ReferenceNotional,
              instr.reference_notional.value_or(ores::utility::decimal::decimal{}));
    if (!instr.settlement_currency.empty())
        d.SettlementCurrency = parse_currency_code(instr.settlement_currency);
    set_exact(d.SettlementNotional,
              instr.settlement_notional.value_or(ores::utility::decimal::decimal{}));
    static_cast<std::string&>(d.FXIndex) = instr.fx_index;
    d.FixedPayer = bool_::false_;
    // Minimal observation schedule
    scheduleData_Rules_t rule;
    static_cast<std::string&>(rule.Tenor) = "1M";
    d.ObservationDates.Rules.push_back(std::move(rule));

    t.FxAverageForwardData = std::move(d);
    return t;
}

// ===========================================================================
// Reverse: FxAccumulator
// ===========================================================================

trade fx_instrument_mapper::reverse_fx_accumulator(const fx_accumulator_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FxAccumulator";
    trade t;
    t.TradeType = oreTradeType::FxAccumulator;

    accumulatorData d;
    if (!instr.currency.empty())
        d.Currency = parse_currency_code(instr.currency);
    set_exact(d.FixingAmount, instr.fixing_amount);
    if (instr.strike != ores::utility::decimal::decimal{})
        set_money(d.Strike, instr.strike);
    static_cast<std::string&>(d.Underlying.Name) = instr.underlying_code;
    static_cast<std::string&>(d.Underlying.Type) = "FX";
    validate_long_short(instr.long_short);
    static_cast<std::string&>(d.OptionData.LongShort) = instr.long_short;
    if (instr.start_date.ok()) {
        date sd;
        static_cast<std::string&>(sd) = to_ore_date(instr.start_date);
        d.StartDate = std::move(sd);
    }
    // Minimal observation schedule
    scheduleData_Rules_t rule;
    if (instr.start_date.ok())
        static_cast<std::string&>(rule.StartDate) = to_ore_date(instr.start_date);
    static_cast<std::string&>(rule.Tenor) = "1D";
    d.ObservationDates.Rules.push_back(std::move(rule));
    for (const auto& rb : parse_json_range_bounds(instr.range_bounds_json))
        d.RangeBounds.RangeBound.push_back(rb);

    if (instr.knock_out_barrier.has_value())
        d.Barriers = [&]() {
            accumulatorData_Barriers_t b;
            b.BarrierData.push_back(make_barrier("UpAndOut", *instr.knock_out_barrier));
            return b;
        }();

    t.FxAccumulatorData = std::move(d);
    return t;
}

// ===========================================================================
// Reverse: FxTaRF
// ===========================================================================

trade fx_instrument_mapper::reverse_fx_tarf(const fx_asian_forward_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FxTaRF";
    trade t;
    t.TradeType = oreTradeType::FxTaRF;

    tarfData2 d;
    if (!instr.currency.empty())
        d.Currency = parse_currency_code(instr.currency);
    set_exact(d.FixingAmount, instr.fixing_amount.value_or(ores::utility::decimal::decimal{}));
    if (instr.strike.has_value())
        set_money(d.Strike, instr.strike);
    static_cast<std::string&>(d.Underlying.Name) = instr.fx_index;
    static_cast<std::string&>(d.Underlying.Type) = "FX";
    static_cast<std::string&>(d.OptionData.LongShort) = "Long";
    // Minimal schedule
    scheduleData_Rules_t rule;
    static_cast<std::string&>(rule.Tenor) = "1Y";
    d.ScheduleData.Rules.push_back(std::move(rule));

    if (instr.target_amount.has_value()) {
        barrierData bd;
        bd.Type = barrierType::CumulatedProfitCap;
        push_money(bd.Levels.Level, *instr.target_amount);
        d.Barriers.BarrierData.push_back(std::move(bd));
    }

    t.FxTaRFData = std::move(d);
    return t;
}

}
