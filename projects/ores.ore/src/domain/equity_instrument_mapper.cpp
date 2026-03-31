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
#include "ores.ore/domain/equity_instrument_mapper.hpp"

#include <map>
#include <stdexcept>

namespace ores::ore::domain {

using namespace ores::logging;
using ores::trading::domain::equity_instrument;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

namespace {

std::string json_escape(const std::string& s) {
    std::string out;
    out.reserve(s.size());
    for (char c : s) {
        if (c == '"')       out += "\\\"";
        else if (c == '\\') out += "\\\\";
        else                out += c;
    }
    return out;
}

std::vector<std::string> parse_json_string_array(const std::string& json) {
    std::vector<std::string> result;
    std::size_t pos = 0;
    while ((pos = json.find('"', pos)) != std::string::npos) {
        ++pos;
        std::string name;
        while (pos < json.size() && json[pos] != '"') {
            if (json[pos] == '\\' && pos + 1 < json.size())
                ++pos; // skip escape char
            name += json[pos++];
        }
        result.push_back(std::move(name));
        ++pos;
    }
    return result;
}

equity_instrument make_base(const std::string& trade_type_code) {
    equity_instrument r;
    r.trade_type_code = trade_type_code;
    r.modified_by = "ores";
    r.performed_by = "ores";
    r.change_reason_code = "system.external_data_import";
    r.change_commentary = "Imported from ORE XML";
    return r;
}

optionData make_option_data(const equity_instrument& instr) {
    optionData od;
    static_cast<std::string&>(od.LongShort) = "Long";
    if (!instr.option_type.empty()) {
        optionData_OptionType_t ot;
        static_cast<std::string&>(ot) = instr.option_type;
        od.OptionType = std::move(ot);
    }
    if (!instr.exercise_type.empty()) {
        optionData_Style_t st;
        static_cast<std::string&>(st) = instr.exercise_type;
        od.Style = std::move(st);
    }
    if (!instr.maturity_date.empty()) {
        _ExerciseDates_t exd;
        date ed;
        static_cast<std::string&>(ed) = instr.maturity_date;
        exd.ExerciseDate.push_back(ed);
        exerciseDatesGroup_group_t eg;
        eg.ExerciseDates = std::move(exd);
        od.exerciseDatesGroup = std::move(eg);
    }
    return od;
}

underlyingTypes_group_t make_underlying_type(const std::string& name) {
    underlyingTypes_group_t u;
    _Name_t n;
    static_cast<std::string&>(n) = name;
    u.Name = std::move(n);
    return u;
}

currencyCode parse_currency_code(const std::string& s) {
    static const std::map<std::string, currencyCode> map = {
        {"AED", currencyCode::AED}, {"AFN", currencyCode::AFN},
        {"ALL", currencyCode::ALL}, {"AMD", currencyCode::AMD},
        {"ANG", currencyCode::ANG}, {"AOA", currencyCode::AOA},
        {"ARS", currencyCode::ARS}, {"AUD", currencyCode::AUD},
        {"AWG", currencyCode::AWG}, {"AZN", currencyCode::AZN},
        {"BAM", currencyCode::BAM}, {"BBD", currencyCode::BBD},
        {"BDT", currencyCode::BDT}, {"BGN", currencyCode::BGN},
        {"BHD", currencyCode::BHD}, {"BIF", currencyCode::BIF},
        {"BMD", currencyCode::BMD}, {"BND", currencyCode::BND},
        {"BOB", currencyCode::BOB}, {"BOV", currencyCode::BOV},
        {"BRL", currencyCode::BRL}, {"BSD", currencyCode::BSD},
        {"BTN", currencyCode::BTN}, {"BWP", currencyCode::BWP},
        {"BYN", currencyCode::BYN}, {"BZD", currencyCode::BZD},
        {"CAD", currencyCode::CAD}, {"CDF", currencyCode::CDF},
        {"CHE", currencyCode::CHE}, {"CHF", currencyCode::CHF},
        {"CHW", currencyCode::CHW}, {"CLF", currencyCode::CLF},
        {"CLP", currencyCode::CLP}, {"CNH", currencyCode::CNH},
        {"CNT", currencyCode::CNT}, {"CNY", currencyCode::CNY},
        {"COP", currencyCode::COP}, {"COU", currencyCode::COU},
        {"CRC", currencyCode::CRC}, {"CUC", currencyCode::CUC},
        {"CUP", currencyCode::CUP}, {"CVE", currencyCode::CVE},
        {"CYP", currencyCode::CYP}, {"CZK", currencyCode::CZK},
        {"DJF", currencyCode::DJF}, {"DKK", currencyCode::DKK},
        {"DOP", currencyCode::DOP}, {"DZD", currencyCode::DZD},
        {"EGP", currencyCode::EGP}, {"ERN", currencyCode::ERN},
        {"ETB", currencyCode::ETB}, {"EUR", currencyCode::EUR},
        {"FJD", currencyCode::FJD}, {"FKP", currencyCode::FKP},
        {"GBP", currencyCode::GBP}, {"GEL", currencyCode::GEL},
        {"GGP", currencyCode::GGP}, {"GHS", currencyCode::GHS},
        {"GIP", currencyCode::GIP}, {"GMD", currencyCode::GMD},
        {"GNF", currencyCode::GNF}, {"GTQ", currencyCode::GTQ},
        {"GYD", currencyCode::GYD}, {"HKD", currencyCode::HKD},
        {"HNL", currencyCode::HNL}, {"HRK", currencyCode::HRK},
        {"HTG", currencyCode::HTG}, {"HUF", currencyCode::HUF},
        {"IDR", currencyCode::IDR}, {"ILS", currencyCode::ILS},
        {"IMP", currencyCode::IMP}, {"INR", currencyCode::INR},
        {"IQD", currencyCode::IQD}, {"IRR", currencyCode::IRR},
        {"ISK", currencyCode::ISK}, {"JEP", currencyCode::JEP},
        {"JMD", currencyCode::JMD}, {"JOD", currencyCode::JOD},
        {"JPY", currencyCode::JPY}, {"KES", currencyCode::KES},
        {"KGS", currencyCode::KGS}, {"KHR", currencyCode::KHR},
        {"KID", currencyCode::KID}, {"KMF", currencyCode::KMF},
        {"KPW", currencyCode::KPW}, {"KRW", currencyCode::KRW},
        {"KWD", currencyCode::KWD}, {"KYD", currencyCode::KYD},
        {"KZT", currencyCode::KZT}, {"LAK", currencyCode::LAK},
        {"LBP", currencyCode::LBP}, {"LKR", currencyCode::LKR},
        {"LRD", currencyCode::LRD}, {"LSL", currencyCode::LSL},
        {"LTL", currencyCode::LTL}, {"LVL", currencyCode::LVL},
        {"LYD", currencyCode::LYD}, {"MAD", currencyCode::MAD},
        {"MDL", currencyCode::MDL}, {"MGA", currencyCode::MGA},
        {"MKD", currencyCode::MKD}, {"MMK", currencyCode::MMK},
        {"MNT", currencyCode::MNT}, {"MOP", currencyCode::MOP},
        {"MRU", currencyCode::MRU}, {"MUR", currencyCode::MUR},
        {"MVR", currencyCode::MVR}, {"MWK", currencyCode::MWK},
        {"MXN", currencyCode::MXN}, {"MXV", currencyCode::MXV},
        {"MYR", currencyCode::MYR}, {"MZN", currencyCode::MZN},
        {"NAD", currencyCode::NAD}, {"NGN", currencyCode::NGN},
        {"NIO", currencyCode::NIO}, {"NOK", currencyCode::NOK},
        {"NPR", currencyCode::NPR}, {"NZD", currencyCode::NZD},
        {"OMR", currencyCode::OMR}, {"PAB", currencyCode::PAB},
        {"PEN", currencyCode::PEN}, {"PGK", currencyCode::PGK},
        {"PHP", currencyCode::PHP}, {"PKR", currencyCode::PKR},
        {"PLN", currencyCode::PLN}, {"PYG", currencyCode::PYG},
        {"QAR", currencyCode::QAR}, {"RON", currencyCode::RON},
        {"RSD", currencyCode::RSD}, {"RUB", currencyCode::RUB},
        {"RWF", currencyCode::RWF}, {"SAR", currencyCode::SAR},
        {"SBD", currencyCode::SBD}, {"SCR", currencyCode::SCR},
        {"SDG", currencyCode::SDG}, {"SEK", currencyCode::SEK},
        {"SGD", currencyCode::SGD}, {"SHP", currencyCode::SHP},
        {"SLL", currencyCode::SLL}, {"SOS", currencyCode::SOS},
        {"SRD", currencyCode::SRD}, {"STN", currencyCode::STN},
        {"SVC", currencyCode::SVC}, {"SYP", currencyCode::SYP},
        {"SZL", currencyCode::SZL}, {"THB", currencyCode::THB},
        {"TJS", currencyCode::TJS}, {"TMT", currencyCode::TMT},
        {"TND", currencyCode::TND}, {"TOP", currencyCode::TOP},
        {"TRY", currencyCode::TRY}, {"TTD", currencyCode::TTD},
        {"TWD", currencyCode::TWD},
        {"TZS", currencyCode::TZS}, {"UAH", currencyCode::UAH},
        {"UGX", currencyCode::UGX}, {"USD", currencyCode::USD},
        {"USN", currencyCode::USN}, {"UYI", currencyCode::UYI},
        {"UYU", currencyCode::UYU}, {"UYW", currencyCode::UYW},
        {"UZS", currencyCode::UZS}, {"VES", currencyCode::VES},
        {"VND", currencyCode::VND}, {"VUV", currencyCode::VUV},
        {"WST", currencyCode::WST}, {"XAF", currencyCode::XAF},
        {"XAG", currencyCode::XAG}, {"XAU", currencyCode::XAU},
        {"XCD", currencyCode::XCD},
        {"XOF", currencyCode::XOF}, {"XPD", currencyCode::XPD},
        {"XPF", currencyCode::XPF}, {"XPT", currencyCode::XPT},
        {"XSU", currencyCode::XSU}, {"XUA", currencyCode::XUA},
        {"YER", currencyCode::YER}, {"ZAR", currencyCode::ZAR},
        {"ZMW", currencyCode::ZMW}, {"ZWL", currencyCode::ZWL},
        {"BTC", currencyCode::BTC}, {"ETH", currencyCode::ETH},
        {"XBT", currencyCode::XBT}, {"ETC", currencyCode::ETC},
        {"BCH", currencyCode::BCH}, {"XRP", currencyCode::XRP},
        {"LTC", currencyCode::LTC}, {"ZUR", currencyCode::ZUR},
        {"ZUG", currencyCode::ZUG},
    };
    const auto it = map.find(s);
    if (it == map.end())
        throw std::runtime_error(
            "parse_currency_code: unrecognised currency code '" + s +
            "' — cannot produce valid ORE XML");
    return it->second;
}

} // namespace

// ---------------------------------------------------------------------------
// Private helpers
// ---------------------------------------------------------------------------

std::string equity_instrument_mapper::extract_underlying_name(
        const underlyingTypes_group_t& u) {
    if (u.Name) return std::string(*u.Name);
    if (u.Underlying) return std::string(u.Underlying->Name);
    return {};
}

std::string equity_instrument_mapper::extract_option_type(
        const optionData& od) {
    if (od.OptionType) return std::string(*od.OptionType);
    return {};
}

std::string equity_instrument_mapper::extract_exercise_style(
        const optionData& od) {
    if (od.Style) return std::string(*od.Style);
    return {};
}

std::string equity_instrument_mapper::first_exercise_date(
        const optionData& od) {
    if (!od.exerciseDatesGroup) return {};
    if (!od.exerciseDatesGroup->ExerciseDates) return {};
    if (od.exerciseDatesGroup->ExerciseDates->ExerciseDate.empty()) return {};
    return std::string(
        od.exerciseDatesGroup->ExerciseDates->ExerciseDate.front());
}

double equity_instrument_mapper::extract_strike(
        const strikeGroup_group_t& sg) {
    if (sg.Strike) {
        const std::string s(*sg.Strike);
        if (!s.empty()) return std::stod(s);
    }
    if (sg.StrikeData && sg.StrikeData->Value)
        return static_cast<double>(*sg.StrikeData->Value);
    return 0.0;
}

std::string equity_instrument_mapper::barrier_type_str(
        const barrierData& bd) {
    return to_string(bd.Type);
}

double equity_instrument_mapper::first_barrier_level(
        const barrierData& bd) {
    if (!bd.Levels.Level.empty())
        return static_cast<double>(bd.Levels.Level.front());
    return 0.0;
}

double equity_instrument_mapper::second_barrier_level(
        const barrierData& bd) {
    if (bd.Levels.Level.size() >= 2)
        return static_cast<double>(bd.Levels.Level[1]);
    return 0.0;
}

std::string equity_instrument_mapper::underlyings_to_json(
        const underlyings& us) {
    std::string json = "[";
    bool first = true;
    for (const auto& u : us.Underlying) {
        if (!first) json += ",";
        json += "\"" + json_escape(std::string(u.Name)) + "\"";
        first = false;
    }
    json += "]";
    return json;
}

// ---------------------------------------------------------------------------
// Forward: EquityOption
// ---------------------------------------------------------------------------

equity_mapping_result equity_instrument_mapper::forward_equity_option(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping EquityOption: "
                               << std::string(t.id);
    equity_mapping_result result;
    result.instrument = make_base("EquityOption");
    if (!t.EquityOptionData) return result;
    const auto& d = *t.EquityOptionData;

    result.instrument.underlying_code =
        extract_underlying_name(d.underlyingTypes);
    result.instrument.currency = std::string(d.Currency);
    result.instrument.quantity = static_cast<double>(d.Quantity);
    result.instrument.option_type = extract_option_type(d.OptionData);
    result.instrument.exercise_type = extract_exercise_style(d.OptionData);
    result.instrument.maturity_date = first_exercise_date(d.OptionData);
    result.instrument.strike_price = extract_strike(d.strikeGroup);
    return result;
}

// ---------------------------------------------------------------------------
// Forward: EquityForward
// ---------------------------------------------------------------------------

equity_mapping_result equity_instrument_mapper::forward_equity_forward(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping EquityForward: "
                               << std::string(t.id);
    equity_mapping_result result;
    result.instrument = make_base("EquityForward");
    if (!t.EquityForwardData) return result;
    const auto& d = *t.EquityForwardData;

    result.instrument.underlying_code =
        extract_underlying_name(d.underlyingTypes);
    result.instrument.currency = std::string(d.Currency);
    result.instrument.quantity = static_cast<double>(d.Quantity);
    result.instrument.strike_price = static_cast<double>(d.Strike);
    result.instrument.maturity_date = std::string(d.Maturity);
    return result;
}

// ---------------------------------------------------------------------------
// Forward: EquitySwap
// ---------------------------------------------------------------------------

equity_mapping_result equity_instrument_mapper::forward_equity_swap(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping EquitySwap: "
                               << std::string(t.id);
    equity_mapping_result result;
    result.instrument = make_base("EquitySwap");
    if (!t.EquitySwapData) return result;
    const auto& d = *t.EquitySwapData;

    for (const auto& ld : d.LegData) {
        if (ld.legDataType && ld.legDataType->EquityLegData) {
            const auto& el = *ld.legDataType->EquityLegData;
            result.instrument.underlying_code =
                extract_underlying_name(el.underlyingTypes);
            result.instrument.return_type = std::string(el.ReturnType);
            if (el.Quantity)
                result.instrument.quantity =
                    static_cast<double>(*el.Quantity);
        } else {
            // Non-equity leg provides currency/notional/schedule
            if (ld.Currency)
                result.instrument.currency = std::string(*ld.Currency);
            if (ld.Notionals && !ld.Notionals->Notional.empty())
                result.instrument.notional =
                    static_cast<double>(ld.Notionals->Notional.front());
            if (ld.DayCounter)
                result.instrument.day_count_code =
                    to_string(*ld.DayCounter);
            if (ld.ScheduleData && !ld.ScheduleData->Rules.empty()) {
                const auto& rule = ld.ScheduleData->Rules.front();
                result.instrument.start_date = std::string(rule.StartDate);
                if (rule.EndDate)
                    result.instrument.maturity_date =
                        std::string(*rule.EndDate);
                result.instrument.payment_frequency_code =
                    std::string(rule.Tenor);
            }
        }
    }
    return result;
}

// ---------------------------------------------------------------------------
// Forward: EquityVarianceSwap
// ---------------------------------------------------------------------------

equity_mapping_result equity_instrument_mapper::forward_equity_variance_swap(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping EquityVarianceSwap: "
                               << std::string(t.id);
    equity_mapping_result result;
    result.instrument = make_base("EquityVarianceSwap");
    if (!t.EquityVarianceSwapData) return result;
    const auto& d = *t.EquityVarianceSwapData;

    result.instrument.underlying_code =
        extract_underlying_name(d.underlyingTypes);
    result.instrument.currency = to_string(d.Currency);
    result.instrument.notional = static_cast<double>(d.Notional);
    result.instrument.variance_strike = static_cast<double>(d.Strike);
    result.instrument.start_date = std::string(d.StartDate);
    result.instrument.maturity_date = std::string(d.EndDate);
    return result;
}

// ---------------------------------------------------------------------------
// Forward: EquityBarrierOption
// ---------------------------------------------------------------------------

equity_mapping_result equity_instrument_mapper::forward_equity_barrier_option(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping EquityBarrierOption: "
                               << std::string(t.id);
    equity_mapping_result result;
    result.instrument = make_base("EquityBarrierOption");
    if (!t.EquityBarrierOptionData) return result;
    const auto& d = *t.EquityBarrierOptionData;

    result.instrument.underlying_code =
        extract_underlying_name(d.underlyingTypes);
    result.instrument.currency = to_string(d.Currency);
    result.instrument.quantity = static_cast<double>(d.Quantity);
    result.instrument.option_type = extract_option_type(d.OptionData);
    result.instrument.exercise_type = extract_exercise_style(d.OptionData);
    result.instrument.maturity_date = first_exercise_date(d.OptionData);
    result.instrument.strike_price = extract_strike(d.strikeGroup);
    if (d.StartDate)
        result.instrument.start_date = std::string(*d.StartDate);
    result.instrument.barrier_type = barrier_type_str(d.BarrierData);
    result.instrument.lower_barrier = first_barrier_level(d.BarrierData);
    result.instrument.upper_barrier = second_barrier_level(d.BarrierData);
    return result;
}

// ---------------------------------------------------------------------------
// Forward: EquityAsianOption
// ---------------------------------------------------------------------------

equity_mapping_result equity_instrument_mapper::forward_equity_asian_option(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping EquityAsianOption: "
                               << std::string(t.id);
    equity_mapping_result result;
    result.instrument = make_base("EquityAsianOption");
    if (!t.EquityAsianOptionData) return result;
    const auto& d = *t.EquityAsianOptionData;

    if (d.Underlying)
        result.instrument.underlying_code = std::string(d.Underlying->Name);
    result.instrument.currency = to_string(d.Currency);
    result.instrument.quantity = static_cast<double>(d.Quantity);
    result.instrument.option_type = extract_option_type(d.OptionData);
    result.instrument.maturity_date = first_exercise_date(d.OptionData);
    result.instrument.strike_price = extract_strike(d.strikeGroup);
    if (d.ObservationDates && !d.ObservationDates->Rules.empty())
        result.instrument.averaging_start_date =
            std::string(d.ObservationDates->Rules.front().StartDate);
    return result;
}

// ---------------------------------------------------------------------------
// Forward: EquityDigitalOption
// ---------------------------------------------------------------------------

equity_mapping_result equity_instrument_mapper::forward_equity_digital_option(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping EquityDigitalOption: "
                               << std::string(t.id);
    equity_mapping_result result;
    result.instrument = make_base("EquityDigitalOption");
    if (!t.EquityDigitalOptionData) return result;
    const auto& d = *t.EquityDigitalOptionData;

    result.instrument.underlying_code =
        extract_underlying_name(d.underlyingTypes);
    result.instrument.quantity = static_cast<double>(d.Quantity);
    result.instrument.option_type = extract_option_type(d.OptionData);
    result.instrument.maturity_date = first_exercise_date(d.OptionData);
    result.instrument.strike_price = static_cast<double>(d.Strike);
    result.instrument.notional = static_cast<double>(d.PayoffAmount);
    if (d.PayoffCurrency)
        result.instrument.currency = to_string(*d.PayoffCurrency);
    return result;
}

// ---------------------------------------------------------------------------
// Forward: EquityTouchOption
// ---------------------------------------------------------------------------

equity_mapping_result equity_instrument_mapper::forward_equity_touch_option(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping EquityTouchOption: "
                               << std::string(t.id);
    equity_mapping_result result;
    result.instrument = make_base("EquityTouchOption");
    if (!t.EquityTouchOptionData) return result;
    const auto& d = *t.EquityTouchOptionData;

    result.instrument.underlying_code =
        extract_underlying_name(d.underlyingTypes);
    result.instrument.currency = to_string(d.PayoffCurrency);
    result.instrument.notional = static_cast<double>(d.PayoffAmount);
    result.instrument.option_type = extract_option_type(d.OptionData);
    result.instrument.maturity_date = first_exercise_date(d.OptionData);
    if (d.StartDate)
        result.instrument.start_date = std::string(*d.StartDate);
    result.instrument.barrier_type = barrier_type_str(d.BarrierData);
    result.instrument.lower_barrier = first_barrier_level(d.BarrierData);
    return result;
}

// ---------------------------------------------------------------------------
// Forward: EquityOutperformanceOption
// ---------------------------------------------------------------------------

equity_mapping_result
equity_instrument_mapper::forward_equity_outperformance_option(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping EquityOutperformanceOption: "
                               << std::string(t.id);
    equity_mapping_result result;
    result.instrument = make_base("EquityOutperformanceOption");
    if (!t.EquityOutperformanceOptionData) return result;
    const auto& d = *t.EquityOutperformanceOptionData;

    result.instrument.currency = to_string(d.Currency);
    result.instrument.notional = static_cast<double>(d.Notional);
    result.instrument.option_type = extract_option_type(d.OptionData);
    result.instrument.maturity_date = first_exercise_date(d.OptionData);
    result.instrument.strike_price = static_cast<double>(d.StrikeReturn);

    // Store both underlyings as a JSON array
    const std::string n1 = std::string(d.Underlying1.Name);
    const std::string n2 = std::string(d.Underlying2.Name);
    result.instrument.basket_json =
        "[\"" + json_escape(n1) + "\",\"" + json_escape(n2) + "\"]";
    result.instrument.underlying_code = n1;
    return result;
}

// ---------------------------------------------------------------------------
// Forward: EquityAccumulator
// ---------------------------------------------------------------------------

equity_mapping_result equity_instrument_mapper::forward_equity_accumulator(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping EquityAccumulator: "
                               << std::string(t.id);
    equity_mapping_result result;
    result.instrument = make_base("EquityAccumulator");
    if (!t.EquityAccumulatorData) return result;
    const auto& d = *t.EquityAccumulatorData;

    result.instrument.underlying_code = std::string(d.Underlying.Name);
    result.instrument.currency = to_string(d.Currency);
    result.instrument.accumulation_amount =
        static_cast<double>(d.FixingAmount);
    if (d.Strike)
        result.instrument.strike_price = static_cast<double>(*d.Strike);
    if (d.StartDate)
        result.instrument.start_date = std::string(*d.StartDate);

    // Capture first knock-out barrier
    if (d.Barriers) {
        for (const auto& bd : d.Barriers->BarrierData) {
            const auto btype = to_string(bd.Type);
            if ((btype == "DownAndOut" || btype == "UpAndOut") &&
                    !bd.Levels.Level.empty()) {
                result.instrument.knock_out_barrier =
                    static_cast<double>(bd.Levels.Level.front());
                break;
            }
        }
    }
    return result;
}

// ---------------------------------------------------------------------------
// Forward: EquityTaRF
// ---------------------------------------------------------------------------

equity_mapping_result equity_instrument_mapper::forward_equity_tarf(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping EquityTaRF: "
                               << std::string(t.id);
    equity_mapping_result result;
    result.instrument = make_base("EquityTaRF");
    if (!t.EquityTaRFData) return result;
    const auto& d = *t.EquityTaRFData;

    result.instrument.underlying_code = std::string(d.Underlying.Name);
    result.instrument.currency = to_string(d.Currency);
    result.instrument.accumulation_amount =
        static_cast<double>(d.FixingAmount);
    if (d.Strike)
        result.instrument.strike_price = static_cast<double>(*d.Strike);

    // Capture FixingCap barrier level as knock_out
    for (const auto& bd : d.Barriers.BarrierData) {
        if (to_string(bd.Type) == "FixingCap" && !bd.Levels.Level.empty()) {
            result.instrument.knock_out_barrier =
                static_cast<double>(bd.Levels.Level.front());
            break;
        }
    }
    return result;
}

// ---------------------------------------------------------------------------
// Forward: EquityCliquetOption
// ---------------------------------------------------------------------------

equity_mapping_result equity_instrument_mapper::forward_equity_cliquet_option(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping EquityCliquetOption: "
                               << std::string(t.id);
    equity_mapping_result result;
    result.instrument = make_base("EquityCliquetOption");
    if (!t.EquityCliquetOptionData) return result;
    const auto& d = *t.EquityCliquetOptionData;

    result.instrument.underlying_code =
        extract_underlying_name(d.underlyingTypes);
    result.instrument.currency = to_string(d.Currency);
    result.instrument.notional = static_cast<double>(d.Notional);
    result.instrument.option_type = to_string(d.OptionType);

    // Extract tenor from schedule rules if available
    if (!d.ScheduleData.Rules.empty())
        result.instrument.cliquet_frequency_code =
            std::string(d.ScheduleData.Rules.front().Tenor);
    else if (!d.ScheduleData.Dates.empty() &&
             d.ScheduleData.Dates.front().Dates.Date.size() >= 2) {
        // For date-based schedules store the maturity date
        result.instrument.maturity_date = std::string(
            d.ScheduleData.Dates.front().Dates.Date.back());
    }
    return result;
}

// ---------------------------------------------------------------------------
// Forward: EquityWorstOfBasketSwap
// ---------------------------------------------------------------------------

equity_mapping_result
equity_instrument_mapper::forward_equity_worst_of_basket_swap(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping EquityWorstOfBasketSwap: "
                               << std::string(t.id);
    equity_mapping_result result;
    result.instrument = make_base("EquityWorstOfBasketSwap");
    if (!t.EquityWorstOfBasketSwapData) return result;
    const auto& d = *t.EquityWorstOfBasketSwapData;

    result.instrument.currency = to_string(d.Currency);
    result.instrument.quantity = static_cast<double>(d.Quantity);
    result.instrument.basket_json = underlyings_to_json(d.Underlyings);
    if (!d.Underlyings.Underlying.empty())
        result.instrument.underlying_code =
            std::string(d.Underlyings.Underlying.front().Name);
    return result;
}

// ---------------------------------------------------------------------------
// Reverse: EquityOption
// ---------------------------------------------------------------------------

trade equity_instrument_mapper::reverse_equity_option(
        const equity_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping EquityOption";
    trade t;
    t.TradeType = oreTradeType::EquityOption;
    equityOptionData d;
    d.OptionData = make_option_data(instr);
    d.underlyingTypes = make_underlying_type(instr.underlying_code);
    static_cast<std::string&>(d.Currency) = instr.currency;
    d.Quantity = static_cast<float>(instr.quantity);
    if (instr.strike_price != 0.0) {
        _Strike_t s;
        static_cast<std::string&>(s) = std::to_string(instr.strike_price);
        d.strikeGroup.Strike = std::move(s);
    }
    t.EquityOptionData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: EquityForward
// ---------------------------------------------------------------------------

trade equity_instrument_mapper::reverse_equity_forward(
        const equity_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping EquityForward";
    trade t;
    t.TradeType = oreTradeType::EquityForward;
    equityForwardData d;
    d.LongShort = longShort::Long;
    static_cast<std::string&>(d.Maturity) = instr.maturity_date;
    d.underlyingTypes = make_underlying_type(instr.underlying_code);
    static_cast<std::string&>(d.Currency) = instr.currency;
    d.Strike = static_cast<float>(instr.strike_price);
    d.Quantity = static_cast<float>(instr.quantity);
    t.EquityForwardData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: EquitySwap
// ---------------------------------------------------------------------------

trade equity_instrument_mapper::reverse_equity_swap(
        const equity_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping EquitySwap";
    trade t;
    t.TradeType = oreTradeType::EquitySwap;
    swapData d;

    // Equity leg
    legData eqLeg;
    eqLeg.LegType = legType::Equity;
    eqLeg.Payer = true;
    _EquityLegData_t el;
    el.underlyingTypes = make_underlying_type(instr.underlying_code);
    static_cast<std::string&>(el.ReturnType) = instr.return_type;
    if (instr.quantity != 0.0)
        el.Quantity = static_cast<float>(instr.quantity);
    legDataType_group_t ldt;
    ldt.EquityLegData = std::move(el);
    eqLeg.legDataType = std::move(ldt);
    d.LegData.push_back(std::move(eqLeg));

    // Funding leg
    legData fundLeg;
    fundLeg.LegType = legType::Floating;
    fundLeg.Payer = false;
    if (!instr.currency.empty())
        fundLeg.Currency = instr.currency;
    if (instr.notional != 0.0) {
        legData_Notionals_t n;
        legData_Notionals_t_Notional_t nv;
        static_cast<float&>(nv) = static_cast<float>(instr.notional);
        n.Notional.push_back(nv);
        fundLeg.Notionals = std::move(n);
    }
    d.LegData.push_back(std::move(fundLeg));

    t.EquitySwapData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: EquityVarianceSwap
// ---------------------------------------------------------------------------

trade equity_instrument_mapper::reverse_equity_variance_swap(
        const equity_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping EquityVarianceSwap";
    trade t;
    t.TradeType = oreTradeType::EquityVarianceSwap;
    varianceSwapData d;
    static_cast<std::string&>(d.StartDate) = instr.start_date;
    static_cast<std::string&>(d.EndDate) = instr.maturity_date;
    d.Currency = parse_currency_code(instr.currency);
    d.underlyingTypes = make_underlying_type(instr.underlying_code);
    static_cast<std::string&>(d.LongShort) = "Long";
    d.Strike = static_cast<float>(instr.variance_strike);
    d.Notional = static_cast<float>(instr.notional);
    static_cast<std::string&>(d.Calendar) = "USD";
    t.EquityVarianceSwapData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: EquityBarrierOption
// ---------------------------------------------------------------------------

trade equity_instrument_mapper::reverse_equity_barrier_option(
        const equity_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping EquityBarrierOption";
    trade t;
    t.TradeType = oreTradeType::EquityBarrierOption;
    eqBarrierOptionData d;
    d.OptionData = make_option_data(instr);
    d.underlyingTypes = make_underlying_type(instr.underlying_code);
    d.Currency = parse_currency_code(instr.currency);
    d.Quantity = static_cast<float>(instr.quantity);
    if (instr.strike_price != 0.0) {
        _Strike_t s;
        static_cast<std::string&>(s) = std::to_string(instr.strike_price);
        d.strikeGroup.Strike = std::move(s);
    }
    if (!instr.barrier_type.empty()) {
        barrierData bd;
        if (instr.barrier_type == "UpAndOut")
            bd.Type = barrierType::UpAndOut;
        else if (instr.barrier_type == "UpAndIn")
            bd.Type = barrierType::UpAndIn;
        else if (instr.barrier_type == "DownAndOut")
            bd.Type = barrierType::DownAndOut;
        else if (instr.barrier_type == "DownAndIn")
            bd.Type = barrierType::DownAndIn;
        else
            throw std::runtime_error(
                "reverse_equity_barrier_option: unrecognized barrier type '"
                + instr.barrier_type + "'");
        if (instr.lower_barrier != 0.0)
            bd.Levels.Level.push_back(
                static_cast<float>(instr.lower_barrier));
        d.BarrierData = std::move(bd);
    }
    t.EquityBarrierOptionData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: EquityAsianOption
// ---------------------------------------------------------------------------

trade equity_instrument_mapper::reverse_equity_asian_option(
        const equity_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping EquityAsianOption";
    trade t;
    t.TradeType = oreTradeType::EquityAsianOption;
    singleUnderlyingAsianOptionData d;
    d.OptionData = make_option_data(instr);
    d.Currency = parse_currency_code(instr.currency);
    d.Quantity = static_cast<float>(instr.quantity);
    if (instr.strike_price != 0.0) {
        _StrikeData_t sd;
        sd.Value = static_cast<float>(instr.strike_price);
        d.strikeGroup.StrikeData = std::move(sd);
    }
    if (!instr.underlying_code.empty()) {
        underlying u;
        static_cast<std::string&>(u.Name) = instr.underlying_code;
        static_cast<std::string&>(u.Type) = "Equity";
        d.Underlying = std::move(u);
    }
    t.EquityAsianOptionData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: EquityDigitalOption
// ---------------------------------------------------------------------------

trade equity_instrument_mapper::reverse_equity_digital_option(
        const equity_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping EquityDigitalOption";
    trade t;
    t.TradeType = oreTradeType::EquityDigitalOption;
    eqDigitalOptionData d;
    d.OptionData = make_option_data(instr);
    d.underlyingTypes = make_underlying_type(instr.underlying_code);
    d.Strike = static_cast<float>(instr.strike_price);
    d.PayoffAmount = static_cast<float>(instr.notional);
    d.Quantity = static_cast<float>(instr.quantity);
    t.EquityDigitalOptionData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: EquityTouchOption
// ---------------------------------------------------------------------------

trade equity_instrument_mapper::reverse_equity_touch_option(
        const equity_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping EquityTouchOption";
    trade t;
    t.TradeType = oreTradeType::EquityTouchOption;
    eqTouchOptionData d;
    d.OptionData = make_option_data(instr);
    d.underlyingTypes = make_underlying_type(instr.underlying_code);
    d.PayoffCurrency = parse_currency_code(instr.currency);
    d.PayoffAmount = static_cast<float>(instr.notional);
    if (!instr.barrier_type.empty()) {
        barrierData bd;
        if (instr.barrier_type == "UpAndIn")
            bd.Type = barrierType::UpAndIn;
        else if (instr.barrier_type == "UpAndOut")
            bd.Type = barrierType::UpAndOut;
        else if (instr.barrier_type == "DownAndIn")
            bd.Type = barrierType::DownAndIn;
        else
            bd.Type = barrierType::DownAndOut;
        if (instr.lower_barrier != 0.0)
            bd.Levels.Level.push_back(static_cast<float>(instr.lower_barrier));
        d.BarrierData = std::move(bd);
    }
    t.EquityTouchOptionData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: EquityOutperformanceOption
// ---------------------------------------------------------------------------

trade equity_instrument_mapper::reverse_equity_outperformance_option(
        const equity_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping EquityOutperformanceOption";
    trade t;
    t.TradeType = oreTradeType::EquityOutperformanceOption;
    eqOutperformanceOptionData d;
    d.OptionData = make_option_data(instr);
    d.Currency = parse_currency_code(instr.currency);
    d.Notional = static_cast<float>(instr.notional);
    d.StrikeReturn = static_cast<float>(instr.strike_price);
    // Reconstruct underlyings from basket_json
    const auto names = parse_json_string_array(instr.basket_json);
    const std::string u1 = names.size() > 0 ? names[0] : instr.underlying_code;
    const std::string u2 = names.size() > 1 ? names[1] : u1;
    static_cast<std::string&>(d.Underlying1.Name) = u1;
    static_cast<std::string&>(d.Underlying1.Type) = "Equity";
    static_cast<std::string&>(d.Underlying2.Name) = u2;
    static_cast<std::string&>(d.Underlying2.Type) = "Equity";
    t.EquityOutperformanceOptionData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: EquityAccumulator
// ---------------------------------------------------------------------------

trade equity_instrument_mapper::reverse_equity_accumulator(
        const equity_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping EquityAccumulator";
    trade t;
    t.TradeType = oreTradeType::EquityAccumulator;
    accumulatorData d;
    d.Currency = parse_currency_code(instr.currency);
    d.FixingAmount = static_cast<float>(instr.accumulation_amount);
    if (instr.strike_price != 0.0)
        d.Strike = static_cast<float>(instr.strike_price);
    static_cast<std::string&>(d.Underlying.Name) = instr.underlying_code;
    static_cast<std::string&>(d.Underlying.Type) = "Equity";
    static_cast<std::string&>(d.OptionData.LongShort) = "Long";
    if (!instr.start_date.empty()) {
        date sd;
        static_cast<std::string&>(sd) = instr.start_date;
        d.StartDate = std::move(sd);
    }
    // Minimal observation schedule
    scheduleData_Rules_t rule;
    if (!instr.start_date.empty())
        static_cast<std::string&>(rule.StartDate) = instr.start_date;
    static_cast<std::string&>(rule.Tenor) = "1D";
    d.ObservationDates.Rules.push_back(std::move(rule));
    t.EquityAccumulatorData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: EquityTaRF
// ---------------------------------------------------------------------------

trade equity_instrument_mapper::reverse_equity_tarf(
        const equity_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping EquityTaRF";
    trade t;
    t.TradeType = oreTradeType::EquityTaRF;
    tarfData2 d;
    d.Currency = parse_currency_code(instr.currency);
    d.FixingAmount = static_cast<float>(instr.accumulation_amount);
    if (instr.strike_price != 0.0)
        d.Strike = static_cast<float>(instr.strike_price);
    static_cast<std::string&>(d.Underlying.Name) = instr.underlying_code;
    static_cast<std::string&>(d.Underlying.Type) = "Equity";
    static_cast<std::string&>(d.OptionData.LongShort) = "Long";
    // Minimal schedule
    scheduleData_Rules_t rule;
    static_cast<std::string&>(rule.Tenor) = "1Y";
    d.ScheduleData.Rules.push_back(std::move(rule));
    t.EquityTaRFData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: EquityCliquetOption
// ---------------------------------------------------------------------------

trade equity_instrument_mapper::reverse_equity_cliquet_option(
        const equity_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping EquityCliquetOption";
    trade t;
    t.TradeType = oreTradeType::EquityCliquetOption;
    cliquetOptionData d;
    d.underlyingTypes = make_underlying_type(instr.underlying_code);
    d.Currency = parse_currency_code(instr.currency);
    d.Notional = static_cast<float>(instr.notional);
    d.LongShort = longShort::Long;
    if (instr.option_type == "Put")
        d.OptionType = optionType::Put;
    else if (instr.option_type == "Call")
        d.OptionType = optionType::Call;
    else
        throw std::runtime_error(
            "reverse_equity_cliquet_option: unrecognized option type '"
            + instr.option_type + "'");
    d.Moneyness = 1.0f;
    // Schedule
    if (!instr.cliquet_frequency_code.empty()) {
        scheduleData_Rules_t rule;
        static_cast<std::string&>(rule.Tenor) =
            instr.cliquet_frequency_code;
        d.ScheduleData.Rules.push_back(std::move(rule));
    }
    t.EquityCliquetOptionData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: EquityWorstOfBasketSwap
// ---------------------------------------------------------------------------

trade equity_instrument_mapper::reverse_equity_worst_of_basket_swap(
        const equity_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping EquityWorstOfBasketSwap";
    trade t;
    t.TradeType = oreTradeType::EquityWorstOfBasketSwap;
    worstOfBasketSwapData2 d;
    d.LongShort = longShort::Long;
    d.Currency = parse_currency_code(instr.currency);
    d.Quantity = static_cast<float>(instr.quantity);
    d.FixedRate = 0.0f;
    static_cast<std::string&>(d.FloatingIndex) = "EUR-EURIBOR-3M";
    d.FloatingDayCountFraction = dayCounter::Actual_360;
    // Reconstruct underlyings from basket_json
    const auto names = parse_json_string_array(instr.basket_json);
    if (!names.empty()) {
        for (const auto& name : names) {
            underlying u;
            static_cast<std::string&>(u.Name) = name;
            static_cast<std::string&>(u.Type) = "Equity";
            d.Underlyings.Underlying.push_back(std::move(u));
        }
    } else if (!instr.underlying_code.empty()) {
        underlying u;
        static_cast<std::string&>(u.Name) = instr.underlying_code;
        static_cast<std::string&>(u.Type) = "Equity";
        d.Underlyings.Underlying.push_back(std::move(u));
    }
    // Minimal schedules required by XSD
    scheduleData_Rules_t rule;
    static_cast<std::string&>(rule.Tenor) = "6M";
    d.FloatingPeriodSchedule.Rules.push_back(rule);
    d.FloatingPayDates.Rules.push_back(std::move(rule));
    t.EquityWorstOfBasketSwapData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Forward: EquityDoubleBarrierOption (same struct as EquityBarrierOption)
// ---------------------------------------------------------------------------

equity_mapping_result
equity_instrument_mapper::forward_equity_double_barrier_option(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping EquityDoubleBarrierOption: "
                               << std::string(t.id);
    equity_mapping_result result;
    result.instrument = make_base("EquityDoubleBarrierOption");
    if (!t.EquityDoubleBarrierOptionData) return result;
    const auto& d = *t.EquityDoubleBarrierOptionData;
    result.instrument.underlying_code =
        extract_underlying_name(d.underlyingTypes);
    result.instrument.currency = to_string(d.Currency);
    result.instrument.quantity = static_cast<double>(d.Quantity);
    result.instrument.option_type = extract_option_type(d.OptionData);
    result.instrument.exercise_type = extract_exercise_style(d.OptionData);
    result.instrument.maturity_date = first_exercise_date(d.OptionData);
    result.instrument.strike_price = extract_strike(d.strikeGroup);
    if (d.StartDate)
        result.instrument.start_date = std::string(*d.StartDate);
    result.instrument.barrier_type = barrier_type_str(d.BarrierData);
    result.instrument.lower_barrier = first_barrier_level(d.BarrierData);
    result.instrument.upper_barrier = second_barrier_level(d.BarrierData);
    return result;
}

// ---------------------------------------------------------------------------
// Forward: EquityEuropeanBarrierOption (same struct as EquityBarrierOption)
// ---------------------------------------------------------------------------

equity_mapping_result
equity_instrument_mapper::forward_equity_european_barrier_option(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug)
        << "Forward-mapping EquityEuropeanBarrierOption: "
        << std::string(t.id);
    equity_mapping_result result;
    result.instrument = make_base("EquityEuropeanBarrierOption");
    if (!t.EquityEuropeanBarrierOptionData) return result;
    const auto& d = *t.EquityEuropeanBarrierOptionData;
    result.instrument.underlying_code =
        extract_underlying_name(d.underlyingTypes);
    result.instrument.currency = to_string(d.Currency);
    result.instrument.quantity = static_cast<double>(d.Quantity);
    result.instrument.option_type = extract_option_type(d.OptionData);
    result.instrument.exercise_type = extract_exercise_style(d.OptionData);
    result.instrument.maturity_date = first_exercise_date(d.OptionData);
    result.instrument.strike_price = extract_strike(d.strikeGroup);
    if (d.StartDate)
        result.instrument.start_date = std::string(*d.StartDate);
    result.instrument.barrier_type = barrier_type_str(d.BarrierData);
    result.instrument.lower_barrier = first_barrier_level(d.BarrierData);
    result.instrument.upper_barrier = second_barrier_level(d.BarrierData);
    return result;
}

// ---------------------------------------------------------------------------
// Reverse: EquityDoubleBarrierOption
// ---------------------------------------------------------------------------

trade equity_instrument_mapper::reverse_equity_double_barrier_option(
        const equity_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping EquityDoubleBarrierOption";
    trade t;
    t.TradeType = oreTradeType::EquityDoubleBarrierOption;
    eqBarrierOptionData d;
    d.OptionData = make_option_data(instr);
    d.underlyingTypes = make_underlying_type(instr.underlying_code);
    d.Currency = parse_currency_code(instr.currency);
    d.Quantity = static_cast<float>(instr.quantity);
    if (instr.strike_price != 0.0) {
        _Strike_t s;
        static_cast<std::string&>(s) = std::to_string(instr.strike_price);
        d.strikeGroup.Strike = std::move(s);
    }
    if (!instr.barrier_type.empty()) {
        barrierData bd;
        if (instr.barrier_type == "UpAndOut")        bd.Type = barrierType::UpAndOut;
        else if (instr.barrier_type == "UpAndIn")    bd.Type = barrierType::UpAndIn;
        else if (instr.barrier_type == "DownAndOut") bd.Type = barrierType::DownAndOut;
        else if (instr.barrier_type == "DownAndIn")  bd.Type = barrierType::DownAndIn;
        else
            throw std::runtime_error(
                "reverse_equity_double_barrier_option: unrecognized barrier type '"
                + instr.barrier_type + "'");
        if (instr.lower_barrier != 0.0)
            bd.Levels.Level.push_back(static_cast<float>(instr.lower_barrier));
        if (instr.upper_barrier != 0.0)
            bd.Levels.Level.push_back(static_cast<float>(instr.upper_barrier));
        d.BarrierData = std::move(bd);
    }
    t.EquityDoubleBarrierOptionData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: EquityEuropeanBarrierOption
// ---------------------------------------------------------------------------

trade equity_instrument_mapper::reverse_equity_european_barrier_option(
        const equity_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug)
        << "Reverse-mapping EquityEuropeanBarrierOption";
    trade t;
    t.TradeType = oreTradeType::EquityEuropeanBarrierOption;
    eqBarrierOptionData d;
    d.OptionData = make_option_data(instr);
    d.underlyingTypes = make_underlying_type(instr.underlying_code);
    d.Currency = parse_currency_code(instr.currency);
    d.Quantity = static_cast<float>(instr.quantity);
    if (instr.strike_price != 0.0) {
        _Strike_t s;
        static_cast<std::string&>(s) = std::to_string(instr.strike_price);
        d.strikeGroup.Strike = std::move(s);
    }
    if (!instr.barrier_type.empty()) {
        barrierData bd;
        if (instr.barrier_type == "UpAndOut")        bd.Type = barrierType::UpAndOut;
        else if (instr.barrier_type == "UpAndIn")    bd.Type = barrierType::UpAndIn;
        else if (instr.barrier_type == "DownAndOut") bd.Type = barrierType::DownAndOut;
        else if (instr.barrier_type == "DownAndIn")  bd.Type = barrierType::DownAndIn;
        else
            throw std::runtime_error(
                "reverse_equity_european_barrier_option: unrecognized barrier type '"
                + instr.barrier_type + "'");
        if (instr.lower_barrier != 0.0)
            bd.Levels.Level.push_back(static_cast<float>(instr.lower_barrier));
        if (instr.upper_barrier != 0.0)
            bd.Levels.Level.push_back(static_cast<float>(instr.upper_barrier));
        d.BarrierData = std::move(bd);
    }
    t.EquityEuropeanBarrierOptionData = std::move(d);
    return t;
}

}
