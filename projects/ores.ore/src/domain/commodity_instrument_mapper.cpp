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
#include "ores.ore/domain/commodity_instrument_mapper.hpp"

#include <map>
#include <stdexcept>

namespace ores::ore::domain {

using namespace ores::logging;
using ores::trading::domain::commodity_instrument;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

namespace {

commodity_instrument make_base(const std::string& trade_type_code) {
    commodity_instrument r;
    r.trade_type_code = trade_type_code;
    r.modified_by = "ores";
    r.performed_by = "ores";
    r.change_reason_code = "system.external_data_import";
    r.change_commentary = "Imported from ORE XML";
    return r;
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

// Extract option type string from optionData.
std::string extract_option_type(const optionData& od) {
    if (od.OptionType) return std::string(*od.OptionType);
    return {};
}

// Extract exercise style string from optionData.
std::string extract_exercise_style(const optionData& od) {
    if (od.Style) return std::string(*od.Style);
    return {};
}

// Extract first exercise date from optionData.
std::string first_exercise_date(const optionData& od) {
    if (!od.exerciseDatesGroup) return {};
    if (!od.exerciseDatesGroup->ExerciseDates) return {};
    if (od.exerciseDatesGroup->ExerciseDates->ExerciseDate.empty()) return {};
    return std::string(
        od.exerciseDatesGroup->ExerciseDates->ExerciseDate.front());
}

// Extract underlying name from underlyingTypes_group_t.
std::string extract_underlying_name(const underlyingTypes_group_t& u) {
    if (u.Name) return std::string(*u.Name);
    if (u.Underlying) return std::string(u.Underlying->Name);
    return {};
}

// Extract commodity code and currency from the first commodity floating leg
// in a legData vector.
struct swap_leg_info {
    std::string commodity_code;
    std::string currency;
    std::string start_date;
    std::string maturity_date;
};

swap_leg_info extract_floating_leg_info(
        const xsd::vector<legData>& legs) {
    swap_leg_info info;
    for (const auto& leg : legs) {
        if (!leg.legDataType) continue;
        if (!leg.legDataType->CommodityFloatingLegData) continue;
        const auto& fl = *leg.legDataType->CommodityFloatingLegData;
        info.commodity_code = std::string(fl.Name);
        if (leg.Currency)
            info.currency = std::string(*leg.Currency);
        if (leg.ScheduleData && !leg.ScheduleData->Rules.empty()) {
            const auto& r = leg.ScheduleData->Rules.front();
            info.start_date = std::string(r.StartDate);
            if (r.EndDate)
                info.maturity_date = std::string(*r.EndDate);
        } else if (leg.ScheduleData && !leg.ScheduleData->Dates.empty()) {
            const auto& dates = leg.ScheduleData->Dates.front().Dates.Date;
            if (!dates.empty()) {
                info.start_date = std::string(dates.front());
                info.maturity_date = std::string(dates.back());
            }
        }
        break; // use first floating leg
    }
    return info;
}

optionData make_option_data(const commodity_instrument& instr) {
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

} // namespace

// ---------------------------------------------------------------------------
// Forward: CommodityForward
// ---------------------------------------------------------------------------

commodity_mapping_result commodity_instrument_mapper::forward_commodity_forward(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping CommodityForward: "
                               << std::string(t.id);
    commodity_mapping_result result;
    result.instrument = make_base("CommodityForward");
    if (!t.CommodityForwardData) return result;
    const auto& d = *t.CommodityForwardData;

    result.instrument.commodity_code = std::string(d.Name);
    result.instrument.currency = to_string(d.Currency);
    result.instrument.quantity = static_cast<double>(d.Quantity);
    result.instrument.fixed_price = static_cast<double>(d.Strike);
    result.instrument.maturity_date = std::string(d.Maturity);
    return result;
}

// ---------------------------------------------------------------------------
// Forward: CommodityOption
// ---------------------------------------------------------------------------

commodity_mapping_result commodity_instrument_mapper::forward_commodity_option(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping CommodityOption: "
                               << std::string(t.id);
    commodity_mapping_result result;
    result.instrument = make_base("CommodityOption");
    if (!t.CommodityOptionData) return result;
    const auto& d = *t.CommodityOptionData;

    result.instrument.commodity_code = std::string(d.Name);
    result.instrument.currency = to_string(d.Currency);
    result.instrument.quantity = static_cast<double>(d.Quantity);
    result.instrument.strike_price = static_cast<double>(d.Strike);
    result.instrument.option_type = extract_option_type(d.OptionData);
    result.instrument.exercise_type = extract_exercise_style(d.OptionData);
    result.instrument.maturity_date = first_exercise_date(d.OptionData);
    return result;
}

// ---------------------------------------------------------------------------
// Forward: CommoditySwap
// ---------------------------------------------------------------------------

commodity_mapping_result commodity_instrument_mapper::forward_commodity_swap(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping CommoditySwap: "
                               << std::string(t.id);
    commodity_mapping_result result;
    result.instrument = make_base("CommoditySwap");
    if (!t.SwapData) return result;
    const auto& d = *t.SwapData;

    const auto info = extract_floating_leg_info(d.LegData);
    result.instrument.commodity_code = info.commodity_code;
    result.instrument.currency = info.currency;
    result.instrument.start_date = info.start_date;
    result.instrument.maturity_date = info.maturity_date;
    return result;
}

// ---------------------------------------------------------------------------
// Forward: CommoditySwaption
// ---------------------------------------------------------------------------

commodity_mapping_result
commodity_instrument_mapper::forward_commodity_swaption(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping CommoditySwaption: "
                               << std::string(t.id);
    commodity_mapping_result result;
    result.instrument = make_base("CommoditySwaption");
    if (!t.CommoditySwaptionData) return result;
    const auto& d = *t.CommoditySwaptionData;

    result.instrument.swaption_expiry_date =
        first_exercise_date(d.OptionData);
    const auto info = extract_floating_leg_info(d.LegData);
    result.instrument.commodity_code = info.commodity_code;
    result.instrument.currency = info.currency;
    result.instrument.start_date = info.start_date;
    result.instrument.maturity_date = info.maturity_date;
    return result;
}

// ---------------------------------------------------------------------------
// Forward: CommodityVarianceSwap
// ---------------------------------------------------------------------------

commodity_mapping_result
commodity_instrument_mapper::forward_commodity_variance_swap(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping CommodityVarianceSwap: "
                               << std::string(t.id);
    commodity_mapping_result result;
    result.instrument = make_base("CommodityVarianceSwap");
    if (!t.CommodityVarianceSwapData) return result;
    const auto& d = *t.CommodityVarianceSwapData;

    result.instrument.commodity_code =
        extract_underlying_name(d.underlyingTypes);
    result.instrument.currency = to_string(d.Currency);
    result.instrument.start_date = std::string(d.StartDate);
    result.instrument.maturity_date = std::string(d.EndDate);
    result.instrument.variance_strike = static_cast<double>(d.Strike);
    result.instrument.quantity = static_cast<double>(d.Notional);
    return result;
}

// ---------------------------------------------------------------------------
// Forward: CommodityAveragePriceOption
// ---------------------------------------------------------------------------

commodity_mapping_result commodity_instrument_mapper::forward_commodity_apo(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping CommodityAveragePriceOption: "
                               << std::string(t.id);
    commodity_mapping_result result;
    result.instrument = make_base("CommodityAveragePriceOption");
    if (!t.CommodityAveragePriceOptionData) return result;
    const auto& d = *t.CommodityAveragePriceOptionData;

    result.instrument.commodity_code = std::string(d.Name);
    result.instrument.currency = to_string(d.Currency);
    result.instrument.quantity = static_cast<double>(d.Quantity);
    result.instrument.strike_price = static_cast<double>(d.Strike);
    result.instrument.option_type = extract_option_type(d.OptionData);
    result.instrument.exercise_type = extract_exercise_style(d.OptionData);
    result.instrument.maturity_date = first_exercise_date(d.OptionData);
    result.instrument.averaging_start_date = std::string(d.StartDate);
    result.instrument.averaging_end_date = std::string(d.EndDate);
    result.instrument.average_type = to_string(d.PriceType);
    return result;
}

// ---------------------------------------------------------------------------
// Forward: CommodityOptionStrip
// ---------------------------------------------------------------------------

commodity_mapping_result
commodity_instrument_mapper::forward_commodity_option_strip(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping CommodityOptionStrip: "
                               << std::string(t.id);
    commodity_mapping_result result;
    result.instrument = make_base("CommodityOptionStrip");
    if (!t.CommodityOptionStripData) return result;
    const auto& d = *t.CommodityOptionStripData;

    // Extract commodity code from the leg's floating data
    if (d.LegData.legDataType &&
            d.LegData.legDataType->CommodityFloatingLegData) {
        const auto& fl = *d.LegData.legDataType->CommodityFloatingLegData;
        result.instrument.commodity_code = std::string(fl.Name);
    }
    if (d.LegData.Currency)
        result.instrument.currency = std::string(*d.LegData.Currency);
    if (d.LegData.ScheduleData && !d.LegData.ScheduleData->Rules.empty())
        result.instrument.strip_frequency_code =
            std::string(d.LegData.ScheduleData->Rules.front().Tenor);
    return result;
}

// ---------------------------------------------------------------------------
// Reverse: CommodityForward
// ---------------------------------------------------------------------------

trade commodity_instrument_mapper::reverse_commodity_forward(
        const commodity_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping CommodityForward";
    trade t;
    t.TradeType = oreTradeType::CommodityForward;
    commodityForwardData d;
    d.Position = longShort::Long;
    static_cast<std::string&>(d.Maturity) = instr.maturity_date;
    static_cast<std::string&>(d.Name) = instr.commodity_code;
    d.Currency = parse_currency_code(instr.currency);
    d.Strike = static_cast<float>(instr.fixed_price.value_or(0.0));
    d.Quantity = static_cast<float>(instr.quantity);
    t.CommodityForwardData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: CommodityOption
// ---------------------------------------------------------------------------

trade commodity_instrument_mapper::reverse_commodity_option(
        const commodity_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping CommodityOption";
    trade t;
    t.TradeType = oreTradeType::CommodityOption;
    commodityOptionData d;
    d.OptionData = make_option_data(instr);
    static_cast<std::string&>(d.Name) = instr.commodity_code;
    d.Currency = parse_currency_code(instr.currency);
    d.Strike = static_cast<float>(instr.strike_price.value_or(0.0));
    d.Quantity = static_cast<float>(instr.quantity);
    t.CommodityOptionData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: CommoditySwap
// ---------------------------------------------------------------------------

trade commodity_instrument_mapper::reverse_commodity_swap(
        const commodity_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping CommoditySwap";
    trade t;
    t.TradeType = oreTradeType::CommoditySwap;
    swapData d;

    // Fixed leg
    legData fixedLeg;
    fixedLeg.LegType = legType::CommodityFixed;
    fixedLeg.Payer = false;
    if (!instr.currency.empty())
        fixedLeg.Currency = instr.currency;
    _CommodityFixedLegData_t fl;
    pricesType_Price_t price;
    static_cast<float&>(price) = 0.0f;
    fl.Prices.Price.push_back(price);
    legDataType_group_t flt;
    flt.CommodityFixedLegData = std::move(fl);
    fixedLeg.legDataType = std::move(flt);
    d.LegData.push_back(std::move(fixedLeg));

    // Floating leg
    legData floatLeg;
    floatLeg.LegType = legType::CommodityFloating;
    floatLeg.Payer = true;
    if (!instr.currency.empty())
        floatLeg.Currency = instr.currency;
    _CommodityFloatingLegData_t cfl;
    static_cast<std::string&>(cfl.Name) = instr.commodity_code;
    cfl.PriceType = priceType::FutureSettlement;
    legDataType_group_t flt2;
    flt2.CommodityFloatingLegData = std::move(cfl);
    floatLeg.legDataType = std::move(flt2);
    if (!instr.start_date.empty()) {
        scheduleData sd;
        scheduleData_Rules_t rule;
        static_cast<std::string&>(rule.StartDate) = instr.start_date;
        if (!instr.maturity_date.empty()) {
            date ed;
            static_cast<std::string&>(ed) = instr.maturity_date;
            rule.EndDate = std::move(ed);
        }
        static_cast<std::string&>(rule.Tenor) = "1M";
        sd.Rules.push_back(std::move(rule));
        floatLeg.ScheduleData = std::move(sd);
    }
    d.LegData.push_back(std::move(floatLeg));

    t.SwapData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: CommoditySwaption
// ---------------------------------------------------------------------------

trade commodity_instrument_mapper::reverse_commodity_swaption(
        const commodity_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping CommoditySwaption";
    trade t;
    t.TradeType = oreTradeType::CommoditySwaption;
    commoditySwaptionData d;

    optionData od;
    static_cast<std::string&>(od.LongShort) = "Long";
    if (!instr.swaption_expiry_date.empty()) {
        _ExerciseDates_t exd;
        date ed;
        static_cast<std::string&>(ed) = instr.swaption_expiry_date;
        exd.ExerciseDate.push_back(ed);
        exerciseDatesGroup_group_t eg;
        eg.ExerciseDates = std::move(exd);
        od.exerciseDatesGroup = std::move(eg);
    }
    d.OptionData = std::move(od);

    // Minimal commodity floating leg
    legData floatLeg;
    floatLeg.LegType = legType::CommodityFloating;
    floatLeg.Payer = false;
    if (!instr.currency.empty())
        floatLeg.Currency = instr.currency;
    _CommodityFloatingLegData_t cfl;
    static_cast<std::string&>(cfl.Name) = instr.commodity_code;
    cfl.PriceType = priceType::FutureSettlement;
    legDataType_group_t ldt;
    ldt.CommodityFloatingLegData = std::move(cfl);
    floatLeg.legDataType = std::move(ldt);
    d.LegData.push_back(std::move(floatLeg));

    t.CommoditySwaptionData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: CommodityVarianceSwap
// ---------------------------------------------------------------------------

trade commodity_instrument_mapper::reverse_commodity_variance_swap(
        const commodity_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping CommodityVarianceSwap";
    trade t;
    t.TradeType = oreTradeType::CommodityVarianceSwap;
    varianceSwapData d;
    static_cast<std::string&>(d.StartDate) = instr.start_date;
    static_cast<std::string&>(d.EndDate) = instr.maturity_date;
    d.Currency = parse_currency_code(instr.currency);
    // Set underlying via Name field of underlyingTypes_group_t
    _Name_t n;
    static_cast<std::string&>(n) = instr.commodity_code;
    d.underlyingTypes.Name = std::move(n);
    static_cast<std::string&>(d.LongShort) = "Long";
    d.Strike = static_cast<float>(instr.variance_strike.value_or(0.0));
    d.Notional = static_cast<float>(instr.quantity);
    static_cast<std::string&>(d.Calendar) = "USD";
    t.CommodityVarianceSwapData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: CommodityAveragePriceOption
// ---------------------------------------------------------------------------

trade commodity_instrument_mapper::reverse_commodity_apo(
        const commodity_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping CommodityAveragePriceOption";
    trade t;
    t.TradeType = oreTradeType::CommodityAveragePriceOption;
    commodityAveragePriceOptionData d;
    d.OptionData = make_option_data(instr);
    static_cast<std::string&>(d.Name) = instr.commodity_code;
    d.Currency = parse_currency_code(instr.currency);
    d.Quantity = static_cast<float>(instr.quantity);
    d.Strike = static_cast<float>(instr.strike_price.value_or(0.0));
    d.PriceType = priceType::FutureSettlement;
    static_cast<std::string&>(d.StartDate) = instr.averaging_start_date;
    static_cast<std::string&>(d.EndDate) = instr.averaging_end_date;
    static_cast<std::string&>(d.PaymentCalendar) = "US-NYSE";
    static_cast<std::string&>(d.PaymentLag) = "5";
    d.PaymentConvention = businessDayConvention::Following;
    static_cast<std::string&>(d.PricingCalendar) = "US-NYSE";
    t.CommodityAveragePriceOptionData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: CommodityOptionStrip
// ---------------------------------------------------------------------------

trade commodity_instrument_mapper::reverse_commodity_option_strip(
        const commodity_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping CommodityOptionStrip";
    trade t;
    t.TradeType = oreTradeType::CommodityOptionStrip;
    commodityOptionStripData d;

    legData leg;
    leg.LegType = legType::CommodityFloating;
    leg.Payer = false;
    if (!instr.currency.empty())
        leg.Currency = instr.currency;
    _CommodityFloatingLegData_t cfl;
    static_cast<std::string&>(cfl.Name) = instr.commodity_code;
    cfl.PriceType = priceType::FutureSettlement;
    legDataType_group_t ldt;
    ldt.CommodityFloatingLegData = std::move(cfl);
    leg.legDataType = std::move(ldt);
    if (!instr.strip_frequency_code.empty()) {
        scheduleData sd;
        scheduleData_Rules_t rule;
        static_cast<std::string&>(rule.Tenor) = instr.strip_frequency_code;
        sd.Rules.push_back(std::move(rule));
        leg.ScheduleData = std::move(sd);
    }
    d.LegData = std::move(leg);

    t.CommodityOptionStripData = std::move(d);
    return t;
}

}
