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
#include "ores.ore/domain/fx_instrument_mapper.hpp"

#include <map>
#include <stdexcept>

namespace ores::ore::domain {

using namespace ores::logging;
using ores::trading::domain::fx_instrument;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

namespace {

fx_instrument make_base(const std::string& trade_type_code) {
    fx_instrument r;
    r.trade_type_code = trade_type_code;
    r.modified_by = "ores";
    r.performed_by = "ores";
    r.change_reason_code = "system.external_data_import";
    r.change_commentary = "Imported from ORE XML";
    return r;
}

currencyCode parse_currency_code(const std::string& s) {
    // Reuse the same exhaustive map as swap_instrument_mapper.
    // Defined here to avoid a shared-header dependency.
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
// Forward: FxForward
// ---------------------------------------------------------------------------

fx_mapping_result fx_instrument_mapper::forward_fx_forward(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxForward: "
                               << std::string(t.id);
    fx_mapping_result result;
    result.instrument = make_base("FxForward");

    if (!t.FxForwardData) return result;
    const auto& fwd = *t.FxForwardData;

    result.instrument.value_date    = std::string(fwd.ValueDate);
    result.instrument.bought_currency = to_string(fwd.BoughtCurrency);
    result.instrument.bought_amount   = static_cast<double>(fwd.BoughtAmount);
    result.instrument.sold_currency   = to_string(fwd.SoldCurrency);
    result.instrument.sold_amount     = static_cast<double>(fwd.SoldAmount);
    if (fwd.Settlement)
        result.instrument.settlement = to_string(*fwd.Settlement);

    return result;
}

// ---------------------------------------------------------------------------
// Forward: FxSwap
// ---------------------------------------------------------------------------

fx_mapping_result fx_instrument_mapper::forward_fx_swap(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxSwap: "
                               << std::string(t.id);
    fx_mapping_result result;
    result.instrument = make_base("FxSwap");

    if (!t.FxSwapData) return result;
    const auto& sw = *t.FxSwapData;

    // Map near leg; far leg amounts are a known coverage gap.
    result.instrument.value_date      = std::string(sw.NearDate);
    result.instrument.bought_currency = to_string(sw.NearBoughtCurrency);
    result.instrument.bought_amount   = static_cast<double>(sw.NearBoughtAmount);
    result.instrument.sold_currency   = to_string(sw.NearSoldCurrency);
    result.instrument.sold_amount     = static_cast<double>(sw.NearSoldAmount);
    if (sw.Settlement)
        result.instrument.settlement  = to_string(*sw.Settlement);

    return result;
}

// ---------------------------------------------------------------------------
// Forward: FxOption
// ---------------------------------------------------------------------------

fx_mapping_result fx_instrument_mapper::forward_fx_option(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxOption: "
                               << std::string(t.id);
    fx_mapping_result result;
    result.instrument = make_base("FxOption");

    if (!t.FxOptionData) return result;
    const auto& opt = *t.FxOptionData;

    result.instrument.bought_currency = to_string(opt.BoughtCurrency);
    result.instrument.bought_amount   = static_cast<double>(opt.BoughtAmount);
    result.instrument.sold_currency   = to_string(opt.SoldCurrency);
    if (opt.SoldAmount)
        result.instrument.sold_amount = static_cast<double>(*opt.SoldAmount);

    const auto& od = opt.OptionData;
    if (od.OptionType)
        result.instrument.option_type = std::string(*od.OptionType);
    if (od.Settlement)
        result.instrument.settlement  = to_string(*od.Settlement);

    // Expiry date from first ExerciseDate
    if (od.exerciseDatesGroup && od.exerciseDatesGroup->ExerciseDates &&
            !od.exerciseDatesGroup->ExerciseDates->ExerciseDate.empty())
        result.instrument.expiry_date =
            std::string(od.exerciseDatesGroup->ExerciseDates->ExerciseDate.front());

    return result;
}

// ---------------------------------------------------------------------------
// Reverse: FxForward
// ---------------------------------------------------------------------------

trade fx_instrument_mapper::reverse_fx_forward(const fx_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FxForward";

    trade t;
    t.TradeType = oreTradeType::FxForward;

    fxForwardData fwd;
    static_cast<std::string&>(fwd.ValueDate)  = instr.value_date;
    fwd.BoughtCurrency = parse_currency_code(instr.bought_currency);
    static_cast<float&>(fwd.BoughtAmount)     = static_cast<float>(instr.bought_amount);
    fwd.SoldCurrency   = parse_currency_code(instr.sold_currency);
    static_cast<float&>(fwd.SoldAmount)       = static_cast<float>(instr.sold_amount);

    t.FxForwardData = std::move(fwd);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: FxSwap
// ---------------------------------------------------------------------------

trade fx_instrument_mapper::reverse_fx_swap(const fx_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FxSwap";

    trade t;
    t.TradeType = oreTradeType::FxSwap;

    fxSwapData sw;
    static_cast<std::string&>(sw.NearDate)     = instr.value_date;
    sw.NearBoughtCurrency = parse_currency_code(instr.bought_currency);
    static_cast<float&>(sw.NearBoughtAmount)   = static_cast<float>(instr.bought_amount);
    sw.NearSoldCurrency   = parse_currency_code(instr.sold_currency);
    static_cast<float&>(sw.NearSoldAmount)     = static_cast<float>(instr.sold_amount);
    // Far leg: not captured in forward mapping — round-trip as empty date/zero amounts.
    static_cast<std::string&>(sw.FarDate)      = "";
    static_cast<float&>(sw.FarBoughtAmount)    = 0.0f;
    static_cast<float&>(sw.FarSoldAmount)      = 0.0f;

    t.FxSwapData = std::move(sw);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: FxOption
// ---------------------------------------------------------------------------

trade fx_instrument_mapper::reverse_fx_option(const fx_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FxOption";

    trade t;
    t.TradeType = oreTradeType::FxOption;

    fxOptionData opt;
    opt.BoughtCurrency = parse_currency_code(instr.bought_currency);
    static_cast<float&>(opt.BoughtAmount) = static_cast<float>(instr.bought_amount);
    opt.SoldCurrency   = parse_currency_code(instr.sold_currency);
    if (instr.sold_amount != 0.0)
        opt.SoldAmount = static_cast<float>(instr.sold_amount);

    if (!instr.option_type.empty()) {
        optionData_OptionType_t ot;
        static_cast<std::string&>(ot) = instr.option_type;
        opt.OptionData.OptionType = std::move(ot);
    }
    if (!instr.expiry_date.empty()) {
        _ExerciseDates_t ed;
        domain::date d;
        static_cast<std::string&>(d) = instr.expiry_date;
        ed.ExerciseDate.push_back(d);
        exerciseDatesGroup_group_t edg;
        edg.ExerciseDates = std::move(ed);
        opt.OptionData.exerciseDatesGroup = std::move(edg);
    }

    t.FxOptionData = std::move(opt);
    return t;
}

// ---------------------------------------------------------------------------
// Phase 6 helpers
// ---------------------------------------------------------------------------

namespace {

std::string option_type_from_vec(const xsd::vector<optionData>& v) {
    if (v.empty()) return {};
    if (v.front().OptionType) return std::string(*v.front().OptionType);
    return {};
}

std::string expiry_date_from_vec(const xsd::vector<optionData>& v) {
    if (v.empty()) return {};
    const auto& od = v.front();
    if (!od.exerciseDatesGroup) return {};
    if (!od.exerciseDatesGroup->ExerciseDates) return {};
    if (od.exerciseDatesGroup->ExerciseDates->ExerciseDate.empty()) return {};
    return std::string(od.exerciseDatesGroup->ExerciseDates->ExerciseDate.front());
}

std::string expiry_date_from_single(const optionData& od) {
    if (!od.exerciseDatesGroup) return {};
    if (!od.exerciseDatesGroup->ExerciseDates) return {};
    if (od.exerciseDatesGroup->ExerciseDates->ExerciseDate.empty()) return {};
    return std::string(od.exerciseDatesGroup->ExerciseDates->ExerciseDate.front());
}

optionData make_fx_option_entry(const fx_instrument& instr) {
    optionData od;
    static_cast<std::string&>(od.LongShort) = "Long";
    if (!instr.option_type.empty()) {
        optionData_OptionType_t ot;
        static_cast<std::string&>(ot) = instr.option_type;
        od.OptionType = std::move(ot);
    }
    if (!instr.expiry_date.empty()) {
        _ExerciseDates_t ed;
        date dt;
        static_cast<std::string&>(dt) = instr.expiry_date;
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
                                               double level) {
    static const std::map<std::string, barrierType> bmap = {
        {"UpAndOut",                barrierType::UpAndOut},
        {"UpAndIn",                 barrierType::UpAndIn},
        {"DownAndOut",              barrierType::DownAndOut},
        {"DownAndIn",               barrierType::DownAndIn},
        {"KnockIn",                 barrierType::KnockIn},
        {"KnockOut",                barrierType::KnockOut},
        {"CumulatedProfitCap",      barrierType::CumulatedProfitCap},
        {"CumulatedProfitCapPoints",barrierType::CumulatedProfitCapPoints},
        {"FixingCap",               barrierType::FixingCap},
        {"FixingFloor",             barrierType::FixingFloor},
    };
    barrierData b;
    const auto it = bmap.find(type);
    b.Type = (it != bmap.end()) ? it->second : barrierType::DownAndOut;
    b.Levels.Level.push_back(static_cast<float>(level));
    return b;
}

// ---------------------------------------------------------------------------
// Forward: FxBarrierOption
// ---------------------------------------------------------------------------

fx_mapping_result fx_instrument_mapper::forward_fx_barrier_option(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxBarrierOption: "
                               << std::string(t.id);
    fx_mapping_result result;
    result.instrument = make_base("FxBarrierOption");
    if (!t.FxBarrierOptionData) return result;
    const auto& d = *t.FxBarrierOptionData;

    result.instrument.bought_currency = to_string(d.BoughtCurrency);
    result.instrument.bought_amount   = static_cast<double>(d.BoughtAmount);
    result.instrument.sold_currency   = to_string(d.SoldCurrency);
    result.instrument.sold_amount     = static_cast<double>(d.SoldAmount);
    result.instrument.option_type     = option_type_from_vec(d.OptionData);
    result.instrument.expiry_date     = expiry_date_from_vec(d.OptionData);

    if (!d.BarrierData.empty()) {
        result.instrument.barrier_type = to_string(d.BarrierData.front().Type);
        if (!d.BarrierData.front().Levels.Level.empty())
            result.instrument.lower_barrier =
                static_cast<double>(d.BarrierData.front().Levels.Level.front());
        if (d.BarrierData.size() > 1 &&
                !d.BarrierData[1].Levels.Level.empty())
            result.instrument.upper_barrier =
                static_cast<double>(d.BarrierData[1].Levels.Level.front());
    }
    return result;
}

// ---------------------------------------------------------------------------
// Forward: FxDigitalOption
// ---------------------------------------------------------------------------

fx_mapping_result fx_instrument_mapper::forward_fx_digital_option(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxDigitalOption: "
                               << std::string(t.id);
    fx_mapping_result result;
    result.instrument = make_base("FxDigitalOption");
    if (!t.FxDigitalOptionData) return result;
    const auto& d = *t.FxDigitalOptionData;

    result.instrument.bought_currency = to_string(d.ForeignCurrency);
    result.instrument.sold_currency   = to_string(d.DomesticCurrency);
    result.instrument.strike_price    = static_cast<double>(d.Strike);
    result.instrument.notional        = static_cast<double>(d.PayoffAmount);
    result.instrument.option_type     = option_type_from_vec(d.OptionData);
    result.instrument.expiry_date     = expiry_date_from_vec(d.OptionData);
    return result;
}

// ---------------------------------------------------------------------------
// Forward: FxDigitalBarrierOption
// ---------------------------------------------------------------------------

fx_mapping_result fx_instrument_mapper::forward_fx_digital_barrier_option(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxDigitalBarrierOption: "
                               << std::string(t.id);
    fx_mapping_result result;
    result.instrument = make_base("FxDigitalBarrierOption");
    if (!t.FxDigitalBarrierOptionData) return result;
    const auto& d = *t.FxDigitalBarrierOptionData;

    result.instrument.bought_currency = to_string(d.ForeignCurrency);
    result.instrument.sold_currency   = to_string(d.DomesticCurrency);
    result.instrument.strike_price    = static_cast<double>(d.Strike);
    result.instrument.notional        = static_cast<double>(d.PayoffAmount);
    result.instrument.option_type     = option_type_from_vec(d.OptionData);
    result.instrument.expiry_date     = expiry_date_from_vec(d.OptionData);

    if (!d.BarrierData.empty()) {
        result.instrument.barrier_type = to_string(d.BarrierData.front().Type);
        if (!d.BarrierData.front().Levels.Level.empty())
            result.instrument.lower_barrier =
                static_cast<double>(d.BarrierData.front().Levels.Level.front());
    }
    return result;
}

// ---------------------------------------------------------------------------
// Forward: FxTouchOption / FxDoubleTouchOption
// ---------------------------------------------------------------------------

fx_mapping_result fx_instrument_mapper::forward_fx_touch_option(
        const trade& t) {
    const std::string type_str = to_string(t.TradeType);
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping " << type_str << ": "
                               << std::string(t.id);
    fx_mapping_result result;
    result.instrument = make_base(type_str);

    const fxTouchOptionData* dp = nullptr;
    if (t.FxTouchOptionData) dp = &(*t.FxTouchOptionData);
    else if (t.FxDoubleTouchOptionData) dp = &(*t.FxDoubleTouchOptionData);
    if (!dp) return result;
    const auto& d = *dp;

    result.instrument.bought_currency = to_string(d.ForeignCurrency);
    result.instrument.sold_currency   = to_string(d.DomesticCurrency);
    result.instrument.notional        = static_cast<double>(d.PayoffAmount);
    result.instrument.option_type     = option_type_from_vec(d.OptionData);
    result.instrument.expiry_date     = expiry_date_from_vec(d.OptionData);

    if (!d.BarrierData.empty()) {
        result.instrument.barrier_type = to_string(d.BarrierData.front().Type);
        if (!d.BarrierData.front().Levels.Level.empty())
            result.instrument.lower_barrier =
                static_cast<double>(d.BarrierData.front().Levels.Level.front());
        if (d.BarrierData.size() > 1 &&
                !d.BarrierData[1].Levels.Level.empty())
            result.instrument.upper_barrier =
                static_cast<double>(d.BarrierData[1].Levels.Level.front());
    }
    return result;
}

// ---------------------------------------------------------------------------
// Forward: FxVarianceSwap
// ---------------------------------------------------------------------------

fx_mapping_result fx_instrument_mapper::forward_fx_variance_swap(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxVarianceSwap: "
                               << std::string(t.id);
    fx_mapping_result result;
    result.instrument = make_base("FxVarianceSwap");
    if (!t.FxVarianceSwapData) return result;
    const auto& d = *t.FxVarianceSwapData;

    if (d.underlyingTypes.Name)
        result.instrument.underlying_code = std::string(*d.underlyingTypes.Name);
    else if (d.underlyingTypes.Underlying)
        result.instrument.underlying_code =
            std::string(d.underlyingTypes.Underlying->Name);

    result.instrument.start_date      = std::string(d.StartDate);
    result.instrument.expiry_date     = std::string(d.EndDate);
    result.instrument.variance_strike = static_cast<double>(d.Strike);
    result.instrument.notional        = static_cast<double>(d.Notional);
    result.instrument.bought_currency = to_string(d.Currency);
    return result;
}

// ---------------------------------------------------------------------------
// Forward: FxAverageForward
// ---------------------------------------------------------------------------

fx_mapping_result fx_instrument_mapper::forward_fx_average_forward(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxAverageForward: "
                               << std::string(t.id);
    fx_mapping_result result;
    result.instrument = make_base("FxAverageForward");
    if (!t.FxAverageForwardData) return result;
    const auto& d = *t.FxAverageForwardData;

    result.instrument.value_date      = std::string(d.PaymentDate);
    result.instrument.bought_currency = to_string(d.ReferenceCurrency);
    result.instrument.bought_amount   = static_cast<double>(d.ReferenceNotional);
    result.instrument.sold_currency   = to_string(d.SettlementCurrency);
    result.instrument.sold_amount     = static_cast<double>(d.SettlementNotional);
    result.instrument.underlying_code = std::string(d.FXIndex);
    return result;
}

// ---------------------------------------------------------------------------
// Forward: FxAccumulator
// ---------------------------------------------------------------------------

fx_mapping_result fx_instrument_mapper::forward_fx_accumulator(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxAccumulator: "
                               << std::string(t.id);
    fx_mapping_result result;
    result.instrument = make_base("FxAccumulator");
    if (!t.FxAccumulatorData) return result;
    const auto& d = *t.FxAccumulatorData;

    result.instrument.bought_currency    = to_string(d.Currency);
    result.instrument.underlying_code    = std::string(d.Underlying.Name);
    result.instrument.accumulation_amount = static_cast<double>(d.FixingAmount);
    if (d.Strike)
        result.instrument.strike_price = static_cast<double>(*d.Strike);
    if (d.StartDate)
        result.instrument.start_date = std::string(*d.StartDate);
    result.instrument.expiry_date = expiry_date_from_single(d.OptionData);

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
// Forward: FxTaRF
// ---------------------------------------------------------------------------

fx_mapping_result fx_instrument_mapper::forward_fx_tarf(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxTaRF: "
                               << std::string(t.id);
    fx_mapping_result result;
    result.instrument = make_base("FxTaRF");
    if (!t.FxTaRFData) return result;
    const auto& d = *t.FxTaRFData;

    result.instrument.bought_currency    = to_string(d.Currency);
    result.instrument.underlying_code    = std::string(d.Underlying.Name);
    result.instrument.accumulation_amount = static_cast<double>(d.FixingAmount);
    if (d.Strike)
        result.instrument.strike_price = static_cast<double>(*d.Strike);
    result.instrument.expiry_date = expiry_date_from_single(d.OptionData);

    for (const auto& bd : d.Barriers.BarrierData) {
        const auto btype = to_string(bd.Type);
        if ((btype == "KnockOut" || btype == "FixingCap") &&
                !bd.Levels.Level.empty()) {
            result.instrument.knock_out_barrier =
                static_cast<double>(bd.Levels.Level.front());
            break;
        }
    }
    return result;
}

// ---------------------------------------------------------------------------
// Forward: FxGenericBarrierOption
// ---------------------------------------------------------------------------

fx_mapping_result fx_instrument_mapper::forward_fx_generic_barrier_option(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxGenericBarrierOption: "
                               << std::string(t.id);
    fx_mapping_result result;
    result.instrument = make_base("FxGenericBarrierOption");
    if (!t.FxGenericBarrierOptionData) return result;
    const auto& d = *t.FxGenericBarrierOptionData;

    if (d.underlyingTypes.Name)
        result.instrument.underlying_code = std::string(*d.underlyingTypes.Name);
    else if (d.underlyingTypes.Underlying)
        result.instrument.underlying_code =
            std::string(d.underlyingTypes.Underlying->Name);

    result.instrument.bought_currency = to_string(d.PayCurrency);
    if (d.OptionData.OptionType)
        result.instrument.option_type = std::string(*d.OptionData.OptionType);
    result.instrument.expiry_date = expiry_date_from_single(d.OptionData);

    if (d.Strike)
        result.instrument.strike_price = static_cast<double>(*d.Strike);
    if (d.Amount)
        result.instrument.notional = static_cast<double>(*d.Amount);

    if (!d.Barriers.BarrierData.empty()) {
        result.instrument.barrier_type = to_string(d.Barriers.BarrierData.front().Type);
        if (!d.Barriers.BarrierData.front().Levels.Level.empty())
            result.instrument.lower_barrier =
                static_cast<double>(d.Barriers.BarrierData.front().Levels.Level.front());
    }
    return result;
}

// ---------------------------------------------------------------------------
// Reverse: FxBarrierOption
// ---------------------------------------------------------------------------

trade fx_instrument_mapper::reverse_fx_barrier_option(
        const fx_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FxBarrierOption";
    trade t;
    t.TradeType = oreTradeType::FxBarrierOption;

    fxBarrierOptionData d;
    d.BoughtCurrency = parse_currency_code(instr.bought_currency);
    d.BoughtAmount   = static_cast<float>(instr.bought_amount);
    d.SoldCurrency   = parse_currency_code(instr.sold_currency);
    d.SoldAmount     = static_cast<float>(instr.sold_amount);
    d.OptionData.push_back(make_fx_option_entry(instr));

    if (!instr.barrier_type.empty() && instr.lower_barrier != 0.0)
        d.BarrierData.push_back(make_barrier(instr.barrier_type, instr.lower_barrier));
    if (instr.upper_barrier != 0.0)
        d.BarrierData.push_back(make_barrier(instr.barrier_type, instr.upper_barrier));

    t.FxBarrierOptionData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: FxDigitalOption
// ---------------------------------------------------------------------------

trade fx_instrument_mapper::reverse_fx_digital_option(
        const fx_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FxDigitalOption";
    trade t;
    t.TradeType = oreTradeType::FxDigitalOption;

    fxDigitalOptionData d;
    d.ForeignCurrency = parse_currency_code(instr.bought_currency);
    d.DomesticCurrency = parse_currency_code(instr.sold_currency);
    d.Strike      = static_cast<float>(instr.strike_price);
    d.PayoffAmount = static_cast<float>(instr.notional);
    d.OptionData.push_back(make_fx_option_entry(instr));

    t.FxDigitalOptionData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: FxDigitalBarrierOption
// ---------------------------------------------------------------------------

trade fx_instrument_mapper::reverse_fx_digital_barrier_option(
        const fx_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FxDigitalBarrierOption";
    trade t;
    t.TradeType = oreTradeType::FxDigitalBarrierOption;

    fxDigitalBarrierOptionData d;
    d.ForeignCurrency  = parse_currency_code(instr.bought_currency);
    d.DomesticCurrency = parse_currency_code(instr.sold_currency);
    d.Strike      = static_cast<float>(instr.strike_price);
    d.PayoffAmount = static_cast<float>(instr.notional);
    d.OptionData.push_back(make_fx_option_entry(instr));

    if (!instr.barrier_type.empty() && instr.lower_barrier != 0.0)
        d.BarrierData.push_back(make_barrier(instr.barrier_type, instr.lower_barrier));

    t.FxDigitalBarrierOptionData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: FxTouchOption / FxDoubleTouchOption
// ---------------------------------------------------------------------------

trade fx_instrument_mapper::reverse_fx_touch_option(
        const fx_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping " << instr.trade_type_code;
    trade t;
    t.TradeType = (instr.trade_type_code == "FxDoubleTouchOption")
                      ? oreTradeType::FxDoubleTouchOption
                      : oreTradeType::FxTouchOption;

    fxTouchOptionData d;
    if (!instr.bought_currency.empty())
        d.ForeignCurrency = parse_currency_code(instr.bought_currency);
    if (!instr.sold_currency.empty())
        d.DomesticCurrency = parse_currency_code(instr.sold_currency);
    if (!instr.bought_currency.empty())
        d.PayoffCurrency = parse_currency_code(instr.bought_currency);
    d.PayoffAmount = static_cast<float>(instr.notional);
    d.OptionData.push_back(make_fx_option_entry(instr));

    if (!instr.barrier_type.empty() && instr.lower_barrier != 0.0)
        d.BarrierData.push_back(make_barrier(instr.barrier_type, instr.lower_barrier));
    if (instr.upper_barrier != 0.0)
        d.BarrierData.push_back(make_barrier(instr.barrier_type, instr.upper_barrier));

    if (instr.trade_type_code == "FxDoubleTouchOption")
        t.FxDoubleTouchOptionData = std::move(d);
    else
        t.FxTouchOptionData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: FxVarianceSwap
// ---------------------------------------------------------------------------

trade fx_instrument_mapper::reverse_fx_variance_swap(
        const fx_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FxVarianceSwap";
    trade t;
    t.TradeType = oreTradeType::FxVarianceSwap;

    varianceSwapData d;
    static_cast<std::string&>(d.StartDate) = instr.start_date;
    static_cast<std::string&>(d.EndDate)   = instr.expiry_date;
    if (!instr.bought_currency.empty())
        d.Currency = parse_currency_code(instr.bought_currency);
    d.underlyingTypes = make_underlying_type_name(instr.underlying_code);
    static_cast<std::string&>(d.LongShort) = "Long";
    d.Strike   = static_cast<float>(instr.variance_strike);
    d.Notional = static_cast<float>(instr.notional);
    static_cast<std::string&>(d.Calendar) = "TARGET";

    t.FxVarianceSwapData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: FxAverageForward
// ---------------------------------------------------------------------------

trade fx_instrument_mapper::reverse_fx_average_forward(
        const fx_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FxAverageForward";
    trade t;
    t.TradeType = oreTradeType::FxAverageForward;

    fxAverageForwardData d;
    static_cast<std::string&>(d.PaymentDate) = instr.value_date;
    d.ReferenceCurrency  = parse_currency_code(instr.bought_currency);
    d.ReferenceNotional  = static_cast<float>(instr.bought_amount);
    d.SettlementCurrency = parse_currency_code(instr.sold_currency);
    d.SettlementNotional = static_cast<float>(instr.sold_amount);
    static_cast<std::string&>(d.FXIndex) = instr.underlying_code;
    d.FixedPayer = bool_::false_;
    // Minimal observation schedule
    scheduleData_Rules_t rule;
    static_cast<std::string&>(rule.Tenor) = "1M";
    d.ObservationDates.Rules.push_back(std::move(rule));

    t.FxAverageForwardData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: FxAccumulator
// ---------------------------------------------------------------------------

trade fx_instrument_mapper::reverse_fx_accumulator(
        const fx_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FxAccumulator";
    trade t;
    t.TradeType = oreTradeType::FxAccumulator;

    accumulatorData d;
    d.Currency = parse_currency_code(instr.bought_currency);
    d.FixingAmount = static_cast<float>(instr.accumulation_amount);
    if (instr.strike_price != 0.0)
        d.Strike = static_cast<float>(instr.strike_price);
    static_cast<std::string&>(d.Underlying.Name) = instr.underlying_code;
    static_cast<std::string&>(d.Underlying.Type) = "FX";
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

    t.FxAccumulatorData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: FxTaRF
// ---------------------------------------------------------------------------

trade fx_instrument_mapper::reverse_fx_tarf(const fx_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FxTaRF";
    trade t;
    t.TradeType = oreTradeType::FxTaRF;

    tarfData2 d;
    d.Currency = parse_currency_code(instr.bought_currency);
    d.FixingAmount = static_cast<float>(instr.accumulation_amount);
    if (instr.strike_price != 0.0)
        d.Strike = static_cast<float>(instr.strike_price);
    static_cast<std::string&>(d.Underlying.Name) = instr.underlying_code;
    static_cast<std::string&>(d.Underlying.Type) = "FX";
    static_cast<std::string&>(d.OptionData.LongShort) = "Long";
    // Minimal schedule
    scheduleData_Rules_t rule;
    static_cast<std::string&>(rule.Tenor) = "1Y";
    d.ScheduleData.Rules.push_back(std::move(rule));

    t.FxTaRFData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: FxGenericBarrierOption
// ---------------------------------------------------------------------------

trade fx_instrument_mapper::reverse_fx_generic_barrier_option(
        const fx_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FxGenericBarrierOption";
    trade t;
    t.TradeType = oreTradeType::FxGenericBarrierOption;

    genericBarrierOptionData d;
    if (!instr.bought_currency.empty())
        d.PayCurrency = parse_currency_code(instr.bought_currency);
    d.underlyingTypes = make_underlying_type_name(instr.underlying_code);

    // Single OptionData
    static_cast<std::string&>(d.OptionData.LongShort) = "Long";
    if (!instr.option_type.empty()) {
        optionData_OptionType_t ot;
        static_cast<std::string&>(ot) = instr.option_type;
        d.OptionData.OptionType = std::move(ot);
    }
    if (!instr.expiry_date.empty()) {
        _ExerciseDates_t ed;
        date dt;
        static_cast<std::string&>(dt) = instr.expiry_date;
        ed.ExerciseDate.push_back(dt);
        exerciseDatesGroup_group_t edg;
        edg.ExerciseDates = std::move(ed);
        d.OptionData.exerciseDatesGroup = std::move(edg);
    }
    if (instr.strike_price != 0.0)
        d.Strike = static_cast<float>(instr.strike_price);
    if (instr.notional != 0.0)
        d.Amount = static_cast<float>(instr.notional);
    if (!instr.barrier_type.empty() && instr.lower_barrier != 0.0)
        d.Barriers.BarrierData.push_back(
            make_barrier(instr.barrier_type, instr.lower_barrier));

    t.FxGenericBarrierOptionData = std::move(d);
    return t;
}

}
