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

}
