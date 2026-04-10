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

constexpr const char* k_modified_by     = "ores";
constexpr const char* k_performed_by    = "ores";
constexpr const char* k_change_reason   = "system.external_data_import";
constexpr const char* k_change_comment  = "Imported from ORE XML";

void set_audit(auto& r) {
    r.modified_by       = k_modified_by;
    r.performed_by      = k_performed_by;
    r.change_reason_code = k_change_reason;
    r.change_commentary  = k_change_comment;
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

std::string exercise_style_from_vec(const xsd::vector<optionData>& v) {
    if (v.empty()) return "European";
    // If ExerciseDates is present it implies European; American exercise uses
    // ExerciseSchedule. We default to European as that is far more common.
    return "European";
}

// Build an OptionData element from option_type + expiry_date strings.
optionData make_option_entry(const std::string& option_type,
                              const std::string& expiry_date,
                              const std::string& long_short = "Long") {
    optionData od;
    static_cast<std::string&>(od.LongShort) = long_short;
    if (!option_type.empty()) {
        optionData_OptionType_t ot;
        static_cast<std::string&>(ot) = option_type;
        od.OptionType = std::move(ot);
    }
    if (!expiry_date.empty()) {
        _ExerciseDates_t ed;
        date dt;
        static_cast<std::string&>(dt) = expiry_date;
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
    if (it == bmap.end())
        throw std::runtime_error(
            "make_barrier: unrecognized barrier type '" + type + "'");
    b.Type = it->second;
    b.Levels.Level.push_back(static_cast<float>(level));
    return b;
}

// ===========================================================================
// Forward: FxForward
// ===========================================================================

fx_mapping_result fx_instrument_mapper::forward_fx_forward(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxForward: "
                               << std::string(t.id);
    fx_forward_instrument r;
    r.trade_type_code = "FxForward";
    set_audit(r);
    if (!t.FxForwardData) return {r};
    const auto& fwd = *t.FxForwardData;

    r.value_date      = std::string(fwd.ValueDate);
    r.bought_currency = to_string(fwd.BoughtCurrency);
    r.bought_amount   = static_cast<double>(fwd.BoughtAmount);
    r.sold_currency   = to_string(fwd.SoldCurrency);
    r.sold_amount     = static_cast<double>(fwd.SoldAmount);
    if (fwd.Settlement)
        r.settlement  = to_string(*fwd.Settlement);

    return {r};
}

// ===========================================================================
// Forward: FxSwap
// ===========================================================================

fx_mapping_result fx_instrument_mapper::forward_fx_swap(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxSwap: "
                               << std::string(t.id);
    fx_forward_instrument r;
    r.trade_type_code = "FxSwap";
    set_audit(r);
    if (!t.FxSwapData) return {r};
    const auto& sw = *t.FxSwapData;

    // Near leg only; far leg is a documented coverage gap.
    r.value_date      = std::string(sw.NearDate);
    r.bought_currency = to_string(sw.NearBoughtCurrency);
    r.bought_amount   = static_cast<double>(sw.NearBoughtAmount);
    r.sold_currency   = to_string(sw.NearSoldCurrency);
    r.sold_amount     = static_cast<double>(sw.NearSoldAmount);
    if (sw.Settlement)
        r.settlement  = to_string(*sw.Settlement);

    return {r};
}

// ===========================================================================
// Forward: FxOption
// ===========================================================================

fx_mapping_result fx_instrument_mapper::forward_fx_option(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxOption: "
                               << std::string(t.id);
    fx_vanilla_option_instrument r;
    r.trade_type_code = "FxOption";
    set_audit(r);
    if (!t.FxOptionData) return {r};
    const auto& opt = *t.FxOptionData;

    r.bought_currency = to_string(opt.BoughtCurrency);
    r.bought_amount   = static_cast<double>(opt.BoughtAmount);
    r.sold_currency   = to_string(opt.SoldCurrency);
    if (opt.SoldAmount)
        r.sold_amount = static_cast<double>(*opt.SoldAmount);

    const auto& od = opt.OptionData;
    if (od.OptionType)
        r.option_type   = std::string(*od.OptionType);
    if (od.Settlement)
        r.settlement    = to_string(*od.Settlement);
    r.exercise_style    = "European"; // default; American not detected here
    r.expiry_date       = expiry_date_from_vec(
        xsd::vector<optionData>{od});

    return {r};
}

// ===========================================================================
// Forward: FxBarrierOption
// ===========================================================================

fx_mapping_result fx_instrument_mapper::forward_fx_barrier_option(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxBarrierOption: "
                               << std::string(t.id);
    fx_barrier_option_instrument r;
    r.trade_type_code = "FxBarrierOption";
    set_audit(r);
    if (!t.FxBarrierOptionData) return {r};
    const auto& d = *t.FxBarrierOptionData;

    r.bought_currency = to_string(d.BoughtCurrency);
    r.bought_amount   = static_cast<double>(d.BoughtAmount);
    r.sold_currency   = to_string(d.SoldCurrency);
    r.sold_amount     = static_cast<double>(d.SoldAmount);
    r.option_type     = option_type_from_vec(d.OptionData);
    r.expiry_date     = expiry_date_from_vec(d.OptionData);

    if (!d.BarrierData.empty()) {
        r.barrier_type = to_string(d.BarrierData.front().Type);
        if (!d.BarrierData.front().Levels.Level.empty())
            r.lower_barrier = static_cast<double>(
                d.BarrierData.front().Levels.Level.front());
        if (d.BarrierData.size() > 1 &&
                !d.BarrierData[1].Levels.Level.empty())
            r.upper_barrier = static_cast<double>(
                d.BarrierData[1].Levels.Level.front());
    }
    return {r};
}

// ===========================================================================
// Forward: FxDoubleBarrierOption
// ===========================================================================

fx_mapping_result fx_instrument_mapper::forward_fx_double_barrier_option(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxDoubleBarrierOption: "
                               << std::string(t.id);
    fx_barrier_option_instrument r;
    r.trade_type_code = "FxDoubleBarrierOption";
    set_audit(r);
    if (!t.FxDoubleBarrierOptionData) return {r};
    const auto& d = *t.FxDoubleBarrierOptionData;

    r.bought_currency = to_string(d.BoughtCurrency);
    r.bought_amount   = static_cast<double>(d.BoughtAmount);
    r.sold_currency   = to_string(d.SoldCurrency);
    r.sold_amount     = static_cast<double>(d.SoldAmount);
    r.option_type     = option_type_from_vec(d.OptionData);
    r.expiry_date     = expiry_date_from_vec(d.OptionData);

    if (!d.BarrierData.empty()) {
        r.barrier_type = to_string(d.BarrierData.front().Type);
        if (!d.BarrierData.front().Levels.Level.empty())
            r.lower_barrier = static_cast<double>(
                d.BarrierData.front().Levels.Level.front());
        if (d.BarrierData.size() > 1 &&
                !d.BarrierData[1].Levels.Level.empty())
            r.upper_barrier = static_cast<double>(
                d.BarrierData[1].Levels.Level.front());
    }
    return {r};
}

// ===========================================================================
// Forward: FxEuropeanBarrierOption
// ===========================================================================

fx_mapping_result fx_instrument_mapper::forward_fx_european_barrier_option(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxEuropeanBarrierOption: "
                               << std::string(t.id);
    fx_barrier_option_instrument r;
    r.trade_type_code = "FxEuropeanBarrierOption";
    set_audit(r);
    if (!t.FxEuropeanBarrierOptionData) return {r};
    const auto& d = *t.FxEuropeanBarrierOptionData;

    r.bought_currency = to_string(d.BoughtCurrency);
    r.bought_amount   = static_cast<double>(d.BoughtAmount);
    r.sold_currency   = to_string(d.SoldCurrency);
    r.sold_amount     = static_cast<double>(d.SoldAmount);
    r.option_type     = option_type_from_vec(d.OptionData);
    r.expiry_date     = expiry_date_from_vec(d.OptionData);

    if (!d.BarrierData.empty()) {
        r.barrier_type = to_string(d.BarrierData.front().Type);
        if (!d.BarrierData.front().Levels.Level.empty())
            r.lower_barrier = static_cast<double>(
                d.BarrierData.front().Levels.Level.front());
        if (d.BarrierData.size() > 1 &&
                !d.BarrierData[1].Levels.Level.empty())
            r.upper_barrier = static_cast<double>(
                d.BarrierData[1].Levels.Level.front());
    }
    return {r};
}

// ===========================================================================
// Forward: FxKIKOBarrierOption
// ===========================================================================

fx_mapping_result fx_instrument_mapper::forward_fx_kiko_barrier_option(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxKIKOBarrierOption: "
                               << std::string(t.id);
    fx_barrier_option_instrument r;
    r.trade_type_code = "FxKIKOBarrierOption";
    set_audit(r);
    if (!t.FxKIKOBarrierOptionData) return {r};
    const auto& d = *t.FxKIKOBarrierOptionData;

    r.bought_currency = to_string(d.BoughtCurrency);
    r.bought_amount   = static_cast<double>(d.BoughtAmount);
    r.sold_currency   = to_string(d.SoldCurrency);
    r.sold_amount     = static_cast<double>(d.SoldAmount);
    if (d.OptionData.OptionType)
        r.option_type = std::string(*d.OptionData.OptionType);
    r.expiry_date     = expiry_date_from_single(d.OptionData);

    if (!d.Barriers.BarrierData.empty()) {
        r.barrier_type = to_string(d.Barriers.BarrierData.front().Type);
        if (!d.Barriers.BarrierData.front().Levels.Level.empty())
            r.lower_barrier = static_cast<double>(
                d.Barriers.BarrierData.front().Levels.Level.front());
        if (d.Barriers.BarrierData.size() > 1 &&
                !d.Barriers.BarrierData[1].Levels.Level.empty())
            r.upper_barrier = static_cast<double>(
                d.Barriers.BarrierData[1].Levels.Level.front());
    }
    return {r};
}

// ===========================================================================
// Forward: FxGenericBarrierOption
// ===========================================================================

fx_mapping_result fx_instrument_mapper::forward_fx_generic_barrier_option(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxGenericBarrierOption: "
                               << std::string(t.id);
    fx_barrier_option_instrument r;
    r.trade_type_code = "FxGenericBarrierOption";
    set_audit(r);
    if (!t.FxGenericBarrierOptionData) return {r};
    const auto& d = *t.FxGenericBarrierOptionData;

    if (d.underlyingTypes.Name)
        r.underlying_code = std::string(*d.underlyingTypes.Name);
    else if (d.underlyingTypes.Underlying)
        r.underlying_code = std::string(d.underlyingTypes.Underlying->Name);

    r.bought_currency = to_string(d.PayCurrency);
    if (d.OptionData.OptionType)
        r.option_type   = std::string(*d.OptionData.OptionType);
    r.expiry_date       = expiry_date_from_single(d.OptionData);

    if (!d.Barriers.BarrierData.empty()) {
        r.barrier_type = to_string(d.Barriers.BarrierData.front().Type);
        if (!d.Barriers.BarrierData.front().Levels.Level.empty())
            r.lower_barrier = static_cast<double>(
                d.Barriers.BarrierData.front().Levels.Level.front());
        if (d.Barriers.BarrierData.size() > 1 &&
                !d.Barriers.BarrierData[1].Levels.Level.empty())
            r.upper_barrier = static_cast<double>(
                d.Barriers.BarrierData[1].Levels.Level.front());
    }
    return {r};
}

// ===========================================================================
// Forward: FxDigitalOption
// ===========================================================================

fx_mapping_result fx_instrument_mapper::forward_fx_digital_option(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxDigitalOption: "
                               << std::string(t.id);
    fx_digital_option_instrument r;
    r.trade_type_code = "FxDigitalOption";
    r.long_short      = "Long";
    set_audit(r);
    if (!t.FxDigitalOptionData) return {r};
    const auto& d = *t.FxDigitalOptionData;

    r.foreign_currency = to_string(d.ForeignCurrency);
    r.domestic_currency = to_string(d.DomesticCurrency);
    r.payoff_currency  = to_string(d.ForeignCurrency); // defaults to foreign
    r.strike           = static_cast<double>(d.Strike);
    r.payoff_amount    = static_cast<double>(d.PayoffAmount);
    r.option_type      = option_type_from_vec(d.OptionData);
    r.expiry_date      = expiry_date_from_vec(d.OptionData);
    return {r};
}

// ===========================================================================
// Forward: FxDigitalBarrierOption
// ===========================================================================

fx_mapping_result fx_instrument_mapper::forward_fx_digital_barrier_option(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxDigitalBarrierOption: "
                               << std::string(t.id);
    fx_digital_option_instrument r;
    r.trade_type_code = "FxDigitalBarrierOption";
    r.long_short      = "Long";
    set_audit(r);
    if (!t.FxDigitalBarrierOptionData) return {r};
    const auto& d = *t.FxDigitalBarrierOptionData;

    r.foreign_currency  = to_string(d.ForeignCurrency);
    r.domestic_currency = to_string(d.DomesticCurrency);
    r.payoff_currency   = to_string(d.ForeignCurrency);
    r.strike            = static_cast<double>(d.Strike);
    r.payoff_amount     = static_cast<double>(d.PayoffAmount);
    r.option_type       = option_type_from_vec(d.OptionData);
    r.expiry_date       = expiry_date_from_vec(d.OptionData);

    if (!d.BarrierData.empty()) {
        r.barrier_type = to_string(d.BarrierData.front().Type);
        if (!d.BarrierData.front().Levels.Level.empty())
            r.lower_barrier = static_cast<double>(
                d.BarrierData.front().Levels.Level.front());
    }
    return {r};
}

// ===========================================================================
// Forward: FxTouchOption / FxDoubleTouchOption
// ===========================================================================

fx_mapping_result fx_instrument_mapper::forward_fx_touch_option(
        const trade& t) {
    const std::string type_str = to_string(t.TradeType);
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping " << type_str << ": "
                               << std::string(t.id);
    fx_digital_option_instrument r;
    r.trade_type_code = type_str;
    r.long_short      = "Long";
    set_audit(r);

    const fxTouchOptionData* dp = nullptr;
    if (t.FxTouchOptionData)       dp = &(*t.FxTouchOptionData);
    else if (t.FxDoubleTouchOptionData) dp = &(*t.FxDoubleTouchOptionData);
    if (!dp) return {r};
    const auto& d = *dp;

    r.foreign_currency  = to_string(d.ForeignCurrency);
    r.domestic_currency = to_string(d.DomesticCurrency);
    r.payoff_currency   = to_string(d.PayoffCurrency);
    r.payoff_amount     = static_cast<double>(d.PayoffAmount);
    r.expiry_date       = expiry_date_from_vec(d.OptionData);

    if (!d.BarrierData.empty()) {
        r.barrier_type = to_string(d.BarrierData.front().Type);
        if (!d.BarrierData.front().Levels.Level.empty())
            r.lower_barrier = static_cast<double>(
                d.BarrierData.front().Levels.Level.front());
        if (d.BarrierData.size() > 1 &&
                !d.BarrierData[1].Levels.Level.empty())
            r.upper_barrier = static_cast<double>(
                d.BarrierData[1].Levels.Level.front());
    }
    return {r};
}

// ===========================================================================
// Forward: FxVarianceSwap
// ===========================================================================

fx_mapping_result fx_instrument_mapper::forward_fx_variance_swap(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxVarianceSwap: "
                               << std::string(t.id);
    fx_variance_swap_instrument r;
    r.trade_type_code = "FxVarianceSwap";
    r.long_short      = "Long";
    set_audit(r);
    if (!t.FxVarianceSwapData) return {r};
    const auto& d = *t.FxVarianceSwapData;

    if (d.underlyingTypes.Name)
        r.underlying_code = std::string(*d.underlyingTypes.Name);
    else if (d.underlyingTypes.Underlying)
        r.underlying_code = std::string(d.underlyingTypes.Underlying->Name);

    r.start_date = std::string(d.StartDate);
    r.end_date   = std::string(d.EndDate);
    r.strike     = static_cast<double>(d.Strike);
    r.notional   = static_cast<double>(d.Notional);
    r.currency   = to_string(d.Currency);
    r.moment_type = "Variance"; // default; MomentType element not always present
    return {r};
}

// ===========================================================================
// Forward: FxAverageForward
// ===========================================================================

fx_mapping_result fx_instrument_mapper::forward_fx_average_forward(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxAverageForward: "
                               << std::string(t.id);
    fx_asian_forward_instrument r;
    r.trade_type_code = "FxAverageForward";
    set_audit(r);
    if (!t.FxAverageForwardData) return {r};
    const auto& d = *t.FxAverageForwardData;

    r.payment_date        = std::string(d.PaymentDate);
    r.reference_currency  = to_string(d.ReferenceCurrency);
    r.reference_notional  = static_cast<double>(d.ReferenceNotional);
    r.settlement_currency = to_string(d.SettlementCurrency);
    r.settlement_notional = static_cast<double>(d.SettlementNotional);
    r.fx_index            = std::string(d.FXIndex);
    r.long_short          = "Long";
    return {r};
}

// ===========================================================================
// Forward: FxAccumulator
// ===========================================================================

fx_mapping_result fx_instrument_mapper::forward_fx_accumulator(
        const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxAccumulator: "
                               << std::string(t.id);
    fx_accumulator_instrument r;
    r.trade_type_code = "FxAccumulator";
    r.long_short      = "Long";
    set_audit(r);
    if (!t.FxAccumulatorData) return {r};
    const auto& d = *t.FxAccumulatorData;

    r.currency          = to_string(d.Currency);
    r.underlying_code   = std::string(d.Underlying.Name);
    r.fixing_amount     = static_cast<double>(d.FixingAmount);
    if (d.Strike)
        r.strike        = static_cast<double>(*d.Strike);
    if (d.StartDate)
        r.start_date    = std::string(*d.StartDate);

    if (d.Barriers) {
        for (const auto& bd : d.Barriers->BarrierData) {
            const auto btype = to_string(bd.Type);
            if ((btype == "DownAndOut" || btype == "UpAndOut") &&
                    !bd.Levels.Level.empty()) {
                r.knock_out_barrier =
                    static_cast<double>(bd.Levels.Level.front());
                break;
            }
        }
    }
    return {r};
}

// ===========================================================================
// Forward: FxTaRF
// ===========================================================================

fx_mapping_result fx_instrument_mapper::forward_fx_tarf(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FxTaRF: "
                               << std::string(t.id);
    fx_asian_forward_instrument r;
    r.trade_type_code = "FxTaRF";
    set_audit(r);
    if (!t.FxTaRFData) return {r};
    const auto& d = *t.FxTaRFData;

    r.currency     = to_string(d.Currency);
    r.fx_index     = std::string(d.Underlying.Name);
    r.fixing_amount = static_cast<double>(d.FixingAmount);
    if (d.Strike)
        r.strike   = static_cast<double>(*d.Strike);

    // Target amount (profit cap) if present
    for (const auto& bd : d.Barriers.BarrierData) {
        const auto btype = to_string(bd.Type);
        if ((btype == "CumulatedProfitCap" || btype == "FixingCap") &&
                !bd.Levels.Level.empty()) {
            r.target_amount = static_cast<double>(bd.Levels.Level.front());
            break;
        }
    }
    return {r};
}

// ===========================================================================
// Reverse: FxForward
// ===========================================================================

trade fx_instrument_mapper::reverse_fx_forward(
        const fx_forward_instrument& instr) {
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

// ===========================================================================
// Reverse: FxSwap
// ===========================================================================

trade fx_instrument_mapper::reverse_fx_swap(
        const fx_forward_instrument& instr) {
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

// ===========================================================================
// Reverse: FxOption
// ===========================================================================

trade fx_instrument_mapper::reverse_fx_option(
        const fx_vanilla_option_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FxOption";
    trade t;
    t.TradeType = oreTradeType::FxOption;

    fxOptionData opt;
    opt.BoughtCurrency = parse_currency_code(instr.bought_currency);
    static_cast<float&>(opt.BoughtAmount) = static_cast<float>(instr.bought_amount);
    opt.SoldCurrency   = parse_currency_code(instr.sold_currency);
    if (instr.sold_amount != 0.0)
        opt.SoldAmount = static_cast<float>(instr.sold_amount);

    opt.OptionData = make_option_entry(instr.option_type, instr.expiry_date);
    if (!instr.settlement.empty()) {
        if (instr.settlement == "Physical")
            opt.OptionData.Settlement = settlementType::Physical;
        else if (instr.settlement == "Cash")
            opt.OptionData.Settlement = settlementType::Cash;
        else
            throw std::invalid_argument(
                "Unrecognised settlement type: " + instr.settlement);
    }

    t.FxOptionData = std::move(opt);
    return t;
}

// ===========================================================================
// Reverse: FxBarrierOption
// ===========================================================================

trade fx_instrument_mapper::reverse_fx_barrier_option(
        const fx_barrier_option_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FxBarrierOption";
    trade t;
    t.TradeType = oreTradeType::FxBarrierOption;

    fxBarrierOptionData d;
    d.BoughtCurrency = parse_currency_code(instr.bought_currency);
    d.BoughtAmount   = static_cast<float>(instr.bought_amount);
    d.SoldCurrency   = parse_currency_code(instr.sold_currency);
    d.SoldAmount     = static_cast<float>(instr.sold_amount);
    d.OptionData.push_back(make_option_entry(instr.option_type, instr.expiry_date));

    if (!instr.barrier_type.empty() && instr.lower_barrier != 0.0)
        d.BarrierData.push_back(make_barrier(instr.barrier_type, instr.lower_barrier));
    if (instr.upper_barrier.has_value())
        d.BarrierData.push_back(make_barrier(instr.barrier_type, *instr.upper_barrier));

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
    d.BoughtAmount   = static_cast<float>(instr.bought_amount);
    d.SoldCurrency   = parse_currency_code(instr.sold_currency);
    d.SoldAmount     = static_cast<float>(instr.sold_amount);
    d.OptionData.push_back(make_option_entry(instr.option_type, instr.expiry_date));
    if (!instr.barrier_type.empty() && instr.lower_barrier != 0.0)
        d.BarrierData.push_back(make_barrier(instr.barrier_type, instr.lower_barrier));
    if (instr.upper_barrier.has_value())
        d.BarrierData.push_back(make_barrier(instr.barrier_type, *instr.upper_barrier));
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
    d.BoughtAmount   = static_cast<float>(instr.bought_amount);
    d.SoldCurrency   = parse_currency_code(instr.sold_currency);
    d.SoldAmount     = static_cast<float>(instr.sold_amount);
    d.OptionData.push_back(make_option_entry(instr.option_type, instr.expiry_date));
    if (!instr.barrier_type.empty() && instr.lower_barrier != 0.0)
        d.BarrierData.push_back(make_barrier(instr.barrier_type, instr.lower_barrier));
    if (instr.upper_barrier.has_value())
        d.BarrierData.push_back(make_barrier(instr.barrier_type, *instr.upper_barrier));
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
    d.BoughtAmount   = static_cast<float>(instr.bought_amount);
    d.SoldCurrency   = parse_currency_code(instr.sold_currency);
    d.SoldAmount     = static_cast<float>(instr.sold_amount);
    d.OptionData     = make_option_entry(instr.option_type, instr.expiry_date);
    if (!instr.barrier_type.empty() && instr.lower_barrier != 0.0)
        d.Barriers.BarrierData.push_back(
            make_barrier(instr.barrier_type, instr.lower_barrier));
    if (instr.upper_barrier.has_value())
        d.Barriers.BarrierData.push_back(
            make_barrier(instr.barrier_type, *instr.upper_barrier));
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

    if (!instr.barrier_type.empty() && instr.lower_barrier != 0.0)
        d.Barriers.BarrierData.push_back(
            make_barrier(instr.barrier_type, instr.lower_barrier));
    if (instr.upper_barrier.has_value())
        d.Barriers.BarrierData.push_back(
            make_barrier(instr.barrier_type, *instr.upper_barrier));

    t.FxGenericBarrierOptionData = std::move(d);
    return t;
}

// ===========================================================================
// Reverse: FxDigitalOption
// ===========================================================================

trade fx_instrument_mapper::reverse_fx_digital_option(
        const fx_digital_option_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FxDigitalOption";
    trade t;
    t.TradeType = oreTradeType::FxDigitalOption;

    fxDigitalOptionData d;
    d.ForeignCurrency  = parse_currency_code(instr.foreign_currency);
    d.DomesticCurrency = parse_currency_code(instr.domestic_currency);
    d.Strike           = static_cast<float>(instr.strike.value_or(0.0));
    d.PayoffAmount     = static_cast<float>(instr.payoff_amount);
    d.OptionData.push_back(make_option_entry(instr.option_type, instr.expiry_date,
                                              instr.long_short));

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
    d.ForeignCurrency  = parse_currency_code(instr.foreign_currency);
    d.DomesticCurrency = parse_currency_code(instr.domestic_currency);
    d.Strike           = static_cast<float>(instr.strike.value_or(0.0));
    d.PayoffAmount     = static_cast<float>(instr.payoff_amount);
    d.OptionData.push_back(make_option_entry(instr.option_type, instr.expiry_date,
                                              instr.long_short));

    if (!instr.barrier_type.empty() && instr.lower_barrier.has_value())
        d.BarrierData.push_back(make_barrier(instr.barrier_type, *instr.lower_barrier));

    t.FxDigitalBarrierOptionData = std::move(d);
    return t;
}

// ===========================================================================
// Reverse: FxTouchOption / FxDoubleTouchOption
// ===========================================================================

trade fx_instrument_mapper::reverse_fx_touch_option(
        const fx_digital_option_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping " << instr.trade_type_code;
    trade t;
    t.TradeType = (instr.trade_type_code == "FxDoubleTouchOption")
                      ? oreTradeType::FxDoubleTouchOption
                      : oreTradeType::FxTouchOption;

    fxTouchOptionData d;
    if (!instr.foreign_currency.empty())
        d.ForeignCurrency = parse_currency_code(instr.foreign_currency);
    if (!instr.domestic_currency.empty())
        d.DomesticCurrency = parse_currency_code(instr.domestic_currency);
    if (!instr.payoff_currency.empty())
        d.PayoffCurrency = parse_currency_code(instr.payoff_currency);
    d.PayoffAmount = static_cast<float>(instr.payoff_amount);
    d.OptionData.push_back(make_option_entry("", instr.expiry_date, instr.long_short));

    if (!instr.barrier_type.empty() && instr.lower_barrier.has_value())
        d.BarrierData.push_back(make_barrier(instr.barrier_type, *instr.lower_barrier));
    if (instr.upper_barrier.has_value())
        d.BarrierData.push_back(make_barrier(instr.barrier_type, *instr.upper_barrier));

    if (instr.trade_type_code == "FxDoubleTouchOption")
        t.FxDoubleTouchOptionData = std::move(d);
    else
        t.FxTouchOptionData = std::move(d);
    return t;
}

// ===========================================================================
// Reverse: FxVarianceSwap
// ===========================================================================

trade fx_instrument_mapper::reverse_fx_variance_swap(
        const fx_variance_swap_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FxVarianceSwap";
    trade t;
    t.TradeType = oreTradeType::FxVarianceSwap;

    varianceSwapData d;
    static_cast<std::string&>(d.StartDate) = instr.start_date;
    static_cast<std::string&>(d.EndDate)   = instr.end_date;
    if (!instr.currency.empty())
        d.Currency = parse_currency_code(instr.currency);
    d.underlyingTypes = make_underlying_type_name(instr.underlying_code);
    static_cast<std::string&>(d.LongShort) = instr.long_short;
    d.Strike   = static_cast<float>(instr.strike);
    d.Notional = static_cast<float>(instr.notional);
    static_cast<std::string&>(d.Calendar) = "TARGET";

    t.FxVarianceSwapData = std::move(d);
    return t;
}

// ===========================================================================
// Reverse: FxAverageForward
// ===========================================================================

trade fx_instrument_mapper::reverse_fx_average_forward(
        const fx_asian_forward_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FxAverageForward";
    trade t;
    t.TradeType = oreTradeType::FxAverageForward;

    fxAverageForwardData d;
    static_cast<std::string&>(d.PaymentDate) = instr.payment_date;
    if (!instr.reference_currency.empty())
        d.ReferenceCurrency  = parse_currency_code(instr.reference_currency);
    d.ReferenceNotional  = static_cast<float>(instr.reference_notional.value_or(0.0));
    if (!instr.settlement_currency.empty())
        d.SettlementCurrency = parse_currency_code(instr.settlement_currency);
    d.SettlementNotional = static_cast<float>(instr.settlement_notional.value_or(0.0));
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

trade fx_instrument_mapper::reverse_fx_accumulator(
        const fx_accumulator_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FxAccumulator";
    trade t;
    t.TradeType = oreTradeType::FxAccumulator;

    accumulatorData d;
    if (!instr.currency.empty())
        d.Currency = parse_currency_code(instr.currency);
    d.FixingAmount = static_cast<float>(instr.fixing_amount);
    if (instr.strike != 0.0)
        d.Strike = static_cast<float>(instr.strike);
    static_cast<std::string&>(d.Underlying.Name) = instr.underlying_code;
    static_cast<std::string&>(d.Underlying.Type) = "FX";
    static_cast<std::string&>(d.OptionData.LongShort) = instr.long_short;
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

trade fx_instrument_mapper::reverse_fx_tarf(
        const fx_asian_forward_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FxTaRF";
    trade t;
    t.TradeType = oreTradeType::FxTaRF;

    tarfData2 d;
    if (!instr.currency.empty())
        d.Currency = parse_currency_code(instr.currency);
    d.FixingAmount = static_cast<float>(instr.fixing_amount.value_or(0.0));
    if (instr.strike.has_value())
        d.Strike = static_cast<float>(*instr.strike);
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
        bd.Levels.Level.push_back(static_cast<float>(*instr.target_amount));
        d.Barriers.BarrierData.push_back(std::move(bd));
    }

    t.FxTaRFData = std::move(d);
    return t;
}

}
