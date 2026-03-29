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
#include "ores.ore/domain/swap_instrument_mapper.hpp"

#include <map>

namespace ores::ore::domain {

using namespace ores::logging;
using ores::trading::domain::instrument;
using ores::trading::domain::swap_leg;

// ---------------------------------------------------------------------------
// Helpers: forward direction (ORE XSD → string)
// ---------------------------------------------------------------------------

namespace {

std::string day_counter_string(const xsd::optional<dayCounter>& dc) {
    if (!dc) return {};
    return to_string(*dc);
}

std::string bdc_string(const xsd::optional<businessDayConvention>& bdc) {
    if (!bdc) return {};
    return to_string(*bdc);
}

float first_notional(const legData_Notionals_t& notionals) {
    if (!notionals.Notional.empty())
        return static_cast<float>(notionals.Notional.front());
    return 0.0f;
}

std::string first_tenor(const xsd::optional<scheduleData>& sd) {
    if (!sd) return {};
    if (!sd->Rules.empty())
        return std::string(sd->Rules.front().Tenor);
    return {};
}

std::string start_date_from_schedule(const xsd::optional<scheduleData>& sd) {
    if (!sd) return {};
    if (!sd->Rules.empty())
        return std::string(sd->Rules.front().StartDate);
    return {};
}

std::string end_date_from_schedule(const xsd::optional<scheduleData>& sd) {
    if (!sd) return {};
    if (!sd->Rules.empty() && sd->Rules.front().EndDate)
        return std::string(*sd->Rules.front().EndDate);
    return {};
}

// CapFloor leg uses non-optional scheduleData
std::string start_date_from_schedule(const scheduleData& sd) {
    if (!sd.Rules.empty())
        return std::string(sd.Rules.front().StartDate);
    return {};
}

std::string end_date_from_schedule(const scheduleData& sd) {
    if (!sd.Rules.empty() && sd.Rules.front().EndDate)
        return std::string(*sd.Rules.front().EndDate);
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

scheduleData make_schedule(const std::string& start, const std::string& end,
                           const std::string& tenor) {
    scheduleData sd;
    scheduleData_Rules_t r;
    r.StartDate = start;
    if (!end.empty()) {
        domain::date d;
        static_cast<std::string&>(d) = end;
        r.EndDate = xsd::optional<domain::date>(d);
    }
    static_cast<std::string&>(r.Tenor) = tenor;
    r.Convention = businessDayConvention::MF;
    sd.Rules.push_back(std::move(r));
    return sd;
}

std::optional<legType> leg_type_from_string(const std::string& s) {
    if (s == "Fixed") return legType::Fixed;
    if (s == "Floating") return legType::Floating;
    if (s == "CMS") return legType::CMS;
    if (s == "CMSSpread") return legType::CMSSpread;
    if (s == "DigitalCMS") return legType::DigitalCMS;
    if (s == "ZeroCouponFixed") return legType::ZeroCouponFixed;
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
        {"SLL", currencyCode::SLL}, {"SKK", currencyCode::SKK},
        {"SOS", currencyCode::SOS}, {"SRD", currencyCode::SRD},
        {"SSP", currencyCode::SSP}, {"STN", currencyCode::STN},
        {"SVC", currencyCode::SVC}, {"SYP", currencyCode::SYP},
        {"SZL", currencyCode::SZL}, {"THB", currencyCode::THB},
        {"TJS", currencyCode::TJS}, {"TMT", currencyCode::TMT},
        {"TND", currencyCode::TND}, {"TOP", currencyCode::TOP},
        {"TRY", currencyCode::TRY}, {"TTD", currencyCode::TTD},
        {"TWD", currencyCode::TWD}, {"TZS", currencyCode::TZS},
        {"UAH", currencyCode::UAH}, {"UGX", currencyCode::UGX},
        {"USD", currencyCode::USD}, {"USN", currencyCode::USN},
        {"UYI", currencyCode::UYI}, {"UYU", currencyCode::UYU},
        {"UYW", currencyCode::UYW}, {"UZS", currencyCode::UZS},
        {"VES", currencyCode::VES}, {"VND", currencyCode::VND},
        {"VUV", currencyCode::VUV}, {"WST", currencyCode::WST},
        {"XAF", currencyCode::XAF}, {"XAG", currencyCode::XAG},
        {"XAU", currencyCode::XAU}, {"XBT", currencyCode::XBT},
        {"XCD", currencyCode::XCD}, {"XOF", currencyCode::XOF},
        {"XPD", currencyCode::XPD}, {"XPF", currencyCode::XPF},
        {"XPT", currencyCode::XPT}, {"XRP", currencyCode::XRP},
        {"XSU", currencyCode::XSU}, {"XUA", currencyCode::XUA},
        {"YER", currencyCode::YER}, {"ZAR", currencyCode::ZAR},
        {"ZMW", currencyCode::ZMW}, {"ZWL", currencyCode::ZWL},
        // Crypto codes recognised by ORE
        {"BTC", currencyCode::BTC}, {"ETH", currencyCode::ETH},
        {"ETC", currencyCode::ETC}, {"BCH", currencyCode::BCH},
        {"LTC", currencyCode::LTC},
        // ORE internal synthetic codes
        {"ZUR", currencyCode::ZUR}, {"ZUG", currencyCode::ZUG},
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
// Forward mapping: legData (swap) → swap_leg
// ---------------------------------------------------------------------------

swap_leg swap_instrument_mapper::map_leg(const legData& ld, int leg_number) {
    swap_leg sl;
    sl.leg_number = leg_number;
    sl.leg_type_code = to_string(ld.LegType);

    if (ld.Currency)
        sl.currency = std::string(*ld.Currency);
    sl.day_count_fraction_code = day_counter_string(ld.DayCounter);
    sl.business_day_convention_code = bdc_string(ld.PaymentConvention);
    sl.payment_frequency_code = first_tenor(ld.ScheduleData);

    if (ld.Notionals)
        sl.notional = static_cast<double>(first_notional(*ld.Notionals));

    if (ld.legDataType) {
        const auto& ldt = *ld.legDataType;
        if (ldt.FixedLegData && !ldt.FixedLegData->Rates.Rate.empty())
            sl.fixed_rate = static_cast<double>(
                ldt.FixedLegData->Rates.Rate.front());
        if (ldt.FloatingLegData) {
            sl.floating_index_code = std::string(ldt.FloatingLegData->Index);
            if (ldt.FloatingLegData->Spreads &&
                    !ldt.FloatingLegData->Spreads->Spread.empty())
                sl.spread = static_cast<double>(
                    ldt.FloatingLegData->Spreads->Spread.front());
        }
    }

    sl.modified_by = "ores";
    sl.performed_by = "ores";
    sl.change_reason_code = "system.external_data_import";
    sl.change_commentary = "Imported from ORE XML";
    return sl;
}

// ---------------------------------------------------------------------------
// Forward: Swap / CrossCurrencySwap / InflationSwap
// ---------------------------------------------------------------------------

swap_mapping_result swap_instrument_mapper::forward_swap(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping swap: " << std::string(t.id);

    const swapData* sd = nullptr;
    std::string trade_type_code;

    if (t.SwapData) {
        sd = &*t.SwapData;
        trade_type_code = "Swap";
    } else if (t.CrossCurrencySwapData) {
        sd = &*t.CrossCurrencySwapData;
        trade_type_code = "CrossCurrencySwap";
    } else if (t.InflationSwapData) {
        sd = &*t.InflationSwapData;
        trade_type_code = "InflationSwap";
    }

    swap_mapping_result result;
    result.instrument.trade_type_code = trade_type_code;
    result.instrument.modified_by = "ores";
    result.instrument.performed_by = "ores";
    result.instrument.change_reason_code = "system.external_data_import";
    result.instrument.change_commentary = "Imported from ORE XML";

    if (!sd) return result;

    if (!sd->LegData.empty()) {
        result.instrument.start_date =
            start_date_from_schedule(sd->LegData.front().ScheduleData);
        result.instrument.maturity_date =
            end_date_from_schedule(sd->LegData.front().ScheduleData);
    }

    int leg_num = 1;
    for (const auto& ld : sd->LegData)
        result.legs.push_back(map_leg(ld, leg_num++));

    return result;
}

// ---------------------------------------------------------------------------
// Forward: ForwardRateAgreement
// ---------------------------------------------------------------------------

swap_mapping_result swap_instrument_mapper::forward_fra(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping FRA: " << std::string(t.id);

    swap_mapping_result result;
    result.instrument.modified_by = "ores";
    result.instrument.performed_by = "ores";
    result.instrument.change_reason_code = "system.external_data_import";
    result.instrument.change_commentary = "Imported from ORE XML";

    if (!t.ForwardRateAgreementData) return result;

    const auto& fra = *t.ForwardRateAgreementData;
    result.instrument.trade_type_code = "ForwardRateAgreement";
    result.instrument.start_date = std::string(fra.StartDate);
    result.instrument.maturity_date = std::string(fra.EndDate);
    result.instrument.currency = to_string(fra.Currency);
    result.instrument.notional = static_cast<double>(fra.Notional);

    swap_leg sl;
    sl.leg_number = 1;
    sl.leg_type_code = "Fixed";
    sl.currency = to_string(fra.Currency);
    sl.floating_index_code = std::string(fra.Index);
    sl.fixed_rate = static_cast<double>(fra.Strike);
    sl.notional = static_cast<double>(fra.Notional);
    sl.modified_by = "ores";
    sl.performed_by = "ores";
    sl.change_reason_code = "system.external_data_import";
    sl.change_commentary = "Imported from ORE XML";
    result.legs.push_back(std::move(sl));

    return result;
}

// ---------------------------------------------------------------------------
// Forward: CapFloor
// ---------------------------------------------------------------------------

swap_mapping_result swap_instrument_mapper::forward_capfloor(const trade& t) {
    BOOST_LOG_SEV(lg(), debug)
        << "Forward-mapping capfloor: " << std::string(t.id);

    swap_mapping_result result;
    result.instrument.modified_by = "ores";
    result.instrument.performed_by = "ores";
    result.instrument.change_reason_code = "system.external_data_import";
    result.instrument.change_commentary = "Imported from ORE XML";

    if (!t.CapFloorData) return result;

    const auto& cf = *t.CapFloorData;
    result.instrument.trade_type_code = "CapFloor";
    result.instrument.currency = to_string(cf.LegData.Currency);
    result.instrument.start_date = start_date_from_schedule(cf.LegData.ScheduleData);
    result.instrument.maturity_date = end_date_from_schedule(cf.LegData.ScheduleData);

    swap_leg sl;
    sl.leg_number = 1;
    sl.leg_type_code = to_string(cf.LegData.LegType);
    sl.currency = to_string(cf.LegData.Currency);
    sl.day_count_fraction_code = to_string(cf.LegData.DayCounter);
    sl.business_day_convention_code = bdc_string(cf.LegData.PaymentConvention);
    sl.payment_frequency_code = first_tenor(cf.LegData.ScheduleData);

    if (!cf.LegData.Notionals.Notional.empty())
        sl.notional = static_cast<double>(cf.LegData.Notionals.Notional.front());

    if (cf.LegData.legDataType.FloatingLegData) {
        sl.floating_index_code = std::string(
            cf.LegData.legDataType.FloatingLegData->Index);
        if (cf.LegData.legDataType.FloatingLegData->Spreads &&
                !cf.LegData.legDataType.FloatingLegData->Spreads->Spread.empty())
            sl.spread = static_cast<double>(
                cf.LegData.legDataType.FloatingLegData->Spreads->Spread.front());
    }

    sl.modified_by = "ores";
    sl.performed_by = "ores";
    sl.change_reason_code = "system.external_data_import";
    sl.change_commentary = "Imported from ORE XML";
    result.legs.push_back(std::move(sl));

    return result;
}

// ---------------------------------------------------------------------------
// Reverse: swap_leg → legData
// ---------------------------------------------------------------------------

legData swap_instrument_mapper::reverse_leg(
        const swap_leg& sl, const instrument& instr) {
    legData ld;

    ld.LegType = leg_type_from_string(sl.leg_type_code).value_or(legType::Fixed);
    ld.Currency = sl.currency;

    if (sl.notional != 0.0)
        ld.Notionals = make_notionals(sl.notional);

    ld.ScheduleData = make_schedule(instr.start_date, instr.maturity_date,
                                    sl.payment_frequency_code);

    legDataType_group_t ldt;
    if (ld.LegType == legType::Fixed && sl.fixed_rate != 0.0) {
        _FixedLegData_t fld;
        _FixedLegData_t_Rates_t_Rate_t rate;
        static_cast<float&>(rate) = static_cast<float>(sl.fixed_rate);
        fld.Rates.Rate.push_back(rate);
        ldt.FixedLegData = std::move(fld);
    } else if (ld.LegType == legType::Floating) {
        _FloatingLegData_t fld;
        fld.Index = sl.floating_index_code;
        if (sl.spread != 0.0) {
            spreads sp;
            floatWithAttribute sv;
            static_cast<float&>(sv) = static_cast<float>(sl.spread);
            sp.Spread.push_back(sv);
            fld.Spreads = std::move(sp);
        }
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
        const instrument& instr,
        const std::vector<swap_leg>& legs) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping swap: "
                               << instr.trade_type_code;

    trade t;
    t.TradeType = oreTradeType::Swap;
    if (instr.trade_type_code == "CrossCurrencySwap")
        t.TradeType = oreTradeType::CrossCurrencySwap;
    else if (instr.trade_type_code == "InflationSwap")
        t.TradeType = oreTradeType::InflationSwap;

    swapData sd;
    for (const auto& sl : legs)
        sd.LegData.push_back(reverse_leg(sl, instr));

    if (instr.trade_type_code == "CrossCurrencySwap")
        t.CrossCurrencySwapData = std::move(sd);
    else if (instr.trade_type_code == "InflationSwap")
        t.InflationSwapData = std::move(sd);
    else
        t.SwapData = std::move(sd);

    return t;
}

// ---------------------------------------------------------------------------
// Reverse: ForwardRateAgreement
// ---------------------------------------------------------------------------

trade swap_instrument_mapper::reverse_fra(
        const instrument& instr,
        const std::vector<swap_leg>& legs) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping FRA";

    trade t;
    t.TradeType = oreTradeType::ForwardRateAgreement;

    forwardRateAgreementData fra;
    fra.StartDate = instr.start_date;
    fra.EndDate = instr.maturity_date;
    fra.Currency = parse_currency_code(instr.currency);
    fra.Notional = static_cast<float>(instr.notional);

    if (!legs.empty()) {
        static_cast<std::string&>(fra.Index) = legs.front().floating_index_code;
        fra.Strike = static_cast<float>(legs.front().fixed_rate);
        fra.LongShort = longShort::Long;
    }

    t.ForwardRateAgreementData = std::move(fra);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: CapFloor
// ---------------------------------------------------------------------------

trade swap_instrument_mapper::reverse_capfloor(
        const instrument& instr,
        const std::vector<swap_leg>& legs) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping capfloor";

    trade t;
    t.TradeType = oreTradeType::CapFloor;

    capFloorData cf;
    cf.LongShort = longShort::Long;

    if (!legs.empty()) {
        const auto& sl = legs.front();

        cf.LegData.LegType = leg_type_from_string(sl.leg_type_code)
                                 .value_or(legType::Floating);

        cf.LegData.Currency = parse_currency_code(sl.currency);

        cf.LegData.DayCounter = dayCounter::ACT_365;
        cf.LegData.ScheduleData = make_schedule(instr.start_date,
                                                instr.maturity_date,
                                                sl.payment_frequency_code);
        if (sl.notional != 0.0) {
            legData_capfloor_Notionals_t_Notional_t nv;
            static_cast<float&>(nv) = static_cast<float>(sl.notional);
            cf.LegData.Notionals.Notional.push_back(nv);
        }

        if (cf.LegData.LegType == legType::Floating) {
            _FloatingLegData_t fld;
            fld.Index = sl.floating_index_code;
            if (sl.spread != 0.0) {
                spreads sp;
                floatWithAttribute sv;
                static_cast<float&>(sv) = static_cast<float>(sl.spread);
                sp.Spread.push_back(sv);
                fld.Spreads = std::move(sp);
            }
            cf.LegData.legDataType.FloatingLegData = std::move(fld);
        }
    }

    t.CapFloorData = std::move(cf);
    return t;
}

} // namespace ores::ore::domain
