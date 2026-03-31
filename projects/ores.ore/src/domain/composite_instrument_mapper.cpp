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
#include "ores.ore/domain/composite_instrument_mapper.hpp"

#include <map>
#include <stdexcept>

namespace ores::ore::domain {

using namespace ores::logging;
using ores::trading::domain::composite_instrument;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

namespace {

composite_instrument make_base(const std::string& trade_type_code) {
    composite_instrument r;
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

} // namespace

// ---------------------------------------------------------------------------
// Forward: CompositeTrade
// ---------------------------------------------------------------------------

composite_mapping_result
composite_instrument_mapper::forward_composite_trade(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping CompositeTrade: "
                               << std::string(t.id);
    composite_mapping_result result;
    result.instrument = make_base("CompositeTrade");
    if (!t.CompositeTradeData) return result;
    const auto& d = *t.CompositeTradeData;

    if (d.BasketName)
        result.instrument.description = std::string(*d.BasketName);
    return result;
}

// ---------------------------------------------------------------------------
// Forward: MultiLegOption
// ---------------------------------------------------------------------------

composite_mapping_result
composite_instrument_mapper::forward_multi_leg_option(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping MultiLegOption: "
                               << std::string(t.id);
    composite_mapping_result result;
    result.instrument = make_base("MultiLegOption");
    // MultiLegOptionData has leg data but composite_instrument only
    // captures the top-level type; legs are stored separately.
    return result;
}

// ---------------------------------------------------------------------------
// Reverse: CompositeTrade
// ---------------------------------------------------------------------------

trade composite_instrument_mapper::reverse_composite_trade(
        const composite_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping CompositeTrade";
    trade t;
    t.TradeType = oreTradeType::CompositeTrade;
    compositeTradeData d;
    // compositeTradeData requires a Currency element in the ORE XSD, but the
    // composite_instrument domain model does not carry a currency field because
    // composite trades are currency-agnostic at the basket level — currency is
    // specified per component leg.  USD is used as a structural placeholder so
    // that the serialised XML is schema-valid; a full export pipeline would
    // need to derive the settlement currency from the component legs.
    d.Currency = parse_currency_code("USD");
    if (!instr.description.empty()) {
        compositeTradeData_BasketName_t bn;
        static_cast<std::string&>(bn) = instr.description;
        d.BasketName = std::move(bn);
    }
    t.CompositeTradeData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: MultiLegOption
// ---------------------------------------------------------------------------

trade composite_instrument_mapper::reverse_multi_leg_option(
        const composite_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping MultiLegOption";
    (void)instr;
    trade t;
    t.TradeType = oreTradeType::MultiLegOption;
    // MultiLegOption leg data (strikes, barriers, underlying leg definitions)
    // is stored in composite_leg records in the ORES domain, not in the
    // composite_instrument itself.  Reconstructing a fully populated
    // MultiLegOptionData therefore requires access to the associated leg
    // records, which are outside the scope of this single-instrument reverse
    // mapper.  The trade shell produced here is structurally valid and
    // sufficient for round-trip type identity; a complete export must enrich
    // it with legs from the composite_leg repository.
    multiLegOptionData d;
    t.MultiLegOptionData = std::move(d);
    return t;
}

}
