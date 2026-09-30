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
#ifndef ORES_ORE_CORE_DOMAIN_ORE_CODE_TABLES_HPP
#define ORES_ORE_CORE_DOMAIN_ORE_CODE_TABLES_HPP

#include "ores.ore.core/domain/domain.hpp"
#include <map>
#include <stdexcept>
#include <string>

/**
 * @file ore_code_tables.hpp
 * @brief Reading ORE's code spellings back into their enumerations.
 *
 * The binding writes an enumeration as ORE spells it and a mapper has to read
 * it back, and the binding's own tables are private to its translation unit. A
 * mapper that needs one carries the table; when more than one needs the same,
 * it belongs here instead.
 */

namespace ores::ore::domain {

inline domain::currencyCode parse_currency_code(const std::string& s) {
    using cc = domain::currencyCode;
    static const std::map<std::string, cc> kMap = {
        {"AED", cc::AED}, {"AFN", cc::AFN}, {"ALL", cc::ALL}, {"AMD", cc::AMD}, {"ANG", cc::ANG},
        {"AOA", cc::AOA}, {"ARS", cc::ARS}, {"AUD", cc::AUD}, {"AWG", cc::AWG}, {"AZN", cc::AZN},
        {"BAM", cc::BAM}, {"BBD", cc::BBD}, {"BDT", cc::BDT}, {"BGN", cc::BGN}, {"BHD", cc::BHD},
        {"BIF", cc::BIF}, {"BMD", cc::BMD}, {"BND", cc::BND}, {"BOB", cc::BOB}, {"BOV", cc::BOV},
        {"BRL", cc::BRL}, {"BSD", cc::BSD}, {"BTN", cc::BTN}, {"BWP", cc::BWP}, {"BYN", cc::BYN},
        {"BZD", cc::BZD}, {"CAD", cc::CAD}, {"CDF", cc::CDF}, {"CHE", cc::CHE}, {"CHF", cc::CHF},
        {"CHW", cc::CHW}, {"CLF", cc::CLF}, {"CLP", cc::CLP}, {"CNH", cc::CNH}, {"CNT", cc::CNT},
        {"CNY", cc::CNY}, {"COP", cc::COP}, {"COU", cc::COU}, {"CRC", cc::CRC}, {"CUC", cc::CUC},
        {"CUP", cc::CUP}, {"CVE", cc::CVE}, {"CYP", cc::CYP}, {"CZK", cc::CZK}, {"DJF", cc::DJF},
        {"DKK", cc::DKK}, {"DOP", cc::DOP}, {"DZD", cc::DZD}, {"EGP", cc::EGP}, {"ERN", cc::ERN},
        {"ETB", cc::ETB}, {"EUR", cc::EUR}, {"FJD", cc::FJD}, {"FKP", cc::FKP}, {"GBP", cc::GBP},
        {"GEL", cc::GEL}, {"GGP", cc::GGP}, {"GHS", cc::GHS}, {"GIP", cc::GIP}, {"GMD", cc::GMD},
        {"GNF", cc::GNF}, {"GTQ", cc::GTQ}, {"GYD", cc::GYD}, {"HKD", cc::HKD}, {"HNL", cc::HNL},
        {"HRK", cc::HRK}, {"HTG", cc::HTG}, {"HUF", cc::HUF}, {"IDR", cc::IDR}, {"ILS", cc::ILS},
        {"IMP", cc::IMP}, {"INR", cc::INR}, {"IQD", cc::IQD}, {"IRR", cc::IRR}, {"ISK", cc::ISK},
        {"JEP", cc::JEP}, {"JMD", cc::JMD}, {"JOD", cc::JOD}, {"JPY", cc::JPY}, {"KES", cc::KES},
        {"KGS", cc::KGS}, {"KHR", cc::KHR}, {"KID", cc::KID}, {"KMF", cc::KMF}, {"KPW", cc::KPW},
        {"KRW", cc::KRW}, {"KWD", cc::KWD}, {"KYD", cc::KYD}, {"KZT", cc::KZT}, {"LAK", cc::LAK},
        {"LBP", cc::LBP}, {"LKR", cc::LKR}, {"LRD", cc::LRD}, {"LSL", cc::LSL}, {"LTL", cc::LTL},
        {"LVL", cc::LVL}, {"LYD", cc::LYD}, {"MAD", cc::MAD}, {"MDL", cc::MDL}, {"MGA", cc::MGA},
        {"MKD", cc::MKD}, {"MMK", cc::MMK}, {"MNT", cc::MNT}, {"MOP", cc::MOP}, {"MRU", cc::MRU},
        {"MUR", cc::MUR}, {"MVR", cc::MVR}, {"MWK", cc::MWK}, {"MXN", cc::MXN}, {"MXV", cc::MXV},
        {"MYR", cc::MYR}, {"MZN", cc::MZN}, {"NAD", cc::NAD}, {"NGN", cc::NGN}, {"NIO", cc::NIO},
        {"NOK", cc::NOK}, {"NPR", cc::NPR}, {"NZD", cc::NZD}, {"OMR", cc::OMR}, {"PAB", cc::PAB},
        {"PEN", cc::PEN}, {"PGK", cc::PGK}, {"PHP", cc::PHP}, {"PKR", cc::PKR}, {"PLN", cc::PLN},
        {"PYG", cc::PYG}, {"QAR", cc::QAR}, {"RON", cc::RON}, {"RSD", cc::RSD}, {"RUB", cc::RUB},
        {"RWF", cc::RWF}, {"SAR", cc::SAR}, {"SBD", cc::SBD}, {"SCR", cc::SCR}, {"SDG", cc::SDG},
        {"SEK", cc::SEK}, {"SGD", cc::SGD}, {"SHP", cc::SHP}, {"SLL", cc::SLL}, {"SKK", cc::SKK},
        {"SOS", cc::SOS}, {"SRD", cc::SRD}, {"SSP", cc::SSP}, {"STN", cc::STN}, {"SVC", cc::SVC},
        {"SYP", cc::SYP}, {"SZL", cc::SZL}, {"THB", cc::THB}, {"TJS", cc::TJS}, {"TMT", cc::TMT},
        {"TND", cc::TND}, {"TOP", cc::TOP}, {"TRY", cc::TRY}, {"TTD", cc::TTD}, {"TWD", cc::TWD},
        {"TZS", cc::TZS}, {"UAH", cc::UAH}, {"UGX", cc::UGX}, {"USD", cc::USD}, {"USN", cc::USN},
        {"UYI", cc::UYI}, {"UYU", cc::UYU}, {"UYW", cc::UYW}, {"UZS", cc::UZS}, {"VES", cc::VES},
        {"VND", cc::VND}, {"VUV", cc::VUV}, {"WST", cc::WST}, {"XAF", cc::XAF}, {"XAG", cc::XAG},
        {"XAU", cc::XAU}, {"XCD", cc::XCD}, {"XOF", cc::XOF}, {"XPD", cc::XPD}, {"XPF", cc::XPF},
        {"XPT", cc::XPT}, {"XSU", cc::XSU}, {"XUA", cc::XUA}, {"YER", cc::YER}, {"ZAR", cc::ZAR},
        {"ZMW", cc::ZMW}, {"ZWL", cc::ZWL}, {"BTC", cc::BTC}, {"ETH", cc::ETH}, {"XBT", cc::XBT},
        {"ETC", cc::ETC}, {"BCH", cc::BCH}, {"XRP", cc::XRP}, {"LTC", cc::LTC}, {"ZUR", cc::ZUR},
        {"ZUG", cc::ZUG},
    };
    const auto it = kMap.find(s);
    if (it == kMap.end())
        throw std::runtime_error("parse_currency_code: unrecognised '" + s + "'");
    return it->second;
}

/**
 * @brief Reads ORE's spelling of a shift type back into the enumeration.
 */
inline domain::shiftType parse_shift_type(const std::string& s) {
    using st = domain::shiftType;
    if (s == "Relative")
        return st::Relative;
    if (s == "Absolute")
        return st::Absolute;
    if (s == "EqualTo")
        return st::EqualTo;
    throw std::runtime_error("parse_shift_type: unrecognised '" + s + "'");
}

}

#endif
