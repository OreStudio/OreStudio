/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.ore/domain/currency_mapper.hpp"

#include <map>
#include "ores.refdata/domain/currency_json_io.hpp" // IWYU pragma: keep.

namespace ores::ore::domain {

using namespace ores::logging;

namespace {

roundingType parse_rounding_type(const std::string& s) {
    static const std::map<std::string, roundingType> rounding_map = {
        {"Up", roundingType::Up},
        {"Down", roundingType::Down},
        {"Closest", roundingType::Closest},
        {"Floor", roundingType::Floor},
        {"Ceiling", roundingType::Ceiling}
    };

    if (auto it = rounding_map.find(s); it != rounding_map.end()) {
        return it->second;
    }
    throw std::runtime_error("Invalid rounding type: " + s);
}

}

refdata::domain::currency currency_mapper::map(const currencyDefinition& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping ORE XML entity: " << std::string(v.ISOCode);

    refdata::domain::currency r;
    r.iso_code = v.ISOCode;
    r.name = v.Name;
    r.numeric_code = v.NumericCode ? std::to_string(*v.NumericCode) : "";
    r.symbol = v.Symbol;
    r.fraction_symbol = v.FractionSymbol;
    r.fractions_per_unit = static_cast<int>(v.FractionsPerUnit);
    r.rounding_type = to_string(v.RoundingType);
    r.rounding_precision = static_cast<int>(v.RoundingPrecision);
    r.format = "";  // Not in XSD
    r.currency_type = v.CurrencyType ? std::string(*v.CurrencyType) : "";
    r.modified_by = "ores";
    r.change_reason_code = "system.external_data_import";
    r.change_commentary = "Imported from ORE XML";

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

currencyDefinition currency_mapper::map(const refdata::domain::currency& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    currencyDefinition r;
    static_cast<xsd::string&>(r.ISOCode) = v.iso_code;
    static_cast<xsd::string&>(r.Name) = v.name;
    if (!v.numeric_code.empty()) {
        try {
            r.NumericCode = std::stoll(v.numeric_code);
        } catch (const std::exception&) {
            // Keep NumericCode empty if not a valid number
        }
    }
    static_cast<xsd::string&>(r.Symbol) = v.symbol;
    static_cast<xsd::string&>(r.FractionSymbol) = v.fraction_symbol;
    r.FractionsPerUnit = v.fractions_per_unit;
    r.RoundingType = parse_rounding_type(v.rounding_type);
    r.RoundingPrecision = v.rounding_precision;
    if (!v.currency_type.empty()) {
        currencyDefinition_CurrencyType_t ct;
        static_cast<xsd::string&>(ct) = v.currency_type;
        r.CurrencyType = ct;
    }

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. ISOCode: " << std::string(r.ISOCode);
    return r;
}

std::vector<refdata::domain::currency> currency_mapper::map(const currencyConfig& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping ORE XML entities. Total: "
                             << v.Currency.size();

    std::vector<refdata::domain::currency> r;
    r.reserve(v.Currency.size());
    std::ranges::transform(v.Currency, std::back_inserter(r),
        [](const auto& ve) { return map(ve); });
    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entities.";
    return r;
}

currencyConfig currency_mapper::map(const std::vector<refdata::domain::currency>& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entities. Total: " << v.size();

    currencyConfig r;
    r.Currency.reserve(v.size());
    std::ranges::transform(v, std::back_inserter(r.Currency),
        [](const auto& ve) { return map(ve); });
    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entities.";
    return r;
}

}
