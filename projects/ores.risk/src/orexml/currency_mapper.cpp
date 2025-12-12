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
#include "ores.risk/orexml/currency_mapper.hpp"

#include "ores.risk/domain/currency_json_io.hpp" // IWYU pragma: keep.

namespace ores::risk::orexml {

using namespace ores::utility::log;

domain::currency currency_mapper::map(const CurrencyElement& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping ORE XML entity: " << v;

    domain::currency r;
    r.iso_code = v.ISOCode;
    r.name = v.Name;
    r.numeric_code = v.NumericCode;
    r.symbol = v.Symbol;
    r.fraction_symbol = v.FractionSymbol;
    r.fractions_per_unit = v.FractionsPerUnit;
    r.rounding_type = v.RoundingType;
    r.rounding_precision = v.RoundingPrecision;
    r.format = v.Format;
    r.currency_type = v.CurrencyType ? *v.CurrencyType : "";

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

CurrencyElement currency_mapper::map(const domain::currency& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    CurrencyElement r;
    r.ISOCode = v.iso_code;
    r.Name = v.name;
    r.NumericCode = v.numeric_code;
    r.Symbol = v.symbol;
    r.FractionSymbol = v.fraction_symbol;
    r.FractionsPerUnit = v.fractions_per_unit;
    r.RoundingType = v.rounding_type;
    r.RoundingPrecision = v.rounding_precision;
    r.Format = v.format;
    r.CurrencyType = v.currency_type;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::currency> currency_mapper::map(const CurrencyConfig& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping ORE XML entities. Total: "
                             << v.Currency.size();

    std::vector<domain::currency> r;
    r.reserve(v.Currency.size());
    std::ranges::transform(v.Currency, std::back_inserter(r),
        [](const auto& ve) { return map(ve); });
    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entities.";
    return r;
}

CurrencyConfig currency_mapper::map(const std::vector<domain::currency>& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entities. Total: " << v.size();

    CurrencyConfig r;
    r.Currency.reserve(v.size());
    std::ranges::transform(v, std::back_inserter(r.Currency),
        [](const auto& ve) { return map(ve); });
    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entities.";
    return r;
}

}
