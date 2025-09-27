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
// #include <chrono>
// #include <format>
#include "ores.utility/log/logger.hpp"
#include "ores.utility/string/converter.hpp"
#include "ores.risk/xml/currency_serialiser.hpp"

namespace {

using namespace ores::utility::log;
auto lg(logger_factory("ores.xml.currency_serialiser"));

const std::string missing_root_node("Could not find a root XML node.");
const std::string missing_currency_config("No CurrencyConfig element found");

}

namespace ores::risk::xml {

using domain::currency;
using namespace rapidxml_ns;

void currency_serialiser::
serialise(rapidxml_ns::xml_node<>& /*parent*/, const currency& /*ccy*/) {
    // FIXME: implement
}

currency currency_serialiser::deserialise(rapidxml_ns::xml_node<>& node) {
    currency r;
    xml_node<> *name = node.first_node("Name");
    if (name != nullptr)
        r.name = name->value();

    xml_node<> *isoCode = node.first_node("ISOCode");
    if (isoCode != nullptr)
        r.iso_code = isoCode->value();

    using ores::utility::string::convert_to_int;
    xml_node<> *numericCode = node.first_node("NumericCode");
    if (numericCode != nullptr) {
        r.numeric_code = convert_to_int(numericCode->value());
    }

    xml_node<> *symbol = node.first_node("Symbol");
    if (symbol != nullptr)
        r.symbol = symbol->value();

    xml_node<> *fractionSymbol = node.first_node("FractionSymbol");
    if (fractionSymbol != nullptr)
        r.fraction_symbol = fractionSymbol->value();

    xml_node<> *fractionsPerUnit = node.first_node("FractionsPerUnit");
    if (fractionsPerUnit != nullptr)
        r.fractions_per_unit = convert_to_int(fractionsPerUnit->value());

    xml_node<> *roundingType = node.first_node("RoundingType");
    if (roundingType != nullptr)
        r.rounding_type = roundingType->value();

    xml_node<> *roundingPrecision = node.first_node("RoundingPrecision");
    if (roundingPrecision != nullptr)
        r.rounding_precision = convert_to_int(roundingPrecision->value());

    xml_node<> *format = node.first_node("Format");
    if (format != nullptr)
        r.format = format->value();

    xml_node<> *currencyType = node.first_node("CurrencyType");
    if (currencyType != nullptr)
        r.currency_type = currencyType->value();

    return r;
}

}
