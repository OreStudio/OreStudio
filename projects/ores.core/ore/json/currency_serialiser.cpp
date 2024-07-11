/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#include <ostream>
#include <istream>
#include "ores.core/ore/json/currency_serialiser.hpp"

namespace ores::core::ore::json {

using rapidjson::Value;
using rapidjson::Document;

void currency_serialiser::
serialise(const model::currency& ccy, Value& v, Document::AllocatorType& a) {
    v.SetObject();

    using rapidjson::StringRef;
    v.AddMember("Name", Value(ccy.name(), a).Move(), a);
    v.AddMember("ISOCode", Value(ccy.iso_code(), a).Move(), a);
    v.AddMember("NumericCode", ccy.numeric_code(), a);
    v.AddMember("Symbol", Value(ccy.symbol(), a).Move(), a);
    v.AddMember("FractionSymbol", Value(ccy.fraction_symbol(), a).Move(), a);
    v.AddMember("FractionsPerUnit", ccy.fractions_per_unit(), a);
    v.AddMember("RoundingType", Value(ccy.rounding_type(), a).Move(), a);
    v.AddMember("RoundingPrecision", ccy.rounding_precision(), a);
    v.AddMember("Format", Value(ccy.format(), a).Move(), a);
    v.AddMember("CurrencyType", Value(ccy.currency_type(), a).Move(), a);
}

model::currency currency_serialiser::deserialise(const Value& value) {
    model::currency r;

    r.name(value["Name"].GetString());
    r.iso_code(value["ISOCode"].GetString());
    r.numeric_code(value["NumericCode"].GetInt());
    r.symbol(value["Symbol"].GetString());
    r.fraction_symbol(value["FractionSymbol"].GetString());
    r.fractions_per_unit(value["FractionsPerUnit"].GetInt());
    r.rounding_type(value["RoundingType"].GetString());
    r.rounding_precision(value["RoundingPrecision"].GetInt());
    r.format(value["Format"].GetString());
    r.currency_type(value["CurrencyType"].GetString());

    return r;
}

}
