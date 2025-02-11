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
#include <boost/date_time/posix_time/posix_time.hpp>
#include "ores.core/json/currency_serialiser.hpp"

namespace ores::core::json {

using rapidjson::Value;
using rapidjson::Document;

void currency_serialiser::
serialise(const types::currency& ccy, Value& v, Document::AllocatorType& a) {
    v.SetObject();

    using rapidjson::StringRef;
    v.AddMember("name", Value(ccy.name(), a).Move(), a);
    v.AddMember("iso_code", Value(ccy.iso_code(), a).Move(), a);
    v.AddMember("numeric_code", ccy.numeric_code(), a);
    v.AddMember("symbol", Value(ccy.symbol(), a).Move(), a);
    v.AddMember("fraction_symbol", Value(ccy.fraction_symbol(), a).Move(), a);
    v.AddMember("fractions_per_unit", ccy.fractions_per_unit(), a);
    v.AddMember("rounding_type", Value(ccy.rounding_type(), a).Move(), a);
    v.AddMember("rounding_precision", ccy.rounding_precision(), a);
    v.AddMember("format", Value(ccy.format(), a).Move(), a);
    v.AddMember("currency_type", Value(ccy.currency_type(), a).Move(), a);
    v.AddMember("modified_by", Value(ccy.modified_by(), a).Move(), a);
    v.AddMember("valid_from", Value(ccy.valid_from(), a).Move(), a);
    v.AddMember("valid_to", Value(ccy.valid_to(), a).Move(), a);
}

types::currency currency_serialiser::deserialise(const Value& value) {
    types::currency r;

    r.name(value["name"].GetString());
    r.iso_code(value["iso_code"].GetString());
    r.numeric_code(value["numeric_code"].GetInt());
    r.symbol(value["symbol"].GetString());
    r.fraction_symbol(value["fraction_symbol"].GetString());
    r.fractions_per_unit(value["fractions_per_unit"].GetInt());
    r.rounding_type(value["rounding_type"].GetString());
    r.rounding_precision(value["rounding_precision"].GetInt());
    r.format(value["format"].GetString());
    r.currency_type(value["currency_type"].GetString());
    r.modified_by(value["modified_by"].GetString());
    r.valid_from(value["valid_from"].GetString());
    r.valid_to(value["valid_to"].GetString());

    return r;
}

}
