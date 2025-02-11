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
#include "rapidjson/writer.h"
#include "rapidjson/document.h"
#include "rapidjson/stringbuffer.h"
#include "ores.utility/log/logger.hpp"
#include "ores.core/json/parsing_error.hpp"
#include "ores.core/json/currency_serialiser.hpp"
#include "ores.core/json/currency_config_serialiser.hpp"

namespace {

// exceptions only
using namespace ores::utility::log;
auto lg(logger_factory("ores.core.json.currency_config_serialiser"));

const std::string missing_currency_config("No CurrencyConfig found");
const std::string missing_currencies("No Currencies found");

}

namespace ores::core::json {

using rapidjson::Value;
using rapidjson::Writer;
using rapidjson::Document;
using rapidjson::StringBuffer;

std::string
currency_config_serialiser::serialise(const types::currency_config& cfg) {
    Document doc;
    doc.SetObject();

    Value currency_config_value(rapidjson::kObjectType);
    currency_config_value.SetObject();

    auto& alloc(doc.GetAllocator());
    Value currencies_array(rapidjson::kArrayType);
    for (const auto& currency : cfg.currencies()) {
        Value currency_value;
        currency_serialiser::serialise(currency, currency_value, alloc);
        currencies_array.PushBack(currency_value, alloc);
    }

    currency_config_value.AddMember("Currencies", currencies_array, alloc);
    doc.AddMember("CurrencyConfig", currency_config_value, alloc);

    StringBuffer buffer;
    Writer<StringBuffer> writer(buffer);
    doc.Accept(writer);
    std::string r(buffer.GetString(), buffer.GetSize());

    return r;
}

types::currency_config
currency_config_serialiser::deserialise(const std::string& s) {
    rapidjson::Document d;
    d.Parse(s.c_str());

    if (!d.HasMember("CurrencyConfig")) {
        BOOST_LOG_SEV(lg, error) << missing_currency_config;
        BOOST_THROW_EXCEPTION(parsing_error(missing_currency_config));
    }

    const auto& currencyConfigValue(d["CurrencyConfig"]);
    if (!currencyConfigValue.HasMember("Currencies")) {
        BOOST_LOG_SEV(lg, error) << missing_currency_config;
        BOOST_THROW_EXCEPTION(parsing_error(missing_currency_config));
    }

    std::vector<types::currency> currencies;
    const auto& currencies_array(currencyConfigValue["Currencies"].GetArray());
    for (const auto& currency_value : currencies_array) {
        currencies.push_back(currency_serialiser::deserialise(currency_value));
    }

    types::currency_config r;
    r.currencies(currencies);

    BOOST_LOG_SEV(lg, debug) << "Finished deserialising JSON. Total currencies found: "
                             << currencies.size();
    return r;
}

}
