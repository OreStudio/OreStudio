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
#include <sstream>
#include "rapidxml-ns/rapidxml_ns.hpp"
#include <rapidxml-ns/rapidxml_ns_print.hpp>
#include "ores.utility/log/logger.hpp"
#include "ores.core/xml/parsing_error.hpp"
#include "ores.core/xml/currency_serialiser.hpp"
#include "ores.core/xml/currency_config_serialiser.hpp"

namespace {

using namespace ores::utility::log;
auto lg(logger_factory("ores.core.xml.currency_config_serialiser"));

const std::string missing_currency_config("No CurrencyConfig element found");

}

namespace ores::core::xml {

using namespace rapidxml_ns;

std::string
currency_config_serialiser::serialise(const risk::currency_config& cfg)
{
    xml_document<> doc;
    xml_node<>* root = doc.allocate_node(node_element, "CurrencyConfig");
    doc.append_node(root);

    currency_serialiser ser;
    for (const risk::currency& ccy : cfg.currencies) {
        ser.serialise(*root, ccy);
    }

    std::ostringstream os;
    rapidxml_ns::print(std::ostreambuf_iterator<char>(os), doc);
    return os.str();
}

risk::currency_config currency_config_serialiser::deserialise(std::string s)
{
    BOOST_LOG_SEV(lg, debug) << "Deserialising XML. Peek: " << s.substr(0, 50);

    std::vector<char> v(s.begin(), s.end());
    s.push_back('\0');

    xml_document<> doc;
    doc.parse<parse_full>(v.data());

    auto* currency_config_node(doc.first_node());
    if (currency_config_node == nullptr) {
        BOOST_LOG_SEV(lg, error) << missing_currency_config;
        BOOST_THROW_EXCEPTION(parsing_error(missing_currency_config));
    }

    std::vector<risk::currency> currencies;
    auto* currency_node(currency_config_node->first_node("Currency"));
    currency_serialiser ccy_ser;
    while(currency_node != nullptr) {
        currencies.push_back(ccy_ser.deserialise(*currency_node));
        currency_node = currency_node->next_sibling("Currency");
    }

    const risk::currency_config r(currencies);

    BOOST_LOG_SEV(lg, debug) << "Finished deserialising XML. Total currencies found: "
                             << currencies.size();
    return r;
}

}
