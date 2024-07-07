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
#include <rapidxml-ns/rapidxml_ns_print.hpp>
#include "ores.core/ore/xml/currency_serialiser.hpp"
#include "ores.core/ore/xml/currency_config_serialiser.hpp"

using namespace rapidxml_ns;

namespace ores::core::ore::xml {

std::string
currency_config_serialiser::serialise(const model::currency_config& cfg)
{
    xml_document<> doc;
    xml_node<>* root = doc.allocate_node(node_element, "CurrencyConfig");
    doc.append_node(root);

    currency_serialiser ser;
    for (const model::currency& ccy : cfg.currencies()) {
        ser.serialise(*root, ccy);
    }

    std::ostringstream os;
    rapidxml_ns::print(std::ostreambuf_iterator<char>(os), doc);
    return os.str();
}

model::currency_config currency_config_serialiser::deserialise(std::istream& /*is*/)
{
    model::currency_config cfg;
    return cfg;
}

model::currency_config currency_config_serialiser::deserialise(const std::string& /*s*/)
{
    model::currency_config cfg;
    return cfg;
}

}
