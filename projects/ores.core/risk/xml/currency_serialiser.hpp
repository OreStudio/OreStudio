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
#ifndef ORES_CORE_RISK_XML_CURRENCY_SERIALISER_HPP
#define ORES_CORE_RISK_XML_CURRENCY_SERIALISER_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <rapidxml-ns/rapidxml_ns.hpp>
#include "ores.core/risk/types/currency.hpp"

namespace ores::core::risk::xml {

class currency_serialiser {
public:
    void serialise(rapidxml_ns::xml_node<>& parent, const types::currency& cfg);
    types::currency deserialise(rapidxml_ns::xml_node<>& node);
};

}

#endif
