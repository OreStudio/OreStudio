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
#ifndef ORES_RISK_OREXML_CURRENCY_CONFIG_HPP
#define ORES_RISK_OREXML_CURRENCY_CONFIG_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <vector>
#include <ostream>
#include "ores.risk/orexml/CurrencyElement.hpp"

namespace ores::risk::orexml {

/**
 * @brief Represents a set of currencies in ORE XML format.
 */
struct CurrencyConfig {
    std::vector<CurrencyElement> Currency;

    static std::string to_xml(const CurrencyConfig& v);
    static CurrencyConfig from_xml(const std::string& xml);
};

std::ostream& operator<<(std::ostream& s, const CurrencyConfig& v);

}

#endif
