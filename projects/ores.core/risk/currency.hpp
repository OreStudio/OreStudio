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
#ifndef ORES_CORE_RISK_CURRENCY_HPP
#define ORES_CORE_RISK_CURRENCY_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <string>

namespace ores::core::risk {

/**
 * @brief Represents an ORE currency.
 */
struct currency {
    std::string name;
    std::string iso_code;
    int numeric_code;
    std::string symbol;
    std::string fraction_symbol;
    int fractions_per_unit;
    std::string rounding_type;
    int rounding_precision;
    std::string format;
    std::string currency_type;
    std::string modified_by;
    std::string valid_from;
    std::string valid_to;
};

std::ostream& operator<<(std::ostream& s, const currency& v);

}

#endif
