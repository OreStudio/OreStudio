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
#ifndef ORES_RISK_REPOSITORY_CURRENCY_HPP
#define ORES_RISK_REPOSITORY_CURRENCY_HPP

#include <string>
#include <optional>
#include "sqlgen/Timestamp.hpp"
#include "sqlgen/PrimaryKey.hpp"

namespace ores::risk::repository {

/**
 * @brief Represents a currency in the database.
 */
struct currency_entity {
    constexpr static const char* schema = "oresdb";
    constexpr static const char* tablename = "currencies";

    sqlgen::PrimaryKey<std::string> iso_code;
    int version = 0;
    std::string name;
    std::string numeric_code;
    std::string symbol;
    std::string fraction_symbol;
    int fractions_per_unit;
    std::string rounding_type;
    int rounding_precision;
    std::string format;
    std::string currency_type;
    std::string modified_by;
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> valid_from = "9999-12-31 23:59:59";
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> valid_to = "9999-12-31 23:59:59";
};

std::ostream& operator<<(std::ostream& s, const currency_entity& v);

}

#endif
