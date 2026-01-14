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
#ifndef ORES_REFDATA_REPOSITORY_COUNTRY_ENTITY_HPP
#define ORES_REFDATA_REPOSITORY_COUNTRY_ENTITY_HPP

#include <string>
#include <optional>
#include "sqlgen/Timestamp.hpp"
#include "sqlgen/PrimaryKey.hpp"

namespace ores::refdata::repository {

/**
 * @brief Represents a country in the database using ISO 3166-1 codes.
 */
struct country_entity {
    constexpr static const char* schema = "ores";
    constexpr static const char* tablename = "refdata_countries_tbl";

    sqlgen::PrimaryKey<std::string> alpha2_code;
    int version = 0;
    std::string alpha3_code;
    std::string numeric_code;
    std::string name;
    std::string official_name;
    std::optional<std::string> image_id;  // UUID stored as string, converted in mapper
    std::string modified_by;
    std::string change_reason_code;
    std::string change_commentary;
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> valid_from = "9999-12-31 23:59:59";
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> valid_to = "9999-12-31 23:59:59";
};

std::ostream& operator<<(std::ostream& s, const country_entity& v);

}

#endif
