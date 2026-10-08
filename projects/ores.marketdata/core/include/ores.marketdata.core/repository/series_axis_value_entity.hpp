/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_entity.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_SERIES_AXIS_VALUE_ENTITY_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_SERIES_AXIS_VALUE_ENTITY_HPP

#include "ores.database/repository/db_types.hpp"
#include "sqlgen/PrimaryKey.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::marketdata::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a series axis value in the database.
 */
struct series_axis_value_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_marketdata_series_axis_values_tbl";

    sqlgen::PrimaryKey<std::string> series_id;
    sqlgen::PrimaryKey<std::string> axis_field;
    sqlgen::PrimaryKey<std::string> value;
    std::string tenant_id;
    std::string party_id;
    int sequence = 0;
};

std::ostream& operator<<(std::ostream& s, const series_axis_value_entity& v);

}

#endif
