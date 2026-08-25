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
#ifndef ORES_SYNTHETIC_CORE_REPOSITORY_YIELD_CURVE_PROCESS_PARAMETER_DEFINITION_ENTITY_HPP
#define ORES_SYNTHETIC_CORE_REPOSITORY_YIELD_CURVE_PROCESS_PARAMETER_DEFINITION_ENTITY_HPP

#include "ores.database/repository/db_types.hpp"
#include "sqlgen/PrimaryKey.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::synthetic::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a yield curve process parameter definition in the database.
 */
struct yield_curve_process_parameter_definition_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_synthetic_process_parameter_definitions_tbl";

    sqlgen::PrimaryKey<std::string> id;
    std::string tenant_id;
    int version = 0;

    std::string process_type_code;


    std::string parameter_name;

    std::string description;
    std::string data_type = "double";
    double default_value = 0.0;
    std::optional<double> min_value;
    std::optional<double> max_value;
    int display_order = 0;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    db_timestamp valid_from = "9999-12-31 23:59:59";
    db_timestamp valid_to = "9999-12-31 23:59:59";
};

std::ostream& operator<<(std::ostream& s, const yield_curve_process_parameter_definition_entity& v);

}

#endif
