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
#ifndef ORES_ANALYTICS_CORE_REPOSITORY_STRESS_TEST_SHIFT_ENTITY_HPP
#define ORES_ANALYTICS_CORE_REPOSITORY_STRESS_TEST_SHIFT_ENTITY_HPP

#include "ores.database/repository/db_types.hpp"
#include "sqlgen/PrimaryKey.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::analytics::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a stress test shift in the database.
 */
struct stress_test_shift_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_analytics_stress_test_shifts_tbl";

    sqlgen::PrimaryKey<std::string> id;
    std::string tenant_id;
    int version = 0;
    std::string stress_test_scenario_id;
    std::string family;
    std::string object_key;
    std::optional<std::string> shift_type;
    std::optional<std::string> shifts;
    std::optional<std::string> shift_tenors;
    std::optional<std::string> shift_expiries;
    std::optional<std::string> extras;
    int position = 0;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    db_timestamp valid_from = "9999-12-31 23:59:59";
    db_timestamp valid_to = "9999-12-31 23:59:59";
};

std::ostream& operator<<(std::ostream& s, const stress_test_shift_entity& v);

}

#endif
