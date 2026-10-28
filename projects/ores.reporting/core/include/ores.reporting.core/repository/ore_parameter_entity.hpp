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
#ifndef ORES_REPORTING_CORE_REPOSITORY_ORE_PARAMETER_ENTITY_HPP
#define ORES_REPORTING_CORE_REPOSITORY_ORE_PARAMETER_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::reporting::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a ore parameter in the database.
 */
struct ore_parameter_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_reporting_ore_parameters_tbl";

    std::string id;
    std::string tenant_id;
    int version = 0;
    std::string scope;
    std::string subtype;
    std::string name;
    std::string schema_name;
    std::string xpath;
    int position = 0;
    std::string ore_parameter_value_domain_code;
    bool is_required = false;
    std::string label;
    std::string help;
    std::string description;
    std::string default_text;
    std::string format;
    std::string introduced_in;
    std::optional<std::string> removed_in;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    db_timestamp valid_from = "9999-12-31 23:59:59";
    db_timestamp valid_to = "9999-12-31 23:59:59";

    /**
     * @brief The columns of the table, named as the members, with the key.
     */
    static constexpr auto columns() {
        using e = ore_parameter_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("scope", &e::scope) |
               query_builder::bind("subtype", &e::subtype) | query_builder::bind("name", &e::name) |
               query_builder::bind("schema_name", &e::schema_name) |
               query_builder::bind("xpath", &e::xpath) |
               query_builder::bind("position", &e::position) |
               query_builder::bind("ore_parameter_value_domain_code",
                                   &e::ore_parameter_value_domain_code) |
               query_builder::bind("is_required", &e::is_required) |
               query_builder::bind("label", &e::label) | query_builder::bind("help", &e::help) |
               query_builder::bind("description", &e::description) |
               query_builder::bind("default_text", &e::default_text) |
               query_builder::bind("format", &e::format) |
               query_builder::bind("introduced_in", &e::introduced_in) |
               query_builder::bind("removed_in", &e::removed_in) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const ore_parameter_entity& v);

}

#endif
