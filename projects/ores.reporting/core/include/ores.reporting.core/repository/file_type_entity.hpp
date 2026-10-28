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
#ifndef ORES_REPORTING_CORE_REPOSITORY_FILE_TYPE_ENTITY_HPP
#define ORES_REPORTING_CORE_REPOSITORY_FILE_TYPE_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::reporting::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a file type in the database.
 */
struct file_type_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_reporting_file_types_tbl";

    std::string code;
    std::string tenant_id;
    int version = 0;
    std::optional<std::string> analytic_type_code;
    std::string ore_parameter;
    std::string direction;
    std::string format;
    std::optional<std::string> xsd_file;
    std::string kind;
    std::optional<std::string> configuration_type_code;
    std::string name_pattern;
    bool is_default = false;
    std::optional<std::string> produced_by_file_type_code;
    int sample_count = 0;
    int sample_total = 0;
    std::string description;
    int display_order = 0;
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
        using e = file_type_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("code", &e::code) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("analytic_type_code", &e::analytic_type_code) |
               query_builder::bind("ore_parameter", &e::ore_parameter) |
               query_builder::bind("direction", &e::direction) |
               query_builder::bind("format", &e::format) |
               query_builder::bind("xsd_file", &e::xsd_file) |
               query_builder::bind("kind", &e::kind) |
               query_builder::bind("configuration_type_code", &e::configuration_type_code) |
               query_builder::bind("name_pattern", &e::name_pattern) |
               query_builder::bind("is_default", &e::is_default) |
               query_builder::bind("produced_by_file_type_code", &e::produced_by_file_type_code) |
               query_builder::bind("sample_count", &e::sample_count) |
               query_builder::bind("sample_total", &e::sample_total) |
               query_builder::bind("description", &e::description) |
               query_builder::bind("display_order", &e::display_order) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const file_type_entity& v);

}

#endif
