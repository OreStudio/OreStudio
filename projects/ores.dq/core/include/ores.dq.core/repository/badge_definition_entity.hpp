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
#ifndef ORES_DQ_CORE_REPOSITORY_BADGE_DEFINITION_ENTITY_HPP
#define ORES_DQ_CORE_REPOSITORY_BADGE_DEFINITION_ENTITY_HPP

#include "ores.database/q/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::dq::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a badge definition in the database.
 */
struct badge_definition_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_dq_badge_definitions_tbl";

    std::string code;
    std::string tenant_id;
    int version = 0;

    std::string name;

    std::string description;
    std::string background_colour;
    std::string text_colour;
    std::string severity_code;
    std::optional<std::string> css_class;
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
        using e = badge_definition_entity;
        namespace q = ores::database::q;
        return q::columns<e>() | q::key("code", &e::code) | q::bind("tenant_id", &e::tenant_id) |
               q::bind("version", &e::version) | q::bind("name", &e::name) |
               q::bind("description", &e::description) |
               q::bind("background_colour", &e::background_colour) |
               q::bind("text_colour", &e::text_colour) |
               q::bind("severity_code", &e::severity_code) | q::bind("css_class", &e::css_class) |
               q::bind("display_order", &e::display_order) |
               q::bind("modified_by", &e::modified_by) | q::bind("performed_by", &e::performed_by) |
               q::bind("change_reason_code", &e::change_reason_code) |
               q::bind("change_commentary", &e::change_commentary) |
               q::bind("valid_from", &e::valid_from) | q::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const badge_definition_entity& v);

}

#endif
