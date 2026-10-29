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
#ifndef ORES_INBOX_CORE_REPOSITORY_DELIVERY_OUTCOME_TYPE_ENTITY_HPP
#define ORES_INBOX_CORE_REPOSITORY_DELIVERY_OUTCOME_TYPE_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include <chrono>
#include <optional>
#include <ostream>
#include <string>

namespace ores::inbox::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a delivery outcome type in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct delivery_outcome_type_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_inbox_delivery_outcome_types_tbl";

    std::string code;
    std::string description;

    /**
     * @brief The columns of the table, named as the members, with the key.
     */
    static constexpr auto columns() {
        using e = delivery_outcome_type_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("code", &e::code) |
               query_builder::bind("description", &e::description);
    }
};

std::ostream& operator<<(std::ostream& s, const delivery_outcome_type_entity& v);

}

#endif
