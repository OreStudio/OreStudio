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
#ifndef ORES_IAM_CORE_REPOSITORY_AGENT_HELD_WRITE_ENTITY_HPP
#define ORES_IAM_CORE_REPOSITORY_AGENT_HELD_WRITE_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include "ores.iam.core/export.hpp"
#include <chrono>
#include <optional>
#include <ostream>
#include <string>

namespace ores::iam::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a agent held write in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct agent_held_write_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_iam_agent_held_writes_tbl";

    std::string request_id;
    db_timestamp held_at;
    std::string tenant_id;
    std::string party_id;
    std::string grant_id;
    std::string grantor_account_id;
    std::string method;
    std::string route;
    std::string target;
    std::string content_type;
    std::string body;
    std::string state;
    std::string outcome;
    int status = 0;

    /**
     * @brief The columns of the table, named as the members, with the key.
     */
    static constexpr auto columns() {
        using e = agent_held_write_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("request_id", &e::request_id) |
               query_builder::bind("held_at", &e::held_at) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("grant_id", &e::grant_id) |
               query_builder::bind("grantor_account_id", &e::grantor_account_id) |
               query_builder::bind("method", &e::method) | query_builder::bind("route", &e::route) |
               query_builder::bind("target", &e::target) |
               query_builder::bind("content_type", &e::content_type) |
               query_builder::bind("body", &e::body) | query_builder::bind("state", &e::state) |
               query_builder::bind("outcome", &e::outcome) |
               query_builder::bind("status", &e::status);
    }
};

ORES_IAM_CORE_EXPORT std::ostream& operator<<(std::ostream& s, const agent_held_write_entity& v);

}

#endif
