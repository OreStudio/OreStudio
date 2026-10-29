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
#ifndef ORES_IAM_CORE_REPOSITORY_AUTH_EVENT_ENTITY_HPP
#define ORES_IAM_CORE_REPOSITORY_AUTH_EVENT_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include <chrono>
#include <optional>
#include <ostream>
#include <string>

namespace ores::iam::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a auth event in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct auth_event_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_iam_auth_events_tbl";

    std::string id;
    db_timestamp event_time;
    std::string tenant_id;
    std::string account_id;
    std::string event_type;
    std::string username;
    std::string session_id;
    std::string party_id;
    std::string error_detail;

    /**
     * @brief The columns of the table, named as the members, with the key.
     */
    static constexpr auto columns() {
        using e = auth_event_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::key("event_time", &e::event_time) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("account_id", &e::account_id) |
               query_builder::bind("event_type", &e::event_type) |
               query_builder::bind("username", &e::username) |
               query_builder::bind("session_id", &e::session_id) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("error_detail", &e::error_detail);
    }
};

std::ostream& operator<<(std::ostream& s, const auth_event_entity& v);

}

#endif
