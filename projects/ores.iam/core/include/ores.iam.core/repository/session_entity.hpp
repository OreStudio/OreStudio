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
#ifndef ORES_IAM_CORE_REPOSITORY_SESSION_ENTITY_HPP
#define ORES_IAM_CORE_REPOSITORY_SESSION_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <ostream>
#include <string>

namespace ores::iam::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a session in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct session_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_iam_sessions_tbl";

    boost::uuids::uuid id;
    db_timestamp start_time;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    boost::uuids::uuid account_id;
    std::optional<db_timestamp> end_time;
    std::string client_ip;
    std::string client_identifier = "";
    std::uint16_t client_version_major = 0;
    std::uint16_t client_version_minor = 0;
    std::uint64_t bytes_sent = 0;
    std::uint64_t bytes_received = 0;
    std::string country_code = "";
    std::string protocol = "binary";

    /**
     * @brief The columns of the table, named as the members, with the key.
     */
    static constexpr auto columns() {
        using e = session_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::key("start_time", &e::start_time) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("account_id", &e::account_id) |
               query_builder::bind("end_time", &e::end_time) |
               query_builder::bind("client_ip", &e::client_ip) |
               query_builder::bind("client_identifier", &e::client_identifier) |
               query_builder::bind("client_version_major", &e::client_version_major) |
               query_builder::bind("client_version_minor", &e::client_version_minor) |
               query_builder::bind("bytes_sent", &e::bytes_sent) |
               query_builder::bind("bytes_received", &e::bytes_received) |
               query_builder::bind("country_code", &e::country_code) |
               query_builder::bind("protocol", &e::protocol);
    }
};

std::ostream& operator<<(std::ostream& s, const session_entity& v);

}

#endif
