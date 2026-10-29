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
#ifndef ORES_IAM_CORE_REPOSITORY_SESSION_SAMPLE_ENTITY_HPP
#define ORES_IAM_CORE_REPOSITORY_SESSION_SAMPLE_ENTITY_HPP

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
 * @brief Represents a session sample in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct session_sample_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_iam_session_samples_tbl";

    boost::uuids::uuid session_id;
    db_timestamp sample_time;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    std::int64_t bytes_sent;
    std::int64_t bytes_received;
    std::int64_t latency_ms;

    /**
     * @brief The columns of the table, named as the members, with the key.
     */
    static constexpr auto columns() {
        using e = session_sample_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("session_id", &e::session_id) |
               query_builder::key("sample_time", &e::sample_time) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("bytes_sent", &e::bytes_sent) |
               query_builder::bind("bytes_received", &e::bytes_received) |
               query_builder::bind("latency_ms", &e::latency_ms);
    }
};

std::ostream& operator<<(std::ostream& s, const session_sample_entity& v);

}

#endif
