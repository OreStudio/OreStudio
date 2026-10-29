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
#ifndef ORES_COMPUTE_CORE_REPOSITORY_NODE_SAMPLE_ENTITY_HPP
#define ORES_COMPUTE_CORE_REPOSITORY_NODE_SAMPLE_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <ostream>
#include <string>

namespace ores::compute::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a node sample in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct node_sample_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_compute_node_samples_tbl";

    boost::uuids::uuid id;
    db_timestamp sampled_at;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    boost::uuids::uuid host_id;
    std::string machine;
    int tasks_completed = 0;
    int tasks_failed = 0;
    int tasks_since_last = 0;
    std::int64_t avg_task_duration_ms;
    std::int64_t max_task_duration_ms;
    std::int64_t input_bytes_fetched;
    std::int64_t output_bytes_uploaded;
    int seconds_since_hb = 0;

    /**
     * @brief The columns of the table, named as the members, with the key.
     */
    static constexpr auto columns() {
        using e = node_sample_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::key("sampled_at", &e::sampled_at) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("host_id", &e::host_id) |
               query_builder::bind("machine", &e::machine) |
               query_builder::bind("tasks_completed", &e::tasks_completed) |
               query_builder::bind("tasks_failed", &e::tasks_failed) |
               query_builder::bind("tasks_since_last", &e::tasks_since_last) |
               query_builder::bind("avg_task_duration_ms", &e::avg_task_duration_ms) |
               query_builder::bind("max_task_duration_ms", &e::max_task_duration_ms) |
               query_builder::bind("input_bytes_fetched", &e::input_bytes_fetched) |
               query_builder::bind("output_bytes_uploaded", &e::output_bytes_uploaded) |
               query_builder::bind("seconds_since_hb", &e::seconds_since_hb);
    }
};

std::ostream& operator<<(std::ostream& s, const node_sample_entity& v);

}

#endif
