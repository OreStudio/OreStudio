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

#include "ores.database/repository/db_types.hpp"
#include "sqlgen/PrimaryKey.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::compute::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a node sample in the database.
 */
struct node_sample_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_compute_node_samples_tbl";

    sqlgen::PrimaryKey<std::string> id;
    sqlgen::PrimaryKey<std::string> sampled_at;
    std::string tenant_id;
    std::string host_id;
    int tasks_completed = 0;
    int tasks_failed = 0;
    int tasks_since_last = 0;
    std::int64_t avg_task_duration_ms;
    std::int64_t max_task_duration_ms;
    std::int64_t input_bytes_fetched;
    std::int64_t output_bytes_uploaded;
    int seconds_since_hb = 0;
};

std::ostream& operator<<(std::ostream& s, const node_sample_entity& v);

}

#endif
