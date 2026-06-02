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
#ifndef ORES_COMPUTE_REPOSITORY_RESULT_ENTITY_HPP
#define ORES_COMPUTE_REPOSITORY_RESULT_ENTITY_HPP

#include <string>
#include <optional>
#include <ostream>
#include "ores.database/repository/db_types.hpp"
#include "sqlgen/PrimaryKey.hpp"

namespace ores::compute::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a result in the database.
 */
struct result_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_compute_results_tbl";

    sqlgen::PrimaryKey<std::string> id;
    std::string tenant_id;
    int version = 0;
    std::string workunit_id;
    std::optional<std::string> host_id;
    std::optional<std::int64_t> pgmq_msg_id;
    int server_state;
    std::optional<int> outcome;
    std::optional<std::string> output_uri;
    std::optional<std::string> error_message;
    std::optional<db_timestamp> received_at;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    std::optional<db_timestamp> valid_from = "9999-12-31 23:59:59";
    std::optional<db_timestamp> valid_to = "9999-12-31 23:59:59";
};

std::ostream& operator<<(std::ostream& s, const result_entity& v);

}

#endif
