/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_DQ_CORE_REPOSITORY_DATASET_DEPENDENCY_ENTITY_HPP
#define ORES_DQ_CORE_REPOSITORY_DATASET_DEPENDENCY_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include <string>

namespace ores::dq::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a dataset dependency in the database.
 */
struct dataset_dependency_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_dq_dataset_dependencies_tbl";

    std::string dataset_code;
    std::string tenant_id;
    std::string dependency_code;
    std::string role;
    std::string modified_by;
    std::string change_reason_code;
    std::string change_commentary;
    db_timestamp valid_from = "9999-12-31 23:59:59";
    db_timestamp valid_to = "9999-12-31 23:59:59";

    /**
     * @brief The columns of the table, named as the members, with the key.
     */
    static constexpr auto columns() {
        using e = dataset_dependency_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("dataset_code", &e::dataset_code) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("dependency_code", &e::dependency_code) |
               query_builder::bind("role", &e::role) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const dataset_dependency_entity& v);

}

#endif
