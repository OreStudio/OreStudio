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
#ifndef ORES_DQ_CORE_REPOSITORY_LEI_RELATIONSHIP_ENTITY_HPP
#define ORES_DQ_CORE_REPOSITORY_LEI_RELATIONSHIP_ENTITY_HPP

#include "ores.database/repository/db_types.hpp"
#include "sqlgen/PrimaryKey.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::dq::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a lei relationship in the database.
 */
struct lei_relationship_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_dq_lei_relationships_tbl";

    sqlgen::PrimaryKey<std::string> relationship_start_node_node_id;
    std::string tenant_id;
    int version = 0;
    std::string relationship_start_node_node_id_type;
    std::string relationship_end_node_node_id;
    std::string relationship_end_node_node_id_type;
    std::string relationship_relationship_type;
    std::string relationship_relationship_status;
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> relationship_period_1_start_date;
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> relationship_period_1_end_date;
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> registration_initial_registration_date;
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> registration_last_update_date;
    std::optional<std::string> registration_registration_status;
    std::optional<std::string> registration_validation_sources;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    db_timestamp valid_from = "9999-12-31 23:59:59";
    db_timestamp valid_to = "9999-12-31 23:59:59";
};

std::ostream& operator<<(std::ostream& s, const lei_relationship_entity& v);

}

#endif
