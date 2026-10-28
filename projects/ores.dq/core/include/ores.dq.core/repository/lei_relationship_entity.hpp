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
#ifndef ORES_DQ_CORE_REPOSITORY_LEI_RELATIONSHIP_ENTITY_HPP
#define ORES_DQ_CORE_REPOSITORY_LEI_RELATIONSHIP_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
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
    constexpr static const char* tablename = "ores_dq_lei_relationships_artefact_tbl";

    std::string relationship_start_node_node_id;
    std::string tenant_id;
    std::string relationship_start_node_node_id_type;
    std::string relationship_end_node_node_id;
    std::string relationship_end_node_node_id_type;
    std::string relationship_relationship_type;
    std::string relationship_relationship_status;
    std::optional<db_timestamp> relationship_period_1_start_date;
    std::optional<db_timestamp> relationship_period_1_end_date;
    std::optional<db_timestamp> registration_initial_registration_date;
    std::optional<db_timestamp> registration_last_update_date;
    std::optional<std::string> registration_registration_status;
    std::optional<std::string> registration_validation_sources;

    /**
     * @brief The columns of the table, named as the members, with the key.
     */
    static constexpr auto columns() {
        using e = lei_relationship_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() |
               query_builder::key("relationship_start_node_node_id",
                                  &e::relationship_start_node_node_id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("relationship_start_node_node_id_type",
                                   &e::relationship_start_node_node_id_type) |
               query_builder::bind("relationship_end_node_node_id",
                                   &e::relationship_end_node_node_id) |
               query_builder::bind("relationship_end_node_node_id_type",
                                   &e::relationship_end_node_node_id_type) |
               query_builder::bind("relationship_relationship_type",
                                   &e::relationship_relationship_type) |
               query_builder::bind("relationship_relationship_status",
                                   &e::relationship_relationship_status) |
               query_builder::bind("relationship_period_1_start_date",
                                   &e::relationship_period_1_start_date) |
               query_builder::bind("relationship_period_1_end_date",
                                   &e::relationship_period_1_end_date) |
               query_builder::bind("registration_initial_registration_date",
                                   &e::registration_initial_registration_date) |
               query_builder::bind("registration_last_update_date",
                                   &e::registration_last_update_date) |
               query_builder::bind("registration_registration_status",
                                   &e::registration_registration_status) |
               query_builder::bind("registration_validation_sources",
                                   &e::registration_validation_sources);
    }
};

std::ostream& operator<<(std::ostream& s, const lei_relationship_entity& v);

}

#endif
