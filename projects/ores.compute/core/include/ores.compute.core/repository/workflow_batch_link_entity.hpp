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
#ifndef ORES_COMPUTE_CORE_REPOSITORY_WORKFLOW_BATCH_LINK_ENTITY_HPP
#define ORES_COMPUTE_CORE_REPOSITORY_WORKFLOW_BATCH_LINK_ENTITY_HPP

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
 * @brief Represents a workflow batch link in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct workflow_batch_link_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_compute_workflow_batch_links_tbl";

    boost::uuids::uuid batch_id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    std::string workflow_step_id;
    std::string workflow_instance_id;
    db_timestamp created_at;

    /**
     * @brief The columns of the table, named as the members, with the key.
     */
    static constexpr auto columns() {
        using e = workflow_batch_link_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("batch_id", &e::batch_id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("workflow_step_id", &e::workflow_step_id) |
               query_builder::bind("workflow_instance_id", &e::workflow_instance_id) |
               query_builder::bind("created_at", &e::created_at);
    }
};

std::ostream& operator<<(std::ostream& s, const workflow_batch_link_entity& v);

}

#endif
