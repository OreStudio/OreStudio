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
#ifndef ORES_COMPUTE_REPOSITORY_WORKFLOW_BATCH_LINK_ENTITY_HPP
#define ORES_COMPUTE_REPOSITORY_WORKFLOW_BATCH_LINK_ENTITY_HPP

#include <string>
#include <optional>
#include <sqlgen/postgres.hpp>

namespace ores::compute::repository {

/**
 * @brief Async bridge record linking a compute batch to a workflow step.
 *
 * Written by report_submit_handler when it defers step completion. Read and
 * deleted by batch_workflow_bridge when the batch reaches a terminal state.
 */
struct workflow_batch_link_entity {
    constexpr static const char* schema    = "public";
    constexpr static const char* tablename =
        "ores_compute_workflow_batch_links_tbl";

    sqlgen::PrimaryKey<std::string> batch_id;
    std::string tenant_id;
    std::string workflow_step_id;
    std::string workflow_instance_id;
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> created_at;
};

}

#endif
