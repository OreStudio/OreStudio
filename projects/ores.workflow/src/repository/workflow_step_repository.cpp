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
#include "ores.workflow/repository/workflow_step_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.workflow/domain/workflow_step_json_io.hpp" // IWYU pragma: keep.
#include "ores.workflow/repository/workflow_step_entity.hpp"
#include "ores.workflow/repository/workflow_step_mapper.hpp"

namespace ores::workflow::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::vector<domain::workflow_step>
workflow_step_repository::read(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Reading workflow steps.";

    const auto query = sqlgen::read<std::vector<workflow_step_entity>>;

    return execute_read_query<workflow_step_entity, domain::workflow_step>(
        ctx, query,
        [](const auto& entities) { return workflow_step_mapper::map(entities); },
        lg(), "Reading workflow steps");
}

}
