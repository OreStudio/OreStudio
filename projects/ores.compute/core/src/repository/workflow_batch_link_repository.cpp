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
#include "ores.compute.core/repository/workflow_batch_link_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"

namespace ores::compute::repository {

using namespace ores::logging;
using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::database::repository;

void workflow_batch_link_repository::create(
    context ctx, const workflow_batch_link_entity& link) {

    BOOST_LOG_SEV(lg(), debug)
        << "Creating workflow batch link: " << link.batch_id.value();

    const auto r = sqlgen::session(ctx.connection_pool())
        .and_then(insert(link));
    ensure_success(r, lg());

    BOOST_LOG_SEV(lg(), debug) << "Workflow batch link created.";
}

std::vector<workflow_batch_link_entity>
workflow_batch_link_repository::find_all(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Finding all pending batch workflow links";

    const auto query = sqlgen::read<std::vector<workflow_batch_link_entity>>;

    return execute_read_query<
        workflow_batch_link_entity, workflow_batch_link_entity>(
        ctx, query,
        [](const auto& entities) { return entities; },
        lg(), "Finding all batch workflow links");
}

void workflow_batch_link_repository::remove(
    context ctx, const std::string& batch_id) {

    BOOST_LOG_SEV(lg(), debug) << "Removing workflow batch link: " << batch_id;

    const auto query = sqlgen::delete_from<workflow_batch_link_entity> |
        where("batch_id"_c == batch_id);

    execute_delete_query(ctx, query, lg(), "Removing workflow batch link.");
}

}
