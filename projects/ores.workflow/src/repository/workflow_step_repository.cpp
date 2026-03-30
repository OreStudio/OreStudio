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

#include <chrono>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
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

void workflow_step_repository::create(
    context ctx, const domain::workflow_step& v) {
    BOOST_LOG_SEV(lg(), debug) << "Creating workflow step: "
                               << boost::uuids::to_string(v.id);

    const auto entity = workflow_step_mapper::to_entity(v);
    const auto r = sqlgen::session(ctx.connection_pool())
        .and_then(begin_transaction)
        .and_then(insert(entity))
        .and_then(commit);
    ensure_success(r, lg());

    BOOST_LOG_SEV(lg(), debug) << "Workflow step created.";
}

void workflow_step_repository::update_status(
    context ctx, const boost::uuids::uuid& id,
    const std::string& status,
    const std::string& response_json,
    const std::string& error) {
    BOOST_LOG_SEV(lg(), debug) << "Updating workflow step status: "
                               << boost::uuids::to_string(id)
                               << " -> " << status;

    const auto id_str = boost::uuids::to_string(id);
    const auto now = timepoint_to_timestamp(
        std::chrono::system_clock::now(), lg());
    const auto opt_response = response_json.empty()
        ? std::optional<std::string>{}
        : std::optional<std::string>(response_json);
    const auto opt_error = error.empty()
        ? std::optional<std::string>{}
        : std::optional<std::string>(error);
    using ts_t = sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">;
    const auto opt_now = std::optional<ts_t>(now);

    const auto query = sqlgen::update<workflow_step_entity>(
        "status"_c.set(status),
        "response_json"_c.set(opt_response),
        "error"_c.set(opt_error),
        "completed_at"_c.set(opt_now)
    ) | where("id"_c == id_str);

    const auto r = sqlgen::session(ctx.connection_pool())
        .and_then(begin_transaction)
        .and_then(query)
        .and_then(commit);
    ensure_success(r, lg());

    BOOST_LOG_SEV(lg(), debug) << "Workflow step status updated.";
}

}
