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
#include "ores.controller.core/repository/service_instance_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.controller.core/repository/service_instance_entity.hpp"
#include "ores.controller.core/repository/service_instance_mapper.hpp"

namespace ores::controller::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::vector<api::domain::service_instance>
service_instance_repository::read_all(context ctx,
    const std::string& service_name_filter) {

    BOOST_LOG_SEV(lg(), debug) << "Reading service instances. Filter: '"
                               << service_name_filter << "'";

    if (!service_name_filter.empty()) {
        const auto query =
            sqlgen::read<std::vector<service_instance_entity>> |
            where("service_name"_c == service_name_filter) |
            order_by("service_name"_c, "replica_index"_c);
        return execute_read_query<service_instance_entity,
            api::domain::service_instance>(
            ctx, query,
            [](const auto& e) { return service_instance_mapper::map(e); },
            lg(), "Reading service instances by service.");
    }

    const auto query = sqlgen::read<std::vector<service_instance_entity>> |
        order_by("service_name"_c, "replica_index"_c);
    return execute_read_query<service_instance_entity,
        api::domain::service_instance>(
        ctx, query,
        [](const auto& e) { return service_instance_mapper::map(e); },
        lg(), "Reading all service instances.");
}

std::optional<api::domain::service_instance>
service_instance_repository::read(context ctx,
    const std::string& service_name, int replica_index) {

    BOOST_LOG_SEV(lg(), debug) << "Reading instance: " << service_name
                               << "[" << replica_index << "]";
    const auto query =
        sqlgen::read<std::vector<service_instance_entity>> |
        where("service_name"_c == service_name &&
              "replica_index"_c == replica_index);
    const auto results =
        execute_read_query<service_instance_entity, api::domain::service_instance>(
        ctx, query,
        [](const auto& e) { return service_instance_mapper::map(e); },
        lg(), "Reading service instance.");
    if (results.empty()) return std::nullopt;
    return results.front();
}

void service_instance_repository::insert(context ctx,
    const api::domain::service_instance& v) {

    BOOST_LOG_SEV(lg(), debug) << "Inserting instance: " << v.service_name
                               << "[" << v.replica_index << "]";
    execute_write_query(ctx, service_instance_mapper::map(v), lg(),
        "Inserting service instance.");
}

void service_instance_repository::update_phase(context ctx,
    const api::domain::service_instance& v) {

    BOOST_LOG_SEV(lg(), debug) << "Updating phase for " << v.service_name
                               << "[" << v.replica_index << "] -> " << v.phase;

    const auto entity = service_instance_mapper::map(v);
    const auto query = sqlgen::update<service_instance_entity>(
        "phase"_c.set(entity.phase),
        "pid"_c.set(entity.pid),
        "started_at"_c.set(entity.started_at),
        "stopped_at"_c.set(entity.stopped_at),
        "restart_count"_c.set(entity.restart_count),
        "last_error"_c.set(entity.last_error),
        "last_log_snippet"_c.set(entity.last_log_snippet),
        "last_stderr_snippet"_c.set(entity.last_stderr_snippet)
    ) | where("id"_c == entity.id.value());

    const auto r = sqlgen::session(ctx.connection_pool())
        .and_then(query);
    ensure_success(r, lg());

    BOOST_LOG_SEV(lg(), debug) << "Phase updated.";
}

}
