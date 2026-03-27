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
#include "ores.reporting.core/repository/report_definition_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.reporting.api/domain/report_definition_json_io.hpp" // IWYU pragma: keep.
#include "ores.reporting.core/repository/report_definition_entity.hpp"
#include "ores.reporting.core/repository/report_definition_mapper.hpp"

namespace ores::reporting::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string report_definition_repository::sql() {
    return generate_create_table_sql<report_definition_entity>(lg());
}

void report_definition_repository::write(context ctx, const domain::report_definition& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing report definition: " << v.id;
    execute_write_query(ctx, report_definition_mapper::map(v),
        lg(), "Writing report definition to database.");
}

void report_definition_repository::write(
    context ctx, const std::vector<domain::report_definition>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing report definitions. Count: " << v.size();
    execute_write_query(ctx, report_definition_mapper::map(v),
        lg(), "Writing report definitions to database.");
}

std::vector<domain::report_definition>
report_definition_repository::read_latest(context ctx) {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<report_definition_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        order_by("id"_c);

    return execute_read_query<report_definition_entity, domain::report_definition>(
        ctx, query,
        [](const auto& entities) { return report_definition_mapper::map(entities); },
        lg(), "Reading latest report definitions");
}

std::vector<domain::report_definition>
report_definition_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest report definition. id: " << id;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<report_definition_entity>> |
        where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<report_definition_entity, domain::report_definition>(
        ctx, query,
        [](const auto& entities) { return report_definition_mapper::map(entities); },
        lg(), "Reading latest report definition by id.");
}

std::vector<domain::report_definition>
report_definition_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all report definition versions. id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<report_definition_entity>> |
        where("tenant_id"_c == tid && "id"_c == id) |
        order_by("version"_c.desc());

    return execute_read_query<report_definition_entity, domain::report_definition>(
        ctx, query,
        [](const auto& entities) { return report_definition_mapper::map(entities); },
        lg(), "Reading all report definition versions by id.");
}

std::vector<domain::report_definition>
report_definition_repository::read_latest_unscheduled(context ctx) {
    const auto tid = ctx.tenant_id().to_string();
    BOOST_LOG_SEV(lg(), debug)
        << "Reading unscheduled report definitions for tenant: " << tid;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<report_definition_entity>> |
        where("tenant_id"_c == tid &&
              "scheduler_job_id"_c.is_null() &&
              "valid_to"_c == max.value()) |
        order_by("id"_c);

    const auto result =
        execute_read_query<report_definition_entity, domain::report_definition>(
            ctx, query,
            [](const auto& entities) { return report_definition_mapper::map(entities); },
            lg(), "Reading unscheduled report definitions.");
    BOOST_LOG_SEV(lg(), debug) << "Found " << result.size()
        << " unscheduled definition(s) for tenant: " << tid;
    return result;
}

void report_definition_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing report definition: " << id;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<report_definition_entity> |
        where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing report definition from database.");
}

}
