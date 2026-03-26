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

namespace {

// Columns selected for cross-tenant raw queries (no sqlgen ORM).
constexpr std::string_view SELECT_COLS =
    "SELECT id::text, tenant_id::text, version, name, party_id::text, "
    "       description, report_type, fsm_state_id::text, schedule_expression, "
    "       concurrency_policy, scheduler_job_id::text, "
    "       modified_by, performed_by, change_reason_code, change_commentary, "
    "       valid_from::text, valid_to::text "
    "FROM ores_reporting_report_definitions_tbl ";

std::vector<domain::report_definition> parse_rows(
    const std::vector<std::vector<std::optional<std::string>>>& rows) {
    std::vector<domain::report_definition> result;
    result.reserve(rows.size());
    for (const auto& row : rows) {
        if (row.size() < 17) continue;
        report_definition_entity entity;
        if (row[0]) entity.id = *row[0];
        if (row[1]) entity.tenant_id = *row[1];
        if (row[2]) entity.version = std::stoi(*row[2]);
        if (row[3]) entity.name = *row[3];
        if (row[4]) entity.party_id = *row[4];
        if (row[5]) entity.description = *row[5];
        if (row[6]) entity.report_type = *row[6];
        entity.fsm_state_id = row[7];
        if (row[8]) entity.schedule_expression = *row[8];
        if (row[9]) entity.concurrency_policy = *row[9];
        entity.scheduler_job_id = row[10];
        if (row[11]) entity.modified_by = *row[11];
        if (row[12]) entity.performed_by = *row[12];
        if (row[13]) entity.change_reason_code = *row[13];
        if (row[14]) entity.change_commentary = *row[14];
        if (row[15]) entity.valid_from = sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">(*row[15]);
        if (row[16]) entity.valid_to = sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">(*row[16]);
        result.push_back(report_definition_mapper::map(entity));
    }
    return result;
}

} // anonymous namespace

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
report_definition_repository::read_all_unscheduled(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all unscheduled report definitions.";
    const std::string sql = std::string(SELECT_COLS) +
        "WHERE scheduler_job_id IS NULL "
        "  AND valid_to = ores_utility_infinity_timestamp_fn() "
        "ORDER BY tenant_id, id";

    const auto rows = execute_raw_multi_column_query(ctx, sql, lg(),
        "Reading all unscheduled report definitions.");
    const auto result = parse_rows(rows);
    BOOST_LOG_SEV(lg(), debug) << "Retrieved " << result.size()
        << " unscheduled report definitions.";
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
