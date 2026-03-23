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
#include "ores.scheduler.core/repository/job_definition_repository.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.scheduler.api/domain/job_definition_json_io.hpp" // IWYU pragma: keep.
#include "ores.scheduler.core/repository/job_definition_entity.hpp"
#include "ores.scheduler.core/repository/job_definition_mapper.hpp"

namespace ores::scheduler::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string job_definition_repository::sql() {
    return generate_create_table_sql<job_definition_entity>(lg());
}

void job_definition_repository::write(context ctx, const domain::job_definition& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing job definition: " << v.id;
    execute_write_query(ctx, job_definition_mapper::map(v),
        lg(), "Writing job definition to database.");
}

void job_definition_repository::write(
    context ctx, const std::vector<domain::job_definition>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing job definitions. Count: " << v.size();
    execute_write_query(ctx, job_definition_mapper::map(v),
        lg(), "Writing job definitions to database.");
}

namespace {

constexpr std::string_view SELECT_COLS =
    "SELECT id::text, tenant_id::text, version, party_id::text, "
    "       job_name, description, command, schedule_expression, "
    "       action_type, action_payload, is_active, "
    "       modified_by, performed_by, change_reason_code, change_commentary, "
    "       valid_from::text, valid_to::text "
    "FROM ores_scheduler_job_definitions_tbl ";

std::vector<domain::job_definition> parse_rows(
    const std::vector<std::vector<std::optional<std::string>>>& rows) {
    std::vector<domain::job_definition> result;
    result.reserve(rows.size());
    for (const auto& row : rows) {
        if (row.size() < 17) continue;
        job_definition_entity entity;
        if (row[0]) entity.id = *row[0];
        if (row[1]) entity.tenant_id = *row[1];
        if (row[2]) entity.version = std::stoi(*row[2]);
        if (row[3]) entity.party_id = *row[3];
        if (row[4]) entity.job_name = *row[4];
        if (row[5]) entity.description = *row[5];
        if (row[6]) entity.command = *row[6];
        if (row[7]) entity.schedule_expression = *row[7];
        if (row[8]) entity.action_type = *row[8];
        if (row[9]) entity.action_payload = *row[9];
        if (row[10]) entity.is_active = std::stoi(*row[10]);
        if (row[11]) entity.modified_by = *row[11];
        if (row[12]) entity.performed_by = *row[12];
        if (row[13]) entity.change_reason_code = *row[13];
        if (row[14]) entity.change_commentary = *row[14];
        if (row[15]) entity.valid_from = sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">(*row[15]);
        if (row[16]) entity.valid_to = sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">(*row[16]);
        result.push_back(job_definition_mapper::map(entity));
    }
    return result;
}

} // anonymous namespace

std::vector<domain::job_definition>
job_definition_repository::read_latest(context ctx) {
    const auto tid = ctx.tenant_id().to_string();
    const std::string sql = std::string(SELECT_COLS) +
        "WHERE tenant_id = $1::uuid "
        "  AND valid_to = ores_utility_infinity_timestamp_fn() "
        "ORDER BY id";

    const auto rows = execute_parameterized_multi_column_query(ctx, sql, {tid},
        lg(), "Reading latest job definitions.");
    return parse_rows(rows);
}

std::vector<domain::job_definition>
job_definition_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest job definition. id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const std::string sql = std::string(SELECT_COLS) +
        "WHERE tenant_id = $1::uuid "
        "  AND id = $2::uuid "
        "  AND valid_to = ores_utility_infinity_timestamp_fn()";

    const auto rows = execute_parameterized_multi_column_query(ctx, sql, {tid, id},
        lg(), "Reading latest job definition by id.");
    return parse_rows(rows);
}

std::vector<domain::job_definition>
job_definition_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all job definition versions. id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const std::string sql = std::string(SELECT_COLS) +
        "WHERE tenant_id = $1::uuid "
        "  AND id = $2::uuid "
        "ORDER BY version DESC";

    const auto rows = execute_parameterized_multi_column_query(ctx, sql, {tid, id},
        lg(), "Reading all job definition versions by id.");
    return parse_rows(rows);
}

std::vector<domain::job_definition>
job_definition_repository::read_all_active(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all active job definitions.";

    // Query all active jobs across all tenants (no tenant filter).
    const std::string sql = std::string(SELECT_COLS) +
        "WHERE is_active = 1 "
        "  AND valid_to = ores_utility_infinity_timestamp_fn() "
        "ORDER BY id";

    const auto rows = execute_raw_multi_column_query(ctx, sql, lg(),
        "Reading all active job definitions.");

    const auto result = parse_rows(rows);
    BOOST_LOG_SEV(lg(), debug) << "Retrieved " << result.size() << " active job definitions.";
    return result;
}

void job_definition_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing job definition: " << id;
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<job_definition_entity> |
        where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing job definition from database.");
}

std::optional<domain::job_definition>
job_definition_repository::find_by_id(context ctx, const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Finding job definition by UUID: " << id;
    const auto id_str = boost::uuids::to_string(id);
    auto results = read_latest(ctx, id_str);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

}
