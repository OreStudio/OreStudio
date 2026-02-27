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
#include "ores.scheduler/repository/job_definition_repository.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.scheduler/domain/job_definition_json_io.hpp" // IWYU pragma: keep.
#include "ores.scheduler/domain/job_status.hpp"
#include "ores.scheduler/repository/job_definition_entity.hpp"
#include "ores.scheduler/repository/job_definition_mapper.hpp"

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

std::vector<domain::job_definition>
job_definition_repository::read_latest(context ctx) {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<job_definition_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        order_by("id"_c);

    return execute_read_query<job_definition_entity, domain::job_definition>(
        ctx, query,
        [](const auto& entities) { return job_definition_mapper::map(entities); },
        lg(), "Reading latest job definitions");
}

std::vector<domain::job_definition>
job_definition_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest job definition. id: " << id;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<job_definition_entity>> |
        where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<job_definition_entity, domain::job_definition>(
        ctx, query,
        [](const auto& entities) { return job_definition_mapper::map(entities); },
        lg(), "Reading latest job definition by id.");
}

std::vector<domain::job_definition>
job_definition_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all job definition versions. id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<job_definition_entity>> |
        where("tenant_id"_c == tid && "id"_c == id) |
        order_by("version"_c.desc());

    return execute_read_query<job_definition_entity, domain::job_definition>(
        ctx, query,
        [](const auto& entities) { return job_definition_mapper::map(entities); },
        lg(), "Reading all job definition versions by id.");
}

void job_definition_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing job definition: " << id;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
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

void job_definition_repository::update_cron_job_id(
    context ctx, const boost::uuids::uuid& id, std::int64_t cron_job_id) {
    BOOST_LOG_SEV(lg(), debug) << "Updating cron_job_id for: " << id
                               << " -> " << cron_job_id;
    const auto tid = ctx.tenant_id().to_string();
    const auto id_str = boost::uuids::to_string(id);
    const auto cron_str = std::to_string(cron_job_id);

    const std::string sql =
        "UPDATE ores_scheduler_job_definitions_tbl "
        "SET cron_job_id = $3::bigint "
        "WHERE tenant_id = $1::uuid AND id = $2::uuid "
        "AND valid_to = ores_utility_infinity_timestamp_fn()";

    execute_parameterized_command(ctx, sql, {tid, id_str, cron_str},
        lg(), "Updating cron_job_id.");
}

void job_definition_repository::clear_cron_job_id(
    context ctx, const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Clearing cron_job_id for: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto id_str = boost::uuids::to_string(id);

    const std::string sql =
        "UPDATE ores_scheduler_job_definitions_tbl "
        "SET cron_job_id = NULL, is_active = 0 "
        "WHERE tenant_id = $1::uuid AND id = $2::uuid "
        "AND valid_to = ores_utility_infinity_timestamp_fn()";

    execute_parameterized_command(ctx, sql, {tid, id_str},
        lg(), "Clearing cron_job_id.");
}

std::vector<domain::job_instance>
job_definition_repository::get_job_history(
    context ctx, const boost::uuids::uuid& id, std::size_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Getting job history for: " << id
                               << " limit: " << limit;

    // First, find the definition to get its cron_job_id.
    auto def = find_by_id(ctx, id);
    if (!def || !def->cron_job_id) {
        BOOST_LOG_SEV(lg(), debug) << "No active pg_cron job found for: " << id;
        return {};
    }
    const auto cron_job_id = *def->cron_job_id;
    const auto parent_id_str = boost::uuids::to_string(id);

    // Query cron.job_run_details in the postgres database.
    auto pg_creds = ctx.credentials();
    pg_creds.dbname = "postgres";

    const std::string sql =
        "SELECT runid::text, jobid::text, status, return_message, "
        "       start_time::text, end_time::text "
        "FROM cron.job_run_details "
        "WHERE jobid = " + std::to_string(cron_job_id) +
        " ORDER BY runid DESC LIMIT " + std::to_string(limit);

    const auto rows = execute_raw_multi_column_query(
        pg_creds, sql, lg(), "Reading pg_cron job history.");

    std::vector<domain::job_instance> result;
    result.reserve(rows.size());

    for (const auto& row : rows) {
        if (row.size() < 6) continue;

        domain::job_instance inst{
            .instance_id = 0,
            .cron_job_id = cron_job_id,
            .parent_job_id = id,
        };

        if (row[0]) inst.instance_id = std::stoll(*row[0]);

        // Status
        const auto& status_str = row[2].value_or("starting");
        if (status_str == "succeeded")
            inst.status = domain::job_status::succeeded;
        else if (status_str == "failed")
            inst.status = domain::job_status::failed;
        else
            inst.status = domain::job_status::starting;

        inst.return_message = row[3].value_or("");

        // Timestamps
        if (row[4]) {
            try {
                inst.start_time =
                    ores::platform::time::datetime::parse_time_point(*row[4]);
            } catch (...) {}
        }
        if (row[5]) {
            try {
                inst.end_time =
                    ores::platform::time::datetime::parse_time_point(*row[5]);
            } catch (...) {}
        }

        result.push_back(std::move(inst));
    }

    BOOST_LOG_SEV(lg(), debug) << "Retrieved " << result.size()
                               << " history entries for: " << id;
    return result;
}

}
