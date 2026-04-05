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
#include "ores.scheduler.core/repository/job_instance_repository.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.platform/time/datetime.hpp"

namespace ores::scheduler::repository {

using namespace ores::logging;
using namespace ores::database::repository;

namespace {

std::string job_status_to_string(domain::job_status s) {
    switch (s) {
    case domain::job_status::starting:  return "starting";
    case domain::job_status::succeeded: return "succeeded";
    case domain::job_status::failed:    return "failed";
    }
    return "starting";
}

domain::job_status job_status_from_string(const std::string& s) {
    if (s == "succeeded") return domain::job_status::succeeded;
    if (s == "failed")    return domain::job_status::failed;
    return domain::job_status::starting;
}

} // anonymous namespace

std::int64_t job_instance_repository::write_started(
    context ctx, const domain::job_instance& inst) {
    BOOST_LOG_SEV(lg(), debug) << "Writing started job instance for: "
                               << inst.job_definition_id;

    const auto job_def_id = boost::uuids::to_string(inst.job_definition_id);
    const auto tenant_id = inst.tenant_id
        ? boost::uuids::to_string(*inst.tenant_id) : "";
    const auto party_id = inst.party_id
        ? boost::uuids::to_string(*inst.party_id) : "";
    const auto triggered_at =
        ores::platform::time::datetime::format_time_point_utc(inst.triggered_at);
    const auto started_at =
        ores::platform::time::datetime::format_time_point_utc(inst.started_at);
    const auto status = job_status_to_string(inst.status);

    // NULLIF converts an empty string to NULL so nullable uuid columns
    // don't fail the ::uuid cast when tenant_id / party_id are absent.
    const std::string insert_sql =
        "INSERT INTO ores_scheduler_job_instances_tbl "
        "  (tenant_id, party_id, job_definition_id, action_type, status, "
        "   triggered_at, started_at) "
        "VALUES "
        "  (NULLIF($1,'')::uuid, NULLIF($2,'')::uuid, $3::uuid, $4, $5, "
        "   $6::timestamptz, $7::timestamptz) "
        "RETURNING id";

    const auto rows = execute_parameterized_multi_column_query(ctx, insert_sql,
        {tenant_id, party_id, job_def_id, inst.action_type, status,
         triggered_at, started_at},
        lg(), "Inserting job instance row.");

    if (rows.empty() || rows[0].empty() || !rows[0][0])
        throw std::runtime_error("Failed to retrieve new job_instance id from database.");

    const auto new_id = std::stoll(*rows[0][0]);
    BOOST_LOG_SEV(lg(), debug) << "Created job instance id: " << new_id;
    return new_id;
}

void job_instance_repository::write_completed(
    context ctx, std::int64_t id,
    const std::chrono::system_clock::time_point& triggered_at,
    domain::job_status status, const std::string& error) {
    BOOST_LOG_SEV(lg(), debug) << "Writing completed status for job instance: " << id;

    const auto completed_at =
        ores::platform::time::datetime::format_time_point_utc(
            std::chrono::system_clock::now());
    const auto triggered_at_str =
        ores::platform::time::datetime::format_time_point_utc(triggered_at);
    const auto status_str = job_status_to_string(status);
    const auto id_str = std::to_string(id);

    const std::string update_sql =
        "UPDATE ores_scheduler_job_instances_tbl "
        "SET status = $1, "
        "    completed_at = $2::timestamptz, "
        "    duration_ms = EXTRACT(EPOCH FROM ($2::timestamptz - triggered_at)) * 1000, "
        "    error_message = $3 "
        "WHERE id = $4::bigint "
        "  AND triggered_at = $5::timestamptz";

    execute_parameterized_command(ctx, update_sql,
        {status_str, completed_at, error, id_str, triggered_at_str},
        lg(), "Updating job instance completion.");
}

std::vector<domain::job_instance> job_instance_repository::read_latest(
    context ctx, const boost::uuids::uuid& job_definition_id, std::size_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest job instances for: "
                               << job_definition_id;

    const auto job_def_id_str = boost::uuids::to_string(job_definition_id);
    const auto limit_str = std::to_string(limit);

    const std::string sql =
        "SELECT id::text, tenant_id::text, party_id::text, "
        "       job_definition_id::text, action_type, status, "
        "       triggered_at::text, started_at::text, "
        "       completed_at::text, duration_ms::text, error_message "
        "FROM ores_scheduler_job_instances_tbl "
        "WHERE job_definition_id = $1::uuid "
        "ORDER BY triggered_at DESC "
        "LIMIT $2::bigint";

    const auto rows = execute_parameterized_multi_column_query(ctx, sql,
        {job_def_id_str, limit_str},
        lg(), "Reading latest job instances.");

    std::vector<domain::job_instance> result;
    result.reserve(rows.size());

    for (const auto& row : rows) {
        if (row.size() < 11) continue;

        domain::job_instance inst;
        if (row[0]) inst.id = std::stoll(*row[0]);
        if (row[1]) inst.tenant_id = boost::lexical_cast<boost::uuids::uuid>(*row[1]);
        if (row[2]) inst.party_id = boost::lexical_cast<boost::uuids::uuid>(*row[2]);
        if (row[3])
            inst.job_definition_id = boost::lexical_cast<boost::uuids::uuid>(*row[3]);
        if (row[4]) inst.action_type = *row[4];
        if (row[5]) inst.status = job_status_from_string(*row[5]);
        if (row[6]) {
            try {
                inst.triggered_at =
                    ores::platform::time::datetime::parse_time_point(*row[6]);
            } catch (...) {}
        }
        if (row[7]) {
            try {
                inst.started_at =
                    ores::platform::time::datetime::parse_time_point(*row[7]);
            } catch (...) {}
        }
        if (row[8]) {
            try {
                inst.completed_at =
                    ores::platform::time::datetime::parse_time_point(*row[8]);
            } catch (...) {}
        }
        if (row[9]) inst.duration_ms = std::stoll(*row[9]);
        if (row[10]) inst.error_message = *row[10];

        result.push_back(std::move(inst));
    }

    BOOST_LOG_SEV(lg(), debug) << "Retrieved " << result.size()
                               << " job instances for: " << job_definition_id;
    return result;
}

std::optional<std::chrono::system_clock::time_point>
job_instance_repository::last_run_at(
    context ctx, const boost::uuids::uuid& job_definition_id) {
    BOOST_LOG_SEV(lg(), debug) << "Querying last_run_at for: " << job_definition_id;

    const auto job_def_id_str = boost::uuids::to_string(job_definition_id);

    const std::string sql =
        "SELECT triggered_at::text "
        "FROM ores_scheduler_job_instances_tbl "
        "WHERE job_definition_id = $1::uuid "
        "ORDER BY triggered_at DESC "
        "LIMIT 1";

    const auto rows = execute_parameterized_multi_column_query(ctx, sql,
        {job_def_id_str},
        lg(), "Querying last_run_at for job definition.");

    if (rows.empty() || rows[0].empty() || !rows[0][0])
        return std::nullopt;

    try {
        return ores::platform::time::datetime::parse_time_point(*rows[0][0]);
    } catch (...) {
        return std::nullopt;
    }
}

} // namespace ores::scheduler::repository
