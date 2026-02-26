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

#include <format>
#include <stdexcept>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/mapper_helpers.hpp"

namespace ores::scheduler::repository {

using namespace ores::logging;
using namespace ores::database::repository;

namespace {

// Column indices for ores_scheduler_job_definitions_tbl SELECT results.
// Order matches: id, tenant_id, version, party_id, cron_job_id, job_name,
//                description, command, schedule_expression, database_name,
//                is_active, modified_by
constexpr std::size_t col_id               = 0;
constexpr std::size_t col_tenant_id        = 1;
constexpr std::size_t col_version          = 2;
constexpr std::size_t col_party_id         = 3;
constexpr std::size_t col_cron_job_id      = 4;
constexpr std::size_t col_job_name         = 5;
constexpr std::size_t col_description      = 6;
constexpr std::size_t col_command          = 7;
constexpr std::size_t col_schedule_expr    = 8;
constexpr std::size_t col_database_name    = 9;
constexpr std::size_t col_is_active        = 10;
constexpr std::size_t col_modified_by      = 11;
constexpr std::size_t expected_columns     = 12;

// Column indices for cron.job_run_details JOIN query results.
// Order matches: runid, jobid, status, return_message, start_time, end_time,
//                job_definition_id (our id)
constexpr std::size_t ri_run_id            = 0;
constexpr std::size_t ri_cron_job_id       = 1;
constexpr std::size_t ri_status           = 2;
constexpr std::size_t ri_return_message   = 3;
constexpr std::size_t ri_start_time       = 4;
constexpr std::size_t ri_end_time         = 5;
constexpr std::size_t ri_parent_job_id    = 6;
constexpr std::size_t ri_expected_cols    = 7;

std::optional<domain::job_definition> row_to_job_definition(
    const std::vector<std::optional<std::string>>& row) {
    auto sched = domain::cron_expression::from_string(*row[col_schedule_expr]);
    if (!sched) return std::nullopt;

    const auto tid = utility::uuid::tenant_id::from_string(*row[col_tenant_id]);
    return domain::job_definition{
        .id = boost::lexical_cast<boost::uuids::uuid>(*row[col_id]),
        .tenant_id = tid ? *tid : utility::uuid::tenant_id::system(),
        .party_id = boost::lexical_cast<boost::uuids::uuid>(*row[col_party_id]),
        .cron_job_id = row[col_cron_job_id]
            ? std::optional(std::stoll(*row[col_cron_job_id]))
            : std::nullopt,
        .job_name = *row[col_job_name],
        .description = row[col_description].value_or(""),
        .command = *row[col_command],
        .schedule_expression = std::move(*sched),
        .database_name = *row[col_database_name],
        .is_active = (std::stoi(*row[col_is_active]) != 0),
        .version = std::stoi(*row[col_version]),
        .modified_by = *row[col_modified_by],
    };
}

domain::job_status parse_status(const std::string& s) {
    if (s == "succeeded") return domain::job_status::succeeded;
    if (s == "failed")    return domain::job_status::failed;
    return domain::job_status::starting;
}

} // anonymous namespace

job_definition_repository::job_definition_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void job_definition_repository::save(
    const domain::job_definition& def,
    const std::string& change_reason_code,
    const std::string& change_commentary) {
    BOOST_LOG_SEV(lg(), debug) << "Saving job definition: " << def.job_name;

    // NULLIF($4, '')::bigint maps an empty string to SQL NULL so that an
    // absent cron_job_id (std::nullopt) does not cause a type-cast error.
    const std::string sql = R"(
        INSERT INTO ores_scheduler_job_definitions_tbl
            (id, tenant_id, version, party_id, cron_job_id,
             job_name, description, command, schedule_expression,
             database_name, is_active, modified_by,
             performed_by, change_reason_code, change_commentary,
             valid_from, valid_to)
        VALUES
            ($1::uuid, $2::uuid, 0, $3::uuid, NULLIF($4, '')::bigint,
             $5, $6, $7, $8,
             $9, $10, $11,
             current_user, $12, $13,
             current_timestamp, 'infinity'::timestamptz)
    )";

    const std::string cron_job_id_str =
        def.cron_job_id ? std::to_string(*def.cron_job_id) : "";

    std::vector<std::string> params{
        boost::uuids::to_string(def.id),
        def.tenant_id.to_string(),
        boost::uuids::to_string(def.party_id),
        cron_job_id_str,
        def.job_name,
        def.description,
        def.command,
        def.schedule_expression.to_string(),
        def.database_name,
        def.is_active ? "1" : "0",
        def.modified_by,
        change_reason_code,
        change_commentary
    };

    execute_parameterized_command(ctx_, sql, params, lg(),
        "saving job definition");
}

std::optional<domain::job_definition>
job_definition_repository::find_by_id(const boost::uuids::uuid& id) const {
    const std::string id_str = boost::uuids::to_string(id);
    const std::string sql = std::format(R"(
        SELECT id, tenant_id, version, party_id, cron_job_id,
               job_name, description, command, schedule_expression,
               database_name, is_active, modified_by
        FROM ores_scheduler_job_definitions_tbl
        WHERE id = '{}'::uuid
          AND valid_to = ores_utility_infinity_timestamp_fn()
    )", id_str);

    const auto rows = execute_raw_multi_column_query(ctx_, sql, lg(),
        "finding job definition by id");

    if (rows.empty()) return std::nullopt;
    const auto& row = rows.front();
    if (row.size() < expected_columns) return std::nullopt;

    return row_to_job_definition(row);
}

std::optional<domain::job_definition>
job_definition_repository::find_by_cron_job_id(std::int64_t cron_job_id) const {
    const std::string sql = std::format(R"(
        SELECT id, tenant_id, version, party_id, cron_job_id,
               job_name, description, command, schedule_expression,
               database_name, is_active, modified_by
        FROM ores_scheduler_job_definitions_tbl
        WHERE cron_job_id = {}
          AND valid_to = ores_utility_infinity_timestamp_fn()
    )", cron_job_id);

    const auto rows = execute_raw_multi_column_query(ctx_, sql, lg(),
        "finding job definition by cron_job_id");

    if (rows.empty()) return std::nullopt;
    const auto& row = rows.front();
    if (row.size() < expected_columns) return std::nullopt;

    return row_to_job_definition(row);
}

std::vector<domain::job_definition>
job_definition_repository::get_all() const {
    constexpr std::string_view sql = R"(
        SELECT id, tenant_id, version, party_id, cron_job_id,
               job_name, description, command, schedule_expression,
               database_name, is_active, modified_by
        FROM ores_scheduler_job_definitions_tbl
        WHERE valid_to = ores_utility_infinity_timestamp_fn()
        ORDER BY job_name
    )";

    const auto rows = execute_raw_multi_column_query(ctx_, std::string(sql), lg(),
        "getting all job definitions");

    std::vector<domain::job_definition> result;
    result.reserve(rows.size());

    for (const auto& row : rows) {
        if (row.size() < expected_columns) continue;
        if (!row[col_id] || !row[col_tenant_id] || !row[col_version] ||
            !row[col_party_id] || !row[col_job_name] || !row[col_command] ||
            !row[col_schedule_expr] || !row[col_database_name] ||
            !row[col_is_active] || !row[col_modified_by])
            continue;
        if (auto def = row_to_job_definition(row)) result.push_back(std::move(*def));
    }

    return result;
}

void job_definition_repository::update_cron_job_id(
    const boost::uuids::uuid& id,
    std::int64_t cron_job_id,
    const std::string& modified_by) {
    BOOST_LOG_SEV(lg(), debug) << "Updating cron_job_id for: "
                               << id << " -> " << cron_job_id;

    const std::string sql = R"(
        INSERT INTO ores_scheduler_job_definitions_tbl
            SELECT gen_random_uuid(), tenant_id, version, party_id, $1::bigint,
                   job_name, description, command, schedule_expression,
                   database_name, is_active, $2,
                   current_user, 'updated', '',
                   current_timestamp, 'infinity'::timestamptz
            FROM ores_scheduler_job_definitions_tbl
            WHERE id = $3::uuid
              AND valid_to = ores_utility_infinity_timestamp_fn()
    )";

    execute_parameterized_command(ctx_, sql,
        {std::to_string(cron_job_id), modified_by,
         boost::uuids::to_string(id)},
        lg(), "updating cron_job_id");
}

void job_definition_repository::clear_cron_job_id(
    const boost::uuids::uuid& id,
    const std::string& modified_by) {
    BOOST_LOG_SEV(lg(), debug) << "Clearing cron_job_id for: " << id;

    const std::string sql = R"(
        INSERT INTO ores_scheduler_job_definitions_tbl
            SELECT gen_random_uuid(), tenant_id, version, party_id, NULL,
                   job_name, description, command, schedule_expression,
                   database_name, 0, $1,
                   current_user, 'paused', '',
                   current_timestamp, 'infinity'::timestamptz
            FROM ores_scheduler_job_definitions_tbl
            WHERE id = $2::uuid
              AND valid_to = ores_utility_infinity_timestamp_fn()
    )";

    execute_parameterized_command(ctx_, sql,
        {modified_by, boost::uuids::to_string(id)},
        lg(), "clearing cron_job_id");
}

std::vector<domain::job_instance>
job_definition_repository::get_job_history(
    const boost::uuids::uuid& job_definition_id,
    std::size_t limit) const {
    BOOST_LOG_SEV(lg(), debug) << "Getting job history for: " << job_definition_id;

    const std::string id_str = boost::uuids::to_string(job_definition_id);
    const std::string sql = std::format(R"(
        SELECT r.runid, r.jobid, r.status, r.return_message,
               r.start_time, r.end_time, d.id
        FROM cron.job_run_details r
        JOIN ores_scheduler_job_definitions_tbl d
          ON d.cron_job_id = r.jobid
         AND d.valid_to = ores_utility_infinity_timestamp_fn()
        WHERE d.id = '{}'::uuid
        ORDER BY r.start_time DESC
        LIMIT {}
    )", id_str, limit);

    const auto rows = execute_raw_multi_column_query(ctx_, sql, lg(),
        "getting job history");

    std::vector<domain::job_instance> result;
    result.reserve(rows.size());

    for (const auto& row : rows) {
        if (row.size() < ri_expected_cols) continue;
        if (!row[ri_run_id] || !row[ri_cron_job_id] || !row[ri_status] ||
            !row[ri_start_time] || !row[ri_parent_job_id])
            continue;

        domain::job_instance inst;
        inst.instance_id = std::stoll(*row[ri_run_id]);
        inst.cron_job_id = std::stoll(*row[ri_cron_job_id]);
        inst.status = parse_status(*row[ri_status]);
        inst.return_message = row[ri_return_message].value_or("");
        inst.start_time = timestamp_to_timepoint(
            std::string_view{*row[ri_start_time]});
        if (row[ri_end_time])
            inst.end_time = timestamp_to_timepoint(
                std::string_view{*row[ri_end_time]});
        inst.parent_job_id =
            boost::lexical_cast<boost::uuids::uuid>(*row[ri_parent_job_id]);
        result.push_back(std::move(inst));
    }

    return result;
}

} // namespace ores::scheduler::repository
