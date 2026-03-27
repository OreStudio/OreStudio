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
#include "ores.compute.core/repository/compute_telemetry_repository.hpp"

#include <chrono>
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.compute.core/repository/grid_sample_entity.hpp"
#include "ores.compute.core/repository/node_sample_entity.hpp"

namespace ores::compute::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

namespace {

auto& lg() {
    static auto instance = ores::logging::make_logger(
        "ores.compute.repository.compute_telemetry_repository");
    return instance;
}

grid_sample_entity to_entity(const domain::grid_sample& s,
                              const std::string& tid) {
    grid_sample_entity e;
    e.sampled_at = timepoint_to_timestamp(s.sampled_at, lg());
    e.tenant_id           = tid;
    e.total_hosts         = s.total_hosts;
    e.online_hosts        = s.online_hosts;
    e.idle_hosts          = s.idle_hosts;
    e.results_inactive    = s.results_inactive;
    e.results_unsent      = s.results_unsent;
    e.results_in_progress = s.results_in_progress;
    e.results_done        = s.results_done;
    e.total_workunits     = s.total_workunits;
    e.total_batches       = s.total_batches;
    e.active_batches      = s.active_batches;
    e.outcomes_success      = s.outcomes_success;
    e.outcomes_client_error = s.outcomes_client_error;
    e.outcomes_no_reply     = s.outcomes_no_reply;
    return e;
}

domain::grid_sample from_entity(const grid_sample_entity& e) {
    domain::grid_sample s;
    s.sampled_at = timestamp_to_timepoint(e.sampled_at.value());
    s.tenant_id  = utility::uuid::tenant_id::from_string(e.tenant_id.value()).value();
    s.total_hosts         = e.total_hosts;
    s.online_hosts        = e.online_hosts;
    s.idle_hosts          = e.idle_hosts;
    s.results_inactive    = e.results_inactive;
    s.results_unsent      = e.results_unsent;
    s.results_in_progress = e.results_in_progress;
    s.results_done        = e.results_done;
    s.total_workunits     = e.total_workunits;
    s.total_batches       = e.total_batches;
    s.active_batches      = e.active_batches;
    s.outcomes_success      = e.outcomes_success;
    s.outcomes_client_error = e.outcomes_client_error;
    s.outcomes_no_reply     = e.outcomes_no_reply;
    return s;
}

node_sample_entity to_entity(const domain::node_sample& s,
                              const std::string& tid) {
    node_sample_entity e;
    e.sampled_at = timepoint_to_timestamp(s.sampled_at, lg());
    e.tenant_id            = tid;
    e.host_id              = boost::uuids::to_string(s.host_id);
    e.tasks_completed      = s.tasks_completed;
    e.tasks_failed         = s.tasks_failed;
    e.tasks_since_last     = s.tasks_since_last;
    e.avg_task_duration_ms = s.avg_task_duration_ms;
    e.max_task_duration_ms = s.max_task_duration_ms;
    e.input_bytes_fetched  = s.input_bytes_fetched;
    e.output_bytes_uploaded = s.output_bytes_uploaded;
    e.seconds_since_hb     = s.seconds_since_hb;
    return e;
}

domain::node_sample from_entity(const node_sample_entity& e) {
    domain::node_sample s;
    s.sampled_at           = timestamp_to_timepoint(e.sampled_at.value());
    s.tenant_id            = utility::uuid::tenant_id::from_string(e.tenant_id.value()).value();
    s.host_id              = boost::lexical_cast<boost::uuids::uuid>(e.host_id.value());
    s.tasks_completed      = e.tasks_completed;
    s.tasks_failed         = e.tasks_failed;
    s.tasks_since_last     = e.tasks_since_last;
    s.avg_task_duration_ms = e.avg_task_duration_ms;
    s.max_task_duration_ms = e.max_task_duration_ms;
    s.input_bytes_fetched  = e.input_bytes_fetched;
    s.output_bytes_uploaded = e.output_bytes_uploaded;
    s.seconds_since_hb     = e.seconds_since_hb;
    return s;
}

} // namespace

void compute_telemetry_repository::insert_grid_sample(
    context ctx, const domain::grid_sample& sample) {
    BOOST_LOG_SEV(lg(), trace) << "Inserting compute grid sample";
    const auto e = to_entity(sample, ctx.tenant_id().to_string());
    const auto r = sqlgen::session(ctx.connection_pool())
        .and_then(begin_transaction)
        .and_then(insert(e))
        .and_then(commit);
    ensure_success(r, lg());
}

void compute_telemetry_repository::insert_node_sample(
    context ctx, const domain::node_sample& sample) {
    BOOST_LOG_SEV(lg(), trace) << "Inserting compute node sample for host: "
                               << boost::uuids::to_string(sample.host_id);
    const auto e = to_entity(sample, ctx.tenant_id().to_string());
    const auto r = sqlgen::session(ctx.connection_pool())
        .and_then(begin_transaction)
        .and_then(insert(e))
        .and_then(commit);
    ensure_success(r, lg());
}

std::optional<domain::grid_sample>
compute_telemetry_repository::latest_grid_sample(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest compute grid sample";
    const auto tid = ctx.tenant_id().to_string();
    const auto qry = sqlgen::read<std::vector<grid_sample_entity>> |
        where("tenant_id"_c == tid) |
        order_by("sampled_at"_c.desc()) |
        sqlgen::limit(1);

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(qry);
    ensure_success(r, lg());

    if (r->empty())
        return std::nullopt;
    return from_entity(r->front());
}

std::vector<domain::node_sample>
compute_telemetry_repository::latest_node_samples(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest compute node samples";
    const auto tid = ctx.tenant_id().to_string();

    // DISTINCT ON (host_id) returns exactly one row per host — the most
    // recent — in a single efficient database query.
    const std::string sql = R"(
        SELECT DISTINCT ON (host_id)
            sampled_at, tenant_id, host_id,
            tasks_completed, tasks_failed, tasks_since_last,
            avg_task_duration_ms, max_task_duration_ms,
            input_bytes_fetched, output_bytes_uploaded,
            seconds_since_hb
        FROM ores_compute_node_samples_tbl
        WHERE tenant_id = $1
        ORDER BY host_id, sampled_at DESC
    )";

    const auto rows = execute_parameterized_multi_column_query(
        ctx, sql, {tid}, lg(), "reading latest node samples");

    std::vector<domain::node_sample> result;
    result.reserve(rows.size());
    for (const auto& row : rows) {
        if (row.size() < 11) continue;
        domain::node_sample s;
        s.sampled_at            = timestamp_to_timepoint(
            std::string_view{row[0].value_or("")});
        s.tenant_id             = utility::uuid::tenant_id::from_string(
            row[1].value_or("")).value();
        s.host_id               = boost::lexical_cast<boost::uuids::uuid>(
            row[2].value_or(""));
        s.tasks_completed       = row[3] ? std::stoi(*row[3]) : 0;
        s.tasks_failed          = row[4] ? std::stoi(*row[4]) : 0;
        s.tasks_since_last      = row[5] ? std::stoi(*row[5]) : 0;
        s.avg_task_duration_ms  = row[6] ? std::stoll(*row[6]) : 0;
        s.max_task_duration_ms  = row[7] ? std::stoll(*row[7]) : 0;
        s.input_bytes_fetched   = row[8] ? std::stoll(*row[8]) : 0;
        s.output_bytes_uploaded = row[9] ? std::stoll(*row[9]) : 0;
        s.seconds_since_hb      = row[10] ? std::stoi(*row[10]) : 0;
        result.push_back(std::move(s));
    }
    return result;
}

domain::grid_sample
compute_telemetry_repository::compute_grid_stats(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Computing grid stats via SQL aggregations";
    const auto tid = ctx.tenant_id().to_string();

    static const std::string sql =
        "SELECT * FROM ores_compute_grid_stats_fn($1::uuid)";

    const auto rows = execute_parameterized_multi_column_query(
        ctx, sql, {tid}, lg(), "computing grid stats");

    domain::grid_sample sample;
    sample.sampled_at = std::chrono::system_clock::now();

    if (!rows.empty() && rows.front().size() >= 13) {
        const auto& row = rows.front();
        auto to_int = [](const std::optional<std::string>& v) -> int {
            return v ? std::stoi(*v) : 0;
        };
        sample.total_hosts           = to_int(row[0]);
        sample.online_hosts          = to_int(row[1]);
        sample.idle_hosts            = to_int(row[2]);
        sample.results_inactive      = to_int(row[3]);
        sample.results_unsent        = to_int(row[4]);
        sample.results_in_progress   = to_int(row[5]);
        sample.results_done          = to_int(row[6]);
        sample.outcomes_success      = to_int(row[7]);
        sample.outcomes_client_error = to_int(row[8]);
        sample.outcomes_no_reply     = to_int(row[9]);
        sample.total_workunits       = to_int(row[10]);
        sample.total_batches         = to_int(row[11]);
        sample.active_batches        = to_int(row[12]);
    }

    return sample;
}

}
