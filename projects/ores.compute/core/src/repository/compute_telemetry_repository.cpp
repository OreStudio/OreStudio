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
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <array>
#include <chrono>
#include <format>
#include <sqlgen/postgres.hpp>
#include <stdexcept>

namespace ores::compute::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;
using ores::platform::time::datetime;

std::vector<domain::node_sample> compute_telemetry_repository::latest_node_samples(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest compute node samples";

    // DISTINCT ON (host_id) returns exactly one row per host, the most recent,
    // in a single query. The row-level-security policy decides which rows the
    // session sees: its own tenant's and the system tenant's.
    const std::string sql = R"(
        SELECT DISTINCT ON (host_id)
            id, sampled_at, tenant_id, host_id, machine,
            tasks_completed, tasks_failed, tasks_since_last,
            avg_task_duration_ms, max_task_duration_ms,
            input_bytes_fetched, output_bytes_uploaded,
            seconds_since_hb
        FROM ores_compute_node_samples_tbl
        ORDER BY host_id, sampled_at DESC
    )";

    const auto rows =
        execute_parameterized_multi_column_query(ctx, sql, {}, lg(), "reading latest node samples");

    std::vector<domain::node_sample> result;
    result.reserve(rows.size());
    for (const auto& row : rows) {
        if (row.size() < 13)
            continue;
        domain::node_sample s;
        s.id = boost::lexical_cast<boost::uuids::uuid>(row[0].value_or(""));
        s.sampled_at = timestamp_to_timepoint(std::string_view{row[1].value_or("")});
        s.tenant_id = utility::uuid::tenant_id::from_string(row[2].value_or("")).value();
        s.host_id = boost::lexical_cast<boost::uuids::uuid>(row[3].value_or(""));
        s.machine = row[4].value_or("");
        s.tasks_completed = row[5] ? std::stoi(*row[5]) : 0;
        s.tasks_failed = row[6] ? std::stoi(*row[6]) : 0;
        s.tasks_since_last = row[7] ? std::stoi(*row[7]) : 0;
        s.avg_task_duration_ms = row[8] ? std::stoll(*row[8]) : 0;
        s.max_task_duration_ms = row[9] ? std::stoll(*row[9]) : 0;
        s.input_bytes_fetched = row[10] ? std::stoll(*row[10]) : 0;
        s.output_bytes_uploaded = row[11] ? std::stoll(*row[11]) : 0;
        s.seconds_since_hb = row[12] ? std::stoi(*row[12]) : 0;
        result.push_back(std::move(s));
    }
    return result;
}

domain::grid_sample compute_telemetry_repository::compute_grid_stats(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Computing grid stats via SQL aggregations";
    const auto tid = ctx.tenant_id().to_string();

    static const std::string sql = "SELECT * FROM ores_compute_grid_stats_fn($1::uuid)";

    const auto rows =
        execute_parameterized_multi_column_query(ctx, sql, {tid}, lg(), "computing grid stats");

    domain::grid_sample sample;
    sample.sampled_at = std::chrono::system_clock::now();

    if (!rows.empty() && rows.front().size() >= 13) {
        const auto& row = rows.front();
        auto to_int = [](const std::optional<std::string>& v) -> int {
            return v ? std::stoi(*v) : 0;
        };
        sample.total_hosts = to_int(row[0]);
        sample.online_hosts = to_int(row[1]);
        sample.idle_hosts = to_int(row[2]);
        sample.results_inactive = to_int(row[3]);
        sample.results_unsent = to_int(row[4]);
        sample.results_in_progress = to_int(row[5]);
        sample.results_done = to_int(row[6]);
        sample.outcomes_success = to_int(row[7]);
        sample.outcomes_client_error = to_int(row[8]);
        sample.outcomes_no_reply = to_int(row[9]);
        sample.total_workunits = to_int(row[10]);
        sample.total_batches = to_int(row[11]);
        sample.active_batches = to_int(row[12]);
    }

    return sample;
}

grid_series
compute_telemetry_repository::grid_series_in_range(context ctx,
                                                   std::chrono::system_clock::time_point start,
                                                   std::chrono::system_clock::time_point end,
                                                   int bucket_seconds) {
    if (bucket_seconds < 1)
        throw std::invalid_argument("The bucket must be at least one second.");
    if (end <= start)
        throw std::invalid_argument("The range must end after it starts.");

    // Widen to whole buckets counted from the epoch, so two reads of the same
    // bucket width draw on the same axis.
    const auto start_s =
        std::chrono::duration_cast<std::chrono::seconds>(start.time_since_epoch()).count();
    const auto end_s =
        std::chrono::duration_cast<std::chrono::seconds>(end.time_since_epoch()).count();
    const long long first_s =
        start_s - (((start_s % bucket_seconds) + bucket_seconds) % bucket_seconds);
    const long long count = (end_s - first_s + bucket_seconds - 1) / bucket_seconds;
    if (count > max_grid_series_buckets)
        throw std::invalid_argument(
            std::format("The range needs {} buckets, and a read returns at most {}. "
                        "Widen the bucket.",
                        count,
                        max_grid_series_buckets));

    BOOST_LOG_SEV(lg(), debug) << "Reading grid samples as " << count << " bucket(s) of "
                               << bucket_seconds << "s";

    // The newest sample of each bucket, taken with array_agg so the read runs
    // on a plain PostgreSQL and needs no TimescaleDB function. Column 0 is the
    // bucket's index on the axis and column 1 the number of samples in it.
    static const std::string sql = R"(
        SELECT floor((extract(epoch FROM sampled_at) - $1::bigint) / $3::int)::int,
            count(*),
            (array_agg(total_hosts ORDER BY sampled_at DESC))[1],
            (array_agg(online_hosts ORDER BY sampled_at DESC))[1],
            (array_agg(idle_hosts ORDER BY sampled_at DESC))[1],
            (array_agg(results_inactive ORDER BY sampled_at DESC))[1],
            (array_agg(results_unsent ORDER BY sampled_at DESC))[1],
            (array_agg(results_in_progress ORDER BY sampled_at DESC))[1],
            (array_agg(results_done ORDER BY sampled_at DESC))[1],
            (array_agg(total_workunits ORDER BY sampled_at DESC))[1],
            (array_agg(total_batches ORDER BY sampled_at DESC))[1],
            (array_agg(active_batches ORDER BY sampled_at DESC))[1],
            (array_agg(outcomes_success ORDER BY sampled_at DESC))[1],
            (array_agg(outcomes_client_error ORDER BY sampled_at DESC))[1],
            (array_agg(outcomes_no_reply ORDER BY sampled_at DESC))[1]
        FROM ores_compute_grid_samples_tbl
        WHERE sampled_at >= to_timestamp($1::bigint) AND sampled_at < to_timestamp($2::bigint)
        GROUP BY 1
    )";

    const long long end_axis_s = first_s + count * bucket_seconds;
    const auto rows = execute_parameterized_multi_column_query(
        ctx,
        sql,
        {std::to_string(first_s), std::to_string(end_axis_s), std::to_string(bucket_seconds)},
        lg(),
        "reading grid samples as a series");

    grid_series series;
    series.start = std::chrono::system_clock::time_point(std::chrono::seconds(first_s));
    series.bucket_seconds = bucket_seconds;
    series.bucket_count = static_cast<int>(count);

    // The columns in the order the query selects them after the index and the count.
    const std::array<std::vector<int>*, 14> columns{&series.samples,
                                                    &series.total_hosts,
                                                    &series.online_hosts,
                                                    &series.idle_hosts,
                                                    &series.results_inactive,
                                                    &series.results_unsent,
                                                    &series.results_in_progress,
                                                    &series.results_done,
                                                    &series.total_workunits,
                                                    &series.total_batches,
                                                    &series.active_batches,
                                                    &series.outcomes_success,
                                                    &series.outcomes_client_error,
                                                    &series.outcomes_no_reply};
    for (auto* column : columns)
        column->assign(static_cast<std::size_t>(count), 0);

    for (const auto& row : rows) {
        if (row.size() < 2 + columns.size() - 1 || !row[0])
            continue;
        const auto index = std::stoll(*row[0]);
        if (index < 0 || index >= count)
            continue;
        for (std::size_t c = 0; c < columns.size(); ++c)
            (*columns[c])[static_cast<std::size_t>(index)] =
                row[1 + c] ? std::stoi(*row[1 + c]) : 0;
    }
    return series;
}

}
