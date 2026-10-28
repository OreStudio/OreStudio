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
#include <algorithm>
#include <array>
#include <chrono>
#include <cstdint>
#include <format>
#include <map>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace ores::compute::repository {

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

domain::grid_sample compute_telemetry_repository::compute_grid_stats(
    context ctx, std::optional<boost::uuids::uuid> party, bool fleet) {
    BOOST_LOG_SEV(lg(), debug) << "Computing grid stats via SQL aggregations";
    const auto tid = ctx.tenant_id().to_string();

    static const std::string sql = "SELECT * FROM ores_compute_grid_stats_fn($1::uuid, "
                                   "nullif($2::text, '')::uuid, $3::boolean)";

    const auto rows = execute_parameterized_multi_column_query(
        ctx,
        sql,
        {tid, party ? boost::uuids::to_string(*party) : std::string{}, fleet ? "true" : "false"},
        lg(),
        "computing grid stats");

    domain::grid_sample sample;
    sample.sampled_at = std::chrono::system_clock::now();
    sample.tenant_id = ctx.tenant_id();
    sample.party_id = party;

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

namespace {

/// The regular time axis of a series: whole buckets counted from the epoch.
struct series_axis {
    long long first_s = 0;
    long long count = 0;
    long long end_s = 0;
};

series_axis make_series_axis(std::chrono::system_clock::time_point start,
                             std::chrono::system_clock::time_point end,
                             int bucket_seconds) {
    if (bucket_seconds < 1)
        throw std::invalid_argument("The bucket must be at least one second.");
    if (end <= start)
        throw std::invalid_argument("The range must end after it starts.");

    // Widen to whole buckets counted from the epoch, so two reads of the same
    // bucket width draw on the same axis.
    const long long start_s =
        std::chrono::duration_cast<std::chrono::seconds>(start.time_since_epoch()).count();
    const long long end_s =
        std::chrono::duration_cast<std::chrono::seconds>(end.time_since_epoch()).count();
    series_axis axis;
    axis.first_s = start_s - (((start_s % bucket_seconds) + bucket_seconds) % bucket_seconds);
    axis.count = (end_s - axis.first_s + bucket_seconds - 1) / bucket_seconds;
    if (axis.count > max_grid_series_buckets)
        throw std::invalid_argument(
            std::format("The range needs {} buckets, and a read returns at most {}. "
                        "Widen the bucket.",
                        axis.count,
                        max_grid_series_buckets));
    axis.end_s = axis.first_s + axis.count * bucket_seconds;
    return axis;
}

std::chrono::system_clock::time_point from_epoch_seconds(long long seconds) {
    return std::chrono::system_clock::time_point(std::chrono::seconds(seconds));
}

} // namespace

std::vector<std::optional<boost::uuids::uuid>>
compute_telemetry_repository::parties_with_work(context ctx) {
    // Each table is read as the session sees it, so the policies decide which parties
    // are listed. A party whose work is all gone is no longer listed and gets no more rows,
    // so its counts leave the sums from then on, as they would at zero. The empty party is always
    // there, because work with no party is the work a tenant had before the party scope and the
    // work a service submits.
    static const std::string sql = R"(
        SELECT party_id::text FROM (
            SELECT party_id FROM ores_compute_batches_tbl
            WHERE valid_to = ores_utility_infinity_timestamp_fn()
          UNION
            SELECT party_id FROM ores_compute_workunits_tbl
            WHERE valid_to = ores_utility_infinity_timestamp_fn()
          UNION
            SELECT party_id FROM ores_compute_results_tbl
            WHERE valid_to = ores_utility_infinity_timestamp_fn()
        ) p
        WHERE party_id IS NOT NULL
    )";
    const auto rows = execute_parameterized_multi_column_query(
        ctx, sql, {}, lg(), "listing the parties with work");
    std::vector<std::optional<boost::uuids::uuid>> parties;
    parties.emplace_back(std::nullopt);
    for (const auto& row : rows) {
        if (!row.empty() && row.front())
            parties.emplace_back(boost::lexical_cast<boost::uuids::uuid>(*row.front()));
    }
    return parties;
}

grid_series
compute_telemetry_repository::grid_series_in_range(context ctx,
                                                   std::chrono::system_clock::time_point start,
                                                   std::chrono::system_clock::time_point end,
                                                   int bucket_seconds) {
    const auto axis = make_series_axis(start, end, bucket_seconds);
    const long long count = axis.count;

    BOOST_LOG_SEV(lg(), debug) << "Reading grid samples as " << count << " bucket(s) of "
                               << bucket_seconds << "s";

    // The job counters are the session's own tenant's. The poller writes one row for
    // each party at one instant, so the rows of an instant are summed, and the session
    // reads only the parties it acts for. A session of the system tenant reads the work
    // totals of the installation instead, which sum every tenant's work and which no other
    // session reads. The newest instant of each bucket then gives the bucket's
    // figures. The host counts are the installation's and sit on the system tenant's
    // row, which every session reads, so they come from the rows that carry them. The newest sample
    // is taken with array_agg so the read runs on a plain PostgreSQL and needs no TimescaleDB
    // function. Column 0 is the bucket's index on the axis and column 1 the number of instants with
    // job counters in it.
    static const std::string sql = R"(
        WITH own_jobs AS (
            SELECT sampled_at,
                sum(results_inactive) AS results_inactive,
                sum(results_unsent) AS results_unsent,
                sum(results_in_progress) AS results_in_progress,
                sum(results_done) AS results_done,
                sum(total_workunits) AS total_workunits,
                sum(total_batches) AS total_batches,
                sum(active_batches) AS active_batches,
                sum(outcomes_success) AS outcomes_success,
                sum(outcomes_client_error) AS outcomes_client_error,
                sum(outcomes_no_reply) AS outcomes_no_reply
            FROM ores_compute_grid_samples_tbl
            WHERE tenant_id = ores_iam_current_tenant_id_fn()
              AND tenant_id <> ores_utility_system_tenant_id_fn()
              AND sampled_at >= to_timestamp($1::bigint) AND sampled_at < to_timestamp($2::bigint)
            GROUP BY sampled_at
        ), total_jobs AS (
            SELECT sampled_at,
                results_inactive::bigint, results_unsent::bigint, results_in_progress::bigint,
                results_done::bigint, total_workunits::bigint, total_batches::bigint,
                active_batches::bigint, outcomes_success::bigint,
                outcomes_client_error::bigint, outcomes_no_reply::bigint
            FROM ores_compute_work_totals_tbl
            WHERE ores_iam_current_tenant_id_fn() = ores_utility_system_tenant_id_fn()
              AND sampled_at >= to_timestamp($1::bigint) AND sampled_at < to_timestamp($2::bigint)
        ), jobs AS (
            SELECT * FROM own_jobs
            UNION ALL
            SELECT * FROM total_jobs
        ), job_buckets AS (
            SELECT floor((extract(epoch FROM sampled_at) - $1::bigint) / $3::int)::int AS b,
                count(*) AS instants,
                (array_agg(results_inactive ORDER BY sampled_at DESC))[1] AS results_inactive,
                (array_agg(results_unsent ORDER BY sampled_at DESC))[1] AS results_unsent,
                (array_agg(results_in_progress ORDER BY sampled_at DESC))[1] AS results_in_progress,
                (array_agg(results_done ORDER BY sampled_at DESC))[1] AS results_done,
                (array_agg(total_workunits ORDER BY sampled_at DESC))[1] AS total_workunits,
                (array_agg(total_batches ORDER BY sampled_at DESC))[1] AS total_batches,
                (array_agg(active_batches ORDER BY sampled_at DESC))[1] AS active_batches,
                (array_agg(outcomes_success ORDER BY sampled_at DESC))[1] AS outcomes_success,
                (array_agg(outcomes_client_error ORDER BY sampled_at DESC))[1] AS outcomes_client_error,
                (array_agg(outcomes_no_reply ORDER BY sampled_at DESC))[1] AS outcomes_no_reply
            FROM jobs
            GROUP BY 1
        ), fleet_buckets AS (
            SELECT floor((extract(epoch FROM sampled_at) - $1::bigint) / $3::int)::int AS b,
                (array_agg(total_hosts ORDER BY sampled_at DESC))[1] AS total_hosts,
                (array_agg(online_hosts ORDER BY sampled_at DESC))[1] AS online_hosts,
                (array_agg(idle_hosts ORDER BY sampled_at DESC))[1] AS idle_hosts
            FROM ores_compute_grid_samples_tbl
            WHERE tenant_id = ores_utility_system_tenant_id_fn() AND total_hosts > 0
              AND sampled_at >= to_timestamp($1::bigint) AND sampled_at < to_timestamp($2::bigint)
            GROUP BY 1
        )
        SELECT coalesce(j.b, f.b), coalesce(j.instants, 0),
            f.total_hosts, f.online_hosts, f.idle_hosts,
            j.results_inactive, j.results_unsent, j.results_in_progress, j.results_done,
            j.total_workunits, j.total_batches, j.active_batches,
            j.outcomes_success, j.outcomes_client_error, j.outcomes_no_reply
        FROM job_buckets j FULL JOIN fleet_buckets f ON f.b = j.b
    )";

    const auto rows = execute_parameterized_multi_column_query(
        ctx,
        sql,
        {std::to_string(axis.first_s), std::to_string(axis.end_s), std::to_string(bucket_seconds)},
        lg(),
        "reading grid samples as a series");

    grid_series series;
    series.start = from_epoch_seconds(axis.first_s);
    series.bucket_seconds = bucket_seconds;
    series.bucket_count = static_cast<int>(count);

    // The columns in the order the query selects them after the index.
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
        if (row.size() < 1 + columns.size() || !row[0])
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

grid_usage compute_telemetry_repository::grid_usage_by_scope(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Reading the grid usage by party";

    // The policies decide the rows: the session's own tenant, and the parties it
    // acts for. The query names no tenant, and a session of the system tenant reads
    // the system tenant's work only. A person who needs another tenant's usage enters
    // that tenant and reads it there. Each table is counted on its own and the parts
    // are summed by party, so a party that has batches and no results still appears.
    static const std::string sql = R"(
        WITH parts AS (
            SELECT coalesce(t.party_id::text, '') AS k,
                count(*) AS batches, 0::bigint AS workunits, 0::bigint AS results,
                0::bigint AS unsent, 0::bigint AS in_progress, 0::bigint AS done,
                0::bigint AS ok, 0::bigint AS failed, 0::bigint AS oldest
            FROM ores_compute_batches_tbl t
            WHERE t.valid_to = ores_utility_infinity_timestamp_fn()
            GROUP BY 1
          UNION ALL
            SELECT coalesce(t.party_id::text, ''),
                0, count(*), 0, 0, 0, 0, 0, 0, 0
            FROM ores_compute_workunits_tbl t
            WHERE t.valid_to = ores_utility_infinity_timestamp_fn()
            GROUP BY 1
          UNION ALL
            SELECT coalesce(t.party_id::text, ''),
                0, 0, count(*),
                count(*) FILTER (WHERE t.server_state = 2),
                count(*) FILTER (WHERE t.server_state = 4),
                count(*) FILTER (WHERE t.server_state = 5),
                count(*) FILTER (WHERE t.server_state = 5 AND t.outcome = 1
                    AND t.received_at > now() - interval '24 hours'),
                count(*) FILTER (WHERE t.server_state = 5 AND t.outcome IN (3, 4)
                    AND t.received_at > now() - interval '24 hours'),
                -- now() is the start of this transaction, so a version written after it has a
                -- later valid_from. The wait of such a row is zero and not negative.
                greatest(coalesce(floor(max(extract(epoch FROM now() - t.valid_from))
                    FILTER (WHERE t.server_state = 2)), 0), 0)::bigint
            FROM ores_compute_results_tbl t
            WHERE t.valid_to = ores_utility_infinity_timestamp_fn()
            GROUP BY 1
        )
        SELECT k, sum(batches)::bigint, sum(workunits)::bigint, sum(results)::bigint,
            sum(unsent)::bigint, sum(in_progress)::bigint, sum(done)::bigint,
            sum(ok)::bigint, sum(failed)::bigint, max(oldest)::bigint
        FROM parts
        GROUP BY k
        ORDER BY sum(results) DESC, k
    )";

    const auto rows =
        execute_parameterized_multi_column_query(ctx, sql, {}, lg(), "reading the grid usage");

    grid_usage usage;
    usage.group_kind = "party";
    const auto number = [](const std::optional<std::string>& cell) -> std::int64_t {
        return cell ? std::stoll(*cell) : 0;
    };
    for (const auto& row : rows) {
        if (row.size() < 10)
            continue;
        usage.keys.push_back(row[0] ? *row[0] : std::string{});
        usage.batches.push_back(number(row[1]));
        usage.workunits.push_back(number(row[2]));
        usage.results.push_back(number(row[3]));
        usage.results_unsent.push_back(number(row[4]));
        usage.results_in_progress.push_back(number(row[5]));
        usage.results_done.push_back(number(row[6]));
        usage.outcomes_success.push_back(number(row[7]));
        usage.outcomes_failed.push_back(number(row[8]));
        usage.oldest_unsent_seconds.push_back(number(row[9]));
    }
    return usage;
}

node_series
compute_telemetry_repository::node_series_in_range(context ctx,
                                                   std::chrono::system_clock::time_point start,
                                                   std::chrono::system_clock::time_point end,
                                                   int bucket_seconds) {
    const auto axis = make_series_axis(start, end, bucket_seconds);
    const long long count = axis.count;

    BOOST_LOG_SEV(lg(), debug) << "Reading node samples as " << count << " bucket(s) of "
                               << bucket_seconds << "s";

    // One row per node and bucket that holds a sample. The interval counters are
    // summed, the counters that count from the wrapper's start take the newest
    // value, and the mean duration is weighted by the tasks of each sample.
    static const std::string sql = R"(
        SELECT host_id::text,
            floor((extract(epoch FROM sampled_at) - $1::bigint) / $3::int)::int,
            (array_agg(machine ORDER BY sampled_at DESC))[1],
            count(*),
            (array_agg(tasks_completed ORDER BY sampled_at DESC))[1],
            (array_agg(tasks_failed ORDER BY sampled_at DESC))[1],
            sum(tasks_since_last),
            round(coalesce(sum(avg_task_duration_ms::numeric * tasks_since_last)
                           / nullif(sum(tasks_since_last), 0), 0))::bigint,
            max(max_task_duration_ms),
            sum(input_bytes_fetched),
            sum(output_bytes_uploaded)
        FROM ores_compute_node_samples_tbl
        WHERE sampled_at >= to_timestamp($1::bigint) AND sampled_at < to_timestamp($2::bigint)
        GROUP BY 1, 2
    )";

    const auto rows = execute_parameterized_multi_column_query(
        ctx,
        sql,
        {std::to_string(axis.first_s), std::to_string(axis.end_s), std::to_string(bucket_seconds)},
        lg(),
        "reading node samples as a series");

    // The nodes that reported, each with the machine of its newest sample that
    // names one.
    struct node_info {
        std::string machine;
        long long machine_bucket = -1;
    };
    std::map<std::string, node_info> found;
    constexpr std::size_t columns_in_row = 11;
    for (const auto& row : rows) {
        if (row.size() < columns_in_row || !row[0] || !row[1])
            continue;
        auto& node = found[*row[0]];
        const auto bucket = std::stoll(*row[1]);
        if (row[2] && !row[2]->empty() && bucket >= node.machine_bucket) {
            node.machine = *row[2];
            node.machine_bucket = bucket;
        }
    }

    // Order by machine and then host id, so the nodes of one computer sit
    // together, and give each node its row on the matrix.
    std::vector<std::pair<std::string, std::string>> ordered;
    ordered.reserve(found.size());
    for (const auto& [host_id, info] : found)
        ordered.emplace_back(info.machine, host_id);
    std::ranges::sort(ordered);

    const long long cells = static_cast<long long>(ordered.size()) * count;
    if (cells > max_node_series_cells)
        throw std::invalid_argument(
            std::format("The range needs {} cells of nodes times buckets, and a read returns at "
                        "most {}. Widen the bucket or narrow the range.",
                        cells,
                        max_node_series_cells));

    node_series series;
    series.start = from_epoch_seconds(axis.first_s);
    series.bucket_seconds = bucket_seconds;
    series.bucket_count = static_cast<int>(count);
    std::map<std::string, std::size_t> row_of;
    for (std::size_t i = 0; i < ordered.size(); ++i) {
        series.nodes.push_back({ordered[i].second, ordered[i].first});
        row_of[ordered[i].second] = i;
    }

    const auto size = static_cast<std::size_t>(cells);
    series.samples.assign(size, 0);
    series.tasks_completed.assign(size, 0);
    series.tasks_failed.assign(size, 0);
    series.tasks_since_last.assign(size, 0);
    series.avg_task_duration_ms.assign(size, 0);
    series.max_task_duration_ms.assign(size, 0);
    series.input_bytes_fetched.assign(size, 0);
    series.output_bytes_uploaded.assign(size, 0);

    auto to_int = [](const std::optional<std::string>& v) -> int {
        return v ? std::stoi(*v) : 0;
    };
    auto to_i64 = [](const std::optional<std::string>& v) -> std::int64_t {
        return v ? std::stoll(*v) : 0;
    };
    for (const auto& row : rows) {
        if (row.size() < columns_in_row || !row[0] || !row[1])
            continue;
        const auto bucket = std::stoll(*row[1]);
        if (bucket < 0 || bucket >= count)
            continue;
        const auto cell =
            row_of.at(*row[0]) * static_cast<std::size_t>(count) + static_cast<std::size_t>(bucket);
        series.samples[cell] = to_int(row[3]);
        series.tasks_completed[cell] = to_int(row[4]);
        series.tasks_failed[cell] = to_int(row[5]);
        series.tasks_since_last[cell] = to_int(row[6]);
        series.avg_task_duration_ms[cell] = to_i64(row[7]);
        series.max_task_duration_ms[cell] = to_i64(row[8]);
        series.input_bytes_fetched[cell] = to_i64(row[9]);
        series.output_bytes_uploaded[cell] = to_i64(row[10]);
    }
    return series;
}

}
