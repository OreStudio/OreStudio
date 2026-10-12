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
#ifndef ORES_COMPUTE_REPOSITORY_COMPUTE_TELEMETRY_REPOSITORY_HPP
#define ORES_COMPUTE_REPOSITORY_COMPUTE_TELEMETRY_REPOSITORY_HPP

#include "ores.compute.api/domain/grid_sample.hpp"
#include "ores.compute.api/domain/node_sample.hpp"
#include "ores.compute.core/export.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <string>
#include <vector>

namespace ores::compute::repository {

/// The most buckets one grid series read returns.
inline constexpr int max_grid_series_buckets = 5000;

/**
 * @brief The work of the grid counted by group, as columns.
 *
 * Entry i of every column belongs to the group keys[i]. The group is a tenant
 * or a party, as group_kind says. A key that is empty stands for work with no
 * party.
 */
struct grid_usage {
    std::string group_kind;
    std::vector<std::string> keys;
    std::vector<std::int64_t> batches;
    std::vector<std::int64_t> workunits;
    std::vector<std::int64_t> results;
    std::vector<std::int64_t> results_unsent;
    std::vector<std::int64_t> results_in_progress;
    std::vector<std::int64_t> results_done;
    std::vector<std::int64_t> outcomes_success;
    std::vector<std::int64_t> outcomes_failed;
};

/**
 * @brief The grid samples of a range as columns on a regular time axis.
 *
 * Bucket i starts at start plus i times bucket_seconds, so every column has
 * bucket_count entries and the axis is implicit. samples says how many
 * samples each bucket held, and 0 marks a gap that the zeros in the columns do
 * not measure.
 */
struct grid_series {
    std::chrono::system_clock::time_point start;
    int bucket_seconds = 0;
    int bucket_count = 0;
    std::vector<int> samples;
    std::vector<int> total_hosts;
    std::vector<int> online_hosts;
    std::vector<int> idle_hosts;
    std::vector<int> results_inactive;
    std::vector<int> results_unsent;
    std::vector<int> results_in_progress;
    std::vector<int> results_done;
    std::vector<int> total_workunits;
    std::vector<int> total_batches;
    std::vector<int> active_batches;
    std::vector<int> outcomes_success;
    std::vector<int> outcomes_client_error;
    std::vector<int> outcomes_no_reply;
};

/// The most cells of nodes times buckets one node series read returns.
inline constexpr long long max_node_series_cells = 200000;

/// One node that reported in a node series.
struct node_series_node {
    std::string host_id;
    std::string machine;
};

/**
 * @brief The node samples of a range as columns on a regular time axis.
 *
 * The nodes are listed once, ordered by machine and then host id. Every column
 * has nodes.size() times bucket_count entries, row-major, so the cell of node i
 * in bucket j is at i times bucket_count plus j. A cell with no sample has
 * samples 0 and zeros in every column.
 */
struct node_series {
    std::chrono::system_clock::time_point start;
    int bucket_seconds = 0;
    int bucket_count = 0;
    std::vector<node_series_node> nodes;
    std::vector<int> samples;
    std::vector<int> tasks_completed;
    std::vector<int> tasks_failed;
    std::vector<int> tasks_since_last;
    std::vector<std::int64_t> avg_task_duration_ms;
    std::vector<std::int64_t> max_task_duration_ms;
    std::vector<std::int64_t> input_bytes_fetched;
    std::vector<std::int64_t> output_bytes_uploaded;
};

/**
 * @brief The telemetry reads no generated repository expresses.
 *
 * The writes and the newest grid sample come from
 * grid_sample_repository and node_sample_repository, which the entity models
 * generate. Two reads are left, and neither has a generated form:
 *
 * - The newest row per node is a DISTINCT ON (host_id). A read scoped to one
 *   host returns that node's rows, and a read of the newest returns the
 *   grid's newest, so one row per node is a third shape.
 * - The live summary is a call to ores_compute_grid_stats_fn, a SQL function
 *   the codegen has no expression for.
 * - The grid samples and the node samples over a range are grouped into
 *   buckets, which the generated paged read does not express.
 */
class ORES_COMPUTE_CORE_EXPORT compute_telemetry_repository {
private:
    inline static std::string_view logger_name =
        "ores.compute.repository.compute_telemetry_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Return the most recent sample per node the policy admits.
     *
     * Uses DISTINCT ON (host_id) to return exactly one row per host, the one
     * with the most recent sampled_at, in a single database query. The read
     * states no tenant predicate: the row-level-security policy decides,
     * admitting the session's own tenant's rows and the system tenant's.
     */
    std::vector<domain::node_sample> latest_node_samples(context ctx);

    /**
     * @brief Compute current grid statistics using SQL aggregations.
     *
     * Runs a single parameterized CTE query against the compute domain tables
     * (hosts, results, workunits, batches) to return counts and breakdowns
     * without loading full tables into memory. Sets sampled_at to now().
     */
    domain::grid_sample compute_grid_stats(context ctx);

    /**
     * @brief Return the stored grid samples of a range as columns.
     *
     * The range widens to whole buckets of bucket_seconds, counted from the
     * Unix epoch. Each column holds the newest sample of the bucket, and a
     * bucket with no sample has samples 0 and zeros in every column. The read
     * states no tenant predicate: the row-level-security policy decides, as
     * for the newest sample.
     *
     * @throws std::invalid_argument when the bucket is under a second, the
     * range is empty, or the range needs more than max_grid_series_buckets.
     */
    grid_series grid_series_in_range(context ctx,
                                     std::chrono::system_clock::time_point start,
                                     std::chrono::system_clock::time_point end,
                                     int bucket_seconds);

    /**
     * @brief Return the work of the grid counted by group.
     *
     * The read states no tenant predicate: the row-level-security policies
     * decide which batches, workunits and results the session reads. A session
     * in the system tenant reads every tenant and gets one group for each
     * tenant. Any other session gets one group for each party it may read.
     */
    grid_usage grid_usage_by_scope(context ctx);

    /**
     * @brief Return the stored node samples of a range as columns.
     *
     * The axis is the grid series' axis. The interval counters are summed over
     * a cell, the counters that count from the wrapper's start take the newest
     * value, the longest task is the maximum, and the mean duration is weighted
     * by the tasks of each sample. Only nodes with a sample in the range are
     * listed. The read states no tenant predicate: the row-level-security
     * policy decides.
     *
     * @throws std::invalid_argument for a bad bucket or range, a range that
     * needs more than max_grid_series_buckets, or one whose nodes times
     * buckets exceed max_node_series_cells.
     */
    node_series node_series_in_range(context ctx,
                                     std::chrono::system_clock::time_point start,
                                     std::chrono::system_clock::time_point end,
                                     int bucket_seconds);
};

}

#endif
