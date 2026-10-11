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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_COMPUTE_API_MESSAGING_TELEMETRY_PROTOCOL_HPP
#define ORES_COMPUTE_API_MESSAGING_TELEMETRY_PROTOCOL_HPP

#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

namespace ores::compute::messaging {

/**
 * @brief Asks for the current grid summary.
 *
 * It carries no fields, because the caller's session decides what the
 * summary covers.
 */
struct get_grid_stats_request {
    using response_type = struct get_grid_stats_response;
    static constexpr std::string_view nats_subject = "compute.v1.ops.get_grid_stats";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

/**
 * @brief One node's contribution to the grid summary.
 */
struct node_stats_summary {
    /**
     * @brief The node the summary describes.
     */
    std::string host_id;
    /**
     * @brief The computer the node runs on, empty when the node did not say.
     */
    std::string machine = "";
    /**
     * @brief Tasks the node finished since its wrapper started.
     */
    int tasks_completed = 0;
    /**
     * @brief Failed tasks the grid summary attributes to the node.
     */
    int tasks_failed = 0;
    /**
     * @brief Tasks the node finished since its previous sample.
     */
    int tasks_since_last = 0;
    /**
     * @brief Mean task duration over the tasks since the last sample.
     */
    std::int64_t avg_task_duration_ms = 0;
    /**
     * @brief The node's slowest task in the interval the summary covers.
     */
    std::int64_t max_task_duration_ms = 0;
    /**
     * @brief Bytes the node fetched for those tasks.
     */
    std::int64_t input_bytes_fetched = 0;
    /**
     * @brief Bytes the node uploaded for those tasks.
     */
    std::int64_t output_bytes_uploaded = 0;
    /**
     * @brief Seconds since the node last heartbeated.
     */
    int seconds_since_hb = 0;
};

/**
 * @brief The grid summary, as the dashboard renders it.
 *
 * The counters mirror one stored grid sample, and the node summaries come
 * from the most recent sample of every node.
 */
struct get_grid_stats_response {
    /**
     * @brief Whether the summary was produced.
     */
    bool success = false;
    /**
     * @brief Why it was not, when it was not.
     */
    std::string message;
    /**
     * @brief Hosts registered for the tenant.
     */
    int total_hosts = 0;
    /**
     * @brief Hosts that heartbeated inside the online window.
     */
    int online_hosts = 0;
    /**
     * @brief Online hosts with nothing in flight.
     */
    int idle_hosts = 0;
    /**
     * @brief Results in server state 1.
     */
    int results_inactive = 0;
    /**
     * @brief Results in server state 2.
     */
    int results_unsent = 0;
    /**
     * @brief Results in server state 4.
     */
    int results_in_progress = 0;
    /**
     * @brief Results in server state 5.
     */
    int results_done = 0;
    /**
     * @brief Workunits the tenant holds.
     */
    int total_workunits = 0;
    /**
     * @brief Batches the tenant holds.
     */
    int total_batches = 0;
    /**
     * @brief Batches that are not closed.
     */
    int active_batches = 0;
    /**
     * @brief Results that finished with outcome 1 in the last day.
     */
    int outcomes_success = 0;
    /**
     * @brief Results that finished with outcome 3 in the last day.
     */
    int outcomes_client_error = 0;
    /**
     * @brief Results that finished with outcome 4 in the last day.
     */
    int outcomes_no_reply = 0;
    /**
     * @brief ISO-8601 timestamp of when the sample was taken.
     */
    std::string sampled_at;
    /**
     * @brief Per-node summaries from the most recent node samples.
     */
    std::vector<node_stats_summary> node_summaries;
};

/**
 * @brief Asks for the stored grid samples over a time range, as columns.
 *
 * The read serves the whole installation and needs the permission that
 * get_grid_stats needs. The range widens to whole buckets, counted from the
 * Unix epoch. A range that needs more than 5000 buckets is refused, so the
 * caller widens the bucket and never pulls raw rows.
 */
struct get_grid_series_request {
    using response_type = struct get_grid_series_response;
    static constexpr std::string_view nats_subject = "compute.v1.ops.get_grid_series";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief ISO-8601 start of the range, inclusive.
     */
    std::string start_time;
    /**
     * @brief ISO-8601 end of the range, exclusive.
     */
    std::string end_time;
    /**
     * @brief Width of a bucket in seconds.
     */
    int bucket_seconds = 60;
};

/**
 * @brief The grid samples of the range as columns on a regular time axis.
 *
 * Bucket i starts at start_time plus i times bucket_seconds, so the axis is
 * implicit and every column has bucket_count entries. A column holds the value
 * of the newest sample in the bucket. A bucket with no sample has samples[i]
 * equal to 0 and zeros in every column, so a caller must read samples before
 * it draws a value: a gap is not a measurement of zero. The outcome counts and
 * results_done are totals that only grow, so a caller draws throughput as the
 * difference between two buckets.
 */
struct get_grid_series_response {
    /**
     * @brief Whether the series was read.
     */
    bool success = false;
    /**
     * @brief Why it was not, when it was not.
     */
    std::string message;
    /**
     * @brief ISO-8601 start of the first bucket.
     */
    std::string start_time;
    /**
     * @brief Width of a bucket in seconds.
     */
    int bucket_seconds = 0;
    /**
     * @brief Number of buckets, and the length of every column.
     */
    int bucket_count = 0;
    /**
     * @brief Samples in each bucket. 0 marks a gap.
     */
    std::vector<int> samples;
    /**
     * @brief Hosts registered, per bucket.
     */
    std::vector<int> total_hosts;
    /**
     * @brief Hosts online, per bucket.
     */
    std::vector<int> online_hosts;
    /**
     * @brief Hosts online with no work, per bucket.
     */
    std::vector<int> idle_hosts;
    /**
     * @brief Results not yet sendable, per bucket.
     */
    std::vector<int> results_inactive;
    /**
     * @brief Results waiting for a node, per bucket.
     */
    std::vector<int> results_unsent;
    /**
     * @brief Results a node is running, per bucket.
     */
    std::vector<int> results_in_progress;
    /**
     * @brief Results finished, a total that only grows, per bucket.
     */
    std::vector<int> results_done;
    /**
     * @brief Work units, per bucket.
     */
    std::vector<int> total_workunits;
    /**
     * @brief Batches, per bucket.
     */
    std::vector<int> total_batches;
    /**
     * @brief Batches with work left, per bucket.
     */
    std::vector<int> active_batches;
    /**
     * @brief Results that ended in success, a total that only grows, per bucket.
     */
    std::vector<int> outcomes_success;
    /**
     * @brief Results that ended in a client error, a total that only grows, per bucket.
     */
    std::vector<int> outcomes_client_error;
    /**
     * @brief Results that ended with no reply, a total that only grows, per bucket.
     */
    std::vector<int> outcomes_no_reply;
};

/**
 * @brief One node's report, published fire-and-forget from a wrapper.
 *
 * It carries no response, so the wrapper does not wait for one. The
 * publishing node has no session and the handler reads it without one, so
 * the message is public rather than claiming a token it never sends.
 */
struct node_sample_message {
    static constexpr std::string_view nats_subject = "compute.v1.ops.node_sample";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = false;
    /**
     * @brief The tenant the node works for.
     */
    std::string tenant_id;
    /**
     * @brief The reporting node.
     */
    std::string host_id;
    /**
     * @brief The computer the node runs on, empty when the node did not say.
     */
    std::string machine = "";
    /**
     * @brief ISO-8601 timestamp of the sample.
     */
    std::string sampled_at;
    /**
     * @brief Tasks finished since the wrapper started.
     */
    int tasks_completed = 0;
    /**
     * @brief Tasks that failed since the wrapper started.
     */
    int tasks_failed = 0;
    /**
     * @brief Tasks finished since the previous sample.
     */
    int tasks_since_last = 0;
    /**
     * @brief Mean task duration over the tasks since the last sample.
     */
    std::int64_t avg_task_duration_ms = 0;
    /**
     * @brief Longest task duration over the tasks since the last sample.
     */
    std::int64_t max_task_duration_ms = 0;
    /**
     * @brief Bytes fetched for those tasks.
     */
    std::int64_t input_bytes_fetched = 0;
    /**
     * @brief Bytes uploaded for those tasks.
     */
    std::int64_t output_bytes_uploaded = 0;
    /**
     * @brief Seconds since this node last heartbeated.
     */
    int seconds_since_hb = 0;
};

}

#endif
