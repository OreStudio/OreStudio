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
    static constexpr std::string_view nats_subject = "compute.v1.telemetry.get_grid_stats";
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
     * @brief Tasks the node finished since its wrapper started.
     */
    int tasks_completed = 0;
    /**
     * @brief Tasks the node finished since its previous sample.
     */
    int tasks_since_last = 0;
    /**
     * @brief Mean task duration over the tasks since the last sample.
     */
    std::int64_t avg_task_duration_ms = 0;
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
 * @brief One node's report, published fire-and-forget from a wrapper.
 *
 * It carries no response, so the wrapper does not wait for one. The
 * publishing node has no session and the handler reads it without one, so
 * the message is public rather than claiming a token it never sends.
 */
struct node_sample_message {
    static constexpr std::string_view nats_subject = "compute.v1.telemetry.node_samples";
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
