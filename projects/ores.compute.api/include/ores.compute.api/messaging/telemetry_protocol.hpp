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
#ifndef ORES_COMPUTE_MESSAGING_TELEMETRY_PROTOCOL_HPP
#define ORES_COMPUTE_MESSAGING_TELEMETRY_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <vector>
#include <cstdint>

namespace ores::compute::messaging {

// =============================================================================
// Dashboard stats query (request/reply)
// =============================================================================

struct get_grid_stats_request {
    using response_type = struct get_grid_stats_response;
    static constexpr std::string_view nats_subject =
        "compute.v1.telemetry.get_grid_stats";
};

struct node_stats_summary {
    std::string host_id;
    int         tasks_completed{0};
    int         tasks_since_last{0};
    std::int64_t avg_task_duration_ms{0};
    std::int64_t input_bytes_fetched{0};
    std::int64_t output_bytes_uploaded{0};
    int         seconds_since_hb{0};
};

struct get_grid_stats_response {
    bool success{false};
    std::string message;

    // Grid-level fields (mirrors domain::grid_sample)
    int total_hosts{0};
    int online_hosts{0};
    int idle_hosts{0};

    int results_inactive{0};
    int results_unsent{0};
    int results_in_progress{0};
    int results_done{0};

    int total_workunits{0};
    int total_batches{0};
    int active_batches{0};

    int outcomes_success{0};
    int outcomes_client_error{0};
    int outcomes_no_reply{0};

    /** @brief ISO-8601 timestamp of when this sample was taken. */
    std::string sampled_at;

    /** @brief Per-node summaries from the most recent node samples. */
    std::vector<node_stats_summary> node_summaries;
};

// =============================================================================
// Per-node sample publish (fire-and-forget from wrapper → service)
// =============================================================================

struct node_sample_message {
    static constexpr std::string_view nats_subject =
        "compute.v1.telemetry.node_samples";

    std::string tenant_id;
    std::string host_id;
    std::string sampled_at;  // ISO-8601

    int tasks_completed{0};
    int tasks_failed{0};
    int tasks_since_last{0};
    std::int64_t avg_task_duration_ms{0};
    std::int64_t max_task_duration_ms{0};
    std::int64_t input_bytes_fetched{0};
    std::int64_t output_bytes_uploaded{0};
    int seconds_since_hb{0};
};

}

#endif
