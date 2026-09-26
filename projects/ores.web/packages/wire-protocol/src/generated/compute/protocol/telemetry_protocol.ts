/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
 * Template: ts_protocol.ts.mustache
 * To modify, update the template and regenerate.
 */
/**
 * @brief Asks for the current grid summary.
 *
 * It carries no fields, because the caller's session decides what the
 * summary covers.
 */
export interface GetGridStatsRequest {
}

/**
 * @brief One node's contribution to the grid summary.
 */
export interface NodeStatsSummary {
    /**
     * @brief The node the summary describes.
     */
    host_id: string;
    /**
     * @brief Tasks the node finished since its wrapper started.
     */
    tasks_completed: number;
    /**
     * @brief Tasks the node finished since its previous sample.
     */
    tasks_since_last: number;
    /**
     * @brief Mean task duration over the tasks since the last sample.
     */
    avg_task_duration_ms: number;
    /**
     * @brief Bytes the node fetched for those tasks.
     */
    input_bytes_fetched: number;
    /**
     * @brief Bytes the node uploaded for those tasks.
     */
    output_bytes_uploaded: number;
    /**
     * @brief Seconds since the node last heartbeated.
     */
    seconds_since_hb: number;
}

/**
 * @brief The grid summary, as the dashboard renders it.
 *
 * The counters mirror one stored grid sample, and the node summaries come
 * from the most recent sample of every node.
 */
export interface GetGridStatsResponse {
    /**
     * @brief Whether the summary was produced.
     */
    success: boolean;
    /**
     * @brief Why it was not, when it was not.
     */
    message: string;
    /**
     * @brief Hosts registered for the tenant.
     */
    total_hosts: number;
    /**
     * @brief Hosts that heartbeated inside the online window.
     */
    online_hosts: number;
    /**
     * @brief Online hosts with nothing in flight.
     */
    idle_hosts: number;
    /**
     * @brief Results in server state 1.
     */
    results_inactive: number;
    /**
     * @brief Results in server state 2.
     */
    results_unsent: number;
    /**
     * @brief Results in server state 4.
     */
    results_in_progress: number;
    /**
     * @brief Results in server state 5.
     */
    results_done: number;
    /**
     * @brief Workunits the tenant holds.
     */
    total_workunits: number;
    /**
     * @brief Batches the tenant holds.
     */
    total_batches: number;
    /**
     * @brief Batches that are not closed.
     */
    active_batches: number;
    /**
     * @brief Results that finished with outcome 1 in the last day.
     */
    outcomes_success: number;
    /**
     * @brief Results that finished with outcome 3 in the last day.
     */
    outcomes_client_error: number;
    /**
     * @brief Results that finished with outcome 4 in the last day.
     */
    outcomes_no_reply: number;
    /**
     * @brief ISO-8601 timestamp of when the sample was taken.
     */
    sampled_at: string;
    /**
     * @brief Per-node summaries from the most recent node samples.
     */
    node_summaries: NodeStatsSummary[];
}

/**
 * @brief One node's report, published fire-and-forget from a wrapper.
 *
 * It carries no response, so the wrapper does not wait for one. The
 * publishing node has no session and the handler reads it without one, so
 * the message is public rather than claiming a token it never sends.
 */
export interface NodeSampleMessage {
    /**
     * @brief The tenant the node works for.
     */
    tenant_id: string;
    /**
     * @brief The reporting node.
     */
    host_id: string;
    /**
     * @brief ISO-8601 timestamp of the sample.
     */
    sampled_at: string;
    /**
     * @brief Tasks finished since the wrapper started.
     */
    tasks_completed: number;
    /**
     * @brief Tasks that failed since the wrapper started.
     */
    tasks_failed: number;
    /**
     * @brief Tasks finished since the previous sample.
     */
    tasks_since_last: number;
    /**
     * @brief Mean task duration over the tasks since the last sample.
     */
    avg_task_duration_ms: number;
    /**
     * @brief Longest task duration over the tasks since the last sample.
     */
    max_task_duration_ms: number;
    /**
     * @brief Bytes fetched for those tasks.
     */
    input_bytes_fetched: number;
    /**
     * @brief Bytes uploaded for those tasks.
     */
    output_bytes_uploaded: number;
    /**
     * @brief Seconds since this node last heartbeated.
     */
    seconds_since_hb: number;
}

export const subjects = {
    get_grid_stats_request: "compute.v1.telemetry.get_grid_stats",
    node_sample_message: "compute.v1.telemetry.node_samples",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    get_grid_stats_request: true,
    node_sample_message: false,
} as const;
