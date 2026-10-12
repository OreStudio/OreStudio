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
export interface GetGridStatsRequest {}

/**
 * @brief One node's contribution to the grid summary.
 */
export interface NodeStatsSummary {
    /**
     * @brief The node the summary describes.
     */
    host_id: string;
    /**
     * @brief The computer the node runs on, empty when the node did not say.
     */
    machine: string;
    /**
     * @brief Tasks the node finished since its wrapper started.
     */
    tasks_completed: number;
    /**
     * @brief Failed tasks the grid summary attributes to the node.
     */
    tasks_failed: number;
    /**
     * @brief Tasks the node finished since its previous sample.
     */
    tasks_since_last: number;
    /**
     * @brief Mean task duration over the tasks since the last sample.
     */
    avg_task_duration_ms: number;
    /**
     * @brief The node's slowest task in the interval the summary covers.
     */
    max_task_duration_ms: number;
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
 * @brief Asks for the stored grid samples over a time range, as columns.
 *
 * The read serves the whole installation and needs the permission that
 * get_grid_stats needs. The range widens to whole buckets, counted from the
 * Unix epoch. A range that needs more than 5000 buckets is refused, so the
 * caller widens the bucket and never pulls raw rows.
 */
export interface GetGridSeriesRequest {
    /**
     * @brief ISO-8601 start of the range, inclusive.
     */
    start_time: string;
    /**
     * @brief ISO-8601 end of the range, exclusive.
     */
    end_time: string;
    /**
     * @brief Width of a bucket in seconds.
     */
    bucket_seconds: number;
}

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
export interface GetGridSeriesResponse {
    /**
     * @brief Whether the series was read.
     */
    success: boolean;
    /**
     * @brief Why it was not, when it was not.
     */
    message: string;
    /**
     * @brief ISO-8601 start of the first bucket.
     */
    start_time: string;
    /**
     * @brief Width of a bucket in seconds.
     */
    bucket_seconds: number;
    /**
     * @brief Number of buckets, and the length of every column.
     */
    bucket_count: number;
    /**
     * @brief Samples in each bucket. 0 marks a gap.
     */
    samples: number[];
    /**
     * @brief Hosts registered, per bucket.
     */
    total_hosts: number[];
    /**
     * @brief Hosts online, per bucket.
     */
    online_hosts: number[];
    /**
     * @brief Hosts online with no work, per bucket.
     */
    idle_hosts: number[];
    /**
     * @brief Results not yet sendable, per bucket.
     */
    results_inactive: number[];
    /**
     * @brief Results waiting for a node, per bucket.
     */
    results_unsent: number[];
    /**
     * @brief Results a node is running, per bucket.
     */
    results_in_progress: number[];
    /**
     * @brief Results finished, a total that only grows, per bucket.
     */
    results_done: number[];
    /**
     * @brief Work units, per bucket.
     */
    total_workunits: number[];
    /**
     * @brief Batches, per bucket.
     */
    total_batches: number[];
    /**
     * @brief Batches with work left, per bucket.
     */
    active_batches: number[];
    /**
     * @brief Results that ended in success, a total that only grows, per bucket.
     */
    outcomes_success: number[];
    /**
     * @brief Results that ended in a client error, a total that only grows, per bucket.
     */
    outcomes_client_error: number[];
    /**
     * @brief Results that ended with no reply, a total that only grows, per bucket.
     */
    outcomes_no_reply: number[];
}

/**
 * @brief Asks for the stored node samples over a time range, as columns.
 *
 * The read serves the whole installation and needs the permission that
 * get_grid_stats needs. The range widens to whole buckets, counted from the
 * Unix epoch. A range that needs more than 5000 buckets, or more than 200000
 * cells of nodes times buckets, is refused, so the caller widens the bucket and
 * never pulls raw rows.
 */
export interface GetNodeSeriesRequest {
    /**
     * @brief ISO-8601 start of the range, inclusive.
     */
    start_time: string;
    /**
     * @brief ISO-8601 end of the range, exclusive.
     */
    end_time: string;
    /**
     * @brief Width of a bucket in seconds.
     */
    bucket_seconds: number;
}

/**
 * @brief One node that reported in the range.
 */
export interface NodeSeriesNode {
    /**
     * @brief The node's host id.
     */
    host_id: string;
    /**
     * @brief The computer the node runs on, from its newest sample that names one. Empty when none did.
     */
    machine: string;
}

/**
 * @brief The node samples of the range as columns on a regular time axis.
 *
 * Bucket j starts at start_time plus j times bucket_seconds, as in the grid
 * series. The nodes are listed once, ordered by machine and then host id, so the
 * nodes of one computer sit together. Every column below has
 * nodes.size() times bucket_count entries, row-major: the cell of node i in
 * bucket j is at i times bucket_count plus j. A cell with no sample has
 * samples 0 and zeros in every column, so a caller must read samples before it
 * draws a value: a gap is not a measurement of zero. Only nodes with a sample in
 * the range are listed.
 */
export interface GetNodeSeriesResponse {
    /**
     * @brief Whether the series was read.
     */
    success: boolean;
    /**
     * @brief Why it was not, when it was not.
     */
    message: string;
    /**
     * @brief ISO-8601 start of the first bucket.
     */
    start_time: string;
    /**
     * @brief Width of a bucket in seconds.
     */
    bucket_seconds: number;
    /**
     * @brief Number of buckets.
     */
    bucket_count: number;
    /**
     * @brief The nodes that reported in the range.
     */
    nodes: NodeSeriesNode[];
    /**
     * @brief Samples in each cell. 0 marks a gap.
     */
    samples: number[];
    /**
     * @brief Tasks finished since the wrapper started, at the cell's newest sample.
     */
    tasks_completed: number[];
    /**
     * @brief Tasks that failed since the wrapper started, at the cell's newest sample.
     */
    tasks_failed: number[];
    /**
     * @brief Tasks finished in the cell, summed over its samples.
     */
    tasks_since_last: number[];
    /**
     * @brief Mean task duration in the cell, weighted by the tasks of each sample. 0 when the cell finished none.
     */
    avg_task_duration_ms: number[];
    /**
     * @brief Longest task in the cell.
     */
    max_task_duration_ms: number[];
    /**
     * @brief Bytes fetched in the cell, summed over its samples.
     */
    input_bytes_fetched: number[];
    /**
     * @brief Bytes uploaded in the cell, summed over its samples.
     */
    output_bytes_uploaded: number[];
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
     * @brief The computer the node runs on, empty when the node did not say.
     */
    machine: string;
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

/**
 * @brief Asks who uses the grid in the session's tenant, with the work counted by party.
 *
 * It carries no fields, because the caller's session decides the tenant and the
 * parties. A tenant's administrator reads the tenant. A session that acts for
 * parties reads those parties. A session of the system tenant reads the system
 * tenant's work only, and another tenant is read by entering it.
 */
export interface GetGridUsageRequest {}

/**
 * @brief The work of the session's tenant counted by party, as columns.
 *
 * Group i is described by keys[i] and by entry i of every other column. The
 * group is a party, and group_kind says "party". A key that is empty stands for
 * work with no party, which predates the party scope. The share of a group is its
 * entry in a column divided by the sum of that column, which the caller
 * computes.
 */
export interface GetGridUsageResponse {
    /**
     * @brief Whether the usage was read.
     */
    success: boolean;
    /**
     * @brief Why it was not, when it was not.
     */
    message: string;
    /**
     * @brief What a group is: always "party".
     */
    group_kind: string;
    /**
     * @brief The id of each group, empty for work with no party.
     */
    keys: string[];
    /**
     * @brief Batches in each group.
     */
    batches: number[];
    /**
     * @brief Workunits in each group.
     */
    workunits: number[];
    /**
     * @brief Results in each group.
     */
    results: number[];
    /**
     * @brief Results waiting to run in each group.
     */
    results_unsent: number[];
    /**
     * @brief Results running in each group.
     */
    results_in_progress: number[];
    /**
     * @brief Results finished in each group.
     */
    results_done: number[];
    /**
     * @brief Results that finished with success in the last 24 hours.
     */
    outcomes_success: number[];
    /**
     * @brief Results that finished with an error or no reply in the last 24 hours.
     */
    outcomes_failed: number[];
    /**
     * @brief How long the oldest waiting result of each group has waited, in seconds.
     *
     * Zero when the group has no result waiting. The wait is counted from the start of the
     * current version of the oldest waiting result, which is the time it was queued while
     * nothing rewrites a result that is still unsent.
     */
    oldest_unsent_seconds: number[];
}

export const subjects = {
    get_grid_stats_request: 'compute.v1.ops.get_grid_stats',
    get_grid_series_request: 'compute.v1.ops.get_grid_series',
    get_node_series_request: 'compute.v1.ops.get_node_series',
    node_sample_message: 'compute.v1.ops.node_sample',
    get_grid_usage_request: 'compute.v1.ops.get_grid_usage',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    get_grid_stats_request: true,
    get_grid_series_request: true,
    get_node_series_request: true,
    node_sample_message: false,
    get_grid_usage_request: true,
} as const;
