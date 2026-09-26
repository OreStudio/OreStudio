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
export interface GetGridStatsRequest {
}

export interface NodeStatsSummary {
    host_id: string;
    tasks_completed: number;
    tasks_since_last: number;
    avg_task_duration_ms: number;
    input_bytes_fetched: number;
    output_bytes_uploaded: number;
    seconds_since_hb: number;
}

export interface GetGridStatsResponse {
    success: boolean;
    message: string;
    total_hosts: number;
    online_hosts: number;
    idle_hosts: number;
    results_inactive: number;
    results_unsent: number;
    results_in_progress: number;
    results_done: number;
    total_workunits: number;
    total_batches: number;
    active_batches: number;
    outcomes_success: number;
    outcomes_client_error: number;
    outcomes_no_reply: number;
    sampled_at: string;
    node_summaries: NodeStatsSummary[];
}

export interface NodeSampleMessage {
    tenant_id: string;
    host_id: string;
    sampled_at: string;
    tasks_completed: number;
    tasks_failed: number;
    tasks_since_last: number;
    avg_task_duration_ms: number;
    max_task_duration_ms: number;
    input_bytes_fetched: number;
    output_bytes_uploaded: number;
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
