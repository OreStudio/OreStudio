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
import type { RetentionRun } from '../domain/retention_run.js';
import type { Order } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface RetentionRunKey {
    id: string;
}

export interface RetentionRunLookup {
    key: RetentionRunKey;
    retention_run: RetentionRun | null;
}

export interface RetentionRunsFilter {
    policy_code: string | null;
    outcome: string | null;
    id_one_of: string[] | null;
    policy_code_one_of: string[] | null;
    outcome_one_of: string[] | null;
}

export interface RetentionRunEvent {
    event_id: string;
    key: RetentionRunKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListRetentionRunsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: RetentionRunsFilter | null;
}

export interface ListRetentionRunsResponse {
    result: Result;
    runs: RetentionRun[];
    total: number;
}

export interface GetRetentionRunRequest {
    key: RetentionRunKey;
}

export interface GetRetentionRunResponse {
    result: Result;
    retention_run: RetentionRun | null;
}

export interface GetManyRetentionRunsRequest {
    keys: RetentionRunKey[];
}

export interface GetManyRetentionRunsResponse {
    result: Result;
    entries: RetentionRunLookup[];
}

export const subjects = {
    list_retention_runs_request: 'scheduler.v1.retention_runs.list',
    get_retention_run_request: 'scheduler.v1.retention_runs.get',
    get_many_retention_runs_request: 'scheduler.v1.retention_runs.get_many',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_retention_runs_request: true,
    get_retention_run_request: true,
    get_many_retention_runs_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'scheduler.v1.events.retention_runs.created',
    updated: 'scheduler.v1.events.retention_runs.updated',
    deleted: 'scheduler.v1.events.retention_runs.deleted',
} as const;
