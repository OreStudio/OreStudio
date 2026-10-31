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
import type { RunResult } from '../domain/run_result.js';
import type { Order } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface RunResultKey {
    id: string;
}

export interface RunResultLookup {
    key: RunResultKey;
    run_result: RunResult | null;
}

export interface RunResultsFilter {
    party_id: string | null;
    report_instance_id: string | null;
    report_definition_id: string | null;
    subject_type: string | null;
    subject_id: string | null;
    measure: string | null;
    currency: string | null;
    counterparty_id: string | null | null;
    id_one_of: string[] | null;
    party_id_one_of: string[] | null;
    report_instance_id_one_of: string[] | null;
    report_definition_id_one_of: string[] | null;
    subject_type_one_of: string[] | null;
    subject_id_one_of: string[] | null;
    measure_one_of: string[] | null;
    currency_one_of: string[] | null;
    counterparty_id_one_of: string[] | null;
}

export interface RunResultEvent {
    event_id: string;
    key: RunResultKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListRunResultsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: RunResultsFilter | null;
}

export interface ListRunResultsResponse {
    result: Result;
    results: RunResult[];
    total: number;
}

export interface GetRunResultRequest {
    key: RunResultKey;
}

export interface GetRunResultResponse {
    result: Result;
    run_result: RunResult | null;
}

export interface GetManyRunResultsRequest {
    keys: RunResultKey[];
}

export interface GetManyRunResultsResponse {
    result: Result;
    entries: RunResultLookup[];
}

export const subjects = {
    list_run_results_request: 'reporting.v1.run_results.list',
    get_run_result_request: 'reporting.v1.run_results.get',
    get_many_run_results_request: 'reporting.v1.run_results.get_many',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_run_results_request: true,
    get_run_result_request: true,
    get_many_run_results_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.run_results_events.created',
    updated: 'reporting.v1.run_results_events.updated',
    deleted: 'reporting.v1.run_results_events.deleted',
} as const;
