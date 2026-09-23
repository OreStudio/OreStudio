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
import type { Result } from '../domain/result.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface ResultKey {
    id: string;
}

export interface ResultWrite {
    id: string;
    workunit_id: string;
    host_id: string;
    pgmq_msg_id: number;
    server_state: number;
    outcome: number;
    output_uri: string;
    error_message: string;
    received_at: string;
}

export interface ResultChange {
    write: ResultWrite;
    precondition: Precondition;
}

export interface ResultRemoval {
    key: ResultKey;
    precondition: Precondition;
}

export interface ResultLookup {
    key: ResultKey;
    result: Result | null;
}

export interface ResultsFilter {
    workunit_id: string | null;
}

export interface ResultEvent {
    event_id: string;
    key: ResultKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ResultVersionKey {
    result: ResultKey;
    version: number;
}

export interface ResultVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListResultsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ResultsFilter | null;
}

export interface ListResultsResponse {
    result: Result;
    results: Result[];
    total: number;
}

export interface GetResultRequest {
    key: ResultKey;
}

export interface GetResultResponse {
    result: Result;
    result_value: Result | null;
}

export interface GetManyResultsRequest {
    keys: ResultKey[];
}

export interface GetManyResultsResponse {
    result: Result;
    entries: ResultLookup[];
}

export interface PutResultRequest {
    change: ResultChange;
    intent: ChangeIntent;
}

export interface PutResultResponse {
    result: Result;
    result_value: Result;
}

export interface PutManyResultsRequest {
    changes: ResultChange[];
    intent: ChangeIntent;
}

export interface PutManyResultsResponse {
    result: Result;
    results: Result[];
}

export interface DeleteResultRequest {
    removal: ResultRemoval;
    intent: ChangeIntent;
}

export interface DeleteResultResponse {
    result: Result;
}

export interface DeleteManyResultsRequest {
    removals: ResultRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyResultsResponse {
    result: Result;
}

export interface ListByWorkunitIdResultsRequest {
    workunit_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: ResultsFilter | null;
}

export interface ListByWorkunitIdResultsResponse {
    result: Result;
    results: Result[];
    total: number;
}

export interface ListResultVersionsRequest {
    key: ResultKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ResultVersionsFilter | null;
}

export interface ListResultVersionsResponse {
    result: Result;
    versions: Result[];
    total: number;
}

export interface GetResultVersionRequest {
    key: ResultVersionKey;
}

export interface GetResultVersionResponse {
    result: Result;
    version: Result;
}

export const subjects = {
    list_results_request: "compute.v1.results.list",
    get_result_request: "compute.v1.results.get",
    get_many_results_request: "compute.v1.results.get_many",
    put_result_request: "compute.v1.results.put",
    put_many_results_request: "compute.v1.results.put_many",
    delete_result_request: "compute.v1.results.delete",
    delete_many_results_request: "compute.v1.results.delete_many",
    list_by_workunit_id_results_request: "compute.v1.results.list_by_workunit_id",
    list_result_versions_request: "compute.v1.results_versions.list",
    get_result_version_request: "compute.v1.results_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_results_request: true,
    get_result_request: true,
    get_many_results_request: true,
    put_result_request: true,
    put_many_results_request: true,
    delete_result_request: true,
    delete_many_results_request: true,
    list_by_workunit_id_results_request: true,
    list_result_versions_request: true,
    get_result_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "compute.v1.results_events.created",
    updated: "compute.v1.results_events.updated",
    deleted: "compute.v1.results_events.deleted",
} as const;
