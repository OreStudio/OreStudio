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
import type { DefaultCurveBenchmark } from '../domain/default_curve_benchmark.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface DefaultCurveBenchmarkKey {
    id: string;
}

export interface DefaultCurveBenchmarkWrite {
    id: string;
    default_curve_configuration_id: string;
    benchmark_curve: string | null;
    source_curve: string | null;
    pillars: string | null;
    spot_lag: number | null;
    calendar: string | null;
}

export interface DefaultCurveBenchmarkChange {
    write: DefaultCurveBenchmarkWrite;
    precondition: Precondition;
}

export interface DefaultCurveBenchmarkRemoval {
    key: DefaultCurveBenchmarkKey;
    precondition: Precondition;
}

export interface DefaultCurveBenchmarkLookup {
    key: DefaultCurveBenchmarkKey;
    default_curve_benchmark: DefaultCurveBenchmark | null;
}

export interface DefaultCurveBenchmarksFilter {
    id_one_of: string[] | null;
}

export interface DefaultCurveBenchmarkEvent {
    event_id: string;
    key: DefaultCurveBenchmarkKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface DefaultCurveBenchmarkVersionKey {
    default_curve_benchmark: DefaultCurveBenchmarkKey;
    version: number;
}

export interface DefaultCurveBenchmarkVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListDefaultCurveBenchmarksRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: DefaultCurveBenchmarksFilter | null;
    as_of: string | null;
}

export interface ListDefaultCurveBenchmarksResponse {
    result: Result;
    benchmarks: DefaultCurveBenchmark[];
    total: number;
}

export interface GetDefaultCurveBenchmarkRequest {
    key: DefaultCurveBenchmarkKey;
}

export interface GetDefaultCurveBenchmarkResponse {
    result: Result;
    default_curve_benchmark: DefaultCurveBenchmark | null;
}

export interface GetManyDefaultCurveBenchmarksRequest {
    keys: DefaultCurveBenchmarkKey[];
}

export interface GetManyDefaultCurveBenchmarksResponse {
    result: Result;
    entries: DefaultCurveBenchmarkLookup[];
}

export interface PutDefaultCurveBenchmarkRequest {
    change: DefaultCurveBenchmarkChange;
    intent: ChangeIntent;
}

export interface PutDefaultCurveBenchmarkResponse {
    result: Result;
    default_curve_benchmark: DefaultCurveBenchmark | null;
}

export interface PutManyDefaultCurveBenchmarksRequest {
    changes: DefaultCurveBenchmarkChange[];
    intent: ChangeIntent;
}

export interface PutManyDefaultCurveBenchmarksResponse {
    result: Result;
    benchmarks: DefaultCurveBenchmark[];
}

export interface DeleteDefaultCurveBenchmarkRequest {
    removal: DefaultCurveBenchmarkRemoval;
    intent: ChangeIntent;
}

export interface DeleteDefaultCurveBenchmarkResponse {
    result: Result;
}

export interface DeleteManyDefaultCurveBenchmarksRequest {
    removals: DefaultCurveBenchmarkRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyDefaultCurveBenchmarksResponse {
    result: Result;
}

export interface ListDefaultCurveBenchmarkVersionsRequest {
    key: DefaultCurveBenchmarkKey;
    offset: number;
    limit: number;
    order: Order;
    filter: DefaultCurveBenchmarkVersionsFilter | null;
}

export interface ListDefaultCurveBenchmarkVersionsResponse {
    result: Result;
    versions: DefaultCurveBenchmark[];
    total: number;
}

export interface GetDefaultCurveBenchmarkVersionRequest {
    key: DefaultCurveBenchmarkVersionKey;
}

export interface GetDefaultCurveBenchmarkVersionResponse {
    result: Result;
    version: DefaultCurveBenchmark | null;
}

export const subjects = {
    list_default_curve_benchmarks_request: 'refdata.v1.default_curve_benchmarks.list',
    get_default_curve_benchmark_request: 'refdata.v1.default_curve_benchmarks.get',
    get_many_default_curve_benchmarks_request: 'refdata.v1.default_curve_benchmarks.get_many',
    put_default_curve_benchmark_request: 'refdata.v1.default_curve_benchmarks.put',
    put_many_default_curve_benchmarks_request: 'refdata.v1.default_curve_benchmarks.put_many',
    delete_default_curve_benchmark_request: 'refdata.v1.default_curve_benchmarks.delete',
    delete_many_default_curve_benchmarks_request: 'refdata.v1.default_curve_benchmarks.delete_many',
    list_default_curve_benchmark_versions_request:
        'refdata.v1.default_curve_benchmarks_versions.list',
    get_default_curve_benchmark_version_request: 'refdata.v1.default_curve_benchmarks_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_default_curve_benchmarks_request: true,
    get_default_curve_benchmark_request: true,
    get_many_default_curve_benchmarks_request: true,
    put_default_curve_benchmark_request: true,
    put_many_default_curve_benchmarks_request: true,
    delete_default_curve_benchmark_request: true,
    delete_many_default_curve_benchmarks_request: true,
    list_default_curve_benchmark_versions_request: true,
    get_default_curve_benchmark_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.default_curve_benchmarks_events.created',
    updated: 'refdata.v1.default_curve_benchmarks_events.updated',
    deleted: 'refdata.v1.default_curve_benchmarks_events.deleted',
} as const;
