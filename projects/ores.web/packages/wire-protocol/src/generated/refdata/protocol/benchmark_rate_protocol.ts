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
import type { BenchmarkRate } from '../domain/benchmark_rate.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BenchmarkRateKey {
    code: string;
    coding_scheme_code: string;
}

export interface BenchmarkRateWrite {
    code: string;
    coding_scheme_code: string;
    source: string;
    description: string;
}

export interface BenchmarkRateChange {
    write: BenchmarkRateWrite;
    precondition: Precondition;
}

export interface BenchmarkRateRemoval {
    key: BenchmarkRateKey;
    precondition: Precondition;
}

export interface BenchmarkRateLookup {
    key: BenchmarkRateKey;
    benchmark_rate: BenchmarkRate | null;
}

export interface BenchmarkRateEvent {
    event_id: string;
    key: BenchmarkRateKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BenchmarkRateVersionKey {
    benchmark_rate: BenchmarkRateKey;
    version: number;
}

export interface BenchmarkRateVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBenchmarkRatesRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListBenchmarkRatesResponse {
    result: Result;
    rates: BenchmarkRate[];
    total: number;
}

export interface GetBenchmarkRateRequest {
    key: BenchmarkRateKey;
}

export interface GetBenchmarkRateResponse {
    result: Result;
    benchmark_rate: BenchmarkRate | null;
}

export interface GetManyBenchmarkRatesRequest {
    keys: BenchmarkRateKey[];
}

export interface GetManyBenchmarkRatesResponse {
    result: Result;
    entries: BenchmarkRateLookup[];
}

export interface PutBenchmarkRateRequest {
    change: BenchmarkRateChange;
    intent: ChangeIntent;
}

export interface PutBenchmarkRateResponse {
    result: Result;
    benchmark_rate: BenchmarkRate | null;
}

export interface PutManyBenchmarkRatesRequest {
    changes: BenchmarkRateChange[];
    intent: ChangeIntent;
}

export interface PutManyBenchmarkRatesResponse {
    result: Result;
    rates: BenchmarkRate[];
}

export interface DeleteBenchmarkRateRequest {
    removal: BenchmarkRateRemoval;
    intent: ChangeIntent;
}

export interface DeleteBenchmarkRateResponse {
    result: Result;
}

export interface DeleteManyBenchmarkRatesRequest {
    removals: BenchmarkRateRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBenchmarkRatesResponse {
    result: Result;
}

export interface ListBenchmarkRateVersionsRequest {
    key: BenchmarkRateKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BenchmarkRateVersionsFilter | null;
}

export interface ListBenchmarkRateVersionsResponse {
    result: Result;
    versions: BenchmarkRate[];
    total: number;
}

export interface GetBenchmarkRateVersionRequest {
    key: BenchmarkRateVersionKey;
}

export interface GetBenchmarkRateVersionResponse {
    result: Result;
    version: BenchmarkRate | null;
}

export const subjects = {
    list_benchmark_rates_request: 'refdata.v1.benchmark_rates.list',
    get_benchmark_rate_request: 'refdata.v1.benchmark_rates.get',
    get_many_benchmark_rates_request: 'refdata.v1.benchmark_rates.get_many',
    put_benchmark_rate_request: 'refdata.v1.benchmark_rates.put',
    put_many_benchmark_rates_request: 'refdata.v1.benchmark_rates.put_many',
    delete_benchmark_rate_request: 'refdata.v1.benchmark_rates.delete',
    delete_many_benchmark_rates_request: 'refdata.v1.benchmark_rates.delete_many',
    list_benchmark_rate_versions_request: 'refdata.v1.benchmark_rates_versions.list',
    get_benchmark_rate_version_request: 'refdata.v1.benchmark_rates_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_benchmark_rates_request: true,
    get_benchmark_rate_request: true,
    get_many_benchmark_rates_request: true,
    put_benchmark_rate_request: true,
    put_many_benchmark_rates_request: true,
    delete_benchmark_rate_request: true,
    delete_many_benchmark_rates_request: true,
    list_benchmark_rate_versions_request: true,
    get_benchmark_rate_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.benchmark_rates_events.created',
    updated: 'refdata.v1.benchmark_rates_events.updated',
    deleted: 'refdata.v1.benchmark_rates_events.deleted',
} as const;
