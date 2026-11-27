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
import type { RetainedStoreSample } from '../domain/retained_store_sample.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface RetainedStoreSampleKey {
    id: string;
    sampled_at: string;
}

export interface RetainedStoreSampleWrite {
    id: string;
    sampled_at: string;
    store_code: string;
    size_bytes: number;
    measure_count: number;
}

export interface RetainedStoreSampleChange {
    write: RetainedStoreSampleWrite;
    precondition: Precondition;
}

export interface RetainedStoreSampleRemoval {
    key: RetainedStoreSampleKey;
    precondition: Precondition;
}

export interface RetainedStoreSampleLookup {
    key: RetainedStoreSampleKey;
    retained_store_sample: RetainedStoreSample | null;
}

export interface RetainedStoreSampleEvent {
    event_id: string;
    key: RetainedStoreSampleKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListRetainedStoreSamplesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListRetainedStoreSamplesResponse {
    result: Result;
    samples: RetainedStoreSample[];
    total: number;
}

export interface GetRetainedStoreSampleRequest {
    key: RetainedStoreSampleKey;
}

export interface GetRetainedStoreSampleResponse {
    result: Result;
    retained_store_sample: RetainedStoreSample | null;
}

export interface GetManyRetainedStoreSamplesRequest {
    keys: RetainedStoreSampleKey[];
}

export interface GetManyRetainedStoreSamplesResponse {
    result: Result;
    entries: RetainedStoreSampleLookup[];
}

export interface PutRetainedStoreSampleRequest {
    change: RetainedStoreSampleChange;
    intent: ChangeIntent;
}

export interface PutRetainedStoreSampleResponse {
    result: Result;
    retained_store_sample: RetainedStoreSample | null;
}

export interface PutManyRetainedStoreSamplesRequest {
    changes: RetainedStoreSampleChange[];
    intent: ChangeIntent;
}

export interface PutManyRetainedStoreSamplesResponse {
    result: Result;
    samples: RetainedStoreSample[];
}

export interface DeleteRetainedStoreSampleRequest {
    removal: RetainedStoreSampleRemoval;
    intent: ChangeIntent;
}

export interface DeleteRetainedStoreSampleResponse {
    result: Result;
}

export interface DeleteManyRetainedStoreSamplesRequest {
    removals: RetainedStoreSampleRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyRetainedStoreSamplesResponse {
    result: Result;
}

export const subjects = {
    list_retained_store_samples_request: 'scheduler.v1.retained_store_samples.list',
    get_retained_store_sample_request: 'scheduler.v1.retained_store_samples.get',
    get_many_retained_store_samples_request: 'scheduler.v1.retained_store_samples.get_many',
    put_retained_store_sample_request: 'scheduler.v1.retained_store_samples.put',
    put_many_retained_store_samples_request: 'scheduler.v1.retained_store_samples.put_many',
    delete_retained_store_sample_request: 'scheduler.v1.retained_store_samples.delete',
    delete_many_retained_store_samples_request: 'scheduler.v1.retained_store_samples.delete_many',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_retained_store_samples_request: true,
    get_retained_store_sample_request: true,
    get_many_retained_store_samples_request: true,
    put_retained_store_sample_request: true,
    put_many_retained_store_samples_request: true,
    delete_retained_store_sample_request: true,
    delete_many_retained_store_samples_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'scheduler.v1.events.retained_store_samples.created',
    updated: 'scheduler.v1.events.retained_store_samples.updated',
    deleted: 'scheduler.v1.events.retained_store_samples.deleted',
} as const;
