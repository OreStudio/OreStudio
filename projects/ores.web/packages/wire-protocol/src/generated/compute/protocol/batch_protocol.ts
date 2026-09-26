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
import type { Batch } from '../domain/batch.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BatchKey {
    external_ref: string;
}

export interface BatchWrite {
    id: string;
    external_ref: string;
    status: string;
}

export interface BatchChange {
    write: BatchWrite;
    precondition: Precondition;
}

export interface BatchRemoval {
    key: BatchKey;
    precondition: Precondition;
}

export interface BatchLookup {
    key: BatchKey;
    batch: Batch | null;
}

export interface BatchEvent {
    event_id: string;
    key: BatchKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BatchVersionKey {
    batch: BatchKey;
    version: number;
}

export interface BatchVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBatchesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListBatchesResponse {
    result: Result;
    batches: Batch[];
    total: number;
}

export interface GetBatchRequest {
    key: BatchKey;
}

export interface GetBatchResponse {
    result: Result;
    batch: Batch | null;
}

export interface GetManyBatchesRequest {
    keys: BatchKey[];
}

export interface GetManyBatchesResponse {
    result: Result;
    entries: BatchLookup[];
}

export interface PutBatchRequest {
    change: BatchChange;
    intent: ChangeIntent;
}

export interface PutBatchResponse {
    result: Result;
    batch: Batch;
}

export interface PutManyBatchesRequest {
    changes: BatchChange[];
    intent: ChangeIntent;
}

export interface PutManyBatchesResponse {
    result: Result;
    batches: Batch[];
}

export interface DeleteBatchRequest {
    removal: BatchRemoval;
    intent: ChangeIntent;
}

export interface DeleteBatchResponse {
    result: Result;
}

export interface DeleteManyBatchesRequest {
    removals: BatchRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBatchesResponse {
    result: Result;
}

export interface ListBatchVersionsRequest {
    key: BatchKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BatchVersionsFilter | null;
}

export interface ListBatchVersionsResponse {
    result: Result;
    versions: Batch[];
    total: number;
}

export interface GetBatchVersionRequest {
    key: BatchVersionKey;
}

export interface GetBatchVersionResponse {
    result: Result;
    version: Batch;
}

export const subjects = {
    list_batches_request: "compute.v1.batches.list",
    get_batch_request: "compute.v1.batches.get",
    get_many_batches_request: "compute.v1.batches.get_many",
    put_batch_request: "compute.v1.batches.put",
    put_many_batches_request: "compute.v1.batches.put_many",
    delete_batch_request: "compute.v1.batches.delete",
    delete_many_batches_request: "compute.v1.batches.delete_many",
    list_batch_versions_request: "compute.v1.batches_versions.list",
    get_batch_version_request: "compute.v1.batches_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_batches_request: true,
    get_batch_request: true,
    get_many_batches_request: true,
    put_batch_request: true,
    put_many_batches_request: true,
    delete_batch_request: true,
    delete_many_batches_request: true,
    list_batch_versions_request: true,
    get_batch_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "compute.v1.batches_events.created",
    updated: "compute.v1.batches_events.updated",
    deleted: "compute.v1.batches_events.deleted",
} as const;
