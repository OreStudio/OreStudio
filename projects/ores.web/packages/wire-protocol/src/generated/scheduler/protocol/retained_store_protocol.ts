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
import type { RetainedStore } from '../domain/retained_store.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface RetainedStoreKey {
    code: string;
}

export interface RetainedStoreWrite {
    code: string;
    display_name: string;
    description: string;
    owner_component: string;
    scope: string;
    target_kind: string;
    target: string;
    time_column: string | null;
    measure_kind: string;
    bound_kind: string;
    default_action: string | null;
    default_retention_days: number | null;
    default_cadence_hours: number;
    default_reason: string | null;
}

export interface RetainedStoreChange {
    write: RetainedStoreWrite;
    precondition: Precondition;
}

export interface RetainedStoreRemoval {
    key: RetainedStoreKey;
    precondition: Precondition;
}

export interface RetainedStoreLookup {
    key: RetainedStoreKey;
    retained_store: RetainedStore | null;
}

export interface RetainedStoresFilter {
    code_one_of: string[] | null;
}

export interface RetainedStoreEvent {
    event_id: string;
    key: RetainedStoreKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface RetainedStoreVersionKey {
    retained_store: RetainedStoreKey;
    version: number;
}

export interface RetainedStoreVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListRetainedStoresRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: RetainedStoresFilter | null;
    as_of: string | null;
}

export interface ListRetainedStoresResponse {
    result: Result;
    stores: RetainedStore[];
    total: number;
}

export interface GetRetainedStoreRequest {
    key: RetainedStoreKey;
}

export interface GetRetainedStoreResponse {
    result: Result;
    retained_store: RetainedStore | null;
}

export interface GetManyRetainedStoresRequest {
    keys: RetainedStoreKey[];
}

export interface GetManyRetainedStoresResponse {
    result: Result;
    entries: RetainedStoreLookup[];
}

export interface PutRetainedStoreRequest {
    change: RetainedStoreChange;
    intent: ChangeIntent;
}

export interface PutRetainedStoreResponse {
    result: Result;
    retained_store: RetainedStore | null;
}

export interface PutManyRetainedStoresRequest {
    changes: RetainedStoreChange[];
    intent: ChangeIntent;
}

export interface PutManyRetainedStoresResponse {
    result: Result;
    stores: RetainedStore[];
}

export interface DeleteRetainedStoreRequest {
    removal: RetainedStoreRemoval;
    intent: ChangeIntent;
}

export interface DeleteRetainedStoreResponse {
    result: Result;
}

export interface DeleteManyRetainedStoresRequest {
    removals: RetainedStoreRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyRetainedStoresResponse {
    result: Result;
}

export interface ListRetainedStoreVersionsRequest {
    key: RetainedStoreKey;
    offset: number;
    limit: number;
    order: Order;
    filter: RetainedStoreVersionsFilter | null;
}

export interface ListRetainedStoreVersionsResponse {
    result: Result;
    versions: RetainedStore[];
    total: number;
}

export interface GetRetainedStoreVersionRequest {
    key: RetainedStoreVersionKey;
}

export interface GetRetainedStoreVersionResponse {
    result: Result;
    version: RetainedStore | null;
}

export const subjects = {
    list_retained_stores_request: 'scheduler.v1.retained_stores.list',
    get_retained_store_request: 'scheduler.v1.retained_stores.get',
    get_many_retained_stores_request: 'scheduler.v1.retained_stores.get_many',
    put_retained_store_request: 'scheduler.v1.retained_stores.put',
    put_many_retained_stores_request: 'scheduler.v1.retained_stores.put_many',
    delete_retained_store_request: 'scheduler.v1.retained_stores.delete',
    delete_many_retained_stores_request: 'scheduler.v1.retained_stores.delete_many',
    list_retained_store_versions_request: 'scheduler.v1.retained_stores_versions.list',
    get_retained_store_version_request: 'scheduler.v1.retained_stores_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_retained_stores_request: true,
    get_retained_store_request: true,
    get_many_retained_stores_request: true,
    put_retained_store_request: true,
    put_many_retained_stores_request: true,
    delete_retained_store_request: true,
    delete_many_retained_stores_request: true,
    list_retained_store_versions_request: true,
    get_retained_store_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'scheduler.v1.events.retained_stores.created',
    updated: 'scheduler.v1.events.retained_stores.updated',
    deleted: 'scheduler.v1.events.retained_stores.deleted',
} as const;
