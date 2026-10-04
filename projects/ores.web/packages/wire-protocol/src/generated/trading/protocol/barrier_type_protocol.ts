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
import type { BarrierType } from '../domain/barrier_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BarrierTypeKey {
    code: string;
}

export interface BarrierTypeWrite {
    code: string;
    description: string;
}

export interface BarrierTypeChange {
    write: BarrierTypeWrite;
    precondition: Precondition;
}

export interface BarrierTypeRemoval {
    key: BarrierTypeKey;
    precondition: Precondition;
}

export interface BarrierTypeLookup {
    key: BarrierTypeKey;
    barrier_type: BarrierType | null;
}

export interface BarrierTypesFilter {
    code_one_of: string[] | null;
}

export interface BarrierTypeEvent {
    event_id: string;
    key: BarrierTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BarrierTypeVersionKey {
    barrier_type: BarrierTypeKey;
    version: number;
}

export interface BarrierTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBarrierTypesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: BarrierTypesFilter | null;
}

export interface ListBarrierTypesResponse {
    result: Result;
    barrier_types: BarrierType[];
    total: number;
}

export interface GetBarrierTypeRequest {
    key: BarrierTypeKey;
}

export interface GetBarrierTypeResponse {
    result: Result;
    barrier_type: BarrierType | null;
}

export interface GetManyBarrierTypesRequest {
    keys: BarrierTypeKey[];
}

export interface GetManyBarrierTypesResponse {
    result: Result;
    entries: BarrierTypeLookup[];
}

export interface PutBarrierTypeRequest {
    change: BarrierTypeChange;
    intent: ChangeIntent;
}

export interface PutBarrierTypeResponse {
    result: Result;
    barrier_type: BarrierType | null;
}

export interface PutManyBarrierTypesRequest {
    changes: BarrierTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyBarrierTypesResponse {
    result: Result;
    barrier_types: BarrierType[];
}

export interface DeleteBarrierTypeRequest {
    removal: BarrierTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteBarrierTypeResponse {
    result: Result;
}

export interface DeleteManyBarrierTypesRequest {
    removals: BarrierTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBarrierTypesResponse {
    result: Result;
}

export interface ListBarrierTypeVersionsRequest {
    key: BarrierTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BarrierTypeVersionsFilter | null;
}

export interface ListBarrierTypeVersionsResponse {
    result: Result;
    versions: BarrierType[];
    total: number;
}

export interface GetBarrierTypeVersionRequest {
    key: BarrierTypeVersionKey;
}

export interface GetBarrierTypeVersionResponse {
    result: Result;
    version: BarrierType | null;
}

export const subjects = {
    list_barrier_types_request: 'trading.v1.barrier_types.list',
    get_barrier_type_request: 'trading.v1.barrier_types.get',
    get_many_barrier_types_request: 'trading.v1.barrier_types.get_many',
    put_barrier_type_request: 'trading.v1.barrier_types.put',
    put_many_barrier_types_request: 'trading.v1.barrier_types.put_many',
    delete_barrier_type_request: 'trading.v1.barrier_types.delete',
    delete_many_barrier_types_request: 'trading.v1.barrier_types.delete_many',
    list_barrier_type_versions_request: 'trading.v1.barrier_types_versions.list',
    get_barrier_type_version_request: 'trading.v1.barrier_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_barrier_types_request: true,
    get_barrier_type_request: true,
    get_many_barrier_types_request: true,
    put_barrier_type_request: true,
    put_many_barrier_types_request: true,
    delete_barrier_type_request: true,
    delete_many_barrier_types_request: true,
    list_barrier_type_versions_request: true,
    get_barrier_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.barrier_types_events.created',
    updated: 'trading.v1.barrier_types_events.updated',
    deleted: 'trading.v1.barrier_types_events.deleted',
} as const;
