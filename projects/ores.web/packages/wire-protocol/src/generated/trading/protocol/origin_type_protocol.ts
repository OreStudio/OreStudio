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
import type { OriginType } from '../domain/origin_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface OriginTypeKey {
    code: string;
}

export interface OriginTypeWrite {
    code: string;
    description: string;
}

export interface OriginTypeChange {
    write: OriginTypeWrite;
    precondition: Precondition;
}

export interface OriginTypeRemoval {
    key: OriginTypeKey;
    precondition: Precondition;
}

export interface OriginTypeLookup {
    key: OriginTypeKey;
    origin_type: OriginType | null;
}

export interface OriginTypesFilter {
    code_one_of: string[] | null;
}

export interface OriginTypeEvent {
    event_id: string;
    key: OriginTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface OriginTypeVersionKey {
    origin_type: OriginTypeKey;
    version: number;
}

export interface OriginTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListOriginTypesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: OriginTypesFilter | null;
    as_of: string | null;
}

export interface ListOriginTypesResponse {
    result: Result;
    origin_types: OriginType[];
    total: number;
}

export interface GetOriginTypeRequest {
    key: OriginTypeKey;
}

export interface GetOriginTypeResponse {
    result: Result;
    origin_type: OriginType | null;
}

export interface GetManyOriginTypesRequest {
    keys: OriginTypeKey[];
}

export interface GetManyOriginTypesResponse {
    result: Result;
    entries: OriginTypeLookup[];
}

export interface PutOriginTypeRequest {
    change: OriginTypeChange;
    intent: ChangeIntent;
}

export interface PutOriginTypeResponse {
    result: Result;
    origin_type: OriginType | null;
}

export interface PutManyOriginTypesRequest {
    changes: OriginTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyOriginTypesResponse {
    result: Result;
    origin_types: OriginType[];
}

export interface DeleteOriginTypeRequest {
    removal: OriginTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteOriginTypeResponse {
    result: Result;
}

export interface DeleteManyOriginTypesRequest {
    removals: OriginTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyOriginTypesResponse {
    result: Result;
}

export interface ListOriginTypeVersionsRequest {
    key: OriginTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: OriginTypeVersionsFilter | null;
}

export interface ListOriginTypeVersionsResponse {
    result: Result;
    versions: OriginType[];
    total: number;
}

export interface GetOriginTypeVersionRequest {
    key: OriginTypeVersionKey;
}

export interface GetOriginTypeVersionResponse {
    result: Result;
    version: OriginType | null;
}

export const subjects = {
    list_origin_types_request: 'trading.v1.origin_types.list',
    get_origin_type_request: 'trading.v1.origin_types.get',
    get_many_origin_types_request: 'trading.v1.origin_types.get_many',
    put_origin_type_request: 'trading.v1.origin_types.put',
    put_many_origin_types_request: 'trading.v1.origin_types.put_many',
    delete_origin_type_request: 'trading.v1.origin_types.delete',
    delete_many_origin_types_request: 'trading.v1.origin_types.delete_many',
    list_origin_type_versions_request: 'trading.v1.origin_types_versions.list',
    get_origin_type_version_request: 'trading.v1.origin_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_origin_types_request: true,
    get_origin_type_request: true,
    get_many_origin_types_request: true,
    put_origin_type_request: true,
    put_many_origin_types_request: true,
    delete_origin_type_request: true,
    delete_many_origin_types_request: true,
    list_origin_type_versions_request: true,
    get_origin_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.origin_types_events.created',
    updated: 'trading.v1.origin_types_events.updated',
    deleted: 'trading.v1.origin_types_events.deleted',
} as const;
