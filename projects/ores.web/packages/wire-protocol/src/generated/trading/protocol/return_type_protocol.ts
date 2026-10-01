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
import type { ReturnType } from '../domain/return_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ReturnTypeKey {
    code: string;
}

export interface ReturnTypeWrite {
    code: string;
    description: string;
}

export interface ReturnTypeChange {
    write: ReturnTypeWrite;
    precondition: Precondition;
}

export interface ReturnTypeRemoval {
    key: ReturnTypeKey;
    precondition: Precondition;
}

export interface ReturnTypeLookup {
    key: ReturnTypeKey;
    return_type: ReturnType | null;
}

export interface ReturnTypeEvent {
    event_id: string;
    key: ReturnTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ReturnTypeVersionKey {
    return_type: ReturnTypeKey;
    version: number;
}

export interface ReturnTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListReturnTypesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListReturnTypesResponse {
    result: Result;
    return_types: ReturnType[];
    total: number;
}

export interface GetReturnTypeRequest {
    key: ReturnTypeKey;
}

export interface GetReturnTypeResponse {
    result: Result;
    return_type: ReturnType | null;
}

export interface GetManyReturnTypesRequest {
    keys: ReturnTypeKey[];
}

export interface GetManyReturnTypesResponse {
    result: Result;
    entries: ReturnTypeLookup[];
}

export interface PutReturnTypeRequest {
    change: ReturnTypeChange;
    intent: ChangeIntent;
}

export interface PutReturnTypeResponse {
    result: Result;
    return_type: ReturnType | null;
}

export interface PutManyReturnTypesRequest {
    changes: ReturnTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyReturnTypesResponse {
    result: Result;
    return_types: ReturnType[];
}

export interface DeleteReturnTypeRequest {
    removal: ReturnTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteReturnTypeResponse {
    result: Result;
}

export interface DeleteManyReturnTypesRequest {
    removals: ReturnTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyReturnTypesResponse {
    result: Result;
}

export interface ListReturnTypeVersionsRequest {
    key: ReturnTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ReturnTypeVersionsFilter | null;
}

export interface ListReturnTypeVersionsResponse {
    result: Result;
    versions: ReturnType[];
    total: number;
}

export interface GetReturnTypeVersionRequest {
    key: ReturnTypeVersionKey;
}

export interface GetReturnTypeVersionResponse {
    result: Result;
    version: ReturnType | null;
}

export const subjects = {
    list_return_types_request: 'trading.v1.return_types.list',
    get_return_type_request: 'trading.v1.return_types.get',
    get_many_return_types_request: 'trading.v1.return_types.get_many',
    put_return_type_request: 'trading.v1.return_types.put',
    put_many_return_types_request: 'trading.v1.return_types.put_many',
    delete_return_type_request: 'trading.v1.return_types.delete',
    delete_many_return_types_request: 'trading.v1.return_types.delete_many',
    list_return_type_versions_request: 'trading.v1.return_types_versions.list',
    get_return_type_version_request: 'trading.v1.return_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_return_types_request: true,
    get_return_type_request: true,
    get_many_return_types_request: true,
    put_return_type_request: true,
    put_many_return_types_request: true,
    delete_return_type_request: true,
    delete_many_return_types_request: true,
    list_return_type_versions_request: true,
    get_return_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.return_types_events.created',
    updated: 'trading.v1.return_types_events.updated',
    deleted: 'trading.v1.return_types_events.deleted',
} as const;
