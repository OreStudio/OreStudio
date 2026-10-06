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
import type { AverageType } from '../domain/average_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface AverageTypeKey {
    code: string;
}

export interface AverageTypeWrite {
    code: string;
    description: string;
}

export interface AverageTypeChange {
    write: AverageTypeWrite;
    precondition: Precondition;
}

export interface AverageTypeRemoval {
    key: AverageTypeKey;
    precondition: Precondition;
}

export interface AverageTypeLookup {
    key: AverageTypeKey;
    average_type: AverageType | null;
}

export interface AverageTypesFilter {
    code_one_of: string[] | null;
}

export interface AverageTypeEvent {
    event_id: string;
    key: AverageTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface AverageTypeVersionKey {
    average_type: AverageTypeKey;
    version: number;
}

export interface AverageTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListAverageTypesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: AverageTypesFilter | null;
    as_of: string | null;
}

export interface ListAverageTypesResponse {
    result: Result;
    average_types: AverageType[];
    total: number;
}

export interface GetAverageTypeRequest {
    key: AverageTypeKey;
}

export interface GetAverageTypeResponse {
    result: Result;
    average_type: AverageType | null;
}

export interface GetManyAverageTypesRequest {
    keys: AverageTypeKey[];
}

export interface GetManyAverageTypesResponse {
    result: Result;
    entries: AverageTypeLookup[];
}

export interface PutAverageTypeRequest {
    change: AverageTypeChange;
    intent: ChangeIntent;
}

export interface PutAverageTypeResponse {
    result: Result;
    average_type: AverageType | null;
}

export interface PutManyAverageTypesRequest {
    changes: AverageTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyAverageTypesResponse {
    result: Result;
    average_types: AverageType[];
}

export interface DeleteAverageTypeRequest {
    removal: AverageTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteAverageTypeResponse {
    result: Result;
}

export interface DeleteManyAverageTypesRequest {
    removals: AverageTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyAverageTypesResponse {
    result: Result;
}

export interface ListAverageTypeVersionsRequest {
    key: AverageTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: AverageTypeVersionsFilter | null;
}

export interface ListAverageTypeVersionsResponse {
    result: Result;
    versions: AverageType[];
    total: number;
}

export interface GetAverageTypeVersionRequest {
    key: AverageTypeVersionKey;
}

export interface GetAverageTypeVersionResponse {
    result: Result;
    version: AverageType | null;
}

export const subjects = {
    list_average_types_request: 'trading.v1.average_types.list',
    get_average_type_request: 'trading.v1.average_types.get',
    get_many_average_types_request: 'trading.v1.average_types.get_many',
    put_average_type_request: 'trading.v1.average_types.put',
    put_many_average_types_request: 'trading.v1.average_types.put_many',
    delete_average_type_request: 'trading.v1.average_types.delete',
    delete_many_average_types_request: 'trading.v1.average_types.delete_many',
    list_average_type_versions_request: 'trading.v1.average_types_versions.list',
    get_average_type_version_request: 'trading.v1.average_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_average_types_request: true,
    get_average_type_request: true,
    get_many_average_types_request: true,
    put_average_type_request: true,
    put_many_average_types_request: true,
    delete_average_type_request: true,
    delete_many_average_types_request: true,
    list_average_type_versions_request: true,
    get_average_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.average_types_events.created',
    updated: 'trading.v1.average_types_events.updated',
    deleted: 'trading.v1.average_types_events.deleted',
} as const;
