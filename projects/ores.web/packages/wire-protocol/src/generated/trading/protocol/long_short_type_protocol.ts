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
import type { LongShortType } from '../domain/long_short_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface LongShortTypeKey {
    code: string;
}

export interface LongShortTypeWrite {
    code: string;
    description: string;
}

export interface LongShortTypeChange {
    write: LongShortTypeWrite;
    precondition: Precondition;
}

export interface LongShortTypeRemoval {
    key: LongShortTypeKey;
    precondition: Precondition;
}

export interface LongShortTypeLookup {
    key: LongShortTypeKey;
    long_short_type: LongShortType | null;
}

export interface LongShortTypesFilter {
    code_one_of: string[] | null;
}

export interface LongShortTypeEvent {
    event_id: string;
    key: LongShortTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface LongShortTypeVersionKey {
    long_short_type: LongShortTypeKey;
    version: number;
}

export interface LongShortTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListLongShortTypesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: LongShortTypesFilter | null;
    as_of: string | null;
}

export interface ListLongShortTypesResponse {
    result: Result;
    long_short_types: LongShortType[];
    total: number;
}

export interface GetLongShortTypeRequest {
    key: LongShortTypeKey;
}

export interface GetLongShortTypeResponse {
    result: Result;
    long_short_type: LongShortType | null;
}

export interface GetManyLongShortTypesRequest {
    keys: LongShortTypeKey[];
}

export interface GetManyLongShortTypesResponse {
    result: Result;
    entries: LongShortTypeLookup[];
}

export interface PutLongShortTypeRequest {
    change: LongShortTypeChange;
    intent: ChangeIntent;
}

export interface PutLongShortTypeResponse {
    result: Result;
    long_short_type: LongShortType | null;
}

export interface PutManyLongShortTypesRequest {
    changes: LongShortTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyLongShortTypesResponse {
    result: Result;
    long_short_types: LongShortType[];
}

export interface DeleteLongShortTypeRequest {
    removal: LongShortTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteLongShortTypeResponse {
    result: Result;
}

export interface DeleteManyLongShortTypesRequest {
    removals: LongShortTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyLongShortTypesResponse {
    result: Result;
}

export interface ListLongShortTypeVersionsRequest {
    key: LongShortTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: LongShortTypeVersionsFilter | null;
}

export interface ListLongShortTypeVersionsResponse {
    result: Result;
    versions: LongShortType[];
    total: number;
}

export interface GetLongShortTypeVersionRequest {
    key: LongShortTypeVersionKey;
}

export interface GetLongShortTypeVersionResponse {
    result: Result;
    version: LongShortType | null;
}

export const subjects = {
    list_long_short_types_request: 'trading.v1.long_short_types.list',
    get_long_short_type_request: 'trading.v1.long_short_types.get',
    get_many_long_short_types_request: 'trading.v1.long_short_types.get_many',
    put_long_short_type_request: 'trading.v1.long_short_types.put',
    put_many_long_short_types_request: 'trading.v1.long_short_types.put_many',
    delete_long_short_type_request: 'trading.v1.long_short_types.delete',
    delete_many_long_short_types_request: 'trading.v1.long_short_types.delete_many',
    list_long_short_type_versions_request: 'trading.v1.long_short_types_versions.list',
    get_long_short_type_version_request: 'trading.v1.long_short_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_long_short_types_request: true,
    get_long_short_type_request: true,
    get_many_long_short_types_request: true,
    put_long_short_type_request: true,
    put_many_long_short_types_request: true,
    delete_long_short_type_request: true,
    delete_many_long_short_types_request: true,
    list_long_short_type_versions_request: true,
    get_long_short_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.long_short_types_events.created',
    updated: 'trading.v1.long_short_types_events.updated',
    deleted: 'trading.v1.long_short_types_events.deleted',
} as const;
