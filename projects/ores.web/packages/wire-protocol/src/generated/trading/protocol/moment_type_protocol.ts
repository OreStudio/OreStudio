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
import type { MomentType } from '../domain/moment_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface MomentTypeKey {
    code: string;
}

export interface MomentTypeWrite {
    code: string;
    description: string;
}

export interface MomentTypeChange {
    write: MomentTypeWrite;
    precondition: Precondition;
}

export interface MomentTypeRemoval {
    key: MomentTypeKey;
    precondition: Precondition;
}

export interface MomentTypeLookup {
    key: MomentTypeKey;
    moment_type: MomentType | null;
}

export interface MomentTypesFilter {
    code_one_of: string[] | null;
}

export interface MomentTypeEvent {
    event_id: string;
    key: MomentTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface MomentTypeVersionKey {
    moment_type: MomentTypeKey;
    version: number;
}

export interface MomentTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListMomentTypesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: MomentTypesFilter | null;
    as_of: string | null;
}

export interface ListMomentTypesResponse {
    result: Result;
    moment_types: MomentType[];
    total: number;
}

export interface GetMomentTypeRequest {
    key: MomentTypeKey;
}

export interface GetMomentTypeResponse {
    result: Result;
    moment_type: MomentType | null;
}

export interface GetManyMomentTypesRequest {
    keys: MomentTypeKey[];
}

export interface GetManyMomentTypesResponse {
    result: Result;
    entries: MomentTypeLookup[];
}

export interface PutMomentTypeRequest {
    change: MomentTypeChange;
    intent: ChangeIntent;
}

export interface PutMomentTypeResponse {
    result: Result;
    moment_type: MomentType | null;
}

export interface PutManyMomentTypesRequest {
    changes: MomentTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyMomentTypesResponse {
    result: Result;
    moment_types: MomentType[];
}

export interface DeleteMomentTypeRequest {
    removal: MomentTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteMomentTypeResponse {
    result: Result;
}

export interface DeleteManyMomentTypesRequest {
    removals: MomentTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyMomentTypesResponse {
    result: Result;
}

export interface ListMomentTypeVersionsRequest {
    key: MomentTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: MomentTypeVersionsFilter | null;
}

export interface ListMomentTypeVersionsResponse {
    result: Result;
    versions: MomentType[];
    total: number;
}

export interface GetMomentTypeVersionRequest {
    key: MomentTypeVersionKey;
}

export interface GetMomentTypeVersionResponse {
    result: Result;
    version: MomentType | null;
}

export const subjects = {
    list_moment_types_request: 'trading.v1.moment_types.list',
    get_moment_type_request: 'trading.v1.moment_types.get',
    get_many_moment_types_request: 'trading.v1.moment_types.get_many',
    put_moment_type_request: 'trading.v1.moment_types.put',
    put_many_moment_types_request: 'trading.v1.moment_types.put_many',
    delete_moment_type_request: 'trading.v1.moment_types.delete',
    delete_many_moment_types_request: 'trading.v1.moment_types.delete_many',
    list_moment_type_versions_request: 'trading.v1.moment_types_versions.list',
    get_moment_type_version_request: 'trading.v1.moment_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_moment_types_request: true,
    get_moment_type_request: true,
    get_many_moment_types_request: true,
    put_moment_type_request: true,
    put_many_moment_types_request: true,
    delete_moment_type_request: true,
    delete_many_moment_types_request: true,
    list_moment_type_versions_request: true,
    get_moment_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.moment_types_events.created',
    updated: 'trading.v1.moment_types_events.updated',
    deleted: 'trading.v1.moment_types_events.deleted',
} as const;
