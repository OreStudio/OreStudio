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
import type { ShiftType } from '../domain/shift_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ShiftTypeKey {
    code: string;
}

export interface ShiftTypeWrite {
    code: string;
    description: string;
}

export interface ShiftTypeChange {
    write: ShiftTypeWrite;
    precondition: Precondition;
}

export interface ShiftTypeRemoval {
    key: ShiftTypeKey;
    precondition: Precondition;
}

export interface ShiftTypeLookup {
    key: ShiftTypeKey;
    shift_type: ShiftType | null;
}

export interface ShiftTypesFilter {
    code_one_of: string[] | null;
}

export interface ShiftTypeEvent {
    event_id: string;
    key: ShiftTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ShiftTypeVersionKey {
    shift_type: ShiftTypeKey;
    version: number;
}

export interface ShiftTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListShiftTypesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ShiftTypesFilter | null;
}

export interface ListShiftTypesResponse {
    result: Result;
    shift_types: ShiftType[];
    total: number;
}

export interface GetShiftTypeRequest {
    key: ShiftTypeKey;
}

export interface GetShiftTypeResponse {
    result: Result;
    shift_type: ShiftType | null;
}

export interface GetManyShiftTypesRequest {
    keys: ShiftTypeKey[];
}

export interface GetManyShiftTypesResponse {
    result: Result;
    entries: ShiftTypeLookup[];
}

export interface PutShiftTypeRequest {
    change: ShiftTypeChange;
    intent: ChangeIntent;
}

export interface PutShiftTypeResponse {
    result: Result;
    shift_type: ShiftType | null;
}

export interface PutManyShiftTypesRequest {
    changes: ShiftTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyShiftTypesResponse {
    result: Result;
    shift_types: ShiftType[];
}

export interface DeleteShiftTypeRequest {
    removal: ShiftTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteShiftTypeResponse {
    result: Result;
}

export interface DeleteManyShiftTypesRequest {
    removals: ShiftTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyShiftTypesResponse {
    result: Result;
}

export interface ListShiftTypeVersionsRequest {
    key: ShiftTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ShiftTypeVersionsFilter | null;
}

export interface ListShiftTypeVersionsResponse {
    result: Result;
    versions: ShiftType[];
    total: number;
}

export interface GetShiftTypeVersionRequest {
    key: ShiftTypeVersionKey;
}

export interface GetShiftTypeVersionResponse {
    result: Result;
    version: ShiftType | null;
}

export const subjects = {
    list_shift_types_request: 'analytics.v1.shift_types.list',
    get_shift_type_request: 'analytics.v1.shift_types.get',
    get_many_shift_types_request: 'analytics.v1.shift_types.get_many',
    put_shift_type_request: 'analytics.v1.shift_types.put',
    put_many_shift_types_request: 'analytics.v1.shift_types.put_many',
    delete_shift_type_request: 'analytics.v1.shift_types.delete',
    delete_many_shift_types_request: 'analytics.v1.shift_types.delete_many',
    list_shift_type_versions_request: 'analytics.v1.shift_types_versions.list',
    get_shift_type_version_request: 'analytics.v1.shift_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_shift_types_request: true,
    get_shift_type_request: true,
    get_many_shift_types_request: true,
    put_shift_type_request: true,
    put_many_shift_types_request: true,
    delete_shift_type_request: true,
    delete_many_shift_types_request: true,
    list_shift_type_versions_request: true,
    get_shift_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'analytics.v1.shift_types_events.created',
    updated: 'analytics.v1.shift_types_events.updated',
    deleted: 'analytics.v1.shift_types_events.deleted',
} as const;
