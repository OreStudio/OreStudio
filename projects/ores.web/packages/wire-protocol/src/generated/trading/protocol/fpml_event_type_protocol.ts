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
import type { FpmlEventType } from '../domain/fpml_event_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FpmlEventTypeKey {
    code: string;
}

export interface FpmlEventTypeWrite {
    code: string;
    description: string;
}

export interface FpmlEventTypeChange {
    write: FpmlEventTypeWrite;
    precondition: Precondition;
}

export interface FpmlEventTypeRemoval {
    key: FpmlEventTypeKey;
    precondition: Precondition;
}

export interface FpmlEventTypeLookup {
    key: FpmlEventTypeKey;
    fpml_event_type: FpmlEventType | null;
}

export interface FpmlEventTypesFilter {
    code_one_of: string[] | null;
}

export interface FpmlEventTypeEvent {
    event_id: string;
    key: FpmlEventTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FpmlEventTypeVersionKey {
    fpml_event_type: FpmlEventTypeKey;
    version: number;
}

export interface FpmlEventTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFpmlEventTypesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FpmlEventTypesFilter | null;
    as_of: string | null;
}

export interface ListFpmlEventTypesResponse {
    result: Result;
    fpml_event_types: FpmlEventType[];
    total: number;
}

export interface GetFpmlEventTypeRequest {
    key: FpmlEventTypeKey;
}

export interface GetFpmlEventTypeResponse {
    result: Result;
    fpml_event_type: FpmlEventType | null;
}

export interface GetManyFpmlEventTypesRequest {
    keys: FpmlEventTypeKey[];
}

export interface GetManyFpmlEventTypesResponse {
    result: Result;
    entries: FpmlEventTypeLookup[];
}

export interface PutFpmlEventTypeRequest {
    change: FpmlEventTypeChange;
    intent: ChangeIntent;
}

export interface PutFpmlEventTypeResponse {
    result: Result;
    fpml_event_type: FpmlEventType | null;
}

export interface PutManyFpmlEventTypesRequest {
    changes: FpmlEventTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyFpmlEventTypesResponse {
    result: Result;
    fpml_event_types: FpmlEventType[];
}

export interface DeleteFpmlEventTypeRequest {
    removal: FpmlEventTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteFpmlEventTypeResponse {
    result: Result;
}

export interface DeleteManyFpmlEventTypesRequest {
    removals: FpmlEventTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFpmlEventTypesResponse {
    result: Result;
}

export interface ListFpmlEventTypeVersionsRequest {
    key: FpmlEventTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FpmlEventTypeVersionsFilter | null;
}

export interface ListFpmlEventTypeVersionsResponse {
    result: Result;
    versions: FpmlEventType[];
    total: number;
}

export interface GetFpmlEventTypeVersionRequest {
    key: FpmlEventTypeVersionKey;
}

export interface GetFpmlEventTypeVersionResponse {
    result: Result;
    version: FpmlEventType | null;
}

export const subjects = {
    list_fpml_event_types_request: 'trading.v1.fpml_event_types.list',
    get_fpml_event_type_request: 'trading.v1.fpml_event_types.get',
    get_many_fpml_event_types_request: 'trading.v1.fpml_event_types.get_many',
    put_fpml_event_type_request: 'trading.v1.fpml_event_types.put',
    put_many_fpml_event_types_request: 'trading.v1.fpml_event_types.put_many',
    delete_fpml_event_type_request: 'trading.v1.fpml_event_types.delete',
    delete_many_fpml_event_types_request: 'trading.v1.fpml_event_types.delete_many',
    list_fpml_event_type_versions_request: 'trading.v1.fpml_event_types_versions.list',
    get_fpml_event_type_version_request: 'trading.v1.fpml_event_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fpml_event_types_request: true,
    get_fpml_event_type_request: true,
    get_many_fpml_event_types_request: true,
    put_fpml_event_type_request: true,
    put_many_fpml_event_types_request: true,
    delete_fpml_event_type_request: true,
    delete_many_fpml_event_types_request: true,
    list_fpml_event_type_versions_request: true,
    get_fpml_event_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.fpml_event_types_events.created',
    updated: 'trading.v1.fpml_event_types_events.updated',
    deleted: 'trading.v1.fpml_event_types_events.deleted',
} as const;
