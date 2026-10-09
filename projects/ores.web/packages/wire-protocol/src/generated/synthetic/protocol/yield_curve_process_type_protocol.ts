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
import type { YieldCurveProcessType } from '../domain/yield_curve_process_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface YieldCurveProcessTypeKey {
    code: string;
}

export interface YieldCurveProcessTypeWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface YieldCurveProcessTypeChange {
    write: YieldCurveProcessTypeWrite;
    precondition: Precondition;
}

export interface YieldCurveProcessTypeRemoval {
    key: YieldCurveProcessTypeKey;
    precondition: Precondition;
}

export interface YieldCurveProcessTypeLookup {
    key: YieldCurveProcessTypeKey;
    yield_curve_process_type: YieldCurveProcessType | null;
}

export interface YieldCurveProcessTypesFilter {
    code_one_of: string[] | null;
}

export interface YieldCurveProcessTypeEvent {
    event_id: string;
    key: YieldCurveProcessTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface YieldCurveProcessTypeVersionKey {
    yield_curve_process_type: YieldCurveProcessTypeKey;
    version: number;
}

export interface YieldCurveProcessTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListYieldCurveProcessTypesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: YieldCurveProcessTypesFilter | null;
    as_of: string | null;
}

export interface ListYieldCurveProcessTypesResponse {
    result: Result;
    process_types: YieldCurveProcessType[];
    total: number;
}

export interface GetYieldCurveProcessTypeRequest {
    key: YieldCurveProcessTypeKey;
}

export interface GetYieldCurveProcessTypeResponse {
    result: Result;
    yield_curve_process_type: YieldCurveProcessType | null;
}

export interface GetManyYieldCurveProcessTypesRequest {
    keys: YieldCurveProcessTypeKey[];
}

export interface GetManyYieldCurveProcessTypesResponse {
    result: Result;
    entries: YieldCurveProcessTypeLookup[];
}

export interface PutYieldCurveProcessTypeRequest {
    change: YieldCurveProcessTypeChange;
    intent: ChangeIntent;
}

export interface PutYieldCurveProcessTypeResponse {
    result: Result;
    yield_curve_process_type: YieldCurveProcessType | null;
}

export interface PutManyYieldCurveProcessTypesRequest {
    changes: YieldCurveProcessTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyYieldCurveProcessTypesResponse {
    result: Result;
    process_types: YieldCurveProcessType[];
}

export interface DeleteYieldCurveProcessTypeRequest {
    removal: YieldCurveProcessTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteYieldCurveProcessTypeResponse {
    result: Result;
}

export interface DeleteManyYieldCurveProcessTypesRequest {
    removals: YieldCurveProcessTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyYieldCurveProcessTypesResponse {
    result: Result;
}

export interface ListYieldCurveProcessTypeVersionsRequest {
    key: YieldCurveProcessTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: YieldCurveProcessTypeVersionsFilter | null;
}

export interface ListYieldCurveProcessTypeVersionsResponse {
    result: Result;
    versions: YieldCurveProcessType[];
    total: number;
}

export interface GetYieldCurveProcessTypeVersionRequest {
    key: YieldCurveProcessTypeVersionKey;
}

export interface GetYieldCurveProcessTypeVersionResponse {
    result: Result;
    version: YieldCurveProcessType | null;
}

export const subjects = {
    list_yield_curve_process_types_request: 'synthetic.v1.yield_curve_process_types.list',
    get_yield_curve_process_type_request: 'synthetic.v1.yield_curve_process_types.get',
    get_many_yield_curve_process_types_request: 'synthetic.v1.yield_curve_process_types.get_many',
    put_yield_curve_process_type_request: 'synthetic.v1.yield_curve_process_types.put',
    put_many_yield_curve_process_types_request: 'synthetic.v1.yield_curve_process_types.put_many',
    delete_yield_curve_process_type_request: 'synthetic.v1.yield_curve_process_types.delete',
    delete_many_yield_curve_process_types_request:
        'synthetic.v1.yield_curve_process_types.delete_many',
    list_yield_curve_process_type_versions_request:
        'synthetic.v1.yield_curve_process_types_versions.list',
    get_yield_curve_process_type_version_request:
        'synthetic.v1.yield_curve_process_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_yield_curve_process_types_request: true,
    get_yield_curve_process_type_request: true,
    get_many_yield_curve_process_types_request: true,
    put_yield_curve_process_type_request: true,
    put_many_yield_curve_process_types_request: true,
    delete_yield_curve_process_type_request: true,
    delete_many_yield_curve_process_types_request: true,
    list_yield_curve_process_type_versions_request: true,
    get_yield_curve_process_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'synthetic.v1.yield_curve_process_types_events.created',
    updated: 'synthetic.v1.yield_curve_process_types_events.updated',
    deleted: 'synthetic.v1.yield_curve_process_types_events.deleted',
} as const;
