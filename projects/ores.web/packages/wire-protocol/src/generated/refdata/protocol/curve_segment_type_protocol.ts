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
import type { CurveSegmentType } from '../domain/curve_segment_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveSegmentTypeKey {
    code: string;
}

export interface CurveSegmentTypeWrite {
    code: string;
    segment_kind: string;
    description: string;
}

export interface CurveSegmentTypeChange {
    write: CurveSegmentTypeWrite;
    precondition: Precondition;
}

export interface CurveSegmentTypeRemoval {
    key: CurveSegmentTypeKey;
    precondition: Precondition;
}

export interface CurveSegmentTypeLookup {
    key: CurveSegmentTypeKey;
    curve_segment_type: CurveSegmentType | null;
}

export interface CurveSegmentTypeEvent {
    event_id: string;
    key: CurveSegmentTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveSegmentTypeVersionKey {
    curve_segment_type: CurveSegmentTypeKey;
    version: number;
}

export interface CurveSegmentTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveSegmentTypesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCurveSegmentTypesResponse {
    result: Result;
    segment_types: CurveSegmentType[];
    total: number;
}

export interface GetCurveSegmentTypeRequest {
    key: CurveSegmentTypeKey;
}

export interface GetCurveSegmentTypeResponse {
    result: Result;
    curve_segment_type: CurveSegmentType | null;
}

export interface GetManyCurveSegmentTypesRequest {
    keys: CurveSegmentTypeKey[];
}

export interface GetManyCurveSegmentTypesResponse {
    result: Result;
    entries: CurveSegmentTypeLookup[];
}

export interface PutCurveSegmentTypeRequest {
    change: CurveSegmentTypeChange;
    intent: ChangeIntent;
}

export interface PutCurveSegmentTypeResponse {
    result: Result;
    curve_segment_type: CurveSegmentType | null;
}

export interface PutManyCurveSegmentTypesRequest {
    changes: CurveSegmentTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveSegmentTypesResponse {
    result: Result;
    segment_types: CurveSegmentType[];
}

export interface DeleteCurveSegmentTypeRequest {
    removal: CurveSegmentTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveSegmentTypeResponse {
    result: Result;
}

export interface DeleteManyCurveSegmentTypesRequest {
    removals: CurveSegmentTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveSegmentTypesResponse {
    result: Result;
}

export interface ListCurveSegmentTypeVersionsRequest {
    key: CurveSegmentTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveSegmentTypeVersionsFilter | null;
}

export interface ListCurveSegmentTypeVersionsResponse {
    result: Result;
    versions: CurveSegmentType[];
    total: number;
}

export interface GetCurveSegmentTypeVersionRequest {
    key: CurveSegmentTypeVersionKey;
}

export interface GetCurveSegmentTypeVersionResponse {
    result: Result;
    version: CurveSegmentType | null;
}

export const subjects = {
    list_curve_segment_types_request: 'refdata.v1.curve_segment_types.list',
    get_curve_segment_type_request: 'refdata.v1.curve_segment_types.get',
    get_many_curve_segment_types_request: 'refdata.v1.curve_segment_types.get_many',
    put_curve_segment_type_request: 'refdata.v1.curve_segment_types.put',
    put_many_curve_segment_types_request: 'refdata.v1.curve_segment_types.put_many',
    delete_curve_segment_type_request: 'refdata.v1.curve_segment_types.delete',
    delete_many_curve_segment_types_request: 'refdata.v1.curve_segment_types.delete_many',
    list_curve_segment_type_versions_request: 'refdata.v1.curve_segment_types_versions.list',
    get_curve_segment_type_version_request: 'refdata.v1.curve_segment_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_segment_types_request: true,
    get_curve_segment_type_request: true,
    get_many_curve_segment_types_request: true,
    put_curve_segment_type_request: true,
    put_many_curve_segment_types_request: true,
    delete_curve_segment_type_request: true,
    delete_many_curve_segment_types_request: true,
    list_curve_segment_type_versions_request: true,
    get_curve_segment_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.curve_segment_types_events.created',
    updated: 'refdata.v1.curve_segment_types_events.updated',
    deleted: 'refdata.v1.curve_segment_types_events.deleted',
} as const;
