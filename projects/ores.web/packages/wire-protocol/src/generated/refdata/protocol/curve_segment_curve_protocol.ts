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
import type { CurveSegmentCurve } from '../domain/curve_segment_curve.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveSegmentCurveKey {
    id: string;
}

export interface CurveSegmentCurveWrite {
    id: string;
    curve_segment_id: string;
    role: string;
    curve: string;
    index_name: string | null;
    weight: number | null;
    position: number;
}

export interface CurveSegmentCurveChange {
    write: CurveSegmentCurveWrite;
    precondition: Precondition;
}

export interface CurveSegmentCurveRemoval {
    key: CurveSegmentCurveKey;
    precondition: Precondition;
}

export interface CurveSegmentCurveLookup {
    key: CurveSegmentCurveKey;
    curve_segment_curve: CurveSegmentCurve | null;
}

export interface CurveSegmentCurveEvent {
    event_id: string;
    key: CurveSegmentCurveKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveSegmentCurveVersionKey {
    curve_segment_curve: CurveSegmentCurveKey;
    version: number;
}

export interface CurveSegmentCurveVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveSegmentCurvesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCurveSegmentCurvesResponse {
    result: Result;
    segment_curves: CurveSegmentCurve[];
    total: number;
}

export interface GetCurveSegmentCurveRequest {
    key: CurveSegmentCurveKey;
}

export interface GetCurveSegmentCurveResponse {
    result: Result;
    curve_segment_curve: CurveSegmentCurve | null;
}

export interface GetManyCurveSegmentCurvesRequest {
    keys: CurveSegmentCurveKey[];
}

export interface GetManyCurveSegmentCurvesResponse {
    result: Result;
    entries: CurveSegmentCurveLookup[];
}

export interface PutCurveSegmentCurveRequest {
    change: CurveSegmentCurveChange;
    intent: ChangeIntent;
}

export interface PutCurveSegmentCurveResponse {
    result: Result;
    curve_segment_curve: CurveSegmentCurve | null;
}

export interface PutManyCurveSegmentCurvesRequest {
    changes: CurveSegmentCurveChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveSegmentCurvesResponse {
    result: Result;
    segment_curves: CurveSegmentCurve[];
}

export interface DeleteCurveSegmentCurveRequest {
    removal: CurveSegmentCurveRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveSegmentCurveResponse {
    result: Result;
}

export interface DeleteManyCurveSegmentCurvesRequest {
    removals: CurveSegmentCurveRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveSegmentCurvesResponse {
    result: Result;
}

export interface ListCurveSegmentCurveVersionsRequest {
    key: CurveSegmentCurveKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveSegmentCurveVersionsFilter | null;
}

export interface ListCurveSegmentCurveVersionsResponse {
    result: Result;
    versions: CurveSegmentCurve[];
    total: number;
}

export interface GetCurveSegmentCurveVersionRequest {
    key: CurveSegmentCurveVersionKey;
}

export interface GetCurveSegmentCurveVersionResponse {
    result: Result;
    version: CurveSegmentCurve | null;
}

export const subjects = {
    list_curve_segment_curves_request: 'refdata.v1.curve_segment_curves.list',
    get_curve_segment_curve_request: 'refdata.v1.curve_segment_curves.get',
    get_many_curve_segment_curves_request: 'refdata.v1.curve_segment_curves.get_many',
    put_curve_segment_curve_request: 'refdata.v1.curve_segment_curves.put',
    put_many_curve_segment_curves_request: 'refdata.v1.curve_segment_curves.put_many',
    delete_curve_segment_curve_request: 'refdata.v1.curve_segment_curves.delete',
    delete_many_curve_segment_curves_request: 'refdata.v1.curve_segment_curves.delete_many',
    list_curve_segment_curve_versions_request: 'refdata.v1.curve_segment_curves_versions.list',
    get_curve_segment_curve_version_request: 'refdata.v1.curve_segment_curves_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_segment_curves_request: true,
    get_curve_segment_curve_request: true,
    get_many_curve_segment_curves_request: true,
    put_curve_segment_curve_request: true,
    put_many_curve_segment_curves_request: true,
    delete_curve_segment_curve_request: true,
    delete_many_curve_segment_curves_request: true,
    list_curve_segment_curve_versions_request: true,
    get_curve_segment_curve_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.curve_segment_curves_events.created',
    updated: 'refdata.v1.curve_segment_curves_events.updated',
    deleted: 'refdata.v1.curve_segment_curves_events.deleted',
} as const;
