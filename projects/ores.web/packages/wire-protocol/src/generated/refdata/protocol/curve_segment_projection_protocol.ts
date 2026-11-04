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
import type { CurveSegmentProjection } from '../domain/curve_segment_projection.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveSegmentProjectionKey {
    id: string;
}

export interface CurveSegmentProjectionWrite {
    id: string;
    curve_segment_id: string;
    projection_curve: string;
}

export interface CurveSegmentProjectionChange {
    write: CurveSegmentProjectionWrite;
    precondition: Precondition;
}

export interface CurveSegmentProjectionRemoval {
    key: CurveSegmentProjectionKey;
    precondition: Precondition;
}

export interface CurveSegmentProjectionLookup {
    key: CurveSegmentProjectionKey;
    curve_segment_projection: CurveSegmentProjection | null;
}

export interface CurveSegmentProjectionsFilter {
    id_one_of: string[] | null;
}

export interface CurveSegmentProjectionEvent {
    event_id: string;
    key: CurveSegmentProjectionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveSegmentProjectionVersionKey {
    curve_segment_projection: CurveSegmentProjectionKey;
    version: number;
}

export interface CurveSegmentProjectionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveSegmentProjectionsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CurveSegmentProjectionsFilter | null;
    as_of: string | null;
}

export interface ListCurveSegmentProjectionsResponse {
    result: Result;
    segment_projections: CurveSegmentProjection[];
    total: number;
}

export interface GetCurveSegmentProjectionRequest {
    key: CurveSegmentProjectionKey;
}

export interface GetCurveSegmentProjectionResponse {
    result: Result;
    curve_segment_projection: CurveSegmentProjection | null;
}

export interface GetManyCurveSegmentProjectionsRequest {
    keys: CurveSegmentProjectionKey[];
}

export interface GetManyCurveSegmentProjectionsResponse {
    result: Result;
    entries: CurveSegmentProjectionLookup[];
}

export interface PutCurveSegmentProjectionRequest {
    change: CurveSegmentProjectionChange;
    intent: ChangeIntent;
}

export interface PutCurveSegmentProjectionResponse {
    result: Result;
    curve_segment_projection: CurveSegmentProjection | null;
}

export interface PutManyCurveSegmentProjectionsRequest {
    changes: CurveSegmentProjectionChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveSegmentProjectionsResponse {
    result: Result;
    segment_projections: CurveSegmentProjection[];
}

export interface DeleteCurveSegmentProjectionRequest {
    removal: CurveSegmentProjectionRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveSegmentProjectionResponse {
    result: Result;
}

export interface DeleteManyCurveSegmentProjectionsRequest {
    removals: CurveSegmentProjectionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveSegmentProjectionsResponse {
    result: Result;
}

export interface ListCurveSegmentProjectionVersionsRequest {
    key: CurveSegmentProjectionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveSegmentProjectionVersionsFilter | null;
}

export interface ListCurveSegmentProjectionVersionsResponse {
    result: Result;
    versions: CurveSegmentProjection[];
    total: number;
}

export interface GetCurveSegmentProjectionVersionRequest {
    key: CurveSegmentProjectionVersionKey;
}

export interface GetCurveSegmentProjectionVersionResponse {
    result: Result;
    version: CurveSegmentProjection | null;
}

export const subjects = {
    list_curve_segment_projections_request: 'refdata.v1.curve_segment_projections.list',
    get_curve_segment_projection_request: 'refdata.v1.curve_segment_projections.get',
    get_many_curve_segment_projections_request: 'refdata.v1.curve_segment_projections.get_many',
    put_curve_segment_projection_request: 'refdata.v1.curve_segment_projections.put',
    put_many_curve_segment_projections_request: 'refdata.v1.curve_segment_projections.put_many',
    delete_curve_segment_projection_request: 'refdata.v1.curve_segment_projections.delete',
    delete_many_curve_segment_projections_request:
        'refdata.v1.curve_segment_projections.delete_many',
    list_curve_segment_projection_versions_request:
        'refdata.v1.curve_segment_projections_versions.list',
    get_curve_segment_projection_version_request:
        'refdata.v1.curve_segment_projections_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_segment_projections_request: true,
    get_curve_segment_projection_request: true,
    get_many_curve_segment_projections_request: true,
    put_curve_segment_projection_request: true,
    put_many_curve_segment_projections_request: true,
    delete_curve_segment_projection_request: true,
    delete_many_curve_segment_projections_request: true,
    list_curve_segment_projection_versions_request: true,
    get_curve_segment_projection_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.events.curve_segment_projections.created',
    updated: 'refdata.v1.events.curve_segment_projections.updated',
    deleted: 'refdata.v1.events.curve_segment_projections.deleted',
} as const;
