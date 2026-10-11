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
import type { CurveSegment } from '../domain/curve_segment.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveSegmentKey {
    id: string;
}

export interface CurveSegmentWrite {
    id: string;
    curve_definition_id: string;
    segment_type: string;
    position: number;
    conventions: string | null;
    pillar_choice: string | null;
    priority: number | null;
    min_distance: number | null;
    projection_curve: string | null;
    discount_curve: string | null;
    spot_rate: string | null;
    projection_curve_domestic: string | null;
    projection_curve_foreign: string | null;
    projection_curve_pay: string | null;
    projection_curve_receive: string | null;
    projection_curve_long: string | null;
    projection_curve_short: string | null;
    reference_curve: string | null;
    reference_curve_2: string | null;
    weight_1: number | null;
    weight_2: number | null;
    ibor_index: string | null;
    rfr_curve: string | null;
    rfr_index: string | null;
    spread: string | null;
    base_curve: string | null;
    base_curve_currency: string | null;
    numerator_curve: string | null;
    numerator_curve_currency: string | null;
    denominator_curve: string | null;
    denominator_curve_currency: string | null;
    extrapolate_flat: boolean | null;
}

export interface CurveSegmentChange {
    write: CurveSegmentWrite;
    precondition: Precondition;
}

export interface CurveSegmentRemoval {
    key: CurveSegmentKey;
    precondition: Precondition;
}

export interface CurveSegmentLookup {
    key: CurveSegmentKey;
    curve_segment: CurveSegment | null;
}

export interface CurveSegmentsFilter {
    id_one_of: string[] | null;
}

export interface CurveSegmentEvent {
    event_id: string;
    key: CurveSegmentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveSegmentVersionKey {
    curve_segment: CurveSegmentKey;
    version: number;
}

export interface CurveSegmentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveSegmentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CurveSegmentsFilter | null;
    as_of: string | null;
}

export interface ListCurveSegmentsResponse {
    result: Result;
    segments: CurveSegment[];
    total: number;
}

export interface GetCurveSegmentRequest {
    key: CurveSegmentKey;
}

export interface GetCurveSegmentResponse {
    result: Result;
    curve_segment: CurveSegment | null;
}

export interface GetManyCurveSegmentsRequest {
    keys: CurveSegmentKey[];
}

export interface GetManyCurveSegmentsResponse {
    result: Result;
    entries: CurveSegmentLookup[];
}

export interface PutCurveSegmentRequest {
    change: CurveSegmentChange;
    intent: ChangeIntent;
}

export interface PutCurveSegmentResponse {
    result: Result;
    curve_segment: CurveSegment | null;
}

export interface PutManyCurveSegmentsRequest {
    changes: CurveSegmentChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveSegmentsResponse {
    result: Result;
    segments: CurveSegment[];
}

export interface DeleteCurveSegmentRequest {
    removal: CurveSegmentRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveSegmentResponse {
    result: Result;
}

export interface DeleteManyCurveSegmentsRequest {
    removals: CurveSegmentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveSegmentsResponse {
    result: Result;
}

export interface ListCurveSegmentVersionsRequest {
    key: CurveSegmentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveSegmentVersionsFilter | null;
}

export interface ListCurveSegmentVersionsResponse {
    result: Result;
    versions: CurveSegment[];
    total: number;
}

export interface GetCurveSegmentVersionRequest {
    key: CurveSegmentVersionKey;
}

export interface GetCurveSegmentVersionResponse {
    result: Result;
    version: CurveSegment | null;
}

export const subjects = {
    list_curve_segments_request: 'refdata.v1.curve_segments.list',
    get_curve_segment_request: 'refdata.v1.curve_segments.get',
    get_many_curve_segments_request: 'refdata.v1.curve_segments.get_many',
    put_curve_segment_request: 'refdata.v1.curve_segments.put',
    put_many_curve_segments_request: 'refdata.v1.curve_segments.put_many',
    delete_curve_segment_request: 'refdata.v1.curve_segments.delete',
    delete_many_curve_segments_request: 'refdata.v1.curve_segments.delete_many',
    list_curve_segment_versions_request: 'refdata.v1.curve_segments_versions.list',
    get_curve_segment_version_request: 'refdata.v1.curve_segments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_segments_request: true,
    get_curve_segment_request: true,
    get_many_curve_segments_request: true,
    put_curve_segment_request: true,
    put_many_curve_segments_request: true,
    delete_curve_segment_request: true,
    delete_many_curve_segments_request: true,
    list_curve_segment_versions_request: true,
    get_curve_segment_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.curve_segments_events.created',
    updated: 'refdata.v1.curve_segments_events.updated',
    deleted: 'refdata.v1.curve_segments_events.deleted',
} as const;
