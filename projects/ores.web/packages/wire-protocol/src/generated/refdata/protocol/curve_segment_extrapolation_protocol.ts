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
import type { CurveSegmentExtrapolation } from '../domain/curve_segment_extrapolation.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveSegmentExtrapolationKey {
    id: string;
}

export interface CurveSegmentExtrapolationWrite {
    id: string;
    curve_segment_id: string;
    extrapolate_flat: boolean;
}

export interface CurveSegmentExtrapolationChange {
    write: CurveSegmentExtrapolationWrite;
    precondition: Precondition;
}

export interface CurveSegmentExtrapolationRemoval {
    key: CurveSegmentExtrapolationKey;
    precondition: Precondition;
}

export interface CurveSegmentExtrapolationLookup {
    key: CurveSegmentExtrapolationKey;
    curve_segment_extrapolation: CurveSegmentExtrapolation | null;
}

export interface CurveSegmentExtrapolationsFilter {
    id_one_of: string[] | null;
}

export interface CurveSegmentExtrapolationEvent {
    event_id: string;
    key: CurveSegmentExtrapolationKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveSegmentExtrapolationVersionKey {
    curve_segment_extrapolation: CurveSegmentExtrapolationKey;
    version: number;
}

export interface CurveSegmentExtrapolationVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveSegmentExtrapolationsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CurveSegmentExtrapolationsFilter | null;
    as_of: string | null;
}

export interface ListCurveSegmentExtrapolationsResponse {
    result: Result;
    segment_extrapolations: CurveSegmentExtrapolation[];
    total: number;
}

export interface GetCurveSegmentExtrapolationRequest {
    key: CurveSegmentExtrapolationKey;
}

export interface GetCurveSegmentExtrapolationResponse {
    result: Result;
    curve_segment_extrapolation: CurveSegmentExtrapolation | null;
}

export interface GetManyCurveSegmentExtrapolationsRequest {
    keys: CurveSegmentExtrapolationKey[];
}

export interface GetManyCurveSegmentExtrapolationsResponse {
    result: Result;
    entries: CurveSegmentExtrapolationLookup[];
}

export interface PutCurveSegmentExtrapolationRequest {
    change: CurveSegmentExtrapolationChange;
    intent: ChangeIntent;
}

export interface PutCurveSegmentExtrapolationResponse {
    result: Result;
    curve_segment_extrapolation: CurveSegmentExtrapolation | null;
}

export interface PutManyCurveSegmentExtrapolationsRequest {
    changes: CurveSegmentExtrapolationChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveSegmentExtrapolationsResponse {
    result: Result;
    segment_extrapolations: CurveSegmentExtrapolation[];
}

export interface DeleteCurveSegmentExtrapolationRequest {
    removal: CurveSegmentExtrapolationRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveSegmentExtrapolationResponse {
    result: Result;
}

export interface DeleteManyCurveSegmentExtrapolationsRequest {
    removals: CurveSegmentExtrapolationRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveSegmentExtrapolationsResponse {
    result: Result;
}

export interface ListCurveSegmentExtrapolationVersionsRequest {
    key: CurveSegmentExtrapolationKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveSegmentExtrapolationVersionsFilter | null;
}

export interface ListCurveSegmentExtrapolationVersionsResponse {
    result: Result;
    versions: CurveSegmentExtrapolation[];
    total: number;
}

export interface GetCurveSegmentExtrapolationVersionRequest {
    key: CurveSegmentExtrapolationVersionKey;
}

export interface GetCurveSegmentExtrapolationVersionResponse {
    result: Result;
    version: CurveSegmentExtrapolation | null;
}

export const subjects = {
    list_curve_segment_extrapolations_request: 'refdata.v1.curve_segment_extrapolations.list',
    get_curve_segment_extrapolation_request: 'refdata.v1.curve_segment_extrapolations.get',
    get_many_curve_segment_extrapolations_request:
        'refdata.v1.curve_segment_extrapolations.get_many',
    put_curve_segment_extrapolation_request: 'refdata.v1.curve_segment_extrapolations.put',
    put_many_curve_segment_extrapolations_request:
        'refdata.v1.curve_segment_extrapolations.put_many',
    delete_curve_segment_extrapolation_request: 'refdata.v1.curve_segment_extrapolations.delete',
    delete_many_curve_segment_extrapolations_request:
        'refdata.v1.curve_segment_extrapolations.delete_many',
    list_curve_segment_extrapolation_versions_request:
        'refdata.v1.curve_segment_extrapolations_versions.list',
    get_curve_segment_extrapolation_version_request:
        'refdata.v1.curve_segment_extrapolations_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_segment_extrapolations_request: true,
    get_curve_segment_extrapolation_request: true,
    get_many_curve_segment_extrapolations_request: true,
    put_curve_segment_extrapolation_request: true,
    put_many_curve_segment_extrapolations_request: true,
    delete_curve_segment_extrapolation_request: true,
    delete_many_curve_segment_extrapolations_request: true,
    list_curve_segment_extrapolation_versions_request: true,
    get_curve_segment_extrapolation_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.curve_segment_extrapolations_events.created',
    updated: 'refdata.v1.curve_segment_extrapolations_events.updated',
    deleted: 'refdata.v1.curve_segment_extrapolations_events.deleted',
} as const;
