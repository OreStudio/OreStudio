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
import type { CurveSegmentTenorBasisCurves } from '../domain/curve_segment_tenor_basis_curves.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveSegmentTenorBasisCurvesKey {
    id: string;
}

export interface CurveSegmentTenorBasisCurvesWrite {
    id: string;
    curve_segment_id: string;
    projection_curve_pay: string | null;
    projection_curve_receive: string | null;
    projection_curve_long: string | null;
    projection_curve_short: string | null;
}

export interface CurveSegmentTenorBasisCurvesChange {
    write: CurveSegmentTenorBasisCurvesWrite;
    precondition: Precondition;
}

export interface CurveSegmentTenorBasisCurvesRemoval {
    key: CurveSegmentTenorBasisCurvesKey;
    precondition: Precondition;
}

export interface CurveSegmentTenorBasisCurvesLookup {
    key: CurveSegmentTenorBasisCurvesKey;
    curve_segment_tenor_basis_curves: CurveSegmentTenorBasisCurves | null;
}

export interface CurveSegmentTenorBasisCurvesFilter {
    id_one_of: string[] | null;
}

export interface CurveSegmentTenorBasisCurvesEvent {
    event_id: string;
    key: CurveSegmentTenorBasisCurvesKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveSegmentTenorBasisCurvesVersionKey {
    curve_segment_tenor_basis_curves: CurveSegmentTenorBasisCurvesKey;
    version: number;
}

export interface CurveSegmentTenorBasisCurvesVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveSegmentTenorBasisCurvesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CurveSegmentTenorBasisCurvesFilter | null;
    as_of: string | null;
}

export interface ListCurveSegmentTenorBasisCurvesResponse {
    result: Result;
    segment_tenor_basis_curves: CurveSegmentTenorBasisCurves[];
    total: number;
}

export interface GetCurveSegmentTenorBasisCurvesRequest {
    key: CurveSegmentTenorBasisCurvesKey;
}

export interface GetCurveSegmentTenorBasisCurvesResponse {
    result: Result;
    curve_segment_tenor_basis_curves: CurveSegmentTenorBasisCurves | null;
}

export interface GetManyCurveSegmentTenorBasisCurvesRequest {
    keys: CurveSegmentTenorBasisCurvesKey[];
}

export interface GetManyCurveSegmentTenorBasisCurvesResponse {
    result: Result;
    entries: CurveSegmentTenorBasisCurvesLookup[];
}

export interface PutCurveSegmentTenorBasisCurvesRequest {
    change: CurveSegmentTenorBasisCurvesChange;
    intent: ChangeIntent;
}

export interface PutCurveSegmentTenorBasisCurvesResponse {
    result: Result;
    curve_segment_tenor_basis_curves: CurveSegmentTenorBasisCurves | null;
}

export interface PutManyCurveSegmentTenorBasisCurvesRequest {
    changes: CurveSegmentTenorBasisCurvesChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveSegmentTenorBasisCurvesResponse {
    result: Result;
    segment_tenor_basis_curves: CurveSegmentTenorBasisCurves[];
}

export interface DeleteCurveSegmentTenorBasisCurvesRequest {
    removal: CurveSegmentTenorBasisCurvesRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveSegmentTenorBasisCurvesResponse {
    result: Result;
}

export interface DeleteManyCurveSegmentTenorBasisCurvesRequest {
    removals: CurveSegmentTenorBasisCurvesRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveSegmentTenorBasisCurvesResponse {
    result: Result;
}

export interface ListCurveSegmentTenorBasisCurvesVersionsRequest {
    key: CurveSegmentTenorBasisCurvesKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveSegmentTenorBasisCurvesVersionsFilter | null;
}

export interface ListCurveSegmentTenorBasisCurvesVersionsResponse {
    result: Result;
    versions: CurveSegmentTenorBasisCurves[];
    total: number;
}

export interface GetCurveSegmentTenorBasisCurvesVersionRequest {
    key: CurveSegmentTenorBasisCurvesVersionKey;
}

export interface GetCurveSegmentTenorBasisCurvesVersionResponse {
    result: Result;
    version: CurveSegmentTenorBasisCurves | null;
}

export const subjects = {
    list_curve_segment_tenor_basis_curves_request:
        'refdata.v1.curve_segment_tenor_basis_curves.list',
    get_curve_segment_tenor_basis_curves_request: 'refdata.v1.curve_segment_tenor_basis_curves.get',
    get_many_curve_segment_tenor_basis_curves_request:
        'refdata.v1.curve_segment_tenor_basis_curves.get_many',
    put_curve_segment_tenor_basis_curves_request: 'refdata.v1.curve_segment_tenor_basis_curves.put',
    put_many_curve_segment_tenor_basis_curves_request:
        'refdata.v1.curve_segment_tenor_basis_curves.put_many',
    delete_curve_segment_tenor_basis_curves_request:
        'refdata.v1.curve_segment_tenor_basis_curves.delete',
    delete_many_curve_segment_tenor_basis_curves_request:
        'refdata.v1.curve_segment_tenor_basis_curves.delete_many',
    list_curve_segment_tenor_basis_curves_versions_request:
        'refdata.v1.curve_segment_tenor_basis_curves_versions.list',
    get_curve_segment_tenor_basis_curves_version_request:
        'refdata.v1.curve_segment_tenor_basis_curves_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_segment_tenor_basis_curves_request: true,
    get_curve_segment_tenor_basis_curves_request: true,
    get_many_curve_segment_tenor_basis_curves_request: true,
    put_curve_segment_tenor_basis_curves_request: true,
    put_many_curve_segment_tenor_basis_curves_request: true,
    delete_curve_segment_tenor_basis_curves_request: true,
    delete_many_curve_segment_tenor_basis_curves_request: true,
    list_curve_segment_tenor_basis_curves_versions_request: true,
    get_curve_segment_tenor_basis_curves_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.events.curve_segment_tenor_basis_curves.created',
    updated: 'refdata.v1.events.curve_segment_tenor_basis_curves.updated',
    deleted: 'refdata.v1.events.curve_segment_tenor_basis_curves.deleted',
} as const;
