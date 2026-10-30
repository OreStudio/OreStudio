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
import type { CurveVolatilityCurve } from '../domain/curve_volatility_curve.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveVolatilityCurveKey {
    id: string;
}

export interface CurveVolatilityCurveWrite {
    id: string;
    curve_volatility_kind_id: string;
    interpolation: string;
    extrapolation: string;
    enforce_monotone_variance: boolean | null;
}

export interface CurveVolatilityCurveChange {
    write: CurveVolatilityCurveWrite;
    precondition: Precondition;
}

export interface CurveVolatilityCurveRemoval {
    key: CurveVolatilityCurveKey;
    precondition: Precondition;
}

export interface CurveVolatilityCurveLookup {
    key: CurveVolatilityCurveKey;
    curve_volatility_curve: CurveVolatilityCurve | null;
}

export interface CurveVolatilityCurvesFilter {
    id_one_of: string[] | null;
}

export interface CurveVolatilityCurveEvent {
    event_id: string;
    key: CurveVolatilityCurveKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveVolatilityCurveVersionKey {
    curve_volatility_curve: CurveVolatilityCurveKey;
    version: number;
}

export interface CurveVolatilityCurveVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveVolatilityCurvesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CurveVolatilityCurvesFilter | null;
    as_of: string | null;
}

export interface ListCurveVolatilityCurvesResponse {
    result: Result;
    curve_volatility_curves: CurveVolatilityCurve[];
    total: number;
}

export interface GetCurveVolatilityCurveRequest {
    key: CurveVolatilityCurveKey;
}

export interface GetCurveVolatilityCurveResponse {
    result: Result;
    curve_volatility_curve: CurveVolatilityCurve | null;
}

export interface GetManyCurveVolatilityCurvesRequest {
    keys: CurveVolatilityCurveKey[];
}

export interface GetManyCurveVolatilityCurvesResponse {
    result: Result;
    entries: CurveVolatilityCurveLookup[];
}

export interface PutCurveVolatilityCurveRequest {
    change: CurveVolatilityCurveChange;
    intent: ChangeIntent;
}

export interface PutCurveVolatilityCurveResponse {
    result: Result;
    curve_volatility_curve: CurveVolatilityCurve | null;
}

export interface PutManyCurveVolatilityCurvesRequest {
    changes: CurveVolatilityCurveChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveVolatilityCurvesResponse {
    result: Result;
    curve_volatility_curves: CurveVolatilityCurve[];
}

export interface DeleteCurveVolatilityCurveRequest {
    removal: CurveVolatilityCurveRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveVolatilityCurveResponse {
    result: Result;
}

export interface DeleteManyCurveVolatilityCurvesRequest {
    removals: CurveVolatilityCurveRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveVolatilityCurvesResponse {
    result: Result;
}

export interface ListCurveVolatilityCurveVersionsRequest {
    key: CurveVolatilityCurveKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveVolatilityCurveVersionsFilter | null;
}

export interface ListCurveVolatilityCurveVersionsResponse {
    result: Result;
    versions: CurveVolatilityCurve[];
    total: number;
}

export interface GetCurveVolatilityCurveVersionRequest {
    key: CurveVolatilityCurveVersionKey;
}

export interface GetCurveVolatilityCurveVersionResponse {
    result: Result;
    version: CurveVolatilityCurve | null;
}

export const subjects = {
    list_curve_volatility_curves_request: 'refdata.v1.curve_volatility_curves.list',
    get_curve_volatility_curve_request: 'refdata.v1.curve_volatility_curves.get',
    get_many_curve_volatility_curves_request: 'refdata.v1.curve_volatility_curves.get_many',
    put_curve_volatility_curve_request: 'refdata.v1.curve_volatility_curves.put',
    put_many_curve_volatility_curves_request: 'refdata.v1.curve_volatility_curves.put_many',
    delete_curve_volatility_curve_request: 'refdata.v1.curve_volatility_curves.delete',
    delete_many_curve_volatility_curves_request: 'refdata.v1.curve_volatility_curves.delete_many',
    list_curve_volatility_curve_versions_request:
        'refdata.v1.curve_volatility_curves_versions.list',
    get_curve_volatility_curve_version_request: 'refdata.v1.curve_volatility_curves_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_volatility_curves_request: true,
    get_curve_volatility_curve_request: true,
    get_many_curve_volatility_curves_request: true,
    put_curve_volatility_curve_request: true,
    put_many_curve_volatility_curves_request: true,
    delete_curve_volatility_curve_request: true,
    delete_many_curve_volatility_curves_request: true,
    list_curve_volatility_curve_versions_request: true,
    get_curve_volatility_curve_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.curve_volatility_curves_events.created',
    updated: 'refdata.v1.curve_volatility_curves_events.updated',
    deleted: 'refdata.v1.curve_volatility_curves_events.deleted',
} as const;
