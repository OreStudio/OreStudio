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
import type { YieldCurve } from '../domain/yield_curve.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface YieldCurveKey {
    id: string;
}

export interface YieldCurveWrite {
    id: string;
    curve_definition_id: string;
    currency: string;
    discount_curve: string;
    interpolation_variable: string | null;
    interpolation_method: string | null;
    mixed_interpolation_cutoff: number | null;
    day_counter: string | null;
    tolerance: number | null;
    extrapolation: string | null;
    extrapolation_method: string | null;
    exclude_t0_from_interpolation: string | null;
    has_report: boolean;
    report_pillar_dates: string | null;
}

export interface YieldCurveChange {
    write: YieldCurveWrite;
    precondition: Precondition;
}

export interface YieldCurveRemoval {
    key: YieldCurveKey;
    precondition: Precondition;
}

export interface YieldCurveLookup {
    key: YieldCurveKey;
    yield_curve: YieldCurve | null;
}

export interface YieldCurveEvent {
    event_id: string;
    key: YieldCurveKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface YieldCurveVersionKey {
    yield_curve: YieldCurveKey;
    version: number;
}

export interface YieldCurveVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListYieldCurvesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListYieldCurvesResponse {
    result: Result;
    yield_curves: YieldCurve[];
    total: number;
}

export interface GetYieldCurveRequest {
    key: YieldCurveKey;
}

export interface GetYieldCurveResponse {
    result: Result;
    yield_curve: YieldCurve | null;
}

export interface GetManyYieldCurvesRequest {
    keys: YieldCurveKey[];
}

export interface GetManyYieldCurvesResponse {
    result: Result;
    entries: YieldCurveLookup[];
}

export interface PutYieldCurveRequest {
    change: YieldCurveChange;
    intent: ChangeIntent;
}

export interface PutYieldCurveResponse {
    result: Result;
    yield_curve: YieldCurve | null;
}

export interface PutManyYieldCurvesRequest {
    changes: YieldCurveChange[];
    intent: ChangeIntent;
}

export interface PutManyYieldCurvesResponse {
    result: Result;
    yield_curves: YieldCurve[];
}

export interface DeleteYieldCurveRequest {
    removal: YieldCurveRemoval;
    intent: ChangeIntent;
}

export interface DeleteYieldCurveResponse {
    result: Result;
}

export interface DeleteManyYieldCurvesRequest {
    removals: YieldCurveRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyYieldCurvesResponse {
    result: Result;
}

export interface ListYieldCurveVersionsRequest {
    key: YieldCurveKey;
    offset: number;
    limit: number;
    order: Order;
    filter: YieldCurveVersionsFilter | null;
}

export interface ListYieldCurveVersionsResponse {
    result: Result;
    versions: YieldCurve[];
    total: number;
}

export interface GetYieldCurveVersionRequest {
    key: YieldCurveVersionKey;
}

export interface GetYieldCurveVersionResponse {
    result: Result;
    version: YieldCurve | null;
}

export const subjects = {
    list_yield_curves_request: 'refdata.v1.yield_curves.list',
    get_yield_curve_request: 'refdata.v1.yield_curves.get',
    get_many_yield_curves_request: 'refdata.v1.yield_curves.get_many',
    put_yield_curve_request: 'refdata.v1.yield_curves.put',
    put_many_yield_curves_request: 'refdata.v1.yield_curves.put_many',
    delete_yield_curve_request: 'refdata.v1.yield_curves.delete',
    delete_many_yield_curves_request: 'refdata.v1.yield_curves.delete_many',
    list_yield_curve_versions_request: 'refdata.v1.yield_curves_versions.list',
    get_yield_curve_version_request: 'refdata.v1.yield_curves_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_yield_curves_request: true,
    get_yield_curve_request: true,
    get_many_yield_curves_request: true,
    put_yield_curve_request: true,
    put_many_yield_curves_request: true,
    delete_yield_curve_request: true,
    delete_many_yield_curves_request: true,
    list_yield_curve_versions_request: true,
    get_yield_curve_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.yield_curves_events.created',
    updated: 'refdata.v1.yield_curves_events.updated',
    deleted: 'refdata.v1.yield_curves_events.deleted',
} as const;
