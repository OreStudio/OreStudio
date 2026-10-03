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
import type { InflationCurve } from '../domain/inflation_curve.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface InflationCurveKey {
    id: string;
}

export interface InflationCurveWrite {
    id: string;
    curve_definition_id: string;
    nominal_term_structure: string;
    inflation_type: string;
    conventions: string | null;
    has_quotes: boolean;
    extrapolation: string | null;
    calendar: string;
    day_counter: string | null;
    lag: string;
    frequency: string;
    base_rate: string | null;
    tolerance: number | null;
    has_seasonality: boolean;
    seasonality_base_date: string | null;
    seasonality_frequency: string | null;
    use_last_fixing_date: string | null;
    interpolation_variable: string | null;
    interpolation_method: string | null;
}

export interface InflationCurveChange {
    write: InflationCurveWrite;
    precondition: Precondition;
}

export interface InflationCurveRemoval {
    key: InflationCurveKey;
    precondition: Precondition;
}

export interface InflationCurveLookup {
    key: InflationCurveKey;
    inflation_curve: InflationCurve | null;
}

export interface InflationCurveEvent {
    event_id: string;
    key: InflationCurveKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface InflationCurveVersionKey {
    inflation_curve: InflationCurveKey;
    version: number;
}

export interface InflationCurveVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListInflationCurvesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListInflationCurvesResponse {
    result: Result;
    inflation_curves: InflationCurve[];
    total: number;
}

export interface GetInflationCurveRequest {
    key: InflationCurveKey;
}

export interface GetInflationCurveResponse {
    result: Result;
    inflation_curve: InflationCurve | null;
}

export interface GetManyInflationCurvesRequest {
    keys: InflationCurveKey[];
}

export interface GetManyInflationCurvesResponse {
    result: Result;
    entries: InflationCurveLookup[];
}

export interface PutInflationCurveRequest {
    change: InflationCurveChange;
    intent: ChangeIntent;
}

export interface PutInflationCurveResponse {
    result: Result;
    inflation_curve: InflationCurve | null;
}

export interface PutManyInflationCurvesRequest {
    changes: InflationCurveChange[];
    intent: ChangeIntent;
}

export interface PutManyInflationCurvesResponse {
    result: Result;
    inflation_curves: InflationCurve[];
}

export interface DeleteInflationCurveRequest {
    removal: InflationCurveRemoval;
    intent: ChangeIntent;
}

export interface DeleteInflationCurveResponse {
    result: Result;
}

export interface DeleteManyInflationCurvesRequest {
    removals: InflationCurveRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyInflationCurvesResponse {
    result: Result;
}

export interface ListInflationCurveVersionsRequest {
    key: InflationCurveKey;
    offset: number;
    limit: number;
    order: Order;
    filter: InflationCurveVersionsFilter | null;
}

export interface ListInflationCurveVersionsResponse {
    result: Result;
    versions: InflationCurve[];
    total: number;
}

export interface GetInflationCurveVersionRequest {
    key: InflationCurveVersionKey;
}

export interface GetInflationCurveVersionResponse {
    result: Result;
    version: InflationCurve | null;
}

export const subjects = {
    list_inflation_curves_request: 'refdata.v1.inflation_curves.list',
    get_inflation_curve_request: 'refdata.v1.inflation_curves.get',
    get_many_inflation_curves_request: 'refdata.v1.inflation_curves.get_many',
    put_inflation_curve_request: 'refdata.v1.inflation_curves.put',
    put_many_inflation_curves_request: 'refdata.v1.inflation_curves.put_many',
    delete_inflation_curve_request: 'refdata.v1.inflation_curves.delete',
    delete_many_inflation_curves_request: 'refdata.v1.inflation_curves.delete_many',
    list_inflation_curve_versions_request: 'refdata.v1.inflation_curves_versions.list',
    get_inflation_curve_version_request: 'refdata.v1.inflation_curves_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_inflation_curves_request: true,
    get_inflation_curve_request: true,
    get_many_inflation_curves_request: true,
    put_inflation_curve_request: true,
    put_many_inflation_curves_request: true,
    delete_inflation_curve_request: true,
    delete_many_inflation_curves_request: true,
    list_inflation_curve_versions_request: true,
    get_inflation_curve_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.inflation_curves_events.created',
    updated: 'refdata.v1.inflation_curves_events.updated',
    deleted: 'refdata.v1.inflation_curves_events.deleted',
} as const;
