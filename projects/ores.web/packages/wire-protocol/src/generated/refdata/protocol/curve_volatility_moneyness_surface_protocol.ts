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
import type { CurveVolatilityMoneynessSurface } from '../domain/curve_volatility_moneyness_surface.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveVolatilityMoneynessSurfaceKey {
    id: string;
}

export interface CurveVolatilityMoneynessSurfaceWrite {
    id: string;
    curve_volatility_kind_id: string;
    moneyness_type: string;
    moneyness_levels: string;
    expiries: string;
    time_interpolation: string;
    strike_interpolation: string;
    extrapolation: boolean;
    time_extrapolation: string;
    time_extrapolation_variance: boolean | null;
    strike_extrapolation: string;
    future_price_correction: boolean | null;
}

export interface CurveVolatilityMoneynessSurfaceChange {
    write: CurveVolatilityMoneynessSurfaceWrite;
    precondition: Precondition;
}

export interface CurveVolatilityMoneynessSurfaceRemoval {
    key: CurveVolatilityMoneynessSurfaceKey;
    precondition: Precondition;
}

export interface CurveVolatilityMoneynessSurfaceLookup {
    key: CurveVolatilityMoneynessSurfaceKey;
    curve_volatility_moneyness_surface: CurveVolatilityMoneynessSurface | null;
}

export interface CurveVolatilityMoneynessSurfacesFilter {
    id_one_of: string[] | null;
}

export interface CurveVolatilityMoneynessSurfaceEvent {
    event_id: string;
    key: CurveVolatilityMoneynessSurfaceKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveVolatilityMoneynessSurfaceVersionKey {
    curve_volatility_moneyness_surface: CurveVolatilityMoneynessSurfaceKey;
    version: number;
}

export interface CurveVolatilityMoneynessSurfaceVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveVolatilityMoneynessSurfacesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CurveVolatilityMoneynessSurfacesFilter | null;
    as_of: string | null;
}

export interface ListCurveVolatilityMoneynessSurfacesResponse {
    result: Result;
    curve_volatility_moneyness_surfaces: CurveVolatilityMoneynessSurface[];
    total: number;
}

export interface GetCurveVolatilityMoneynessSurfaceRequest {
    key: CurveVolatilityMoneynessSurfaceKey;
}

export interface GetCurveVolatilityMoneynessSurfaceResponse {
    result: Result;
    curve_volatility_moneyness_surface: CurveVolatilityMoneynessSurface | null;
}

export interface GetManyCurveVolatilityMoneynessSurfacesRequest {
    keys: CurveVolatilityMoneynessSurfaceKey[];
}

export interface GetManyCurveVolatilityMoneynessSurfacesResponse {
    result: Result;
    entries: CurveVolatilityMoneynessSurfaceLookup[];
}

export interface PutCurveVolatilityMoneynessSurfaceRequest {
    change: CurveVolatilityMoneynessSurfaceChange;
    intent: ChangeIntent;
}

export interface PutCurveVolatilityMoneynessSurfaceResponse {
    result: Result;
    curve_volatility_moneyness_surface: CurveVolatilityMoneynessSurface | null;
}

export interface PutManyCurveVolatilityMoneynessSurfacesRequest {
    changes: CurveVolatilityMoneynessSurfaceChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveVolatilityMoneynessSurfacesResponse {
    result: Result;
    curve_volatility_moneyness_surfaces: CurveVolatilityMoneynessSurface[];
}

export interface DeleteCurveVolatilityMoneynessSurfaceRequest {
    removal: CurveVolatilityMoneynessSurfaceRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveVolatilityMoneynessSurfaceResponse {
    result: Result;
}

export interface DeleteManyCurveVolatilityMoneynessSurfacesRequest {
    removals: CurveVolatilityMoneynessSurfaceRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveVolatilityMoneynessSurfacesResponse {
    result: Result;
}

export interface ListCurveVolatilityMoneynessSurfaceVersionsRequest {
    key: CurveVolatilityMoneynessSurfaceKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveVolatilityMoneynessSurfaceVersionsFilter | null;
}

export interface ListCurveVolatilityMoneynessSurfaceVersionsResponse {
    result: Result;
    versions: CurveVolatilityMoneynessSurface[];
    total: number;
}

export interface GetCurveVolatilityMoneynessSurfaceVersionRequest {
    key: CurveVolatilityMoneynessSurfaceVersionKey;
}

export interface GetCurveVolatilityMoneynessSurfaceVersionResponse {
    result: Result;
    version: CurveVolatilityMoneynessSurface | null;
}

export const subjects = {
    list_curve_volatility_moneyness_surfaces_request:
        'refdata.v1.curve_volatility_moneyness_surfaces.list',
    get_curve_volatility_moneyness_surface_request:
        'refdata.v1.curve_volatility_moneyness_surfaces.get',
    get_many_curve_volatility_moneyness_surfaces_request:
        'refdata.v1.curve_volatility_moneyness_surfaces.get_many',
    put_curve_volatility_moneyness_surface_request:
        'refdata.v1.curve_volatility_moneyness_surfaces.put',
    put_many_curve_volatility_moneyness_surfaces_request:
        'refdata.v1.curve_volatility_moneyness_surfaces.put_many',
    delete_curve_volatility_moneyness_surface_request:
        'refdata.v1.curve_volatility_moneyness_surfaces.delete',
    delete_many_curve_volatility_moneyness_surfaces_request:
        'refdata.v1.curve_volatility_moneyness_surfaces.delete_many',
    list_curve_volatility_moneyness_surface_versions_request:
        'refdata.v1.curve_volatility_moneyness_surfaces_versions.list',
    get_curve_volatility_moneyness_surface_version_request:
        'refdata.v1.curve_volatility_moneyness_surfaces_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_volatility_moneyness_surfaces_request: true,
    get_curve_volatility_moneyness_surface_request: true,
    get_many_curve_volatility_moneyness_surfaces_request: true,
    put_curve_volatility_moneyness_surface_request: true,
    put_many_curve_volatility_moneyness_surfaces_request: true,
    delete_curve_volatility_moneyness_surface_request: true,
    delete_many_curve_volatility_moneyness_surfaces_request: true,
    list_curve_volatility_moneyness_surface_versions_request: true,
    get_curve_volatility_moneyness_surface_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.curve_volatility_moneyness_surfaces_events.created',
    updated: 'refdata.v1.curve_volatility_moneyness_surfaces_events.updated',
    deleted: 'refdata.v1.curve_volatility_moneyness_surfaces_events.deleted',
} as const;
