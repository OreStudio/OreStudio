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
import type { CurveVolatilityDeltaSurface } from '../domain/curve_volatility_delta_surface.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveVolatilityDeltaSurfaceKey {
    id: string;
}

export interface CurveVolatilityDeltaSurfaceWrite {
    id: string;
    curve_volatility_kind_id: string;
    delta_type: string;
    atm_type: string;
    atm_delta_type: string | null;
    put_deltas: string;
    call_deltas: string;
    expiries: string;
    time_interpolation: string;
    strike_interpolation: string;
    extrapolation: boolean;
    time_extrapolation: string;
    time_extrapolation_variance: boolean | null;
    strike_extrapolation: string;
    future_price_correction: boolean | null;
}

export interface CurveVolatilityDeltaSurfaceChange {
    write: CurveVolatilityDeltaSurfaceWrite;
    precondition: Precondition;
}

export interface CurveVolatilityDeltaSurfaceRemoval {
    key: CurveVolatilityDeltaSurfaceKey;
    precondition: Precondition;
}

export interface CurveVolatilityDeltaSurfaceLookup {
    key: CurveVolatilityDeltaSurfaceKey;
    curve_volatility_delta_surface: CurveVolatilityDeltaSurface | null;
}

export interface CurveVolatilityDeltaSurfacesFilter {
    id_one_of: string[] | null;
}

export interface CurveVolatilityDeltaSurfaceEvent {
    event_id: string;
    key: CurveVolatilityDeltaSurfaceKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveVolatilityDeltaSurfaceVersionKey {
    curve_volatility_delta_surface: CurveVolatilityDeltaSurfaceKey;
    version: number;
}

export interface CurveVolatilityDeltaSurfaceVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveVolatilityDeltaSurfacesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CurveVolatilityDeltaSurfacesFilter | null;
    as_of: string | null;
}

export interface ListCurveVolatilityDeltaSurfacesResponse {
    result: Result;
    curve_volatility_delta_surfaces: CurveVolatilityDeltaSurface[];
    total: number;
}

export interface GetCurveVolatilityDeltaSurfaceRequest {
    key: CurveVolatilityDeltaSurfaceKey;
}

export interface GetCurveVolatilityDeltaSurfaceResponse {
    result: Result;
    curve_volatility_delta_surface: CurveVolatilityDeltaSurface | null;
}

export interface GetManyCurveVolatilityDeltaSurfacesRequest {
    keys: CurveVolatilityDeltaSurfaceKey[];
}

export interface GetManyCurveVolatilityDeltaSurfacesResponse {
    result: Result;
    entries: CurveVolatilityDeltaSurfaceLookup[];
}

export interface PutCurveVolatilityDeltaSurfaceRequest {
    change: CurveVolatilityDeltaSurfaceChange;
    intent: ChangeIntent;
}

export interface PutCurveVolatilityDeltaSurfaceResponse {
    result: Result;
    curve_volatility_delta_surface: CurveVolatilityDeltaSurface | null;
}

export interface PutManyCurveVolatilityDeltaSurfacesRequest {
    changes: CurveVolatilityDeltaSurfaceChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveVolatilityDeltaSurfacesResponse {
    result: Result;
    curve_volatility_delta_surfaces: CurveVolatilityDeltaSurface[];
}

export interface DeleteCurveVolatilityDeltaSurfaceRequest {
    removal: CurveVolatilityDeltaSurfaceRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveVolatilityDeltaSurfaceResponse {
    result: Result;
}

export interface DeleteManyCurveVolatilityDeltaSurfacesRequest {
    removals: CurveVolatilityDeltaSurfaceRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveVolatilityDeltaSurfacesResponse {
    result: Result;
}

export interface ListCurveVolatilityDeltaSurfaceVersionsRequest {
    key: CurveVolatilityDeltaSurfaceKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveVolatilityDeltaSurfaceVersionsFilter | null;
}

export interface ListCurveVolatilityDeltaSurfaceVersionsResponse {
    result: Result;
    versions: CurveVolatilityDeltaSurface[];
    total: number;
}

export interface GetCurveVolatilityDeltaSurfaceVersionRequest {
    key: CurveVolatilityDeltaSurfaceVersionKey;
}

export interface GetCurveVolatilityDeltaSurfaceVersionResponse {
    result: Result;
    version: CurveVolatilityDeltaSurface | null;
}

export const subjects = {
    list_curve_volatility_delta_surfaces_request: 'refdata.v1.curve_volatility_delta_surfaces.list',
    get_curve_volatility_delta_surface_request: 'refdata.v1.curve_volatility_delta_surfaces.get',
    get_many_curve_volatility_delta_surfaces_request:
        'refdata.v1.curve_volatility_delta_surfaces.get_many',
    put_curve_volatility_delta_surface_request: 'refdata.v1.curve_volatility_delta_surfaces.put',
    put_many_curve_volatility_delta_surfaces_request:
        'refdata.v1.curve_volatility_delta_surfaces.put_many',
    delete_curve_volatility_delta_surface_request:
        'refdata.v1.curve_volatility_delta_surfaces.delete',
    delete_many_curve_volatility_delta_surfaces_request:
        'refdata.v1.curve_volatility_delta_surfaces.delete_many',
    list_curve_volatility_delta_surface_versions_request:
        'refdata.v1.curve_volatility_delta_surfaces_versions.list',
    get_curve_volatility_delta_surface_version_request:
        'refdata.v1.curve_volatility_delta_surfaces_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_volatility_delta_surfaces_request: true,
    get_curve_volatility_delta_surface_request: true,
    get_many_curve_volatility_delta_surfaces_request: true,
    put_curve_volatility_delta_surface_request: true,
    put_many_curve_volatility_delta_surfaces_request: true,
    delete_curve_volatility_delta_surface_request: true,
    delete_many_curve_volatility_delta_surfaces_request: true,
    list_curve_volatility_delta_surface_versions_request: true,
    get_curve_volatility_delta_surface_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.curve_volatility_delta_surfaces_events.created',
    updated: 'refdata.v1.curve_volatility_delta_surfaces_events.updated',
    deleted: 'refdata.v1.curve_volatility_delta_surfaces_events.deleted',
} as const;
