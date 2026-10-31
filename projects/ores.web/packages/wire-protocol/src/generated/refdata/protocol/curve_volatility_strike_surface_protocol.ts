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
import type { CurveVolatilityStrikeSurface } from '../domain/curve_volatility_strike_surface.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveVolatilityStrikeSurfaceKey {
    id: string;
}

export interface CurveVolatilityStrikeSurfaceWrite {
    id: string;
    curve_volatility_kind_id: string;
    strikes: string;
    expiries: string;
    time_interpolation: string;
    strike_interpolation: string;
    extrapolation: boolean;
    time_extrapolation: string;
    time_extrapolation_variance: boolean | null;
    strike_extrapolation: string;
}

export interface CurveVolatilityStrikeSurfaceChange {
    write: CurveVolatilityStrikeSurfaceWrite;
    precondition: Precondition;
}

export interface CurveVolatilityStrikeSurfaceRemoval {
    key: CurveVolatilityStrikeSurfaceKey;
    precondition: Precondition;
}

export interface CurveVolatilityStrikeSurfaceLookup {
    key: CurveVolatilityStrikeSurfaceKey;
    curve_volatility_strike_surface: CurveVolatilityStrikeSurface | null;
}

export interface CurveVolatilityStrikeSurfacesFilter {
    id_one_of: string[] | null;
}

export interface CurveVolatilityStrikeSurfaceEvent {
    event_id: string;
    key: CurveVolatilityStrikeSurfaceKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveVolatilityStrikeSurfaceVersionKey {
    curve_volatility_strike_surface: CurveVolatilityStrikeSurfaceKey;
    version: number;
}

export interface CurveVolatilityStrikeSurfaceVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveVolatilityStrikeSurfacesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CurveVolatilityStrikeSurfacesFilter | null;
    as_of: string | null;
}

export interface ListCurveVolatilityStrikeSurfacesResponse {
    result: Result;
    curve_volatility_strike_surfaces: CurveVolatilityStrikeSurface[];
    total: number;
}

export interface GetCurveVolatilityStrikeSurfaceRequest {
    key: CurveVolatilityStrikeSurfaceKey;
}

export interface GetCurveVolatilityStrikeSurfaceResponse {
    result: Result;
    curve_volatility_strike_surface: CurveVolatilityStrikeSurface | null;
}

export interface GetManyCurveVolatilityStrikeSurfacesRequest {
    keys: CurveVolatilityStrikeSurfaceKey[];
}

export interface GetManyCurveVolatilityStrikeSurfacesResponse {
    result: Result;
    entries: CurveVolatilityStrikeSurfaceLookup[];
}

export interface PutCurveVolatilityStrikeSurfaceRequest {
    change: CurveVolatilityStrikeSurfaceChange;
    intent: ChangeIntent;
}

export interface PutCurveVolatilityStrikeSurfaceResponse {
    result: Result;
    curve_volatility_strike_surface: CurveVolatilityStrikeSurface | null;
}

export interface PutManyCurveVolatilityStrikeSurfacesRequest {
    changes: CurveVolatilityStrikeSurfaceChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveVolatilityStrikeSurfacesResponse {
    result: Result;
    curve_volatility_strike_surfaces: CurveVolatilityStrikeSurface[];
}

export interface DeleteCurveVolatilityStrikeSurfaceRequest {
    removal: CurveVolatilityStrikeSurfaceRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveVolatilityStrikeSurfaceResponse {
    result: Result;
}

export interface DeleteManyCurveVolatilityStrikeSurfacesRequest {
    removals: CurveVolatilityStrikeSurfaceRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveVolatilityStrikeSurfacesResponse {
    result: Result;
}

export interface ListCurveVolatilityStrikeSurfaceVersionsRequest {
    key: CurveVolatilityStrikeSurfaceKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveVolatilityStrikeSurfaceVersionsFilter | null;
}

export interface ListCurveVolatilityStrikeSurfaceVersionsResponse {
    result: Result;
    versions: CurveVolatilityStrikeSurface[];
    total: number;
}

export interface GetCurveVolatilityStrikeSurfaceVersionRequest {
    key: CurveVolatilityStrikeSurfaceVersionKey;
}

export interface GetCurveVolatilityStrikeSurfaceVersionResponse {
    result: Result;
    version: CurveVolatilityStrikeSurface | null;
}

export const subjects = {
    list_curve_volatility_strike_surfaces_request:
        'refdata.v1.curve_volatility_strike_surfaces.list',
    get_curve_volatility_strike_surface_request: 'refdata.v1.curve_volatility_strike_surfaces.get',
    get_many_curve_volatility_strike_surfaces_request:
        'refdata.v1.curve_volatility_strike_surfaces.get_many',
    put_curve_volatility_strike_surface_request: 'refdata.v1.curve_volatility_strike_surfaces.put',
    put_many_curve_volatility_strike_surfaces_request:
        'refdata.v1.curve_volatility_strike_surfaces.put_many',
    delete_curve_volatility_strike_surface_request:
        'refdata.v1.curve_volatility_strike_surfaces.delete',
    delete_many_curve_volatility_strike_surfaces_request:
        'refdata.v1.curve_volatility_strike_surfaces.delete_many',
    list_curve_volatility_strike_surface_versions_request:
        'refdata.v1.curve_volatility_strike_surfaces_versions.list',
    get_curve_volatility_strike_surface_version_request:
        'refdata.v1.curve_volatility_strike_surfaces_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_volatility_strike_surfaces_request: true,
    get_curve_volatility_strike_surface_request: true,
    get_many_curve_volatility_strike_surfaces_request: true,
    put_curve_volatility_strike_surface_request: true,
    put_many_curve_volatility_strike_surfaces_request: true,
    delete_curve_volatility_strike_surface_request: true,
    delete_many_curve_volatility_strike_surfaces_request: true,
    list_curve_volatility_strike_surface_versions_request: true,
    get_curve_volatility_strike_surface_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.curve_volatility_strike_surfaces_events.created',
    updated: 'refdata.v1.curve_volatility_strike_surfaces_events.updated',
    deleted: 'refdata.v1.curve_volatility_strike_surfaces_events.deleted',
} as const;
