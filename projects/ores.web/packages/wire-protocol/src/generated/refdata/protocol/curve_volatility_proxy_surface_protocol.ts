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
import type { CurveVolatilityProxySurface } from '../domain/curve_volatility_proxy_surface.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveVolatilityProxySurfaceKey {
    id: string;
}

export interface CurveVolatilityProxySurfaceWrite {
    id: string;
    curve_volatility_kind_id: string;
    proxy_volatility_curve: string;
    fx_volatility_curve: string | null;
    correlation_curve: string | null;
    cds_volatility_curve: string | null;
}

export interface CurveVolatilityProxySurfaceChange {
    write: CurveVolatilityProxySurfaceWrite;
    precondition: Precondition;
}

export interface CurveVolatilityProxySurfaceRemoval {
    key: CurveVolatilityProxySurfaceKey;
    precondition: Precondition;
}

export interface CurveVolatilityProxySurfaceLookup {
    key: CurveVolatilityProxySurfaceKey;
    curve_volatility_proxy_surface: CurveVolatilityProxySurface | null;
}

export interface CurveVolatilityProxySurfacesFilter {
    id_one_of: string[] | null;
}

export interface CurveVolatilityProxySurfaceEvent {
    event_id: string;
    key: CurveVolatilityProxySurfaceKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveVolatilityProxySurfaceVersionKey {
    curve_volatility_proxy_surface: CurveVolatilityProxySurfaceKey;
    version: number;
}

export interface CurveVolatilityProxySurfaceVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveVolatilityProxySurfacesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CurveVolatilityProxySurfacesFilter | null;
    as_of: string | null;
}

export interface ListCurveVolatilityProxySurfacesResponse {
    result: Result;
    curve_volatility_proxy_surfaces: CurveVolatilityProxySurface[];
    total: number;
}

export interface GetCurveVolatilityProxySurfaceRequest {
    key: CurveVolatilityProxySurfaceKey;
}

export interface GetCurveVolatilityProxySurfaceResponse {
    result: Result;
    curve_volatility_proxy_surface: CurveVolatilityProxySurface | null;
}

export interface GetManyCurveVolatilityProxySurfacesRequest {
    keys: CurveVolatilityProxySurfaceKey[];
}

export interface GetManyCurveVolatilityProxySurfacesResponse {
    result: Result;
    entries: CurveVolatilityProxySurfaceLookup[];
}

export interface PutCurveVolatilityProxySurfaceRequest {
    change: CurveVolatilityProxySurfaceChange;
    intent: ChangeIntent;
}

export interface PutCurveVolatilityProxySurfaceResponse {
    result: Result;
    curve_volatility_proxy_surface: CurveVolatilityProxySurface | null;
}

export interface PutManyCurveVolatilityProxySurfacesRequest {
    changes: CurveVolatilityProxySurfaceChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveVolatilityProxySurfacesResponse {
    result: Result;
    curve_volatility_proxy_surfaces: CurveVolatilityProxySurface[];
}

export interface DeleteCurveVolatilityProxySurfaceRequest {
    removal: CurveVolatilityProxySurfaceRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveVolatilityProxySurfaceResponse {
    result: Result;
}

export interface DeleteManyCurveVolatilityProxySurfacesRequest {
    removals: CurveVolatilityProxySurfaceRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveVolatilityProxySurfacesResponse {
    result: Result;
}

export interface ListCurveVolatilityProxySurfaceVersionsRequest {
    key: CurveVolatilityProxySurfaceKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveVolatilityProxySurfaceVersionsFilter | null;
}

export interface ListCurveVolatilityProxySurfaceVersionsResponse {
    result: Result;
    versions: CurveVolatilityProxySurface[];
    total: number;
}

export interface GetCurveVolatilityProxySurfaceVersionRequest {
    key: CurveVolatilityProxySurfaceVersionKey;
}

export interface GetCurveVolatilityProxySurfaceVersionResponse {
    result: Result;
    version: CurveVolatilityProxySurface | null;
}

export const subjects = {
    list_curve_volatility_proxy_surfaces_request: 'refdata.v1.curve_volatility_proxy_surfaces.list',
    get_curve_volatility_proxy_surface_request: 'refdata.v1.curve_volatility_proxy_surfaces.get',
    get_many_curve_volatility_proxy_surfaces_request:
        'refdata.v1.curve_volatility_proxy_surfaces.get_many',
    put_curve_volatility_proxy_surface_request: 'refdata.v1.curve_volatility_proxy_surfaces.put',
    put_many_curve_volatility_proxy_surfaces_request:
        'refdata.v1.curve_volatility_proxy_surfaces.put_many',
    delete_curve_volatility_proxy_surface_request:
        'refdata.v1.curve_volatility_proxy_surfaces.delete',
    delete_many_curve_volatility_proxy_surfaces_request:
        'refdata.v1.curve_volatility_proxy_surfaces.delete_many',
    list_curve_volatility_proxy_surface_versions_request:
        'refdata.v1.curve_volatility_proxy_surfaces_versions.list',
    get_curve_volatility_proxy_surface_version_request:
        'refdata.v1.curve_volatility_proxy_surfaces_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_volatility_proxy_surfaces_request: true,
    get_curve_volatility_proxy_surface_request: true,
    get_many_curve_volatility_proxy_surfaces_request: true,
    put_curve_volatility_proxy_surface_request: true,
    put_many_curve_volatility_proxy_surfaces_request: true,
    delete_curve_volatility_proxy_surface_request: true,
    delete_many_curve_volatility_proxy_surfaces_request: true,
    list_curve_volatility_proxy_surface_versions_request: true,
    get_curve_volatility_proxy_surface_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.curve_volatility_proxy_surfaces_events.created',
    updated: 'refdata.v1.curve_volatility_proxy_surfaces_events.updated',
    deleted: 'refdata.v1.curve_volatility_proxy_surfaces_events.deleted',
} as const;
