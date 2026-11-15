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
import type { FxVolSurfaceCalibration } from '../domain/fx_vol_surface_calibration.js';
import type { Order } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FxVolSurfaceCalibrationKey {
    id: string;
}

export interface FxVolSurfaceCalibrationLookup {
    key: FxVolSurfaceCalibrationKey;
    fx_vol_surface_calibration: FxVolSurfaceCalibration | null;
}

export interface FxVolSurfaceCalibrationsFilter {
    party_id: string | null;
    id_one_of: string[] | null;
    party_id_one_of: string[] | null;
}

export interface FxVolSurfaceCalibrationEvent {
    event_id: string;
    key: FxVolSurfaceCalibrationKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FxVolSurfaceCalibrationVersionKey {
    fx_vol_surface_calibration: FxVolSurfaceCalibrationKey;
    version: number;
}

export interface FxVolSurfaceCalibrationVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFxVolSurfaceCalibrationsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FxVolSurfaceCalibrationsFilter | null;
    as_of: string | null;
}

export interface ListFxVolSurfaceCalibrationsResponse {
    result: Result;
    calibrations: FxVolSurfaceCalibration[];
    total: number;
}

export interface GetFxVolSurfaceCalibrationRequest {
    key: FxVolSurfaceCalibrationKey;
}

export interface GetFxVolSurfaceCalibrationResponse {
    result: Result;
    fx_vol_surface_calibration: FxVolSurfaceCalibration | null;
}

export interface GetManyFxVolSurfaceCalibrationsRequest {
    keys: FxVolSurfaceCalibrationKey[];
}

export interface GetManyFxVolSurfaceCalibrationsResponse {
    result: Result;
    entries: FxVolSurfaceCalibrationLookup[];
}

export interface ListFxVolSurfaceCalibrationVersionsRequest {
    key: FxVolSurfaceCalibrationKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FxVolSurfaceCalibrationVersionsFilter | null;
}

export interface ListFxVolSurfaceCalibrationVersionsResponse {
    result: Result;
    versions: FxVolSurfaceCalibration[];
    total: number;
}

export interface GetFxVolSurfaceCalibrationVersionRequest {
    key: FxVolSurfaceCalibrationVersionKey;
}

export interface GetFxVolSurfaceCalibrationVersionResponse {
    result: Result;
    version: FxVolSurfaceCalibration | null;
}

export const subjects = {
    list_fx_vol_surface_calibrations_request: 'marketdata.v1.fx_vol_surface_calibrations.list',
    get_fx_vol_surface_calibration_request: 'marketdata.v1.fx_vol_surface_calibrations.get',
    get_many_fx_vol_surface_calibrations_request:
        'marketdata.v1.fx_vol_surface_calibrations.get_many',
    list_fx_vol_surface_calibration_versions_request:
        'marketdata.v1.fx_vol_surface_calibrations_versions.list',
    get_fx_vol_surface_calibration_version_request:
        'marketdata.v1.fx_vol_surface_calibrations_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fx_vol_surface_calibrations_request: true,
    get_fx_vol_surface_calibration_request: true,
    get_many_fx_vol_surface_calibrations_request: true,
    list_fx_vol_surface_calibration_versions_request: true,
    get_fx_vol_surface_calibration_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.events.fx_vol_surface_calibrations.created',
    updated: 'marketdata.v1.events.fx_vol_surface_calibrations.updated',
    deleted: 'marketdata.v1.events.fx_vol_surface_calibrations.deleted',
} as const;
