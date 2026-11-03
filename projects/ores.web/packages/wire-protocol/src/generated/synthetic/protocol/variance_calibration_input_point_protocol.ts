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
import type { VarianceCalibrationInputPoint } from '../domain/variance_calibration_input_point.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface VarianceCalibrationInputPointKey {
    id: string;
}

export interface VarianceCalibrationInputPointWrite {
    id: string;
    input_id: string;
    fixing_date: string;
    fixing_text: string;
    fixing_value: number | null;
    log_return: number | null;
    used: boolean;
    exclusion_reason: string;
}

export interface VarianceCalibrationInputPointChange {
    write: VarianceCalibrationInputPointWrite;
    precondition: Precondition;
}

export interface VarianceCalibrationInputPointRemoval {
    key: VarianceCalibrationInputPointKey;
    precondition: Precondition;
}

export interface VarianceCalibrationInputPointLookup {
    key: VarianceCalibrationInputPointKey;
    variance_calibration_input_point: VarianceCalibrationInputPoint | null;
}

export interface VarianceCalibrationInputPointsFilter {
    input_id: string | null;
    id_one_of: string[] | null;
    input_id_one_of: string[] | null;
}

export interface VarianceCalibrationInputPointEvent {
    event_id: string;
    key: VarianceCalibrationInputPointKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface VarianceCalibrationInputPointVersionKey {
    variance_calibration_input_point: VarianceCalibrationInputPointKey;
    version: number;
}

export interface VarianceCalibrationInputPointVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListVarianceCalibrationInputPointsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: VarianceCalibrationInputPointsFilter | null;
    as_of: string | null;
}

export interface ListVarianceCalibrationInputPointsResponse {
    result: Result;
    variance_calibration_input_points: VarianceCalibrationInputPoint[];
    total: number;
}

export interface GetVarianceCalibrationInputPointRequest {
    key: VarianceCalibrationInputPointKey;
}

export interface GetVarianceCalibrationInputPointResponse {
    result: Result;
    variance_calibration_input_point: VarianceCalibrationInputPoint | null;
}

export interface GetManyVarianceCalibrationInputPointsRequest {
    keys: VarianceCalibrationInputPointKey[];
}

export interface GetManyVarianceCalibrationInputPointsResponse {
    result: Result;
    entries: VarianceCalibrationInputPointLookup[];
}

export interface PutVarianceCalibrationInputPointRequest {
    change: VarianceCalibrationInputPointChange;
    intent: ChangeIntent;
}

export interface PutVarianceCalibrationInputPointResponse {
    result: Result;
    variance_calibration_input_point: VarianceCalibrationInputPoint | null;
}

export interface PutManyVarianceCalibrationInputPointsRequest {
    changes: VarianceCalibrationInputPointChange[];
    intent: ChangeIntent;
}

export interface PutManyVarianceCalibrationInputPointsResponse {
    result: Result;
    variance_calibration_input_points: VarianceCalibrationInputPoint[];
}

export interface DeleteVarianceCalibrationInputPointRequest {
    removal: VarianceCalibrationInputPointRemoval;
    intent: ChangeIntent;
}

export interface DeleteVarianceCalibrationInputPointResponse {
    result: Result;
}

export interface DeleteManyVarianceCalibrationInputPointsRequest {
    removals: VarianceCalibrationInputPointRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyVarianceCalibrationInputPointsResponse {
    result: Result;
}

export interface ListVarianceCalibrationInputPointVersionsRequest {
    key: VarianceCalibrationInputPointKey;
    offset: number;
    limit: number;
    order: Order;
    filter: VarianceCalibrationInputPointVersionsFilter | null;
}

export interface ListVarianceCalibrationInputPointVersionsResponse {
    result: Result;
    versions: VarianceCalibrationInputPoint[];
    total: number;
}

export interface GetVarianceCalibrationInputPointVersionRequest {
    key: VarianceCalibrationInputPointVersionKey;
}

export interface GetVarianceCalibrationInputPointVersionResponse {
    result: Result;
    version: VarianceCalibrationInputPoint | null;
}

export const subjects = {
    list_variance_calibration_input_points_request:
        'synthetic.v1.variance_calibration_input_points.list',
    get_variance_calibration_input_point_request:
        'synthetic.v1.variance_calibration_input_points.get',
    get_many_variance_calibration_input_points_request:
        'synthetic.v1.variance_calibration_input_points.get_many',
    put_variance_calibration_input_point_request:
        'synthetic.v1.variance_calibration_input_points.put',
    put_many_variance_calibration_input_points_request:
        'synthetic.v1.variance_calibration_input_points.put_many',
    delete_variance_calibration_input_point_request:
        'synthetic.v1.variance_calibration_input_points.delete',
    delete_many_variance_calibration_input_points_request:
        'synthetic.v1.variance_calibration_input_points.delete_many',
    list_variance_calibration_input_point_versions_request:
        'synthetic.v1.variance_calibration_input_points_versions.list',
    get_variance_calibration_input_point_version_request:
        'synthetic.v1.variance_calibration_input_points_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_variance_calibration_input_points_request: true,
    get_variance_calibration_input_point_request: true,
    get_many_variance_calibration_input_points_request: true,
    put_variance_calibration_input_point_request: true,
    put_many_variance_calibration_input_points_request: true,
    delete_variance_calibration_input_point_request: true,
    delete_many_variance_calibration_input_points_request: true,
    list_variance_calibration_input_point_versions_request: true,
    get_variance_calibration_input_point_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'synthetic.v1.variance_calibration_input_points_events.created',
    updated: 'synthetic.v1.variance_calibration_input_points_events.updated',
    deleted: 'synthetic.v1.variance_calibration_input_points_events.deleted',
} as const;
