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
import type { VarianceCalibration } from '../domain/variance_calibration.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface VarianceCalibrationKey {
    id: string;
}

export interface VarianceCalibrationWrite {
    id: string;
    base_currency_code: string;
    quote_currency_code: string;
    input_id: string | null;
    method: string;
    method_version: number;
    alpha: number | null;
    gamma: number | null;
    beta: number | null;
    persistence: number | null;
    long_run_variance: number | null;
    starting_variance_ratio: number | null;
    log_likelihood: number | null;
    returns_used: number;
    starts: number;
    starts_agreed: boolean;
    state: string;
    state_reason: string;
}

export interface VarianceCalibrationChange {
    write: VarianceCalibrationWrite;
    precondition: Precondition;
}

export interface VarianceCalibrationRemoval {
    key: VarianceCalibrationKey;
    precondition: Precondition;
}

export interface VarianceCalibrationLookup {
    key: VarianceCalibrationKey;
    variance_calibration: VarianceCalibration | null;
}

export interface VarianceCalibrationsFilter {
    base_currency_code: string | null;
    quote_currency_code: string | null;
    input_id: string | null | null;
    method: string | null;
    state: string | null;
    id_one_of: string[] | null;
    base_currency_code_one_of: string[] | null;
    quote_currency_code_one_of: string[] | null;
    input_id_one_of: string[] | null;
    method_one_of: string[] | null;
    state_one_of: string[] | null;
}

export interface VarianceCalibrationEvent {
    event_id: string;
    key: VarianceCalibrationKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface VarianceCalibrationVersionKey {
    variance_calibration: VarianceCalibrationKey;
    version: number;
}

export interface VarianceCalibrationVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListVarianceCalibrationsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: VarianceCalibrationsFilter | null;
    as_of: string | null;
}

export interface ListVarianceCalibrationsResponse {
    result: Result;
    variance_calibrations: VarianceCalibration[];
    total: number;
}

export interface GetVarianceCalibrationRequest {
    key: VarianceCalibrationKey;
}

export interface GetVarianceCalibrationResponse {
    result: Result;
    variance_calibration: VarianceCalibration | null;
}

export interface GetManyVarianceCalibrationsRequest {
    keys: VarianceCalibrationKey[];
}

export interface GetManyVarianceCalibrationsResponse {
    result: Result;
    entries: VarianceCalibrationLookup[];
}

export interface PutVarianceCalibrationRequest {
    change: VarianceCalibrationChange;
    intent: ChangeIntent;
}

export interface PutVarianceCalibrationResponse {
    result: Result;
    variance_calibration: VarianceCalibration | null;
}

export interface PutManyVarianceCalibrationsRequest {
    changes: VarianceCalibrationChange[];
    intent: ChangeIntent;
}

export interface PutManyVarianceCalibrationsResponse {
    result: Result;
    variance_calibrations: VarianceCalibration[];
}

export interface DeleteVarianceCalibrationRequest {
    removal: VarianceCalibrationRemoval;
    intent: ChangeIntent;
}

export interface DeleteVarianceCalibrationResponse {
    result: Result;
}

export interface DeleteManyVarianceCalibrationsRequest {
    removals: VarianceCalibrationRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyVarianceCalibrationsResponse {
    result: Result;
}

export interface ListVarianceCalibrationVersionsRequest {
    key: VarianceCalibrationKey;
    offset: number;
    limit: number;
    order: Order;
    filter: VarianceCalibrationVersionsFilter | null;
}

export interface ListVarianceCalibrationVersionsResponse {
    result: Result;
    versions: VarianceCalibration[];
    total: number;
}

export interface GetVarianceCalibrationVersionRequest {
    key: VarianceCalibrationVersionKey;
}

export interface GetVarianceCalibrationVersionResponse {
    result: Result;
    version: VarianceCalibration | null;
}

export const subjects = {
    list_variance_calibrations_request: 'synthetic.v1.variance_calibrations.list',
    get_variance_calibration_request: 'synthetic.v1.variance_calibrations.get',
    get_many_variance_calibrations_request: 'synthetic.v1.variance_calibrations.get_many',
    put_variance_calibration_request: 'synthetic.v1.variance_calibrations.put',
    put_many_variance_calibrations_request: 'synthetic.v1.variance_calibrations.put_many',
    delete_variance_calibration_request: 'synthetic.v1.variance_calibrations.delete',
    delete_many_variance_calibrations_request: 'synthetic.v1.variance_calibrations.delete_many',
    list_variance_calibration_versions_request: 'synthetic.v1.variance_calibrations_versions.list',
    get_variance_calibration_version_request: 'synthetic.v1.variance_calibrations_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_variance_calibrations_request: true,
    get_variance_calibration_request: true,
    get_many_variance_calibrations_request: true,
    put_variance_calibration_request: true,
    put_many_variance_calibrations_request: true,
    delete_variance_calibration_request: true,
    delete_many_variance_calibrations_request: true,
    list_variance_calibration_versions_request: true,
    get_variance_calibration_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'synthetic.v1.events.variance_calibrations.created',
    updated: 'synthetic.v1.events.variance_calibrations.updated',
    deleted: 'synthetic.v1.events.variance_calibrations.deleted',
} as const;
