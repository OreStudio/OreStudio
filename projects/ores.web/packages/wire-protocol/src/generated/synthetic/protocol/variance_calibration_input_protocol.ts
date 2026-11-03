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
import type { VarianceCalibrationInput } from '../domain/variance_calibration_input.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface VarianceCalibrationInputKey {
    id: string;
}

export interface VarianceCalibrationInputWrite {
    id: string;
    fixing_uri: string;
    window_end: string;
    window_days: number;
    base_currency_code: string;
    quote_currency_code: string;
    provider_code: string;
    inverted: boolean;
    policy_id: string;
    policy_version: number;
    point_count: number;
    used_count: number;
}

export interface VarianceCalibrationInputChange {
    write: VarianceCalibrationInputWrite;
    precondition: Precondition;
}

export interface VarianceCalibrationInputRemoval {
    key: VarianceCalibrationInputKey;
    precondition: Precondition;
}

export interface VarianceCalibrationInputLookup {
    key: VarianceCalibrationInputKey;
    variance_calibration_input: VarianceCalibrationInput | null;
}

export interface VarianceCalibrationInputsFilter {
    base_currency_code: string | null;
    quote_currency_code: string | null;
    id_one_of: string[] | null;
    base_currency_code_one_of: string[] | null;
    quote_currency_code_one_of: string[] | null;
}

export interface VarianceCalibrationInputEvent {
    event_id: string;
    key: VarianceCalibrationInputKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface VarianceCalibrationInputVersionKey {
    variance_calibration_input: VarianceCalibrationInputKey;
    version: number;
}

export interface VarianceCalibrationInputVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListVarianceCalibrationInputsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: VarianceCalibrationInputsFilter | null;
    as_of: string | null;
}

export interface ListVarianceCalibrationInputsResponse {
    result: Result;
    variance_calibration_inputs: VarianceCalibrationInput[];
    total: number;
}

export interface GetVarianceCalibrationInputRequest {
    key: VarianceCalibrationInputKey;
}

export interface GetVarianceCalibrationInputResponse {
    result: Result;
    variance_calibration_input: VarianceCalibrationInput | null;
}

export interface GetManyVarianceCalibrationInputsRequest {
    keys: VarianceCalibrationInputKey[];
}

export interface GetManyVarianceCalibrationInputsResponse {
    result: Result;
    entries: VarianceCalibrationInputLookup[];
}

export interface PutVarianceCalibrationInputRequest {
    change: VarianceCalibrationInputChange;
    intent: ChangeIntent;
}

export interface PutVarianceCalibrationInputResponse {
    result: Result;
    variance_calibration_input: VarianceCalibrationInput | null;
}

export interface PutManyVarianceCalibrationInputsRequest {
    changes: VarianceCalibrationInputChange[];
    intent: ChangeIntent;
}

export interface PutManyVarianceCalibrationInputsResponse {
    result: Result;
    variance_calibration_inputs: VarianceCalibrationInput[];
}

export interface DeleteVarianceCalibrationInputRequest {
    removal: VarianceCalibrationInputRemoval;
    intent: ChangeIntent;
}

export interface DeleteVarianceCalibrationInputResponse {
    result: Result;
}

export interface DeleteManyVarianceCalibrationInputsRequest {
    removals: VarianceCalibrationInputRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyVarianceCalibrationInputsResponse {
    result: Result;
}

export interface ListVarianceCalibrationInputVersionsRequest {
    key: VarianceCalibrationInputKey;
    offset: number;
    limit: number;
    order: Order;
    filter: VarianceCalibrationInputVersionsFilter | null;
}

export interface ListVarianceCalibrationInputVersionsResponse {
    result: Result;
    versions: VarianceCalibrationInput[];
    total: number;
}

export interface GetVarianceCalibrationInputVersionRequest {
    key: VarianceCalibrationInputVersionKey;
}

export interface GetVarianceCalibrationInputVersionResponse {
    result: Result;
    version: VarianceCalibrationInput | null;
}

export const subjects = {
    list_variance_calibration_inputs_request: 'synthetic.v1.variance_calibration_inputs.list',
    get_variance_calibration_input_request: 'synthetic.v1.variance_calibration_inputs.get',
    get_many_variance_calibration_inputs_request:
        'synthetic.v1.variance_calibration_inputs.get_many',
    put_variance_calibration_input_request: 'synthetic.v1.variance_calibration_inputs.put',
    put_many_variance_calibration_inputs_request:
        'synthetic.v1.variance_calibration_inputs.put_many',
    delete_variance_calibration_input_request: 'synthetic.v1.variance_calibration_inputs.delete',
    delete_many_variance_calibration_inputs_request:
        'synthetic.v1.variance_calibration_inputs.delete_many',
    list_variance_calibration_input_versions_request:
        'synthetic.v1.variance_calibration_inputs_versions.list',
    get_variance_calibration_input_version_request:
        'synthetic.v1.variance_calibration_inputs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_variance_calibration_inputs_request: true,
    get_variance_calibration_input_request: true,
    get_many_variance_calibration_inputs_request: true,
    put_variance_calibration_input_request: true,
    put_many_variance_calibration_inputs_request: true,
    delete_variance_calibration_input_request: true,
    delete_many_variance_calibration_inputs_request: true,
    list_variance_calibration_input_versions_request: true,
    get_variance_calibration_input_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'synthetic.v1.events.variance_calibration_inputs.created',
    updated: 'synthetic.v1.events.variance_calibration_inputs.updated',
    deleted: 'synthetic.v1.events.variance_calibration_inputs.deleted',
} as const;
