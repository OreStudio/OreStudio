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
import type { CurveVolatilityParametricSmileParameter } from '../domain/curve_volatility_parametric_smile_parameter.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveVolatilityParametricSmileParameterKey {
    id: string;
}

export interface CurveVolatilityParametricSmileParameterWrite {
    id: string;
    curve_volatility_parametric_smile_id: string;
    name: string;
    initial_value: string | null;
    calibration: string;
    position: number;
}

export interface CurveVolatilityParametricSmileParameterChange {
    write: CurveVolatilityParametricSmileParameterWrite;
    precondition: Precondition;
}

export interface CurveVolatilityParametricSmileParameterRemoval {
    key: CurveVolatilityParametricSmileParameterKey;
    precondition: Precondition;
}

export interface CurveVolatilityParametricSmileParameterLookup {
    key: CurveVolatilityParametricSmileParameterKey;
    curve_volatility_parametric_smile_parameter: CurveVolatilityParametricSmileParameter | null;
}

export interface CurveVolatilityParametricSmileParametersFilter {
    id_one_of: string[] | null;
}

export interface CurveVolatilityParametricSmileParameterEvent {
    event_id: string;
    key: CurveVolatilityParametricSmileParameterKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveVolatilityParametricSmileParameterVersionKey {
    curve_volatility_parametric_smile_parameter: CurveVolatilityParametricSmileParameterKey;
    version: number;
}

export interface CurveVolatilityParametricSmileParameterVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveVolatilityParametricSmileParametersRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CurveVolatilityParametricSmileParametersFilter | null;
    as_of: string | null;
}

export interface ListCurveVolatilityParametricSmileParametersResponse {
    result: Result;
    curve_volatility_parametric_smile_parameters: CurveVolatilityParametricSmileParameter[];
    total: number;
}

export interface GetCurveVolatilityParametricSmileParameterRequest {
    key: CurveVolatilityParametricSmileParameterKey;
}

export interface GetCurveVolatilityParametricSmileParameterResponse {
    result: Result;
    curve_volatility_parametric_smile_parameter: CurveVolatilityParametricSmileParameter | null;
}

export interface GetManyCurveVolatilityParametricSmileParametersRequest {
    keys: CurveVolatilityParametricSmileParameterKey[];
}

export interface GetManyCurveVolatilityParametricSmileParametersResponse {
    result: Result;
    entries: CurveVolatilityParametricSmileParameterLookup[];
}

export interface PutCurveVolatilityParametricSmileParameterRequest {
    change: CurveVolatilityParametricSmileParameterChange;
    intent: ChangeIntent;
}

export interface PutCurveVolatilityParametricSmileParameterResponse {
    result: Result;
    curve_volatility_parametric_smile_parameter: CurveVolatilityParametricSmileParameter | null;
}

export interface PutManyCurveVolatilityParametricSmileParametersRequest {
    changes: CurveVolatilityParametricSmileParameterChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveVolatilityParametricSmileParametersResponse {
    result: Result;
    curve_volatility_parametric_smile_parameters: CurveVolatilityParametricSmileParameter[];
}

export interface DeleteCurveVolatilityParametricSmileParameterRequest {
    removal: CurveVolatilityParametricSmileParameterRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveVolatilityParametricSmileParameterResponse {
    result: Result;
}

export interface DeleteManyCurveVolatilityParametricSmileParametersRequest {
    removals: CurveVolatilityParametricSmileParameterRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveVolatilityParametricSmileParametersResponse {
    result: Result;
}

export interface ListCurveVolatilityParametricSmileParameterVersionsRequest {
    key: CurveVolatilityParametricSmileParameterKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveVolatilityParametricSmileParameterVersionsFilter | null;
}

export interface ListCurveVolatilityParametricSmileParameterVersionsResponse {
    result: Result;
    versions: CurveVolatilityParametricSmileParameter[];
    total: number;
}

export interface GetCurveVolatilityParametricSmileParameterVersionRequest {
    key: CurveVolatilityParametricSmileParameterVersionKey;
}

export interface GetCurveVolatilityParametricSmileParameterVersionResponse {
    result: Result;
    version: CurveVolatilityParametricSmileParameter | null;
}

export const subjects = {
    list_curve_volatility_parametric_smile_parameters_request:
        'refdata.v1.curve_volatility_parametric_smile_parameters.list',
    get_curve_volatility_parametric_smile_parameter_request:
        'refdata.v1.curve_volatility_parametric_smile_parameters.get',
    get_many_curve_volatility_parametric_smile_parameters_request:
        'refdata.v1.curve_volatility_parametric_smile_parameters.get_many',
    put_curve_volatility_parametric_smile_parameter_request:
        'refdata.v1.curve_volatility_parametric_smile_parameters.put',
    put_many_curve_volatility_parametric_smile_parameters_request:
        'refdata.v1.curve_volatility_parametric_smile_parameters.put_many',
    delete_curve_volatility_parametric_smile_parameter_request:
        'refdata.v1.curve_volatility_parametric_smile_parameters.delete',
    delete_many_curve_volatility_parametric_smile_parameters_request:
        'refdata.v1.curve_volatility_parametric_smile_parameters.delete_many',
    list_curve_volatility_parametric_smile_parameter_versions_request:
        'refdata.v1.curve_volatility_parametric_smile_parameters_versions.list',
    get_curve_volatility_parametric_smile_parameter_version_request:
        'refdata.v1.curve_volatility_parametric_smile_parameters_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_volatility_parametric_smile_parameters_request: true,
    get_curve_volatility_parametric_smile_parameter_request: true,
    get_many_curve_volatility_parametric_smile_parameters_request: true,
    put_curve_volatility_parametric_smile_parameter_request: true,
    put_many_curve_volatility_parametric_smile_parameters_request: true,
    delete_curve_volatility_parametric_smile_parameter_request: true,
    delete_many_curve_volatility_parametric_smile_parameters_request: true,
    list_curve_volatility_parametric_smile_parameter_versions_request: true,
    get_curve_volatility_parametric_smile_parameter_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.curve_volatility_parametric_smile_parameters_events.created',
    updated: 'refdata.v1.curve_volatility_parametric_smile_parameters_events.updated',
    deleted: 'refdata.v1.curve_volatility_parametric_smile_parameters_events.deleted',
} as const;
