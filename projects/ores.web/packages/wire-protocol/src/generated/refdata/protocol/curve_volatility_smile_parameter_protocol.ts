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
import type { CurveVolatilitySmileParameter } from '../domain/curve_volatility_smile_parameter.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveVolatilitySmileParameterKey {
    id: string;
}

export interface CurveVolatilitySmileParameterWrite {
    id: string;
    curve_volatility_smile_id: string;
    position: number;
    name: string;
    initial_value: string | null;
    calibration: string;
}

export interface CurveVolatilitySmileParameterChange {
    write: CurveVolatilitySmileParameterWrite;
    precondition: Precondition;
}

export interface CurveVolatilitySmileParameterRemoval {
    key: CurveVolatilitySmileParameterKey;
    precondition: Precondition;
}

export interface CurveVolatilitySmileParameterLookup {
    key: CurveVolatilitySmileParameterKey;
    curve_volatility_smile_parameter: CurveVolatilitySmileParameter | null;
}

export interface CurveVolatilitySmileParametersFilter {
    id_one_of: string[] | null;
}

export interface CurveVolatilitySmileParameterEvent {
    event_id: string;
    key: CurveVolatilitySmileParameterKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveVolatilitySmileParameterVersionKey {
    curve_volatility_smile_parameter: CurveVolatilitySmileParameterKey;
    version: number;
}

export interface CurveVolatilitySmileParameterVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveVolatilitySmileParametersRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CurveVolatilitySmileParametersFilter | null;
    as_of: string | null;
}

export interface ListCurveVolatilitySmileParametersResponse {
    result: Result;
    curve_volatility_smile_parameters: CurveVolatilitySmileParameter[];
    total: number;
}

export interface GetCurveVolatilitySmileParameterRequest {
    key: CurveVolatilitySmileParameterKey;
}

export interface GetCurveVolatilitySmileParameterResponse {
    result: Result;
    curve_volatility_smile_parameter: CurveVolatilitySmileParameter | null;
}

export interface GetManyCurveVolatilitySmileParametersRequest {
    keys: CurveVolatilitySmileParameterKey[];
}

export interface GetManyCurveVolatilitySmileParametersResponse {
    result: Result;
    entries: CurveVolatilitySmileParameterLookup[];
}

export interface PutCurveVolatilitySmileParameterRequest {
    change: CurveVolatilitySmileParameterChange;
    intent: ChangeIntent;
}

export interface PutCurveVolatilitySmileParameterResponse {
    result: Result;
    curve_volatility_smile_parameter: CurveVolatilitySmileParameter | null;
}

export interface PutManyCurveVolatilitySmileParametersRequest {
    changes: CurveVolatilitySmileParameterChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveVolatilitySmileParametersResponse {
    result: Result;
    curve_volatility_smile_parameters: CurveVolatilitySmileParameter[];
}

export interface DeleteCurveVolatilitySmileParameterRequest {
    removal: CurveVolatilitySmileParameterRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveVolatilitySmileParameterResponse {
    result: Result;
}

export interface DeleteManyCurveVolatilitySmileParametersRequest {
    removals: CurveVolatilitySmileParameterRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveVolatilitySmileParametersResponse {
    result: Result;
}

export interface ListCurveVolatilitySmileParameterVersionsRequest {
    key: CurveVolatilitySmileParameterKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveVolatilitySmileParameterVersionsFilter | null;
}

export interface ListCurveVolatilitySmileParameterVersionsResponse {
    result: Result;
    versions: CurveVolatilitySmileParameter[];
    total: number;
}

export interface GetCurveVolatilitySmileParameterVersionRequest {
    key: CurveVolatilitySmileParameterVersionKey;
}

export interface GetCurveVolatilitySmileParameterVersionResponse {
    result: Result;
    version: CurveVolatilitySmileParameter | null;
}

export const subjects = {
    list_curve_volatility_smile_parameters_request:
        'refdata.v1.curve_volatility_smile_parameters.list',
    get_curve_volatility_smile_parameter_request:
        'refdata.v1.curve_volatility_smile_parameters.get',
    get_many_curve_volatility_smile_parameters_request:
        'refdata.v1.curve_volatility_smile_parameters.get_many',
    put_curve_volatility_smile_parameter_request:
        'refdata.v1.curve_volatility_smile_parameters.put',
    put_many_curve_volatility_smile_parameters_request:
        'refdata.v1.curve_volatility_smile_parameters.put_many',
    delete_curve_volatility_smile_parameter_request:
        'refdata.v1.curve_volatility_smile_parameters.delete',
    delete_many_curve_volatility_smile_parameters_request:
        'refdata.v1.curve_volatility_smile_parameters.delete_many',
    list_curve_volatility_smile_parameter_versions_request:
        'refdata.v1.curve_volatility_smile_parameters_versions.list',
    get_curve_volatility_smile_parameter_version_request:
        'refdata.v1.curve_volatility_smile_parameters_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_volatility_smile_parameters_request: true,
    get_curve_volatility_smile_parameter_request: true,
    get_many_curve_volatility_smile_parameters_request: true,
    put_curve_volatility_smile_parameter_request: true,
    put_many_curve_volatility_smile_parameters_request: true,
    delete_curve_volatility_smile_parameter_request: true,
    delete_many_curve_volatility_smile_parameters_request: true,
    list_curve_volatility_smile_parameter_versions_request: true,
    get_curve_volatility_smile_parameter_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.curve_volatility_smile_parameters_events.created',
    updated: 'refdata.v1.curve_volatility_smile_parameters_events.updated',
    deleted: 'refdata.v1.curve_volatility_smile_parameters_events.deleted',
} as const;
