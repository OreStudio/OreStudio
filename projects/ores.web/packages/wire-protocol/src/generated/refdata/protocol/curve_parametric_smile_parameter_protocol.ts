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
import type { CurveParametricSmileParameter } from '../domain/curve_parametric_smile_parameter.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveParametricSmileParameterKey {
    id: string;
}

export interface CurveParametricSmileParameterWrite {
    id: string;
    curve_definition_id: string;
    name: string;
    initial_value: string | null;
    calibration: string;
    position: number;
}

export interface CurveParametricSmileParameterChange {
    write: CurveParametricSmileParameterWrite;
    precondition: Precondition;
}

export interface CurveParametricSmileParameterRemoval {
    key: CurveParametricSmileParameterKey;
    precondition: Precondition;
}

export interface CurveParametricSmileParameterLookup {
    key: CurveParametricSmileParameterKey;
    curve_parametric_smile_parameter: CurveParametricSmileParameter | null;
}

export interface CurveParametricSmileParametersFilter {
    id_one_of: string[] | null;
}

export interface CurveParametricSmileParameterEvent {
    event_id: string;
    key: CurveParametricSmileParameterKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveParametricSmileParameterVersionKey {
    curve_parametric_smile_parameter: CurveParametricSmileParameterKey;
    version: number;
}

export interface CurveParametricSmileParameterVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveParametricSmileParametersRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CurveParametricSmileParametersFilter | null;
}

export interface ListCurveParametricSmileParametersResponse {
    result: Result;
    smile_parameters: CurveParametricSmileParameter[];
    total: number;
}

export interface GetCurveParametricSmileParameterRequest {
    key: CurveParametricSmileParameterKey;
}

export interface GetCurveParametricSmileParameterResponse {
    result: Result;
    curve_parametric_smile_parameter: CurveParametricSmileParameter | null;
}

export interface GetManyCurveParametricSmileParametersRequest {
    keys: CurveParametricSmileParameterKey[];
}

export interface GetManyCurveParametricSmileParametersResponse {
    result: Result;
    entries: CurveParametricSmileParameterLookup[];
}

export interface PutCurveParametricSmileParameterRequest {
    change: CurveParametricSmileParameterChange;
    intent: ChangeIntent;
}

export interface PutCurveParametricSmileParameterResponse {
    result: Result;
    curve_parametric_smile_parameter: CurveParametricSmileParameter | null;
}

export interface PutManyCurveParametricSmileParametersRequest {
    changes: CurveParametricSmileParameterChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveParametricSmileParametersResponse {
    result: Result;
    smile_parameters: CurveParametricSmileParameter[];
}

export interface DeleteCurveParametricSmileParameterRequest {
    removal: CurveParametricSmileParameterRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveParametricSmileParameterResponse {
    result: Result;
}

export interface DeleteManyCurveParametricSmileParametersRequest {
    removals: CurveParametricSmileParameterRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveParametricSmileParametersResponse {
    result: Result;
}

export interface ListCurveParametricSmileParameterVersionsRequest {
    key: CurveParametricSmileParameterKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveParametricSmileParameterVersionsFilter | null;
}

export interface ListCurveParametricSmileParameterVersionsResponse {
    result: Result;
    versions: CurveParametricSmileParameter[];
    total: number;
}

export interface GetCurveParametricSmileParameterVersionRequest {
    key: CurveParametricSmileParameterVersionKey;
}

export interface GetCurveParametricSmileParameterVersionResponse {
    result: Result;
    version: CurveParametricSmileParameter | null;
}

export const subjects = {
    list_curve_parametric_smile_parameters_request:
        'refdata.v1.curve_parametric_smile_parameters.list',
    get_curve_parametric_smile_parameter_request:
        'refdata.v1.curve_parametric_smile_parameters.get',
    get_many_curve_parametric_smile_parameters_request:
        'refdata.v1.curve_parametric_smile_parameters.get_many',
    put_curve_parametric_smile_parameter_request:
        'refdata.v1.curve_parametric_smile_parameters.put',
    put_many_curve_parametric_smile_parameters_request:
        'refdata.v1.curve_parametric_smile_parameters.put_many',
    delete_curve_parametric_smile_parameter_request:
        'refdata.v1.curve_parametric_smile_parameters.delete',
    delete_many_curve_parametric_smile_parameters_request:
        'refdata.v1.curve_parametric_smile_parameters.delete_many',
    list_curve_parametric_smile_parameter_versions_request:
        'refdata.v1.curve_parametric_smile_parameters_versions.list',
    get_curve_parametric_smile_parameter_version_request:
        'refdata.v1.curve_parametric_smile_parameters_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_parametric_smile_parameters_request: true,
    get_curve_parametric_smile_parameter_request: true,
    get_many_curve_parametric_smile_parameters_request: true,
    put_curve_parametric_smile_parameter_request: true,
    put_many_curve_parametric_smile_parameters_request: true,
    delete_curve_parametric_smile_parameter_request: true,
    delete_many_curve_parametric_smile_parameters_request: true,
    list_curve_parametric_smile_parameter_versions_request: true,
    get_curve_parametric_smile_parameter_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.curve_parametric_smile_parameters_events.created',
    updated: 'refdata.v1.curve_parametric_smile_parameters_events.updated',
    deleted: 'refdata.v1.curve_parametric_smile_parameters_events.deleted',
} as const;
