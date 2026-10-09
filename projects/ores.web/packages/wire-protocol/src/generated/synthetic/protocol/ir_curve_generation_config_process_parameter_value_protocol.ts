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
import type { IrCurveGenerationConfigProcessParameterValue } from '../domain/ir_curve_generation_config_process_parameter_value.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface IrCurveGenerationConfigProcessParameterValueKey {
    id: string;
}

export interface IrCurveGenerationConfigProcessParameterValueWrite {
    id: string;
    config_id: string;
    parameter_definition_id: string;
    parameter_value: number;
}

export interface IrCurveGenerationConfigProcessParameterValueChange {
    write: IrCurveGenerationConfigProcessParameterValueWrite;
    precondition: Precondition;
}

export interface IrCurveGenerationConfigProcessParameterValueRemoval {
    key: IrCurveGenerationConfigProcessParameterValueKey;
    precondition: Precondition;
}

export interface IrCurveGenerationConfigProcessParameterValueLookup {
    key: IrCurveGenerationConfigProcessParameterValueKey;
    ir_curve_generation_config_process_parameter_value: IrCurveGenerationConfigProcessParameterValue | null;
}

export interface IrCurveGenerationConfigProcessParameterValuesFilter {
    id_one_of: string[] | null;
}

export interface IrCurveGenerationConfigProcessParameterValueEvent {
    event_id: string;
    key: IrCurveGenerationConfigProcessParameterValueKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface IrCurveGenerationConfigProcessParameterValueVersionKey {
    ir_curve_generation_config_process_parameter_value: IrCurveGenerationConfigProcessParameterValueKey;
    version: number;
}

export interface IrCurveGenerationConfigProcessParameterValueVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListIrCurveGenerationConfigProcessParameterValuesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: IrCurveGenerationConfigProcessParameterValuesFilter | null;
    as_of: string | null;
}

export interface ListIrCurveGenerationConfigProcessParameterValuesResponse {
    result: Result;
    process_parameter_values: IrCurveGenerationConfigProcessParameterValue[];
    total: number;
}

export interface GetIrCurveGenerationConfigProcessParameterValueRequest {
    key: IrCurveGenerationConfigProcessParameterValueKey;
}

export interface GetIrCurveGenerationConfigProcessParameterValueResponse {
    result: Result;
    ir_curve_generation_config_process_parameter_value: IrCurveGenerationConfigProcessParameterValue | null;
}

export interface GetManyIrCurveGenerationConfigProcessParameterValuesRequest {
    keys: IrCurveGenerationConfigProcessParameterValueKey[];
}

export interface GetManyIrCurveGenerationConfigProcessParameterValuesResponse {
    result: Result;
    entries: IrCurveGenerationConfigProcessParameterValueLookup[];
}

export interface PutIrCurveGenerationConfigProcessParameterValueRequest {
    change: IrCurveGenerationConfigProcessParameterValueChange;
    intent: ChangeIntent;
}

export interface PutIrCurveGenerationConfigProcessParameterValueResponse {
    result: Result;
    ir_curve_generation_config_process_parameter_value: IrCurveGenerationConfigProcessParameterValue | null;
}

export interface PutManyIrCurveGenerationConfigProcessParameterValuesRequest {
    changes: IrCurveGenerationConfigProcessParameterValueChange[];
    intent: ChangeIntent;
}

export interface PutManyIrCurveGenerationConfigProcessParameterValuesResponse {
    result: Result;
    process_parameter_values: IrCurveGenerationConfigProcessParameterValue[];
}

export interface DeleteIrCurveGenerationConfigProcessParameterValueRequest {
    removal: IrCurveGenerationConfigProcessParameterValueRemoval;
    intent: ChangeIntent;
}

export interface DeleteIrCurveGenerationConfigProcessParameterValueResponse {
    result: Result;
}

export interface DeleteManyIrCurveGenerationConfigProcessParameterValuesRequest {
    removals: IrCurveGenerationConfigProcessParameterValueRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyIrCurveGenerationConfigProcessParameterValuesResponse {
    result: Result;
}

export interface ListIrCurveGenerationConfigProcessParameterValueVersionsRequest {
    key: IrCurveGenerationConfigProcessParameterValueKey;
    offset: number;
    limit: number;
    order: Order;
    filter: IrCurveGenerationConfigProcessParameterValueVersionsFilter | null;
}

export interface ListIrCurveGenerationConfigProcessParameterValueVersionsResponse {
    result: Result;
    versions: IrCurveGenerationConfigProcessParameterValue[];
    total: number;
}

export interface GetIrCurveGenerationConfigProcessParameterValueVersionRequest {
    key: IrCurveGenerationConfigProcessParameterValueVersionKey;
}

export interface GetIrCurveGenerationConfigProcessParameterValueVersionResponse {
    result: Result;
    version: IrCurveGenerationConfigProcessParameterValue | null;
}

export const subjects = {
    list_ir_curve_generation_config_process_parameter_values_request:
        'synthetic.v1.ir_curve_generation_config_process_parameter_values.list',
    get_ir_curve_generation_config_process_parameter_value_request:
        'synthetic.v1.ir_curve_generation_config_process_parameter_values.get',
    get_many_ir_curve_generation_config_process_parameter_values_request:
        'synthetic.v1.ir_curve_generation_config_process_parameter_values.get_many',
    put_ir_curve_generation_config_process_parameter_value_request:
        'synthetic.v1.ir_curve_generation_config_process_parameter_values.put',
    put_many_ir_curve_generation_config_process_parameter_values_request:
        'synthetic.v1.ir_curve_generation_config_process_parameter_values.put_many',
    delete_ir_curve_generation_config_process_parameter_value_request:
        'synthetic.v1.ir_curve_generation_config_process_parameter_values.delete',
    delete_many_ir_curve_generation_config_process_parameter_values_request:
        'synthetic.v1.ir_curve_generation_config_process_parameter_values.delete_many',
    list_ir_curve_generation_config_process_parameter_value_versions_request:
        'synthetic.v1.ir_curve_generation_config_process_parameter_values_versions.list',
    get_ir_curve_generation_config_process_parameter_value_version_request:
        'synthetic.v1.ir_curve_generation_config_process_parameter_values_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_ir_curve_generation_config_process_parameter_values_request: true,
    get_ir_curve_generation_config_process_parameter_value_request: true,
    get_many_ir_curve_generation_config_process_parameter_values_request: true,
    put_ir_curve_generation_config_process_parameter_value_request: true,
    put_many_ir_curve_generation_config_process_parameter_values_request: true,
    delete_ir_curve_generation_config_process_parameter_value_request: true,
    delete_many_ir_curve_generation_config_process_parameter_values_request: true,
    list_ir_curve_generation_config_process_parameter_value_versions_request: true,
    get_ir_curve_generation_config_process_parameter_value_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'synthetic.v1.ir_curve_generation_config_process_parameter_values_events.created',
    updated: 'synthetic.v1.ir_curve_generation_config_process_parameter_values_events.updated',
    deleted: 'synthetic.v1.ir_curve_generation_config_process_parameter_values_events.deleted',
} as const;
