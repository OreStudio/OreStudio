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
import type { YieldCurveProcessParameterDefinition } from '../domain/yield_curve_process_parameter_definition.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface YieldCurveProcessParameterDefinitionKey {
    parameter_name: string;
}

export interface YieldCurveProcessParameterDefinitionWrite {
    id: string;
    process_type_code: string;
    parameter_name: string;
    display_name: string;
    symbol: string | null;
    short_label: string;
    description: string;
    data_type: string;
    default_value: number;
    min_value: number | null;
    max_value: number | null;
    display_order: number;
}

export interface YieldCurveProcessParameterDefinitionChange {
    write: YieldCurveProcessParameterDefinitionWrite;
    precondition: Precondition;
}

export interface YieldCurveProcessParameterDefinitionRemoval {
    key: YieldCurveProcessParameterDefinitionKey;
    precondition: Precondition;
}

export interface YieldCurveProcessParameterDefinitionLookup {
    key: YieldCurveProcessParameterDefinitionKey;
    yield_curve_process_parameter_definition: YieldCurveProcessParameterDefinition | null;
}

export interface YieldCurveProcessParameterDefinitionsFilter {
    id_one_of: string[] | null;
}

export interface YieldCurveProcessParameterDefinitionEvent {
    event_id: string;
    key: YieldCurveProcessParameterDefinitionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface YieldCurveProcessParameterDefinitionVersionKey {
    yield_curve_process_parameter_definition: YieldCurveProcessParameterDefinitionKey;
    version: number;
}

export interface YieldCurveProcessParameterDefinitionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListYieldCurveProcessParameterDefinitionsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: YieldCurveProcessParameterDefinitionsFilter | null;
    as_of: string | null;
}

export interface ListYieldCurveProcessParameterDefinitionsResponse {
    result: Result;
    parameter_definitions: YieldCurveProcessParameterDefinition[];
    total: number;
}

export interface GetYieldCurveProcessParameterDefinitionRequest {
    key: YieldCurveProcessParameterDefinitionKey;
}

export interface GetYieldCurveProcessParameterDefinitionResponse {
    result: Result;
    yield_curve_process_parameter_definition: YieldCurveProcessParameterDefinition | null;
}

export interface GetManyYieldCurveProcessParameterDefinitionsRequest {
    keys: YieldCurveProcessParameterDefinitionKey[];
}

export interface GetManyYieldCurveProcessParameterDefinitionsResponse {
    result: Result;
    entries: YieldCurveProcessParameterDefinitionLookup[];
}

export interface PutYieldCurveProcessParameterDefinitionRequest {
    change: YieldCurveProcessParameterDefinitionChange;
    intent: ChangeIntent;
}

export interface PutYieldCurveProcessParameterDefinitionResponse {
    result: Result;
    yield_curve_process_parameter_definition: YieldCurveProcessParameterDefinition | null;
}

export interface PutManyYieldCurveProcessParameterDefinitionsRequest {
    changes: YieldCurveProcessParameterDefinitionChange[];
    intent: ChangeIntent;
}

export interface PutManyYieldCurveProcessParameterDefinitionsResponse {
    result: Result;
    parameter_definitions: YieldCurveProcessParameterDefinition[];
}

export interface DeleteYieldCurveProcessParameterDefinitionRequest {
    removal: YieldCurveProcessParameterDefinitionRemoval;
    intent: ChangeIntent;
}

export interface DeleteYieldCurveProcessParameterDefinitionResponse {
    result: Result;
}

export interface DeleteManyYieldCurveProcessParameterDefinitionsRequest {
    removals: YieldCurveProcessParameterDefinitionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyYieldCurveProcessParameterDefinitionsResponse {
    result: Result;
}

export interface ListYieldCurveProcessParameterDefinitionVersionsRequest {
    key: YieldCurveProcessParameterDefinitionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: YieldCurveProcessParameterDefinitionVersionsFilter | null;
}

export interface ListYieldCurveProcessParameterDefinitionVersionsResponse {
    result: Result;
    versions: YieldCurveProcessParameterDefinition[];
    total: number;
}

export interface GetYieldCurveProcessParameterDefinitionVersionRequest {
    key: YieldCurveProcessParameterDefinitionVersionKey;
}

export interface GetYieldCurveProcessParameterDefinitionVersionResponse {
    result: Result;
    version: YieldCurveProcessParameterDefinition | null;
}

export const subjects = {
    list_yield_curve_process_parameter_definitions_request:
        'synthetic.v1.yield_curve_process_parameter_definitions.list',
    get_yield_curve_process_parameter_definition_request:
        'synthetic.v1.yield_curve_process_parameter_definitions.get',
    get_many_yield_curve_process_parameter_definitions_request:
        'synthetic.v1.yield_curve_process_parameter_definitions.get_many',
    put_yield_curve_process_parameter_definition_request:
        'synthetic.v1.yield_curve_process_parameter_definitions.put',
    put_many_yield_curve_process_parameter_definitions_request:
        'synthetic.v1.yield_curve_process_parameter_definitions.put_many',
    delete_yield_curve_process_parameter_definition_request:
        'synthetic.v1.yield_curve_process_parameter_definitions.delete',
    delete_many_yield_curve_process_parameter_definitions_request:
        'synthetic.v1.yield_curve_process_parameter_definitions.delete_many',
    list_yield_curve_process_parameter_definition_versions_request:
        'synthetic.v1.yield_curve_process_parameter_definitions_versions.list',
    get_yield_curve_process_parameter_definition_version_request:
        'synthetic.v1.yield_curve_process_parameter_definitions_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_yield_curve_process_parameter_definitions_request: true,
    get_yield_curve_process_parameter_definition_request: true,
    get_many_yield_curve_process_parameter_definitions_request: true,
    put_yield_curve_process_parameter_definition_request: true,
    put_many_yield_curve_process_parameter_definitions_request: true,
    delete_yield_curve_process_parameter_definition_request: true,
    delete_many_yield_curve_process_parameter_definitions_request: true,
    list_yield_curve_process_parameter_definition_versions_request: true,
    get_yield_curve_process_parameter_definition_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'synthetic.v1.yield_curve_process_parameter_definitions_events.created',
    updated: 'synthetic.v1.yield_curve_process_parameter_definitions_events.updated',
    deleted: 'synthetic.v1.yield_curve_process_parameter_definitions_events.deleted',
} as const;
