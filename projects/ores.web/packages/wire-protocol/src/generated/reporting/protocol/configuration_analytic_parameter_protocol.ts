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
import type { ConfigurationAnalyticParameter } from '../domain/configuration_analytic_parameter.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface ConfigurationAnalyticParameterKey {
    id: string;
}

export interface ConfigurationAnalyticParameterWrite {
    id: string;
    configuration_analytic_id: string;
    ore_parameter_id: string;
    value: string;
    position: number;
}

export interface ConfigurationAnalyticParameterChange {
    write: ConfigurationAnalyticParameterWrite;
    precondition: Precondition;
}

export interface ConfigurationAnalyticParameterRemoval {
    key: ConfigurationAnalyticParameterKey;
    precondition: Precondition;
}

export interface ConfigurationAnalyticParameterLookup {
    key: ConfigurationAnalyticParameterKey;
    configuration_analytic_parameter: ConfigurationAnalyticParameter | null;
}

export interface ConfigurationAnalyticParametersFilter {
    configuration_analytic_id: string | null;
    ore_parameter_id: string | null;
    id_one_of: string[] | null;
    configuration_analytic_id_one_of: string[] | null;
    ore_parameter_id_one_of: string[] | null;
}

export interface ConfigurationAnalyticParameterEvent {
    event_id: string;
    key: ConfigurationAnalyticParameterKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ConfigurationAnalyticParameterVersionKey {
    configuration_analytic_parameter: ConfigurationAnalyticParameterKey;
    version: number;
}

export interface ConfigurationAnalyticParameterVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListConfigurationAnalyticParametersRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ConfigurationAnalyticParametersFilter | null;
    as_of: string | null;
}

export interface ListConfigurationAnalyticParametersResponse {
    result: Result;
    parameter_values: ConfigurationAnalyticParameter[];
    total: number;
}

export interface GetConfigurationAnalyticParameterRequest {
    key: ConfigurationAnalyticParameterKey;
}

export interface GetConfigurationAnalyticParameterResponse {
    result: Result;
    configuration_analytic_parameter: ConfigurationAnalyticParameter | null;
}

export interface GetManyConfigurationAnalyticParametersRequest {
    keys: ConfigurationAnalyticParameterKey[];
}

export interface GetManyConfigurationAnalyticParametersResponse {
    result: Result;
    entries: ConfigurationAnalyticParameterLookup[];
}

export interface PutConfigurationAnalyticParameterRequest {
    change: ConfigurationAnalyticParameterChange;
    intent: ChangeIntent;
}

export interface PutConfigurationAnalyticParameterResponse {
    result: Result;
    configuration_analytic_parameter: ConfigurationAnalyticParameter | null;
}

export interface PutManyConfigurationAnalyticParametersRequest {
    changes: ConfigurationAnalyticParameterChange[];
    intent: ChangeIntent;
}

export interface PutManyConfigurationAnalyticParametersResponse {
    result: Result;
    parameter_values: ConfigurationAnalyticParameter[];
}

export interface DeleteConfigurationAnalyticParameterRequest {
    removal: ConfigurationAnalyticParameterRemoval;
    intent: ChangeIntent;
}

export interface DeleteConfigurationAnalyticParameterResponse {
    result: Result;
}

export interface DeleteManyConfigurationAnalyticParametersRequest {
    removals: ConfigurationAnalyticParameterRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyConfigurationAnalyticParametersResponse {
    result: Result;
}

export interface ListByConfigurationAnalyticIdConfigurationAnalyticParametersRequest {
    configuration_analytic_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: ConfigurationAnalyticParametersFilter | null;
}

export interface ListByConfigurationAnalyticIdConfigurationAnalyticParametersResponse {
    result: Result;
    parameter_values: ConfigurationAnalyticParameter[];
    total: number;
}

export interface ListByOreParameterIdConfigurationAnalyticParametersRequest {
    ore_parameter_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: ConfigurationAnalyticParametersFilter | null;
}

export interface ListByOreParameterIdConfigurationAnalyticParametersResponse {
    result: Result;
    parameter_values: ConfigurationAnalyticParameter[];
    total: number;
}

export interface ListConfigurationAnalyticParameterVersionsRequest {
    key: ConfigurationAnalyticParameterKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ConfigurationAnalyticParameterVersionsFilter | null;
}

export interface ListConfigurationAnalyticParameterVersionsResponse {
    result: Result;
    versions: ConfigurationAnalyticParameter[];
    total: number;
}

export interface GetConfigurationAnalyticParameterVersionRequest {
    key: ConfigurationAnalyticParameterVersionKey;
}

export interface GetConfigurationAnalyticParameterVersionResponse {
    result: Result;
    version: ConfigurationAnalyticParameter | null;
}

export const subjects = {
    list_configuration_analytic_parameters_request:
        'reporting.v1.configuration_analytic_parameters.list',
    get_configuration_analytic_parameter_request:
        'reporting.v1.configuration_analytic_parameters.get',
    get_many_configuration_analytic_parameters_request:
        'reporting.v1.configuration_analytic_parameters.get_many',
    put_configuration_analytic_parameter_request:
        'reporting.v1.configuration_analytic_parameters.put',
    put_many_configuration_analytic_parameters_request:
        'reporting.v1.configuration_analytic_parameters.put_many',
    delete_configuration_analytic_parameter_request:
        'reporting.v1.configuration_analytic_parameters.delete',
    delete_many_configuration_analytic_parameters_request:
        'reporting.v1.configuration_analytic_parameters.delete_many',
    list_by_configuration_analytic_id_configuration_analytic_parameters_request:
        'reporting.v1.configuration_analytic_parameters.list_by_configuration_analytic_id',
    list_by_ore_parameter_id_configuration_analytic_parameters_request:
        'reporting.v1.configuration_analytic_parameters.list_by_ore_parameter_id',
    list_configuration_analytic_parameter_versions_request:
        'reporting.v1.configuration_analytic_parameters_versions.list',
    get_configuration_analytic_parameter_version_request:
        'reporting.v1.configuration_analytic_parameters_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_configuration_analytic_parameters_request: true,
    get_configuration_analytic_parameter_request: true,
    get_many_configuration_analytic_parameters_request: true,
    put_configuration_analytic_parameter_request: true,
    put_many_configuration_analytic_parameters_request: true,
    delete_configuration_analytic_parameter_request: true,
    delete_many_configuration_analytic_parameters_request: true,
    list_by_configuration_analytic_id_configuration_analytic_parameters_request: true,
    list_by_ore_parameter_id_configuration_analytic_parameters_request: true,
    list_configuration_analytic_parameter_versions_request: true,
    get_configuration_analytic_parameter_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.configuration_analytic_parameters_events.created',
    updated: 'reporting.v1.configuration_analytic_parameters_events.updated',
    deleted: 'reporting.v1.configuration_analytic_parameters_events.deleted',
} as const;
