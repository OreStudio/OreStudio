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
import type { ConfigurationParameter } from '../domain/configuration_parameter.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface ConfigurationParameterKey {
    value: string;
}

export interface ConfigurationParameterWrite {
    id: string;
    configuration_id: string;
    parameter_definition_id: string;
    value: string;
    position: number;
}

export interface ConfigurationParameterChange {
    write: ConfigurationParameterWrite;
    precondition: Precondition;
}

export interface ConfigurationParameterRemoval {
    key: ConfigurationParameterKey;
    precondition: Precondition;
}

export interface ConfigurationParameterLookup {
    key: ConfigurationParameterKey;
    configuration_parameter: ConfigurationParameter | null;
}

export interface ConfigurationParametersFilter {
    configuration_id: string | null;
    parameter_definition_id: string | null;
}

export interface ConfigurationParameterEvent {
    event_id: string;
    key: ConfigurationParameterKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ConfigurationParameterVersionKey {
    configuration_parameter: ConfigurationParameterKey;
    version: number;
}

export interface ConfigurationParameterVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListConfigurationParametersRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ConfigurationParametersFilter | null;
}

export interface ListConfigurationParametersResponse {
    result: Result;
    parameter_values: ConfigurationParameter[];
    total: number;
}

export interface GetConfigurationParameterRequest {
    key: ConfigurationParameterKey;
}

export interface GetConfigurationParameterResponse {
    result: Result;
    configuration_parameter: ConfigurationParameter | null;
}

export interface GetManyConfigurationParametersRequest {
    keys: ConfigurationParameterKey[];
}

export interface GetManyConfigurationParametersResponse {
    result: Result;
    entries: ConfigurationParameterLookup[];
}

export interface PutConfigurationParameterRequest {
    change: ConfigurationParameterChange;
    intent: ChangeIntent;
}

export interface PutConfigurationParameterResponse {
    result: Result;
    configuration_parameter: ConfigurationParameter | null;
}

export interface PutManyConfigurationParametersRequest {
    changes: ConfigurationParameterChange[];
    intent: ChangeIntent;
}

export interface PutManyConfigurationParametersResponse {
    result: Result;
    parameter_values: ConfigurationParameter[];
}

export interface DeleteConfigurationParameterRequest {
    removal: ConfigurationParameterRemoval;
    intent: ChangeIntent;
}

export interface DeleteConfigurationParameterResponse {
    result: Result;
}

export interface DeleteManyConfigurationParametersRequest {
    removals: ConfigurationParameterRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyConfigurationParametersResponse {
    result: Result;
}

export interface ListByConfigurationIdConfigurationParametersRequest {
    configuration_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: ConfigurationParametersFilter | null;
}

export interface ListByConfigurationIdConfigurationParametersResponse {
    result: Result;
    parameter_values: ConfigurationParameter[];
    total: number;
}

export interface ListByParameterDefinitionIdConfigurationParametersRequest {
    parameter_definition_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: ConfigurationParametersFilter | null;
}

export interface ListByParameterDefinitionIdConfigurationParametersResponse {
    result: Result;
    parameter_values: ConfigurationParameter[];
    total: number;
}

export interface ListConfigurationParameterVersionsRequest {
    key: ConfigurationParameterKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ConfigurationParameterVersionsFilter | null;
}

export interface ListConfigurationParameterVersionsResponse {
    result: Result;
    versions: ConfigurationParameter[];
    total: number;
}

export interface GetConfigurationParameterVersionRequest {
    key: ConfigurationParameterVersionKey;
}

export interface GetConfigurationParameterVersionResponse {
    result: Result;
    version: ConfigurationParameter | null;
}

export const subjects = {
    list_configuration_parameters_request: 'reporting.v1.configuration_parameters.list',
    get_configuration_parameter_request: 'reporting.v1.configuration_parameters.get',
    get_many_configuration_parameters_request: 'reporting.v1.configuration_parameters.get_many',
    put_configuration_parameter_request: 'reporting.v1.configuration_parameters.put',
    put_many_configuration_parameters_request: 'reporting.v1.configuration_parameters.put_many',
    delete_configuration_parameter_request: 'reporting.v1.configuration_parameters.delete',
    delete_many_configuration_parameters_request:
        'reporting.v1.configuration_parameters.delete_many',
    list_by_configuration_id_configuration_parameters_request:
        'reporting.v1.configuration_parameters.list_by_configuration_id',
    list_by_parameter_definition_id_configuration_parameters_request:
        'reporting.v1.configuration_parameters.list_by_parameter_definition_id',
    list_configuration_parameter_versions_request:
        'reporting.v1.configuration_parameters_versions.list',
    get_configuration_parameter_version_request:
        'reporting.v1.configuration_parameters_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_configuration_parameters_request: true,
    get_configuration_parameter_request: true,
    get_many_configuration_parameters_request: true,
    put_configuration_parameter_request: true,
    put_many_configuration_parameters_request: true,
    delete_configuration_parameter_request: true,
    delete_many_configuration_parameters_request: true,
    list_by_configuration_id_configuration_parameters_request: true,
    list_by_parameter_definition_id_configuration_parameters_request: true,
    list_configuration_parameter_versions_request: true,
    get_configuration_parameter_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.configuration_parameters_events.created',
    updated: 'reporting.v1.configuration_parameters_events.updated',
    deleted: 'reporting.v1.configuration_parameters_events.deleted',
} as const;
