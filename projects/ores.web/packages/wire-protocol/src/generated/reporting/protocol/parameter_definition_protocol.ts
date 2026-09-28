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
import type { ParameterDefinition } from '../domain/parameter_definition.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface ParameterDefinitionKey {
    name: string;
}

export interface ParameterDefinitionWrite {
    id: string;
    scope: string;
    subtype: string;
    name: string;
    position: number;
    parameter_value_domain_code: string;
    is_required: boolean;
}

export interface ParameterDefinitionChange {
    write: ParameterDefinitionWrite;
    precondition: Precondition;
}

export interface ParameterDefinitionRemoval {
    key: ParameterDefinitionKey;
    precondition: Precondition;
}

export interface ParameterDefinitionLookup {
    key: ParameterDefinitionKey;
    parameter_definition: ParameterDefinition | null;
}

export interface ParameterDefinitionsFilter {
    parameter_value_domain_code: string | null;
}

export interface ParameterDefinitionEvent {
    event_id: string;
    key: ParameterDefinitionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ParameterDefinitionVersionKey {
    parameter_definition: ParameterDefinitionKey;
    version: number;
}

export interface ParameterDefinitionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListParameterDefinitionsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ParameterDefinitionsFilter | null;
}

export interface ListParameterDefinitionsResponse {
    result: Result;
    parameters: ParameterDefinition[];
    total: number;
}

export interface GetParameterDefinitionRequest {
    key: ParameterDefinitionKey;
}

export interface GetParameterDefinitionResponse {
    result: Result;
    parameter_definition: ParameterDefinition | null;
}

export interface GetManyParameterDefinitionsRequest {
    keys: ParameterDefinitionKey[];
}

export interface GetManyParameterDefinitionsResponse {
    result: Result;
    entries: ParameterDefinitionLookup[];
}

export interface PutParameterDefinitionRequest {
    change: ParameterDefinitionChange;
    intent: ChangeIntent;
}

export interface PutParameterDefinitionResponse {
    result: Result;
    parameter_definition: ParameterDefinition;
}

export interface PutManyParameterDefinitionsRequest {
    changes: ParameterDefinitionChange[];
    intent: ChangeIntent;
}

export interface PutManyParameterDefinitionsResponse {
    result: Result;
    parameters: ParameterDefinition[];
}

export interface DeleteParameterDefinitionRequest {
    removal: ParameterDefinitionRemoval;
    intent: ChangeIntent;
}

export interface DeleteParameterDefinitionResponse {
    result: Result;
}

export interface DeleteManyParameterDefinitionsRequest {
    removals: ParameterDefinitionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyParameterDefinitionsResponse {
    result: Result;
}

export interface ListByParameterValueDomainCodeParameterDefinitionsRequest {
    parameter_value_domain_code: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: ParameterDefinitionsFilter | null;
}

export interface ListByParameterValueDomainCodeParameterDefinitionsResponse {
    result: Result;
    parameters: ParameterDefinition[];
    total: number;
}

export interface ListParameterDefinitionVersionsRequest {
    key: ParameterDefinitionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ParameterDefinitionVersionsFilter | null;
}

export interface ListParameterDefinitionVersionsResponse {
    result: Result;
    versions: ParameterDefinition[];
    total: number;
}

export interface GetParameterDefinitionVersionRequest {
    key: ParameterDefinitionVersionKey;
}

export interface GetParameterDefinitionVersionResponse {
    result: Result;
    version: ParameterDefinition;
}

export const subjects = {
    list_parameter_definitions_request: 'reporting.v1.parameter_definitions.list',
    get_parameter_definition_request: 'reporting.v1.parameter_definitions.get',
    get_many_parameter_definitions_request: 'reporting.v1.parameter_definitions.get_many',
    put_parameter_definition_request: 'reporting.v1.parameter_definitions.put',
    put_many_parameter_definitions_request: 'reporting.v1.parameter_definitions.put_many',
    delete_parameter_definition_request: 'reporting.v1.parameter_definitions.delete',
    delete_many_parameter_definitions_request: 'reporting.v1.parameter_definitions.delete_many',
    list_by_parameter_value_domain_code_parameter_definitions_request:
        'reporting.v1.parameter_definitions.list_by_parameter_value_domain_code',
    list_parameter_definition_versions_request: 'reporting.v1.parameter_definitions_versions.list',
    get_parameter_definition_version_request: 'reporting.v1.parameter_definitions_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_parameter_definitions_request: true,
    get_parameter_definition_request: true,
    get_many_parameter_definitions_request: true,
    put_parameter_definition_request: true,
    put_many_parameter_definitions_request: true,
    delete_parameter_definition_request: true,
    delete_many_parameter_definitions_request: true,
    list_by_parameter_value_domain_code_parameter_definitions_request: true,
    list_parameter_definition_versions_request: true,
    get_parameter_definition_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.parameter_definitions_events.created',
    updated: 'reporting.v1.parameter_definitions_events.updated',
    deleted: 'reporting.v1.parameter_definitions_events.deleted',
} as const;
