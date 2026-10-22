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
import type { OreParameter } from '../domain/ore_parameter.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface OreParameterKey {
    name: string;
}

export interface OreParameterWrite {
    id: string;
    scope: string;
    subtype: string;
    name: string;
    position: number;
    ore_parameter_value_domain_code: string;
    is_required: boolean;
    label: string;
    help: string;
    description: string;
    default_text: string;
    format: string;
    introduced_in: string;
    removed_in: string;
}

export interface OreParameterChange {
    write: OreParameterWrite;
    precondition: Precondition;
}

export interface OreParameterRemoval {
    key: OreParameterKey;
    precondition: Precondition;
}

export interface OreParameterLookup {
    key: OreParameterKey;
    ore_parameter: OreParameter | null;
}

export interface OreParametersFilter {
    ore_parameter_value_domain_code: string | null;
    id_one_of: string[] | null;
    ore_parameter_value_domain_code_one_of: string[] | null;
}

export interface OreParameterEvent {
    event_id: string;
    key: OreParameterKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface OreParameterVersionKey {
    ore_parameter: OreParameterKey;
    version: number;
}

export interface OreParameterVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListOreParametersRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: OreParametersFilter | null;
    as_of: string | null;
}

export interface ListOreParametersResponse {
    result: Result;
    parameters: OreParameter[];
    total: number;
}

export interface GetOreParameterRequest {
    key: OreParameterKey;
}

export interface GetOreParameterResponse {
    result: Result;
    ore_parameter: OreParameter | null;
}

export interface GetManyOreParametersRequest {
    keys: OreParameterKey[];
}

export interface GetManyOreParametersResponse {
    result: Result;
    entries: OreParameterLookup[];
}

export interface PutOreParameterRequest {
    change: OreParameterChange;
    intent: ChangeIntent;
}

export interface PutOreParameterResponse {
    result: Result;
    ore_parameter: OreParameter | null;
}

export interface PutManyOreParametersRequest {
    changes: OreParameterChange[];
    intent: ChangeIntent;
}

export interface PutManyOreParametersResponse {
    result: Result;
    parameters: OreParameter[];
}

export interface DeleteOreParameterRequest {
    removal: OreParameterRemoval;
    intent: ChangeIntent;
}

export interface DeleteOreParameterResponse {
    result: Result;
}

export interface DeleteManyOreParametersRequest {
    removals: OreParameterRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyOreParametersResponse {
    result: Result;
}

export interface ListByOreParameterValueDomainCodeOreParametersRequest {
    ore_parameter_value_domain_code: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: OreParametersFilter | null;
}

export interface ListByOreParameterValueDomainCodeOreParametersResponse {
    result: Result;
    parameters: OreParameter[];
    total: number;
}

export interface ListOreParameterVersionsRequest {
    key: OreParameterKey;
    offset: number;
    limit: number;
    order: Order;
    filter: OreParameterVersionsFilter | null;
}

export interface ListOreParameterVersionsResponse {
    result: Result;
    versions: OreParameter[];
    total: number;
}

export interface GetOreParameterVersionRequest {
    key: OreParameterVersionKey;
}

export interface GetOreParameterVersionResponse {
    result: Result;
    version: OreParameter | null;
}

export const subjects = {
    list_ore_parameters_request: 'reporting.v1.ore_parameters.list',
    get_ore_parameter_request: 'reporting.v1.ore_parameters.get',
    get_many_ore_parameters_request: 'reporting.v1.ore_parameters.get_many',
    put_ore_parameter_request: 'reporting.v1.ore_parameters.put',
    put_many_ore_parameters_request: 'reporting.v1.ore_parameters.put_many',
    delete_ore_parameter_request: 'reporting.v1.ore_parameters.delete',
    delete_many_ore_parameters_request: 'reporting.v1.ore_parameters.delete_many',
    list_by_ore_parameter_value_domain_code_ore_parameters_request:
        'reporting.v1.ore_parameters.list_by_ore_parameter_value_domain_code',
    list_ore_parameter_versions_request: 'reporting.v1.ore_parameters_versions.list',
    get_ore_parameter_version_request: 'reporting.v1.ore_parameters_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_ore_parameters_request: true,
    get_ore_parameter_request: true,
    get_many_ore_parameters_request: true,
    put_ore_parameter_request: true,
    put_many_ore_parameters_request: true,
    delete_ore_parameter_request: true,
    delete_many_ore_parameters_request: true,
    list_by_ore_parameter_value_domain_code_ore_parameters_request: true,
    list_ore_parameter_versions_request: true,
    get_ore_parameter_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.ore_parameters_events.created',
    updated: 'reporting.v1.ore_parameters_events.updated',
    deleted: 'reporting.v1.ore_parameters_events.deleted',
} as const;
