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
import type { ConfigurationType } from '../domain/configuration_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ConfigurationTypeKey {
    code: string;
}

export interface ConfigurationTypeWrite {
    code: string;
    name: string;
    display_order: number;
    ore_root_element: string;
    owning_component: string;
    run_parameter: string | null;
}

export interface ConfigurationTypeChange {
    write: ConfigurationTypeWrite;
    precondition: Precondition;
}

export interface ConfigurationTypeRemoval {
    key: ConfigurationTypeKey;
    precondition: Precondition;
}

export interface ConfigurationTypeLookup {
    key: ConfigurationTypeKey;
    configuration_type: ConfigurationType | null;
}

export interface ConfigurationTypeEvent {
    event_id: string;
    key: ConfigurationTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ConfigurationTypeVersionKey {
    configuration_type: ConfigurationTypeKey;
    version: number;
}

export interface ConfigurationTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListConfigurationTypesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListConfigurationTypesResponse {
    result: Result;
    types: ConfigurationType[];
    total: number;
}

export interface GetConfigurationTypeRequest {
    key: ConfigurationTypeKey;
}

export interface GetConfigurationTypeResponse {
    result: Result;
    configuration_type: ConfigurationType | null;
}

export interface GetManyConfigurationTypesRequest {
    keys: ConfigurationTypeKey[];
}

export interface GetManyConfigurationTypesResponse {
    result: Result;
    entries: ConfigurationTypeLookup[];
}

export interface PutConfigurationTypeRequest {
    change: ConfigurationTypeChange;
    intent: ChangeIntent;
}

export interface PutConfigurationTypeResponse {
    result: Result;
    configuration_type: ConfigurationType | null;
}

export interface PutManyConfigurationTypesRequest {
    changes: ConfigurationTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyConfigurationTypesResponse {
    result: Result;
    types: ConfigurationType[];
}

export interface DeleteConfigurationTypeRequest {
    removal: ConfigurationTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteConfigurationTypeResponse {
    result: Result;
}

export interface DeleteManyConfigurationTypesRequest {
    removals: ConfigurationTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyConfigurationTypesResponse {
    result: Result;
}

export interface ListConfigurationTypeVersionsRequest {
    key: ConfigurationTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ConfigurationTypeVersionsFilter | null;
}

export interface ListConfigurationTypeVersionsResponse {
    result: Result;
    versions: ConfigurationType[];
    total: number;
}

export interface GetConfigurationTypeVersionRequest {
    key: ConfigurationTypeVersionKey;
}

export interface GetConfigurationTypeVersionResponse {
    result: Result;
    version: ConfigurationType | null;
}

export const subjects = {
    list_configuration_types_request: 'reporting.v1.configuration_types.list',
    get_configuration_type_request: 'reporting.v1.configuration_types.get',
    get_many_configuration_types_request: 'reporting.v1.configuration_types.get_many',
    put_configuration_type_request: 'reporting.v1.configuration_types.put',
    put_many_configuration_types_request: 'reporting.v1.configuration_types.put_many',
    delete_configuration_type_request: 'reporting.v1.configuration_types.delete',
    delete_many_configuration_types_request: 'reporting.v1.configuration_types.delete_many',
    list_configuration_type_versions_request: 'reporting.v1.configuration_types_versions.list',
    get_configuration_type_version_request: 'reporting.v1.configuration_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_configuration_types_request: true,
    get_configuration_type_request: true,
    get_many_configuration_types_request: true,
    put_configuration_type_request: true,
    put_many_configuration_types_request: true,
    delete_configuration_type_request: true,
    delete_many_configuration_types_request: true,
    list_configuration_type_versions_request: true,
    get_configuration_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.configuration_types_events.created',
    updated: 'reporting.v1.configuration_types_events.updated',
    deleted: 'reporting.v1.configuration_types_events.deleted',
} as const;
