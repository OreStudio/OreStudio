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
import type { Configuration } from '../domain/configuration.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface ConfigurationKey {
    name: string;
}

export interface ConfigurationWrite {
    id: string;
    name: string;
    configuration_type_code: string;
    owning_component: string;
}

export interface ConfigurationChange {
    write: ConfigurationWrite;
    precondition: Precondition;
}

export interface ConfigurationRemoval {
    key: ConfigurationKey;
    precondition: Precondition;
}

export interface ConfigurationLookup {
    key: ConfigurationKey;
    configuration: Configuration | null;
}

export interface ConfigurationsFilter {
    configuration_type_code: string | null;
    id_one_of: string[] | null;
    configuration_type_code_one_of: string[] | null;
}

export interface ConfigurationEvent {
    event_id: string;
    key: ConfigurationKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ConfigurationVersionKey {
    configuration: ConfigurationKey;
    version: number;
}

export interface ConfigurationVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListConfigurationsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ConfigurationsFilter | null;
}

export interface ListConfigurationsResponse {
    result: Result;
    configurations: Configuration[];
    total: number;
}

export interface GetConfigurationRequest {
    key: ConfigurationKey;
}

export interface GetConfigurationResponse {
    result: Result;
    configuration: Configuration | null;
}

export interface GetManyConfigurationsRequest {
    keys: ConfigurationKey[];
}

export interface GetManyConfigurationsResponse {
    result: Result;
    entries: ConfigurationLookup[];
}

export interface PutConfigurationRequest {
    change: ConfigurationChange;
    intent: ChangeIntent;
}

export interface PutConfigurationResponse {
    result: Result;
    configuration: Configuration | null;
}

export interface PutManyConfigurationsRequest {
    changes: ConfigurationChange[];
    intent: ChangeIntent;
}

export interface PutManyConfigurationsResponse {
    result: Result;
    configurations: Configuration[];
}

export interface DeleteConfigurationRequest {
    removal: ConfigurationRemoval;
    intent: ChangeIntent;
}

export interface DeleteConfigurationResponse {
    result: Result;
}

export interface DeleteManyConfigurationsRequest {
    removals: ConfigurationRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyConfigurationsResponse {
    result: Result;
}

export interface ListByConfigurationTypeCodeConfigurationsRequest {
    configuration_type_code: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: ConfigurationsFilter | null;
}

export interface ListByConfigurationTypeCodeConfigurationsResponse {
    result: Result;
    configurations: Configuration[];
    total: number;
}

export interface ListConfigurationVersionsRequest {
    key: ConfigurationKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ConfigurationVersionsFilter | null;
}

export interface ListConfigurationVersionsResponse {
    result: Result;
    versions: Configuration[];
    total: number;
}

export interface GetConfigurationVersionRequest {
    key: ConfigurationVersionKey;
}

export interface GetConfigurationVersionResponse {
    result: Result;
    version: Configuration | null;
}

export const subjects = {
    list_configurations_request: 'reporting.v1.configurations.list',
    get_configuration_request: 'reporting.v1.configurations.get',
    get_many_configurations_request: 'reporting.v1.configurations.get_many',
    put_configuration_request: 'reporting.v1.configurations.put',
    put_many_configurations_request: 'reporting.v1.configurations.put_many',
    delete_configuration_request: 'reporting.v1.configurations.delete',
    delete_many_configurations_request: 'reporting.v1.configurations.delete_many',
    list_by_configuration_type_code_configurations_request:
        'reporting.v1.configurations.list_by_configuration_type_code',
    list_configuration_versions_request: 'reporting.v1.configurations_versions.list',
    get_configuration_version_request: 'reporting.v1.configurations_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_configurations_request: true,
    get_configuration_request: true,
    get_many_configurations_request: true,
    put_configuration_request: true,
    put_many_configurations_request: true,
    delete_configuration_request: true,
    delete_many_configurations_request: true,
    list_by_configuration_type_code_configurations_request: true,
    list_configuration_versions_request: true,
    get_configuration_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.configurations_events.created',
    updated: 'reporting.v1.configurations_events.updated',
    deleted: 'reporting.v1.configurations_events.deleted',
} as const;
