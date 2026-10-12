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
import type { ConfigurationMarketBinding } from '../domain/configuration_market_binding.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface ConfigurationMarketBindingKey {
    id: string;
}

export interface ConfigurationMarketBindingWrite {
    id: string;
    configuration_id: string;
    role: string;
    market_configuration_name: string;
    position: number;
}

export interface ConfigurationMarketBindingChange {
    write: ConfigurationMarketBindingWrite;
    precondition: Precondition;
}

export interface ConfigurationMarketBindingRemoval {
    key: ConfigurationMarketBindingKey;
    precondition: Precondition;
}

export interface ConfigurationMarketBindingLookup {
    key: ConfigurationMarketBindingKey;
    configuration_market_binding: ConfigurationMarketBinding | null;
}

export interface ConfigurationMarketBindingsFilter {
    configuration_id: string | null;
    id_one_of: string[] | null;
    configuration_id_one_of: string[] | null;
}

export interface ConfigurationMarketBindingEvent {
    event_id: string;
    key: ConfigurationMarketBindingKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ConfigurationMarketBindingVersionKey {
    configuration_market_binding: ConfigurationMarketBindingKey;
    version: number;
}

export interface ConfigurationMarketBindingVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListConfigurationMarketBindingsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ConfigurationMarketBindingsFilter | null;
    as_of: string | null;
}

export interface ListConfigurationMarketBindingsResponse {
    result: Result;
    bindings: ConfigurationMarketBinding[];
    total: number;
}

export interface GetConfigurationMarketBindingRequest {
    key: ConfigurationMarketBindingKey;
}

export interface GetConfigurationMarketBindingResponse {
    result: Result;
    configuration_market_binding: ConfigurationMarketBinding | null;
}

export interface GetManyConfigurationMarketBindingsRequest {
    keys: ConfigurationMarketBindingKey[];
}

export interface GetManyConfigurationMarketBindingsResponse {
    result: Result;
    entries: ConfigurationMarketBindingLookup[];
}

export interface PutConfigurationMarketBindingRequest {
    change: ConfigurationMarketBindingChange;
    intent: ChangeIntent;
}

export interface PutConfigurationMarketBindingResponse {
    result: Result;
    configuration_market_binding: ConfigurationMarketBinding | null;
}

export interface PutManyConfigurationMarketBindingsRequest {
    changes: ConfigurationMarketBindingChange[];
    intent: ChangeIntent;
}

export interface PutManyConfigurationMarketBindingsResponse {
    result: Result;
    bindings: ConfigurationMarketBinding[];
}

export interface DeleteConfigurationMarketBindingRequest {
    removal: ConfigurationMarketBindingRemoval;
    intent: ChangeIntent;
}

export interface DeleteConfigurationMarketBindingResponse {
    result: Result;
}

export interface DeleteManyConfigurationMarketBindingsRequest {
    removals: ConfigurationMarketBindingRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyConfigurationMarketBindingsResponse {
    result: Result;
}

export interface ListByConfigurationIdConfigurationMarketBindingsRequest {
    configuration_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: ConfigurationMarketBindingsFilter | null;
}

export interface ListByConfigurationIdConfigurationMarketBindingsResponse {
    result: Result;
    bindings: ConfigurationMarketBinding[];
    total: number;
}

export interface ListConfigurationMarketBindingVersionsRequest {
    key: ConfigurationMarketBindingKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ConfigurationMarketBindingVersionsFilter | null;
}

export interface ListConfigurationMarketBindingVersionsResponse {
    result: Result;
    versions: ConfigurationMarketBinding[];
    total: number;
}

export interface GetConfigurationMarketBindingVersionRequest {
    key: ConfigurationMarketBindingVersionKey;
}

export interface GetConfigurationMarketBindingVersionResponse {
    result: Result;
    version: ConfigurationMarketBinding | null;
}

export const subjects = {
    list_configuration_market_bindings_request: 'reporting.v1.configuration_market_bindings.list',
    get_configuration_market_binding_request: 'reporting.v1.configuration_market_bindings.get',
    get_many_configuration_market_bindings_request:
        'reporting.v1.configuration_market_bindings.get_many',
    put_configuration_market_binding_request: 'reporting.v1.configuration_market_bindings.put',
    put_many_configuration_market_bindings_request:
        'reporting.v1.configuration_market_bindings.put_many',
    delete_configuration_market_binding_request:
        'reporting.v1.configuration_market_bindings.delete',
    delete_many_configuration_market_bindings_request:
        'reporting.v1.configuration_market_bindings.delete_many',
    list_by_configuration_id_configuration_market_bindings_request:
        'reporting.v1.configuration_market_bindings.list_by_configuration_id',
    list_configuration_market_binding_versions_request:
        'reporting.v1.configuration_market_bindings_versions.list',
    get_configuration_market_binding_version_request:
        'reporting.v1.configuration_market_bindings_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_configuration_market_bindings_request: true,
    get_configuration_market_binding_request: true,
    get_many_configuration_market_bindings_request: true,
    put_configuration_market_binding_request: true,
    put_many_configuration_market_bindings_request: true,
    delete_configuration_market_binding_request: true,
    delete_many_configuration_market_bindings_request: true,
    list_by_configuration_id_configuration_market_bindings_request: true,
    list_configuration_market_binding_versions_request: true,
    get_configuration_market_binding_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.configuration_market_bindings_events.created',
    updated: 'reporting.v1.configuration_market_bindings_events.updated',
    deleted: 'reporting.v1.configuration_market_bindings_events.deleted',
} as const;
