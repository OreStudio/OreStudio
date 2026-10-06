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
import type { TodaysMarketConfigurationBinding } from '../domain/todays_market_configuration_binding.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface TodaysMarketConfigurationBindingKey {
    reference: string;
}

export interface TodaysMarketConfigurationBindingWrite {
    id: string;
    todays_market_configuration_id: string;
    collection: string;
    reference: string;
    position: number;
}

export interface TodaysMarketConfigurationBindingChange {
    write: TodaysMarketConfigurationBindingWrite;
    precondition: Precondition;
}

export interface TodaysMarketConfigurationBindingRemoval {
    key: TodaysMarketConfigurationBindingKey;
    precondition: Precondition;
}

export interface TodaysMarketConfigurationBindingLookup {
    key: TodaysMarketConfigurationBindingKey;
    todays_market_configuration_binding: TodaysMarketConfigurationBinding | null;
}

export interface TodaysMarketConfigurationBindingsFilter {
    todays_market_configuration_id: string | null;
    id_one_of: string[] | null;
    todays_market_configuration_id_one_of: string[] | null;
}

export interface TodaysMarketConfigurationBindingEvent {
    event_id: string;
    key: TodaysMarketConfigurationBindingKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TodaysMarketConfigurationBindingVersionKey {
    todays_market_configuration_binding: TodaysMarketConfigurationBindingKey;
    version: number;
}

export interface TodaysMarketConfigurationBindingVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTodaysMarketConfigurationBindingsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: TodaysMarketConfigurationBindingsFilter | null;
    as_of: string | null;
}

export interface ListTodaysMarketConfigurationBindingsResponse {
    result: Result;
    bindings: TodaysMarketConfigurationBinding[];
    total: number;
}

export interface GetTodaysMarketConfigurationBindingRequest {
    key: TodaysMarketConfigurationBindingKey;
}

export interface GetTodaysMarketConfigurationBindingResponse {
    result: Result;
    todays_market_configuration_binding: TodaysMarketConfigurationBinding | null;
}

export interface GetManyTodaysMarketConfigurationBindingsRequest {
    keys: TodaysMarketConfigurationBindingKey[];
}

export interface GetManyTodaysMarketConfigurationBindingsResponse {
    result: Result;
    entries: TodaysMarketConfigurationBindingLookup[];
}

export interface PutTodaysMarketConfigurationBindingRequest {
    change: TodaysMarketConfigurationBindingChange;
    intent: ChangeIntent;
}

export interface PutTodaysMarketConfigurationBindingResponse {
    result: Result;
    todays_market_configuration_binding: TodaysMarketConfigurationBinding | null;
}

export interface PutManyTodaysMarketConfigurationBindingsRequest {
    changes: TodaysMarketConfigurationBindingChange[];
    intent: ChangeIntent;
}

export interface PutManyTodaysMarketConfigurationBindingsResponse {
    result: Result;
    bindings: TodaysMarketConfigurationBinding[];
}

export interface DeleteTodaysMarketConfigurationBindingRequest {
    removal: TodaysMarketConfigurationBindingRemoval;
    intent: ChangeIntent;
}

export interface DeleteTodaysMarketConfigurationBindingResponse {
    result: Result;
}

export interface DeleteManyTodaysMarketConfigurationBindingsRequest {
    removals: TodaysMarketConfigurationBindingRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTodaysMarketConfigurationBindingsResponse {
    result: Result;
}

export interface ListByTodaysMarketConfigurationIdTodaysMarketConfigurationBindingsRequest {
    todays_market_configuration_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: TodaysMarketConfigurationBindingsFilter | null;
}

export interface ListByTodaysMarketConfigurationIdTodaysMarketConfigurationBindingsResponse {
    result: Result;
    bindings: TodaysMarketConfigurationBinding[];
    total: number;
}

export interface ListTodaysMarketConfigurationBindingVersionsRequest {
    key: TodaysMarketConfigurationBindingKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TodaysMarketConfigurationBindingVersionsFilter | null;
}

export interface ListTodaysMarketConfigurationBindingVersionsResponse {
    result: Result;
    versions: TodaysMarketConfigurationBinding[];
    total: number;
}

export interface GetTodaysMarketConfigurationBindingVersionRequest {
    key: TodaysMarketConfigurationBindingVersionKey;
}

export interface GetTodaysMarketConfigurationBindingVersionResponse {
    result: Result;
    version: TodaysMarketConfigurationBinding | null;
}

export const subjects = {
    list_todays_market_configuration_bindings_request:
        'analytics.v1.todays_market_configuration_bindings.list',
    get_todays_market_configuration_binding_request:
        'analytics.v1.todays_market_configuration_bindings.get',
    get_many_todays_market_configuration_bindings_request:
        'analytics.v1.todays_market_configuration_bindings.get_many',
    put_todays_market_configuration_binding_request:
        'analytics.v1.todays_market_configuration_bindings.put',
    put_many_todays_market_configuration_bindings_request:
        'analytics.v1.todays_market_configuration_bindings.put_many',
    delete_todays_market_configuration_binding_request:
        'analytics.v1.todays_market_configuration_bindings.delete',
    delete_many_todays_market_configuration_bindings_request:
        'analytics.v1.todays_market_configuration_bindings.delete_many',
    list_by_todays_market_configuration_id_todays_market_configuration_bindings_request:
        'analytics.v1.todays_market_configuration_bindings.list_by_todays_market_configuration_id',
    list_todays_market_configuration_binding_versions_request:
        'analytics.v1.todays_market_configuration_bindings_versions.list',
    get_todays_market_configuration_binding_version_request:
        'analytics.v1.todays_market_configuration_bindings_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_todays_market_configuration_bindings_request: true,
    get_todays_market_configuration_binding_request: true,
    get_many_todays_market_configuration_bindings_request: true,
    put_todays_market_configuration_binding_request: true,
    put_many_todays_market_configuration_bindings_request: true,
    delete_todays_market_configuration_binding_request: true,
    delete_many_todays_market_configuration_bindings_request: true,
    list_by_todays_market_configuration_id_todays_market_configuration_bindings_request: true,
    list_todays_market_configuration_binding_versions_request: true,
    get_todays_market_configuration_binding_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'analytics.v1.todays_market_configuration_bindings_events.created',
    updated: 'analytics.v1.todays_market_configuration_bindings_events.updated',
    deleted: 'analytics.v1.todays_market_configuration_bindings_events.deleted',
} as const;
