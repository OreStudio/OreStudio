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
import type { Adapter } from '../domain/adapter.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface AdapterKey {
    code: string;
}

export interface AdapterWrite {
    code: string;
    name: string;
    module_version: string;
    description: string;
    process: string;
    asset_classes: string;
    services: string;
    configuration_schema: string;
    display_order: number;
}

export interface AdapterChange {
    write: AdapterWrite;
    precondition: Precondition;
}

export interface AdapterRemoval {
    key: AdapterKey;
    precondition: Precondition;
}

export interface AdapterLookup {
    key: AdapterKey;
    adapter: Adapter | null;
}

export interface AdaptersFilter {
    code_one_of: string[] | null;
}

export interface AdapterEvent {
    event_id: string;
    key: AdapterKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface AdapterVersionKey {
    adapter: AdapterKey;
    version: number;
}

export interface AdapterVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListAdaptersRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: AdaptersFilter | null;
    as_of: string | null;
}

export interface ListAdaptersResponse {
    result: Result;
    adapters: Adapter[];
    total: number;
}

export interface GetAdapterRequest {
    key: AdapterKey;
}

export interface GetAdapterResponse {
    result: Result;
    adapter: Adapter | null;
}

export interface GetManyAdaptersRequest {
    keys: AdapterKey[];
}

export interface GetManyAdaptersResponse {
    result: Result;
    entries: AdapterLookup[];
}

export interface PutAdapterRequest {
    change: AdapterChange;
    intent: ChangeIntent;
}

export interface PutAdapterResponse {
    result: Result;
    adapter: Adapter | null;
}

export interface PutManyAdaptersRequest {
    changes: AdapterChange[];
    intent: ChangeIntent;
}

export interface PutManyAdaptersResponse {
    result: Result;
    adapters: Adapter[];
}

export interface DeleteAdapterRequest {
    removal: AdapterRemoval;
    intent: ChangeIntent;
}

export interface DeleteAdapterResponse {
    result: Result;
}

export interface DeleteManyAdaptersRequest {
    removals: AdapterRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyAdaptersResponse {
    result: Result;
}

export interface ListAdapterVersionsRequest {
    key: AdapterKey;
    offset: number;
    limit: number;
    order: Order;
    filter: AdapterVersionsFilter | null;
}

export interface ListAdapterVersionsResponse {
    result: Result;
    versions: Adapter[];
    total: number;
}

export interface GetAdapterVersionRequest {
    key: AdapterVersionKey;
}

export interface GetAdapterVersionResponse {
    result: Result;
    version: Adapter | null;
}

export const subjects = {
    list_adapters_request: 'marketdata.v1.adapters.list',
    get_adapter_request: 'marketdata.v1.adapters.get',
    get_many_adapters_request: 'marketdata.v1.adapters.get_many',
    put_adapter_request: 'marketdata.v1.adapters.put',
    put_many_adapters_request: 'marketdata.v1.adapters.put_many',
    delete_adapter_request: 'marketdata.v1.adapters.delete',
    delete_many_adapters_request: 'marketdata.v1.adapters.delete_many',
    list_adapter_versions_request: 'marketdata.v1.adapters_versions.list',
    get_adapter_version_request: 'marketdata.v1.adapters_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_adapters_request: true,
    get_adapter_request: true,
    get_many_adapters_request: true,
    put_adapter_request: true,
    put_many_adapters_request: true,
    delete_adapter_request: true,
    delete_many_adapters_request: true,
    list_adapter_versions_request: true,
    get_adapter_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.adapters_events.created',
    updated: 'marketdata.v1.adapters_events.updated',
    deleted: 'marketdata.v1.adapters_events.deleted',
} as const;
