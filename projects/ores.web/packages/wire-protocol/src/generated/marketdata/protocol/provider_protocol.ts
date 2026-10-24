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
import type { Provider } from '../domain/provider.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ProviderKey {
    code: string;
}

export interface ProviderWrite {
    code: string;
    name: string;
    kind: string;
    description: string;
    display_order: number;
}

export interface ProviderChange {
    write: ProviderWrite;
    precondition: Precondition;
}

export interface ProviderRemoval {
    key: ProviderKey;
    precondition: Precondition;
}

export interface ProviderLookup {
    key: ProviderKey;
    provider: Provider | null;
}

export interface ProvidersFilter {
    code_one_of: string[] | null;
}

export interface ProviderEvent {
    event_id: string;
    key: ProviderKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ProviderVersionKey {
    provider: ProviderKey;
    version: number;
}

export interface ProviderVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListProvidersRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ProvidersFilter | null;
    as_of: string | null;
}

export interface ListProvidersResponse {
    result: Result;
    providers: Provider[];
    total: number;
}

export interface GetProviderRequest {
    key: ProviderKey;
}

export interface GetProviderResponse {
    result: Result;
    provider: Provider | null;
}

export interface GetManyProvidersRequest {
    keys: ProviderKey[];
}

export interface GetManyProvidersResponse {
    result: Result;
    entries: ProviderLookup[];
}

export interface PutProviderRequest {
    change: ProviderChange;
    intent: ChangeIntent;
}

export interface PutProviderResponse {
    result: Result;
    provider: Provider | null;
}

export interface PutManyProvidersRequest {
    changes: ProviderChange[];
    intent: ChangeIntent;
}

export interface PutManyProvidersResponse {
    result: Result;
    providers: Provider[];
}

export interface DeleteProviderRequest {
    removal: ProviderRemoval;
    intent: ChangeIntent;
}

export interface DeleteProviderResponse {
    result: Result;
}

export interface DeleteManyProvidersRequest {
    removals: ProviderRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyProvidersResponse {
    result: Result;
}

export interface ListProviderVersionsRequest {
    key: ProviderKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ProviderVersionsFilter | null;
}

export interface ListProviderVersionsResponse {
    result: Result;
    versions: Provider[];
    total: number;
}

export interface GetProviderVersionRequest {
    key: ProviderVersionKey;
}

export interface GetProviderVersionResponse {
    result: Result;
    version: Provider | null;
}

export const subjects = {
    list_providers_request: 'marketdata.v1.providers.list',
    get_provider_request: 'marketdata.v1.providers.get',
    get_many_providers_request: 'marketdata.v1.providers.get_many',
    put_provider_request: 'marketdata.v1.providers.put',
    put_many_providers_request: 'marketdata.v1.providers.put_many',
    delete_provider_request: 'marketdata.v1.providers.delete',
    delete_many_providers_request: 'marketdata.v1.providers.delete_many',
    list_provider_versions_request: 'marketdata.v1.providers_versions.list',
    get_provider_version_request: 'marketdata.v1.providers_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_providers_request: true,
    get_provider_request: true,
    get_many_providers_request: true,
    put_provider_request: true,
    put_many_providers_request: true,
    delete_provider_request: true,
    delete_many_providers_request: true,
    list_provider_versions_request: true,
    get_provider_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.providers_events.created',
    updated: 'marketdata.v1.providers_events.updated',
    deleted: 'marketdata.v1.providers_events.deleted',
} as const;
