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
import type { IdentityProvider } from '../domain/identity_provider.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface IdentityProviderKey {
    code: string;
}

export interface IdentityProviderWrite {
    id: string;
    code: string;
    display_name: string;
    issuer: string;
    client_id: string;
    client_secret_ref: string;
    scopes: string;
    allowed_email_domains: string;
    is_enabled: boolean;
}

export interface IdentityProviderChange {
    write: IdentityProviderWrite;
    precondition: Precondition;
}

export interface IdentityProviderRemoval {
    key: IdentityProviderKey;
    precondition: Precondition;
}

export interface IdentityProviderLookup {
    key: IdentityProviderKey;
    identity_provider: IdentityProvider | null;
}

export interface IdentityProvidersFilter {
    id_one_of: string[] | null;
    search: string | null;
}

export interface IdentityProviderEvent {
    event_id: string;
    key: IdentityProviderKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface IdentityProviderVersionKey {
    identity_provider: IdentityProviderKey;
    version: number;
}

export interface IdentityProviderVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListIdentityProvidersRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: IdentityProvidersFilter | null;
    as_of: string | null;
}

export interface ListIdentityProvidersResponse {
    result: Result;
    providers: IdentityProvider[];
    total: number;
}

export interface GetIdentityProviderRequest {
    key: IdentityProviderKey;
}

export interface GetIdentityProviderResponse {
    result: Result;
    identity_provider: IdentityProvider | null;
}

export interface GetManyIdentityProvidersRequest {
    keys: IdentityProviderKey[];
}

export interface GetManyIdentityProvidersResponse {
    result: Result;
    entries: IdentityProviderLookup[];
}

export interface PutIdentityProviderRequest {
    change: IdentityProviderChange;
    intent: ChangeIntent;
}

export interface PutIdentityProviderResponse {
    result: Result;
    identity_provider: IdentityProvider | null;
}

export interface PutManyIdentityProvidersRequest {
    changes: IdentityProviderChange[];
    intent: ChangeIntent;
}

export interface PutManyIdentityProvidersResponse {
    result: Result;
    providers: IdentityProvider[];
}

export interface DeleteIdentityProviderRequest {
    removal: IdentityProviderRemoval;
    intent: ChangeIntent;
}

export interface DeleteIdentityProviderResponse {
    result: Result;
}

export interface DeleteManyIdentityProvidersRequest {
    removals: IdentityProviderRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyIdentityProvidersResponse {
    result: Result;
}

export interface ListIdentityProviderVersionsRequest {
    key: IdentityProviderKey;
    offset: number;
    limit: number;
    order: Order;
    filter: IdentityProviderVersionsFilter | null;
}

export interface ListIdentityProviderVersionsResponse {
    result: Result;
    versions: IdentityProvider[];
    total: number;
}

export interface GetIdentityProviderVersionRequest {
    key: IdentityProviderVersionKey;
}

export interface GetIdentityProviderVersionResponse {
    result: Result;
    version: IdentityProvider | null;
}

export const subjects = {
    list_identity_providers_request: 'iam.v1.identity_providers.list',
    get_identity_provider_request: 'iam.v1.identity_providers.get',
    get_many_identity_providers_request: 'iam.v1.identity_providers.get_many',
    put_identity_provider_request: 'iam.v1.identity_providers.put',
    put_many_identity_providers_request: 'iam.v1.identity_providers.put_many',
    delete_identity_provider_request: 'iam.v1.identity_providers.delete',
    delete_many_identity_providers_request: 'iam.v1.identity_providers.delete_many',
    list_identity_provider_versions_request: 'iam.v1.identity_providers_versions.list',
    get_identity_provider_version_request: 'iam.v1.identity_providers_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_identity_providers_request: true,
    get_identity_provider_request: true,
    get_many_identity_providers_request: true,
    put_identity_provider_request: true,
    put_many_identity_providers_request: true,
    delete_identity_provider_request: true,
    delete_many_identity_providers_request: true,
    list_identity_provider_versions_request: true,
    get_identity_provider_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'iam.v1.events.identity_providers.created',
    updated: 'iam.v1.events.identity_providers.updated',
    deleted: 'iam.v1.events.identity_providers.deleted',
} as const;
