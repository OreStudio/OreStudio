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
import type { ExternalIdentity } from '../domain/external_identity.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface ExternalIdentityKey {
    email: string;
}

export interface ExternalIdentityWrite {
    id: string;
    issuer: string;
    subject: string;
    account_id: string;
    identity_provider_id: string;
    email: string;
    email_verified: boolean;
    linked_at: string;
    last_used_at: string;
}

export interface ExternalIdentityChange {
    write: ExternalIdentityWrite;
    precondition: Precondition;
}

export interface ExternalIdentityRemoval {
    key: ExternalIdentityKey;
    precondition: Precondition;
}

export interface ExternalIdentityLookup {
    key: ExternalIdentityKey;
    external_identity: ExternalIdentity | null;
}

export interface ExternalIdentitiesFilter {
    account_id: string | null;
    identity_provider_id: string | null;
    id_one_of: string[] | null;
    account_id_one_of: string[] | null;
    identity_provider_id_one_of: string[] | null;
}

export interface ExternalIdentityEvent {
    event_id: string;
    key: ExternalIdentityKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ExternalIdentityVersionKey {
    external_identity: ExternalIdentityKey;
    version: number;
}

export interface ExternalIdentityVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListExternalIdentitiesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ExternalIdentitiesFilter | null;
    as_of: string | null;
}

export interface ListExternalIdentitiesResponse {
    result: Result;
    links: ExternalIdentity[];
    total: number;
}

export interface GetExternalIdentityRequest {
    key: ExternalIdentityKey;
}

export interface GetExternalIdentityResponse {
    result: Result;
    external_identity: ExternalIdentity | null;
}

export interface GetManyExternalIdentitiesRequest {
    keys: ExternalIdentityKey[];
}

export interface GetManyExternalIdentitiesResponse {
    result: Result;
    entries: ExternalIdentityLookup[];
}

export interface PutExternalIdentityRequest {
    change: ExternalIdentityChange;
    intent: ChangeIntent;
}

export interface PutExternalIdentityResponse {
    result: Result;
    external_identity: ExternalIdentity | null;
}

export interface PutManyExternalIdentitiesRequest {
    changes: ExternalIdentityChange[];
    intent: ChangeIntent;
}

export interface PutManyExternalIdentitiesResponse {
    result: Result;
    links: ExternalIdentity[];
}

export interface DeleteExternalIdentityRequest {
    removal: ExternalIdentityRemoval;
    intent: ChangeIntent;
}

export interface DeleteExternalIdentityResponse {
    result: Result;
}

export interface DeleteManyExternalIdentitiesRequest {
    removals: ExternalIdentityRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyExternalIdentitiesResponse {
    result: Result;
}

export interface ListByAccountIdExternalIdentitiesRequest {
    account_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: ExternalIdentitiesFilter | null;
}

export interface ListByAccountIdExternalIdentitiesResponse {
    result: Result;
    links: ExternalIdentity[];
    total: number;
}

export interface ListByIdentityProviderIdExternalIdentitiesRequest {
    identity_provider_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: ExternalIdentitiesFilter | null;
}

export interface ListByIdentityProviderIdExternalIdentitiesResponse {
    result: Result;
    links: ExternalIdentity[];
    total: number;
}

export interface ListExternalIdentityVersionsRequest {
    key: ExternalIdentityKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ExternalIdentityVersionsFilter | null;
}

export interface ListExternalIdentityVersionsResponse {
    result: Result;
    versions: ExternalIdentity[];
    total: number;
}

export interface GetExternalIdentityVersionRequest {
    key: ExternalIdentityVersionKey;
}

export interface GetExternalIdentityVersionResponse {
    result: Result;
    version: ExternalIdentity | null;
}

export const subjects = {
    list_external_identities_request: 'iam.v1.external_identities.list',
    get_external_identity_request: 'iam.v1.external_identities.get',
    get_many_external_identities_request: 'iam.v1.external_identities.get_many',
    put_external_identity_request: 'iam.v1.external_identities.put',
    put_many_external_identities_request: 'iam.v1.external_identities.put_many',
    delete_external_identity_request: 'iam.v1.external_identities.delete',
    delete_many_external_identities_request: 'iam.v1.external_identities.delete_many',
    list_by_account_id_external_identities_request: 'iam.v1.external_identities.list_by_account_id',
    list_by_identity_provider_id_external_identities_request:
        'iam.v1.external_identities.list_by_identity_provider_id',
    list_external_identity_versions_request: 'iam.v1.external_identities_versions.list',
    get_external_identity_version_request: 'iam.v1.external_identities_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_external_identities_request: true,
    get_external_identity_request: true,
    get_many_external_identities_request: true,
    put_external_identity_request: true,
    put_many_external_identities_request: true,
    delete_external_identity_request: true,
    delete_many_external_identities_request: true,
    list_by_account_id_external_identities_request: true,
    list_by_identity_provider_id_external_identities_request: true,
    list_external_identity_versions_request: true,
    get_external_identity_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'iam.v1.events.external_identities.created',
    updated: 'iam.v1.events.external_identities.updated',
    deleted: 'iam.v1.events.external_identities.deleted',
} as const;
