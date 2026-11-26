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
import type { Secret } from '../domain/secret.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface SecretKey {
    name: string;
}

export interface SecretWrite {
    id: string;
    name: string;
    owner: string;
    sealed_value: string;
    key_version: number;
    description: string;
}

export interface SecretChange {
    write: SecretWrite;
    precondition: Precondition;
}

export interface SecretRemoval {
    key: SecretKey;
    precondition: Precondition;
}

export interface SecretLookup {
    key: SecretKey;
    secret: Secret | null;
}

export interface SecretsFilter {
    id_one_of: string[] | null;
    search: string | null;
}

export interface SecretEvent {
    event_id: string;
    key: SecretKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SecretVersionKey {
    secret: SecretKey;
    version: number;
}

export interface SecretVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSecretsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: SecretsFilter | null;
    as_of: string | null;
}

export interface ListSecretsResponse {
    result: Result;
    secrets: Secret[];
    total: number;
}

export interface GetSecretRequest {
    key: SecretKey;
}

export interface GetSecretResponse {
    result: Result;
    secret: Secret | null;
}

export interface GetManySecretsRequest {
    keys: SecretKey[];
}

export interface GetManySecretsResponse {
    result: Result;
    entries: SecretLookup[];
}

export interface PutSecretRequest {
    change: SecretChange;
    intent: ChangeIntent;
}

export interface PutSecretResponse {
    result: Result;
    secret: Secret | null;
}

export interface PutManySecretsRequest {
    changes: SecretChange[];
    intent: ChangeIntent;
}

export interface PutManySecretsResponse {
    result: Result;
    secrets: Secret[];
}

export interface DeleteSecretRequest {
    removal: SecretRemoval;
    intent: ChangeIntent;
}

export interface DeleteSecretResponse {
    result: Result;
}

export interface DeleteManySecretsRequest {
    removals: SecretRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySecretsResponse {
    result: Result;
}

export interface ListSecretVersionsRequest {
    key: SecretKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SecretVersionsFilter | null;
}

export interface ListSecretVersionsResponse {
    result: Result;
    versions: Secret[];
    total: number;
}

export interface GetSecretVersionRequest {
    key: SecretVersionKey;
}

export interface GetSecretVersionResponse {
    result: Result;
    version: Secret | null;
}

export const subjects = {
    list_secrets_request: 'vault.v1.secrets.list',
    get_secret_request: 'vault.v1.secrets.get',
    get_many_secrets_request: 'vault.v1.secrets.get_many',
    put_secret_request: 'vault.v1.secrets.put',
    put_many_secrets_request: 'vault.v1.secrets.put_many',
    delete_secret_request: 'vault.v1.secrets.delete',
    delete_many_secrets_request: 'vault.v1.secrets.delete_many',
    list_secret_versions_request: 'vault.v1.secrets_versions.list',
    get_secret_version_request: 'vault.v1.secrets_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_secrets_request: true,
    get_secret_request: true,
    get_many_secrets_request: true,
    put_secret_request: true,
    put_many_secrets_request: true,
    delete_secret_request: true,
    delete_many_secrets_request: true,
    list_secret_versions_request: true,
    get_secret_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'vault.v1.events.secrets.created',
    updated: 'vault.v1.events.secrets.updated',
    deleted: 'vault.v1.events.secrets.deleted',
} as const;
