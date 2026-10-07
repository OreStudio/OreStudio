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
import type { AccountCredential } from '../domain/account_credential.js';
import type { Order } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface AccountCredentialKey {
    id: string;
}

export interface AccountCredentialLookup {
    key: AccountCredentialKey;
    account_credential: AccountCredential | null;
}

export interface AccountCredentialsFilter {
    account_id: string | null;
    id_one_of: string[] | null;
    account_id_one_of: string[] | null;
}

export interface AccountCredentialEvent {
    event_id: string;
    key: AccountCredentialKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface AccountCredentialVersionKey {
    account_credential: AccountCredentialKey;
    version: number;
}

export interface AccountCredentialVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListAccountCredentialsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: AccountCredentialsFilter | null;
    as_of: string | null;
}

export interface ListAccountCredentialsResponse {
    result: Result;
    account_credentials: AccountCredential[];
    total: number;
}

export interface GetAccountCredentialRequest {
    key: AccountCredentialKey;
}

export interface GetAccountCredentialResponse {
    result: Result;
    account_credential: AccountCredential | null;
}

export interface GetManyAccountCredentialsRequest {
    keys: AccountCredentialKey[];
}

export interface GetManyAccountCredentialsResponse {
    result: Result;
    entries: AccountCredentialLookup[];
}

export interface ListByAccountIdAccountCredentialsRequest {
    account_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: AccountCredentialsFilter | null;
}

export interface ListByAccountIdAccountCredentialsResponse {
    result: Result;
    account_credentials: AccountCredential[];
    total: number;
}

export interface ListAccountCredentialVersionsRequest {
    key: AccountCredentialKey;
    offset: number;
    limit: number;
    order: Order;
    filter: AccountCredentialVersionsFilter | null;
}

export interface ListAccountCredentialVersionsResponse {
    result: Result;
    versions: AccountCredential[];
    total: number;
}

export interface GetAccountCredentialVersionRequest {
    key: AccountCredentialVersionKey;
}

export interface GetAccountCredentialVersionResponse {
    result: Result;
    version: AccountCredential | null;
}

export const subjects = {
    list_account_credentials_request: 'iam.v1.account_credentials.list',
    get_account_credential_request: 'iam.v1.account_credentials.get',
    get_many_account_credentials_request: 'iam.v1.account_credentials.get_many',
    list_by_account_id_account_credentials_request: 'iam.v1.account_credentials.list_by_account_id',
    list_account_credential_versions_request: 'iam.v1.account_credentials_versions.list',
    get_account_credential_version_request: 'iam.v1.account_credentials_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_account_credentials_request: true,
    get_account_credential_request: true,
    get_many_account_credentials_request: true,
    list_by_account_id_account_credentials_request: true,
    list_account_credential_versions_request: true,
    get_account_credential_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'iam.v1.account_credentials_events.created',
    updated: 'iam.v1.account_credentials_events.updated',
    deleted: 'iam.v1.account_credentials_events.deleted',
} as const;
