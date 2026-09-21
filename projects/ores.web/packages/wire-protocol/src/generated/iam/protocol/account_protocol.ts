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
import type { Account } from '../domain/account.js';
import type { Order } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface AccountKey {
    id: string;
}

export interface AccountLookup {
    key: AccountKey;
    account: Account | null;
}

export interface AccountEvent {
    event_id: string;
    key: AccountKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface AccountVersionKey {
    account: AccountKey;
    version: number;
}

export interface AccountVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListAccountsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListAccountsResponse {
    result: Result;
    accounts: Account[];
    total: number;
}

export interface GetAccountRequest {
    key: AccountKey;
}

export interface GetAccountResponse {
    result: Result;
    account: Account | null;
}

export interface GetManyAccountsRequest {
    keys: AccountKey[];
}

export interface GetManyAccountsResponse {
    result: Result;
    entries: AccountLookup[];
}

export interface ListAccountVersionsRequest {
    key: AccountKey;
    offset: number;
    limit: number;
    order: Order;
    filter: AccountVersionsFilter | null;
}

export interface ListAccountVersionsResponse {
    result: Result;
    versions: Account[];
    total: number;
}

export interface GetAccountVersionRequest {
    key: AccountVersionKey;
}

export interface GetAccountVersionResponse {
    result: Result;
    version: Account;
}

export const subjects = {
    list_accounts_request: "iam.v1.accounts.list",
    get_account_request: "iam.v1.accounts.get",
    get_many_accounts_request: "iam.v1.accounts.get_many",
    list_account_versions_request: "iam.v1.accounts_versions.list",
    get_account_version_request: "iam.v1.accounts_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_accounts_request: true,
    get_account_request: true,
    get_many_accounts_request: true,
    list_account_versions_request: true,
    get_account_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "iam.v1.accounts_events.created",
    updated: "iam.v1.accounts_events.updated",
    deleted: "iam.v1.accounts_events.deleted",
} as const;
