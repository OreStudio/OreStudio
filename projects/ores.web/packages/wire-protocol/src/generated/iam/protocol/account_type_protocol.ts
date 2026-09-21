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
import type { AccountType } from '../domain/account_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface AccountTypeKey {
    type: string;
}

export interface AccountTypeWrite {
    type: string;
    name: string;
    description: string;
    display_order: number;
}

export interface AccountTypeChange {
    write: AccountTypeWrite;
    precondition: Precondition;
}

export interface AccountTypeRemoval {
    key: AccountTypeKey;
    precondition: Precondition;
}

export interface AccountTypeLookup {
    key: AccountTypeKey;
    account_type: AccountType | null;
}

export interface AccountTypeEvent {
    event_id: string;
    key: AccountTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface AccountTypeVersionKey {
    account_type: AccountTypeKey;
    version: number;
}

export interface AccountTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListAccountTypesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListAccountTypesResponse {
    result: Result;
    types: AccountType[];
    total: number;
}

export interface GetAccountTypeRequest {
    key: AccountTypeKey;
}

export interface GetAccountTypeResponse {
    result: Result;
    account_type: AccountType | null;
}

export interface GetManyAccountTypesRequest {
    keys: AccountTypeKey[];
}

export interface GetManyAccountTypesResponse {
    result: Result;
    entries: AccountTypeLookup[];
}

export interface PutAccountTypeRequest {
    change: AccountTypeChange;
    intent: ChangeIntent;
}

export interface PutAccountTypeResponse {
    result: Result;
    account_type: AccountType;
}

export interface PutManyAccountTypesRequest {
    changes: AccountTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyAccountTypesResponse {
    result: Result;
    types: AccountType[];
}

export interface DeleteAccountTypeRequest {
    removal: AccountTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteAccountTypeResponse {
    result: Result;
}

export interface DeleteManyAccountTypesRequest {
    removals: AccountTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyAccountTypesResponse {
    result: Result;
}

export interface ListAccountTypeVersionsRequest {
    key: AccountTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: AccountTypeVersionsFilter | null;
}

export interface ListAccountTypeVersionsResponse {
    result: Result;
    versions: AccountType[];
    total: number;
}

export interface GetAccountTypeVersionRequest {
    key: AccountTypeVersionKey;
}

export interface GetAccountTypeVersionResponse {
    result: Result;
    version: AccountType;
}

export const subjects = {
    list_account_types_request: "iam.v1.account_types.list",
    get_account_type_request: "iam.v1.account_types.get",
    get_many_account_types_request: "iam.v1.account_types.get_many",
    put_account_type_request: "iam.v1.account_types.put",
    put_many_account_types_request: "iam.v1.account_types.put_many",
    delete_account_type_request: "iam.v1.account_types.delete",
    delete_many_account_types_request: "iam.v1.account_types.delete_many",
    list_account_type_versions_request: "iam.v1.account_types_versions.list",
    get_account_type_version_request: "iam.v1.account_types_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_account_types_request: true,
    get_account_type_request: true,
    get_many_account_types_request: true,
    put_account_type_request: true,
    put_many_account_types_request: true,
    delete_account_type_request: true,
    delete_many_account_types_request: true,
    list_account_type_versions_request: true,
    get_account_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "iam.v1.account_types_events.created",
    updated: "iam.v1.account_types_events.updated",
    deleted: "iam.v1.account_types_events.deleted",
} as const;
