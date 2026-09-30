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
import type { AccountStatus } from '../domain/account_status.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface AccountStatusKey {
    status: string;
}

export interface AccountStatusWrite {
    status: string;
    name: string;
    description: string;
    display_order: number;
}

export interface AccountStatusChange {
    write: AccountStatusWrite;
    precondition: Precondition;
}

export interface AccountStatusRemoval {
    key: AccountStatusKey;
    precondition: Precondition;
}

export interface AccountStatusLookup {
    key: AccountStatusKey;
    account_status: AccountStatus | null;
}

export interface AccountStatusEvent {
    event_id: string;
    key: AccountStatusKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface AccountStatusVersionKey {
    account_status: AccountStatusKey;
    version: number;
}

export interface AccountStatusVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListAccountStatusesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListAccountStatusesResponse {
    result: Result;
    statuses: AccountStatus[];
    total: number;
}

export interface GetAccountStatusRequest {
    key: AccountStatusKey;
}

export interface GetAccountStatusResponse {
    result: Result;
    account_status: AccountStatus | null;
}

export interface GetManyAccountStatusesRequest {
    keys: AccountStatusKey[];
}

export interface GetManyAccountStatusesResponse {
    result: Result;
    entries: AccountStatusLookup[];
}

export interface PutAccountStatusRequest {
    change: AccountStatusChange;
    intent: ChangeIntent;
}

export interface PutAccountStatusResponse {
    result: Result;
    account_status: AccountStatus | null;
}

export interface PutManyAccountStatusesRequest {
    changes: AccountStatusChange[];
    intent: ChangeIntent;
}

export interface PutManyAccountStatusesResponse {
    result: Result;
    statuses: AccountStatus[];
}

export interface DeleteAccountStatusRequest {
    removal: AccountStatusRemoval;
    intent: ChangeIntent;
}

export interface DeleteAccountStatusResponse {
    result: Result;
}

export interface DeleteManyAccountStatusesRequest {
    removals: AccountStatusRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyAccountStatusesResponse {
    result: Result;
}

export interface ListAccountStatusVersionsRequest {
    key: AccountStatusKey;
    offset: number;
    limit: number;
    order: Order;
    filter: AccountStatusVersionsFilter | null;
}

export interface ListAccountStatusVersionsResponse {
    result: Result;
    versions: AccountStatus[];
    total: number;
}

export interface GetAccountStatusVersionRequest {
    key: AccountStatusVersionKey;
}

export interface GetAccountStatusVersionResponse {
    result: Result;
    version: AccountStatus | null;
}

export const subjects = {
    list_account_statuses_request: 'iam.v1.account_statuses.list',
    get_account_status_request: 'iam.v1.account_statuses.get',
    get_many_account_statuses_request: 'iam.v1.account_statuses.get_many',
    put_account_status_request: 'iam.v1.account_statuses.put',
    put_many_account_statuses_request: 'iam.v1.account_statuses.put_many',
    delete_account_status_request: 'iam.v1.account_statuses.delete',
    delete_many_account_statuses_request: 'iam.v1.account_statuses.delete_many',
    list_account_status_versions_request: 'iam.v1.account_statuses_versions.list',
    get_account_status_version_request: 'iam.v1.account_statuses_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_account_statuses_request: true,
    get_account_status_request: true,
    get_many_account_statuses_request: true,
    put_account_status_request: true,
    put_many_account_statuses_request: true,
    delete_account_status_request: true,
    delete_many_account_statuses_request: true,
    list_account_status_versions_request: true,
    get_account_status_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'iam.v1.account_statuses_events.created',
    updated: 'iam.v1.account_statuses_events.updated',
    deleted: 'iam.v1.account_statuses_events.deleted',
} as const;
