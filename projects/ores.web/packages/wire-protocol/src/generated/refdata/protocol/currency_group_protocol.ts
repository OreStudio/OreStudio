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
import type { CurrencyGroup } from '../domain/currency_group.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurrencyGroupKey {
    code: string;
}

export interface CurrencyGroupWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface CurrencyGroupChange {
    write: CurrencyGroupWrite;
    precondition: Precondition;
}

export interface CurrencyGroupRemoval {
    key: CurrencyGroupKey;
    precondition: Precondition;
}

export interface CurrencyGroupLookup {
    key: CurrencyGroupKey;
    currency_group: CurrencyGroup | null;
}

export interface CurrencyGroupEvent {
    event_id: string;
    key: CurrencyGroupKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurrencyGroupVersionKey {
    currency_group: CurrencyGroupKey;
    version: number;
}

export interface CurrencyGroupVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurrencyGroupsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCurrencyGroupsResponse {
    result: Result;
    groups: CurrencyGroup[];
    total: number;
}

export interface GetCurrencyGroupRequest {
    key: CurrencyGroupKey;
}

export interface GetCurrencyGroupResponse {
    result: Result;
    currency_group: CurrencyGroup | null;
}

export interface GetManyCurrencyGroupsRequest {
    keys: CurrencyGroupKey[];
}

export interface GetManyCurrencyGroupsResponse {
    result: Result;
    entries: CurrencyGroupLookup[];
}

export interface PutCurrencyGroupRequest {
    change: CurrencyGroupChange;
    intent: ChangeIntent;
}

export interface PutCurrencyGroupResponse {
    result: Result;
    currency_group: CurrencyGroup;
}

export interface PutManyCurrencyGroupsRequest {
    changes: CurrencyGroupChange[];
    intent: ChangeIntent;
}

export interface PutManyCurrencyGroupsResponse {
    result: Result;
    groups: CurrencyGroup[];
}

export interface DeleteCurrencyGroupRequest {
    removal: CurrencyGroupRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurrencyGroupResponse {
    result: Result;
}

export interface DeleteManyCurrencyGroupsRequest {
    removals: CurrencyGroupRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurrencyGroupsResponse {
    result: Result;
}

export interface ListCurrencyGroupVersionsRequest {
    key: CurrencyGroupKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurrencyGroupVersionsFilter | null;
}

export interface ListCurrencyGroupVersionsResponse {
    result: Result;
    versions: CurrencyGroup[];
    total: number;
}

export interface GetCurrencyGroupVersionRequest {
    key: CurrencyGroupVersionKey;
}

export interface GetCurrencyGroupVersionResponse {
    result: Result;
    version: CurrencyGroup;
}

export const subjects = {
    list_currency_groups_request: "refdata.v1.currency_groups.list",
    get_currency_group_request: "refdata.v1.currency_groups.get",
    get_many_currency_groups_request: "refdata.v1.currency_groups.get_many",
    put_currency_group_request: "refdata.v1.currency_groups.put",
    put_many_currency_groups_request: "refdata.v1.currency_groups.put_many",
    delete_currency_group_request: "refdata.v1.currency_groups.delete",
    delete_many_currency_groups_request: "refdata.v1.currency_groups.delete_many",
    list_currency_group_versions_request: "refdata.v1.currency_groups_versions.list",
    get_currency_group_version_request: "refdata.v1.currency_groups_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_currency_groups_request: true,
    get_currency_group_request: true,
    get_many_currency_groups_request: true,
    put_currency_group_request: true,
    put_many_currency_groups_request: true,
    delete_currency_group_request: true,
    delete_many_currency_groups_request: true,
    list_currency_group_versions_request: true,
    get_currency_group_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.currency_groups_events.created",
    updated: "refdata.v1.currency_groups_events.updated",
    deleted: "refdata.v1.currency_groups_events.deleted",
} as const;
