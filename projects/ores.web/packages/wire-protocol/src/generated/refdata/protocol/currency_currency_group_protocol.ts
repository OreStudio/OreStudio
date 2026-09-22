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
import type { CurrencyCurrencyGroup } from '../domain/currency_currency_group.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CurrencyCurrencyGroupKey {
    currency_iso_code: string;
    currency_group_code: string;
}

export interface CurrencyCurrencyGroupWrite {
    currency_iso_code: string;
    currency_group_code: string;
}

export interface CurrencyCurrencyGroupChange {
    write: CurrencyCurrencyGroupWrite;
    precondition: Precondition;
}

export interface CurrencyCurrencyGroupRemoval {
    key: CurrencyCurrencyGroupKey;
    precondition: Precondition;
}

export interface CurrencyCurrencyGroupLookup {
    key: CurrencyCurrencyGroupKey;
    currency_currency_group: CurrencyCurrencyGroup | null;
}

export interface CurrencyCurrencyGroupsFilter {
    currency_iso_code: string | null;
}

export interface CurrencyCurrencyGroupEvent {
    event_id: string;
    key: CurrencyCurrencyGroupKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListCurrencyCurrencyGroupsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CurrencyCurrencyGroupsFilter | null;
}

export interface ListCurrencyCurrencyGroupsResponse {
    result: Result;
    currency_currency_groups: CurrencyCurrencyGroup[];
    total: number;
}

export interface GetCurrencyCurrencyGroupRequest {
    key: CurrencyCurrencyGroupKey;
}

export interface GetCurrencyCurrencyGroupResponse {
    result: Result;
    currency_currency_group: CurrencyCurrencyGroup | null;
}

export interface GetManyCurrencyCurrencyGroupsRequest {
    keys: CurrencyCurrencyGroupKey[];
}

export interface GetManyCurrencyCurrencyGroupsResponse {
    result: Result;
    entries: CurrencyCurrencyGroupLookup[];
}

export interface PutCurrencyCurrencyGroupRequest {
    change: CurrencyCurrencyGroupChange;
    intent: ChangeIntent;
}

export interface PutCurrencyCurrencyGroupResponse {
    result: Result;
    currency_currency_group: CurrencyCurrencyGroup;
}

export interface PutManyCurrencyCurrencyGroupsRequest {
    changes: CurrencyCurrencyGroupChange[];
    intent: ChangeIntent;
}

export interface PutManyCurrencyCurrencyGroupsResponse {
    result: Result;
    currency_currency_groups: CurrencyCurrencyGroup[];
}

export interface DeleteCurrencyCurrencyGroupRequest {
    removal: CurrencyCurrencyGroupRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurrencyCurrencyGroupResponse {
    result: Result;
}

export interface DeleteManyCurrencyCurrencyGroupsRequest {
    removals: CurrencyCurrencyGroupRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurrencyCurrencyGroupsResponse {
    result: Result;
}

export interface ListByCurrencyIsoCodeCurrencyCurrencyGroupsRequest {
    currency_iso_code: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CurrencyCurrencyGroupsFilter | null;
}

export interface ListByCurrencyIsoCodeCurrencyCurrencyGroupsResponse {
    result: Result;
    currency_currency_groups: CurrencyCurrencyGroup[];
    total: number;
}

export const subjects = {
    list_currency_currency_groups_request: "refdata.v1.currency_currency_groups.list",
    get_currency_currency_group_request: "refdata.v1.currency_currency_groups.get",
    get_many_currency_currency_groups_request: "refdata.v1.currency_currency_groups.get_many",
    put_currency_currency_group_request: "refdata.v1.currency_currency_groups.put",
    put_many_currency_currency_groups_request: "refdata.v1.currency_currency_groups.put_many",
    delete_currency_currency_group_request: "refdata.v1.currency_currency_groups.delete",
    delete_many_currency_currency_groups_request: "refdata.v1.currency_currency_groups.delete_many",
    list_by_currency_iso_code_currency_currency_groups_request: "refdata.v1.currency_currency_groups.list_by_currency_iso_code",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_currency_currency_groups_request: true,
    get_currency_currency_group_request: true,
    get_many_currency_currency_groups_request: true,
    put_currency_currency_group_request: true,
    put_many_currency_currency_groups_request: true,
    delete_currency_currency_group_request: true,
    delete_many_currency_currency_groups_request: true,
    list_by_currency_iso_code_currency_currency_groups_request: true,
} as const;
