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
import type { TradingAccountType } from '../domain/trading_account_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TradingAccountTypeKey {
    code: string;
    coding_scheme_code: string;
}

export interface TradingAccountTypeWrite {
    code: string;
    coding_scheme_code: string;
    source: string;
    description: string;
}

export interface TradingAccountTypeChange {
    write: TradingAccountTypeWrite;
    precondition: Precondition;
}

export interface TradingAccountTypeRemoval {
    key: TradingAccountTypeKey;
    precondition: Precondition;
}

export interface TradingAccountTypeLookup {
    key: TradingAccountTypeKey;
    trading_account_type: TradingAccountType | null;
}

export interface TradingAccountTypeEvent {
    event_id: string;
    key: TradingAccountTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TradingAccountTypeVersionKey {
    trading_account_type: TradingAccountTypeKey;
    version: number;
}

export interface TradingAccountTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTradingAccountTypesRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListTradingAccountTypesResponse {
    result: Result;
    types: TradingAccountType[];
    total: number;
}

export interface GetTradingAccountTypeRequest {
    key: TradingAccountTypeKey;
}

export interface GetTradingAccountTypeResponse {
    result: Result;
    trading_account_type: TradingAccountType | null;
}

export interface GetManyTradingAccountTypesRequest {
    keys: TradingAccountTypeKey[];
}

export interface GetManyTradingAccountTypesResponse {
    result: Result;
    entries: TradingAccountTypeLookup[];
}

export interface PutTradingAccountTypeRequest {
    change: TradingAccountTypeChange;
    intent: ChangeIntent;
}

export interface PutTradingAccountTypeResponse {
    result: Result;
    trading_account_type: TradingAccountType | null;
}

export interface PutManyTradingAccountTypesRequest {
    changes: TradingAccountTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyTradingAccountTypesResponse {
    result: Result;
    types: TradingAccountType[];
}

export interface DeleteTradingAccountTypeRequest {
    removal: TradingAccountTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteTradingAccountTypeResponse {
    result: Result;
}

export interface DeleteManyTradingAccountTypesRequest {
    removals: TradingAccountTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTradingAccountTypesResponse {
    result: Result;
}

export interface ListTradingAccountTypeVersionsRequest {
    key: TradingAccountTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TradingAccountTypeVersionsFilter | null;
}

export interface ListTradingAccountTypeVersionsResponse {
    result: Result;
    versions: TradingAccountType[];
    total: number;
}

export interface GetTradingAccountTypeVersionRequest {
    key: TradingAccountTypeVersionKey;
}

export interface GetTradingAccountTypeVersionResponse {
    result: Result;
    version: TradingAccountType | null;
}

export const subjects = {
    list_trading_account_types_request: 'refdata.v1.trading_account_types.list',
    get_trading_account_type_request: 'refdata.v1.trading_account_types.get',
    get_many_trading_account_types_request: 'refdata.v1.trading_account_types.get_many',
    put_trading_account_type_request: 'refdata.v1.trading_account_types.put',
    put_many_trading_account_types_request: 'refdata.v1.trading_account_types.put_many',
    delete_trading_account_type_request: 'refdata.v1.trading_account_types.delete',
    delete_many_trading_account_types_request: 'refdata.v1.trading_account_types.delete_many',
    list_trading_account_type_versions_request: 'refdata.v1.trading_account_types_versions.list',
    get_trading_account_type_version_request: 'refdata.v1.trading_account_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_trading_account_types_request: true,
    get_trading_account_type_request: true,
    get_many_trading_account_types_request: true,
    put_trading_account_type_request: true,
    put_many_trading_account_types_request: true,
    delete_trading_account_type_request: true,
    delete_many_trading_account_types_request: true,
    list_trading_account_type_versions_request: true,
    get_trading_account_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.trading_account_types_events.created',
    updated: 'refdata.v1.trading_account_types_events.updated',
    deleted: 'refdata.v1.trading_account_types_events.deleted',
} as const;
