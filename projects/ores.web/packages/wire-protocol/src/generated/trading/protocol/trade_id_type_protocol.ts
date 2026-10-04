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
import type { TradeIdType } from '../domain/trade_id_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TradeIdTypeKey {
    code: string;
}

export interface TradeIdTypeWrite {
    code: string;
    description: string;
}

export interface TradeIdTypeChange {
    write: TradeIdTypeWrite;
    precondition: Precondition;
}

export interface TradeIdTypeRemoval {
    key: TradeIdTypeKey;
    precondition: Precondition;
}

export interface TradeIdTypeLookup {
    key: TradeIdTypeKey;
    trade_id_type: TradeIdType | null;
}

export interface TradeIdTypesFilter {
    code_one_of: string[] | null;
}

export interface TradeIdTypeEvent {
    event_id: string;
    key: TradeIdTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TradeIdTypeVersionKey {
    trade_id_type: TradeIdTypeKey;
    version: number;
}

export interface TradeIdTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTradeIdTypesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: TradeIdTypesFilter | null;
}

export interface ListTradeIdTypesResponse {
    result: Result;
    id_types: TradeIdType[];
    total: number;
}

export interface GetTradeIdTypeRequest {
    key: TradeIdTypeKey;
}

export interface GetTradeIdTypeResponse {
    result: Result;
    trade_id_type: TradeIdType | null;
}

export interface GetManyTradeIdTypesRequest {
    keys: TradeIdTypeKey[];
}

export interface GetManyTradeIdTypesResponse {
    result: Result;
    entries: TradeIdTypeLookup[];
}

export interface PutTradeIdTypeRequest {
    change: TradeIdTypeChange;
    intent: ChangeIntent;
}

export interface PutTradeIdTypeResponse {
    result: Result;
    trade_id_type: TradeIdType | null;
}

export interface PutManyTradeIdTypesRequest {
    changes: TradeIdTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyTradeIdTypesResponse {
    result: Result;
    id_types: TradeIdType[];
}

export interface DeleteTradeIdTypeRequest {
    removal: TradeIdTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteTradeIdTypeResponse {
    result: Result;
}

export interface DeleteManyTradeIdTypesRequest {
    removals: TradeIdTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTradeIdTypesResponse {
    result: Result;
}

export interface ListTradeIdTypeVersionsRequest {
    key: TradeIdTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TradeIdTypeVersionsFilter | null;
}

export interface ListTradeIdTypeVersionsResponse {
    result: Result;
    versions: TradeIdType[];
    total: number;
}

export interface GetTradeIdTypeVersionRequest {
    key: TradeIdTypeVersionKey;
}

export interface GetTradeIdTypeVersionResponse {
    result: Result;
    version: TradeIdType | null;
}

export const subjects = {
    list_trade_id_types_request: 'trading.v1.trade_id_types.list',
    get_trade_id_type_request: 'trading.v1.trade_id_types.get',
    get_many_trade_id_types_request: 'trading.v1.trade_id_types.get_many',
    put_trade_id_type_request: 'trading.v1.trade_id_types.put',
    put_many_trade_id_types_request: 'trading.v1.trade_id_types.put_many',
    delete_trade_id_type_request: 'trading.v1.trade_id_types.delete',
    delete_many_trade_id_types_request: 'trading.v1.trade_id_types.delete_many',
    list_trade_id_type_versions_request: 'trading.v1.trade_id_types_versions.list',
    get_trade_id_type_version_request: 'trading.v1.trade_id_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_trade_id_types_request: true,
    get_trade_id_type_request: true,
    get_many_trade_id_types_request: true,
    put_trade_id_type_request: true,
    put_many_trade_id_types_request: true,
    delete_trade_id_type_request: true,
    delete_many_trade_id_types_request: true,
    list_trade_id_type_versions_request: true,
    get_trade_id_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.trade_id_types_events.created',
    updated: 'trading.v1.trade_id_types_events.updated',
    deleted: 'trading.v1.trade_id_types_events.deleted',
} as const;
