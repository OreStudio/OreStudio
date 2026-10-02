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
import type { TradeType } from '../domain/trade_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TradeTypeKey {
    code: string;
}

export interface TradeTypeWrite {
    code: string;
    description: string;
    product_type: string;
    has_options: boolean;
    has_extension: boolean;
}

export interface TradeTypeChange {
    write: TradeTypeWrite;
    precondition: Precondition;
}

export interface TradeTypeRemoval {
    key: TradeTypeKey;
    precondition: Precondition;
}

export interface TradeTypeLookup {
    key: TradeTypeKey;
    trade_type: TradeType | null;
}

export interface TradeTypeEvent {
    event_id: string;
    key: TradeTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TradeTypeVersionKey {
    trade_type: TradeTypeKey;
    version: number;
}

export interface TradeTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTradeTypesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListTradeTypesResponse {
    result: Result;
    types: TradeType[];
    total: number;
}

export interface GetTradeTypeRequest {
    key: TradeTypeKey;
}

export interface GetTradeTypeResponse {
    result: Result;
    trade_type: TradeType | null;
}

export interface GetManyTradeTypesRequest {
    keys: TradeTypeKey[];
}

export interface GetManyTradeTypesResponse {
    result: Result;
    entries: TradeTypeLookup[];
}

export interface PutTradeTypeRequest {
    change: TradeTypeChange;
    intent: ChangeIntent;
}

export interface PutTradeTypeResponse {
    result: Result;
    trade_type: TradeType | null;
}

export interface PutManyTradeTypesRequest {
    changes: TradeTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyTradeTypesResponse {
    result: Result;
    types: TradeType[];
}

export interface DeleteTradeTypeRequest {
    removal: TradeTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteTradeTypeResponse {
    result: Result;
}

export interface DeleteManyTradeTypesRequest {
    removals: TradeTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTradeTypesResponse {
    result: Result;
}

export interface ListTradeTypeVersionsRequest {
    key: TradeTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TradeTypeVersionsFilter | null;
}

export interface ListTradeTypeVersionsResponse {
    result: Result;
    versions: TradeType[];
    total: number;
}

export interface GetTradeTypeVersionRequest {
    key: TradeTypeVersionKey;
}

export interface GetTradeTypeVersionResponse {
    result: Result;
    version: TradeType | null;
}

export const subjects = {
    list_trade_types_request: 'trading.v1.trade_types.list',
    get_trade_type_request: 'trading.v1.trade_types.get',
    get_many_trade_types_request: 'trading.v1.trade_types.get_many',
    put_trade_type_request: 'trading.v1.trade_types.put',
    put_many_trade_types_request: 'trading.v1.trade_types.put_many',
    delete_trade_type_request: 'trading.v1.trade_types.delete',
    delete_many_trade_types_request: 'trading.v1.trade_types.delete_many',
    list_trade_type_versions_request: 'trading.v1.trade_types_versions.list',
    get_trade_type_version_request: 'trading.v1.trade_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_trade_types_request: true,
    get_trade_type_request: true,
    get_many_trade_types_request: true,
    put_trade_type_request: true,
    put_many_trade_types_request: true,
    delete_trade_type_request: true,
    delete_many_trade_types_request: true,
    list_trade_type_versions_request: true,
    get_trade_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.trade_types_events.created',
    updated: 'trading.v1.trade_types_events.updated',
    deleted: 'trading.v1.trade_types_events.deleted',
} as const;
