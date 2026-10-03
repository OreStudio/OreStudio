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
import type { TradeIdentifier } from '../domain/trade_identifier.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TradeIdentifierKey {
    trade_id: string;
    id_type: string;
}

export interface TradeIdentifierWrite {
    trade_id: string;
    id_type: string;
    id_value: string;
    issuing_party_id: string | null;
}

export interface TradeIdentifierChange {
    write: TradeIdentifierWrite;
    precondition: Precondition;
}

export interface TradeIdentifierRemoval {
    key: TradeIdentifierKey;
    precondition: Precondition;
}

export interface TradeIdentifierLookup {
    key: TradeIdentifierKey;
    trade_identifier: TradeIdentifier | null;
}

export interface TradeIdentifierEvent {
    event_id: string;
    key: TradeIdentifierKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TradeIdentifierVersionKey {
    trade_identifier: TradeIdentifierKey;
    version: number;
}

export interface TradeIdentifierVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTradeIdentifiersRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListTradeIdentifiersResponse {
    result: Result;
    identifiers: TradeIdentifier[];
    total: number;
}

export interface GetTradeIdentifierRequest {
    key: TradeIdentifierKey;
}

export interface GetTradeIdentifierResponse {
    result: Result;
    trade_identifier: TradeIdentifier | null;
}

export interface GetManyTradeIdentifiersRequest {
    keys: TradeIdentifierKey[];
}

export interface GetManyTradeIdentifiersResponse {
    result: Result;
    entries: TradeIdentifierLookup[];
}

export interface PutTradeIdentifierRequest {
    change: TradeIdentifierChange;
    intent: ChangeIntent;
}

export interface PutTradeIdentifierResponse {
    result: Result;
    trade_identifier: TradeIdentifier | null;
}

export interface PutManyTradeIdentifiersRequest {
    changes: TradeIdentifierChange[];
    intent: ChangeIntent;
}

export interface PutManyTradeIdentifiersResponse {
    result: Result;
    identifiers: TradeIdentifier[];
}

export interface DeleteTradeIdentifierRequest {
    removal: TradeIdentifierRemoval;
    intent: ChangeIntent;
}

export interface DeleteTradeIdentifierResponse {
    result: Result;
}

export interface DeleteManyTradeIdentifiersRequest {
    removals: TradeIdentifierRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTradeIdentifiersResponse {
    result: Result;
}

export interface ListTradeIdentifierVersionsRequest {
    key: TradeIdentifierKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TradeIdentifierVersionsFilter | null;
}

export interface ListTradeIdentifierVersionsResponse {
    result: Result;
    versions: TradeIdentifier[];
    total: number;
}

export interface GetTradeIdentifierVersionRequest {
    key: TradeIdentifierVersionKey;
}

export interface GetTradeIdentifierVersionResponse {
    result: Result;
    version: TradeIdentifier | null;
}

export const subjects = {
    list_trade_identifiers_request: 'trading.v1.trade_identifiers.list',
    get_trade_identifier_request: 'trading.v1.trade_identifiers.get',
    get_many_trade_identifiers_request: 'trading.v1.trade_identifiers.get_many',
    put_trade_identifier_request: 'trading.v1.trade_identifiers.put',
    put_many_trade_identifiers_request: 'trading.v1.trade_identifiers.put_many',
    delete_trade_identifier_request: 'trading.v1.trade_identifiers.delete',
    delete_many_trade_identifiers_request: 'trading.v1.trade_identifiers.delete_many',
    list_trade_identifier_versions_request: 'trading.v1.trade_identifiers_versions.list',
    get_trade_identifier_version_request: 'trading.v1.trade_identifiers_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_trade_identifiers_request: true,
    get_trade_identifier_request: true,
    get_many_trade_identifiers_request: true,
    put_trade_identifier_request: true,
    put_many_trade_identifiers_request: true,
    delete_trade_identifier_request: true,
    delete_many_trade_identifiers_request: true,
    list_trade_identifier_versions_request: true,
    get_trade_identifier_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.trade_identifiers_events.created',
    updated: 'trading.v1.trade_identifiers_events.updated',
    deleted: 'trading.v1.trade_identifiers_events.deleted',
} as const;
