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
import type { Trade } from '../domain/trade.js';
import type { Order } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TradeKey {
    id: string;
}

export interface TradeLookup {
    key: TradeKey;
    trade: Trade | null;
}

export interface TradesFilter {
    id_one_of: string[] | null;
}

export interface TradeEvent {
    event_id: string;
    key: TradeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListTradesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: TradesFilter | null;
}

export interface ListTradesResponse {
    result: Result;
    trades: Trade[];
    total: number;
}

export interface GetTradeRequest {
    key: TradeKey;
}

export interface GetTradeResponse {
    result: Result;
    trade: Trade | null;
}

export interface GetManyTradesRequest {
    keys: TradeKey[];
}

export interface GetManyTradesResponse {
    result: Result;
    entries: TradeLookup[];
}

export const subjects = {
    list_trades_request: 'trading.v1.trades.list',
    get_trade_request: 'trading.v1.trades.get',
    get_many_trades_request: 'trading.v1.trades.get_many',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_trades_request: true,
    get_trade_request: true,
    get_many_trades_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.trades_events.created',
    updated: 'trading.v1.trades_events.updated',
    deleted: 'trading.v1.trades_events.deleted',
} as const;
