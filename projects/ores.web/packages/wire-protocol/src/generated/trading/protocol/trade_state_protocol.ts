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
import type { TradeState } from '../domain/trade_state.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TradeStateKey {
    trade_id: string;
}

export interface TradeStateWrite {
    trade_id: string;
    trade_activity_id: string;
    status_id: string;
}

export interface TradeStateChange {
    write: TradeStateWrite;
    precondition: Precondition;
}

export interface TradeStateRemoval {
    key: TradeStateKey;
    precondition: Precondition;
}

export interface TradeStateLookup {
    key: TradeStateKey;
    trade_state: TradeState | null;
}

export interface TradeStatesFilter {
    trade_id_one_of: string[] | null;
}

export interface TradeStateEvent {
    event_id: string;
    key: TradeStateKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TradeStateVersionKey {
    trade_state: TradeStateKey;
    version: number;
}

export interface TradeStateVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTradeStatesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: TradeStatesFilter | null;
    as_of: string | null;
}

export interface ListTradeStatesResponse {
    result: Result;
    trade_states: TradeState[];
    total: number;
}

export interface GetTradeStateRequest {
    key: TradeStateKey;
}

export interface GetTradeStateResponse {
    result: Result;
    trade_state: TradeState | null;
}

export interface GetManyTradeStatesRequest {
    keys: TradeStateKey[];
}

export interface GetManyTradeStatesResponse {
    result: Result;
    entries: TradeStateLookup[];
}

export interface PutTradeStateRequest {
    change: TradeStateChange;
    intent: ChangeIntent;
}

export interface PutTradeStateResponse {
    result: Result;
    trade_state: TradeState | null;
}

export interface PutManyTradeStatesRequest {
    changes: TradeStateChange[];
    intent: ChangeIntent;
}

export interface PutManyTradeStatesResponse {
    result: Result;
    trade_states: TradeState[];
}

export interface DeleteTradeStateRequest {
    removal: TradeStateRemoval;
    intent: ChangeIntent;
}

export interface DeleteTradeStateResponse {
    result: Result;
}

export interface DeleteManyTradeStatesRequest {
    removals: TradeStateRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTradeStatesResponse {
    result: Result;
}

export interface ListTradeStateVersionsRequest {
    key: TradeStateKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TradeStateVersionsFilter | null;
}

export interface ListTradeStateVersionsResponse {
    result: Result;
    versions: TradeState[];
    total: number;
}

export interface GetTradeStateVersionRequest {
    key: TradeStateVersionKey;
}

export interface GetTradeStateVersionResponse {
    result: Result;
    version: TradeState | null;
}

export const subjects = {
    list_trade_states_request: 'trading.v1.trade_states.list',
    get_trade_state_request: 'trading.v1.trade_states.get',
    get_many_trade_states_request: 'trading.v1.trade_states.get_many',
    put_trade_state_request: 'trading.v1.trade_states.put',
    put_many_trade_states_request: 'trading.v1.trade_states.put_many',
    delete_trade_state_request: 'trading.v1.trade_states.delete',
    delete_many_trade_states_request: 'trading.v1.trade_states.delete_many',
    list_trade_state_versions_request: 'trading.v1.trade_states_versions.list',
    get_trade_state_version_request: 'trading.v1.trade_states_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_trade_states_request: true,
    get_trade_state_request: true,
    get_many_trade_states_request: true,
    put_trade_state_request: true,
    put_many_trade_states_request: true,
    delete_trade_state_request: true,
    delete_many_trade_states_request: true,
    list_trade_state_versions_request: true,
    get_trade_state_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.trade_states_events.created',
    updated: 'trading.v1.trade_states_events.updated',
    deleted: 'trading.v1.trade_states_events.deleted',
} as const;
