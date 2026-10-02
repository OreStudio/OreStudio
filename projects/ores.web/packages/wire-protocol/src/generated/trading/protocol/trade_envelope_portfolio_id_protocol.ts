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
import type { TradeEnvelopePortfolioId } from '../domain/trade_envelope_portfolio_id.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TradeEnvelopePortfolioIdKey {
    trade_id: string;
    sequence_number: number;
}

export interface TradeEnvelopePortfolioIdWrite {
    trade_id: string;
    sequence_number: number;
    portfolio_id: string;
}

export interface TradeEnvelopePortfolioIdChange {
    write: TradeEnvelopePortfolioIdWrite;
    precondition: Precondition;
}

export interface TradeEnvelopePortfolioIdRemoval {
    key: TradeEnvelopePortfolioIdKey;
    precondition: Precondition;
}

export interface TradeEnvelopePortfolioIdLookup {
    key: TradeEnvelopePortfolioIdKey;
    trade_envelope_portfolio_id: TradeEnvelopePortfolioId | null;
}

export interface TradeEnvelopePortfolioIdEvent {
    event_id: string;
    key: TradeEnvelopePortfolioIdKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TradeEnvelopePortfolioIdVersionKey {
    trade_envelope_portfolio_id: TradeEnvelopePortfolioIdKey;
    version: number;
}

export interface TradeEnvelopePortfolioIdVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTradeEnvelopePortfolioIdsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListTradeEnvelopePortfolioIdsResponse {
    result: Result;
    trade_envelope_portfolio_ids: TradeEnvelopePortfolioId[];
    total: number;
}

export interface GetTradeEnvelopePortfolioIdRequest {
    key: TradeEnvelopePortfolioIdKey;
}

export interface GetTradeEnvelopePortfolioIdResponse {
    result: Result;
    trade_envelope_portfolio_id: TradeEnvelopePortfolioId | null;
}

export interface GetManyTradeEnvelopePortfolioIdsRequest {
    keys: TradeEnvelopePortfolioIdKey[];
}

export interface GetManyTradeEnvelopePortfolioIdsResponse {
    result: Result;
    entries: TradeEnvelopePortfolioIdLookup[];
}

export interface PutTradeEnvelopePortfolioIdRequest {
    change: TradeEnvelopePortfolioIdChange;
    intent: ChangeIntent;
}

export interface PutTradeEnvelopePortfolioIdResponse {
    result: Result;
    trade_envelope_portfolio_id: TradeEnvelopePortfolioId | null;
}

export interface PutManyTradeEnvelopePortfolioIdsRequest {
    changes: TradeEnvelopePortfolioIdChange[];
    intent: ChangeIntent;
}

export interface PutManyTradeEnvelopePortfolioIdsResponse {
    result: Result;
    trade_envelope_portfolio_ids: TradeEnvelopePortfolioId[];
}

export interface DeleteTradeEnvelopePortfolioIdRequest {
    removal: TradeEnvelopePortfolioIdRemoval;
    intent: ChangeIntent;
}

export interface DeleteTradeEnvelopePortfolioIdResponse {
    result: Result;
}

export interface DeleteManyTradeEnvelopePortfolioIdsRequest {
    removals: TradeEnvelopePortfolioIdRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTradeEnvelopePortfolioIdsResponse {
    result: Result;
}

export interface ListTradeEnvelopePortfolioIdVersionsRequest {
    key: TradeEnvelopePortfolioIdKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TradeEnvelopePortfolioIdVersionsFilter | null;
}

export interface ListTradeEnvelopePortfolioIdVersionsResponse {
    result: Result;
    versions: TradeEnvelopePortfolioId[];
    total: number;
}

export interface GetTradeEnvelopePortfolioIdVersionRequest {
    key: TradeEnvelopePortfolioIdVersionKey;
}

export interface GetTradeEnvelopePortfolioIdVersionResponse {
    result: Result;
    version: TradeEnvelopePortfolioId | null;
}

export const subjects = {
    list_trade_envelope_portfolio_ids_request: 'trading.v1.trade_envelope_portfolio_ids.list',
    get_trade_envelope_portfolio_id_request: 'trading.v1.trade_envelope_portfolio_ids.get',
    get_many_trade_envelope_portfolio_ids_request:
        'trading.v1.trade_envelope_portfolio_ids.get_many',
    put_trade_envelope_portfolio_id_request: 'trading.v1.trade_envelope_portfolio_ids.put',
    put_many_trade_envelope_portfolio_ids_request:
        'trading.v1.trade_envelope_portfolio_ids.put_many',
    delete_trade_envelope_portfolio_id_request: 'trading.v1.trade_envelope_portfolio_ids.delete',
    delete_many_trade_envelope_portfolio_ids_request:
        'trading.v1.trade_envelope_portfolio_ids.delete_many',
    list_trade_envelope_portfolio_id_versions_request:
        'trading.v1.trade_envelope_portfolio_ids_versions.list',
    get_trade_envelope_portfolio_id_version_request:
        'trading.v1.trade_envelope_portfolio_ids_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_trade_envelope_portfolio_ids_request: true,
    get_trade_envelope_portfolio_id_request: true,
    get_many_trade_envelope_portfolio_ids_request: true,
    put_trade_envelope_portfolio_id_request: true,
    put_many_trade_envelope_portfolio_ids_request: true,
    delete_trade_envelope_portfolio_id_request: true,
    delete_many_trade_envelope_portfolio_ids_request: true,
    list_trade_envelope_portfolio_id_versions_request: true,
    get_trade_envelope_portfolio_id_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.trade_envelope_portfolio_ids_events.created',
    updated: 'trading.v1.trade_envelope_portfolio_ids_events.updated',
    deleted: 'trading.v1.trade_envelope_portfolio_ids_events.deleted',
} as const;
