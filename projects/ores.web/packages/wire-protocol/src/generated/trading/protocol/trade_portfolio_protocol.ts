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
import type { TradePortfolio } from '../domain/trade_portfolio.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TradePortfolioKey {
    trade_id: string;
    sequence_number: number;
}

export interface TradePortfolioWrite {
    trade_id: string;
    sequence_number: number;
    trade_activity_id: string;
    portfolio_id: string;
}

export interface TradePortfolioChange {
    write: TradePortfolioWrite;
    precondition: Precondition;
}

export interface TradePortfolioRemoval {
    key: TradePortfolioKey;
    precondition: Precondition;
}

export interface TradePortfolioLookup {
    key: TradePortfolioKey;
    trade_portfolio: TradePortfolio | null;
}

export interface TradePortfolioEvent {
    event_id: string;
    key: TradePortfolioKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TradePortfolioVersionKey {
    trade_portfolio: TradePortfolioKey;
    version: number;
}

export interface TradePortfolioVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTradePortfoliosRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListTradePortfoliosResponse {
    result: Result;
    trade_portfolios: TradePortfolio[];
    total: number;
}

export interface GetTradePortfolioRequest {
    key: TradePortfolioKey;
}

export interface GetTradePortfolioResponse {
    result: Result;
    trade_portfolio: TradePortfolio | null;
}

export interface GetManyTradePortfoliosRequest {
    keys: TradePortfolioKey[];
}

export interface GetManyTradePortfoliosResponse {
    result: Result;
    entries: TradePortfolioLookup[];
}

export interface PutTradePortfolioRequest {
    change: TradePortfolioChange;
    intent: ChangeIntent;
}

export interface PutTradePortfolioResponse {
    result: Result;
    trade_portfolio: TradePortfolio | null;
}

export interface PutManyTradePortfoliosRequest {
    changes: TradePortfolioChange[];
    intent: ChangeIntent;
}

export interface PutManyTradePortfoliosResponse {
    result: Result;
    trade_portfolios: TradePortfolio[];
}

export interface DeleteTradePortfolioRequest {
    removal: TradePortfolioRemoval;
    intent: ChangeIntent;
}

export interface DeleteTradePortfolioResponse {
    result: Result;
}

export interface DeleteManyTradePortfoliosRequest {
    removals: TradePortfolioRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTradePortfoliosResponse {
    result: Result;
}

export interface ListTradePortfolioVersionsRequest {
    key: TradePortfolioKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TradePortfolioVersionsFilter | null;
}

export interface ListTradePortfolioVersionsResponse {
    result: Result;
    versions: TradePortfolio[];
    total: number;
}

export interface GetTradePortfolioVersionRequest {
    key: TradePortfolioVersionKey;
}

export interface GetTradePortfolioVersionResponse {
    result: Result;
    version: TradePortfolio | null;
}

export const subjects = {
    list_trade_portfolios_request: 'trading.v1.trade_portfolios.list',
    get_trade_portfolio_request: 'trading.v1.trade_portfolios.get',
    get_many_trade_portfolios_request: 'trading.v1.trade_portfolios.get_many',
    put_trade_portfolio_request: 'trading.v1.trade_portfolios.put',
    put_many_trade_portfolios_request: 'trading.v1.trade_portfolios.put_many',
    delete_trade_portfolio_request: 'trading.v1.trade_portfolios.delete',
    delete_many_trade_portfolios_request: 'trading.v1.trade_portfolios.delete_many',
    list_trade_portfolio_versions_request: 'trading.v1.trade_portfolios_versions.list',
    get_trade_portfolio_version_request: 'trading.v1.trade_portfolios_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_trade_portfolios_request: true,
    get_trade_portfolio_request: true,
    get_many_trade_portfolios_request: true,
    put_trade_portfolio_request: true,
    put_many_trade_portfolios_request: true,
    delete_trade_portfolio_request: true,
    delete_many_trade_portfolios_request: true,
    list_trade_portfolio_versions_request: true,
    get_trade_portfolio_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.trade_portfolios_events.created',
    updated: 'trading.v1.trade_portfolios_events.updated',
    deleted: 'trading.v1.trade_portfolios_events.deleted',
} as const;
