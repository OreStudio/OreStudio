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
import type { TradeLink } from '../domain/trade_link.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TradeLinkKey {
    from_trade_id: string;
    to_trade_id: string;
    link_type: string;
}

export interface TradeLinkWrite {
    from_trade_id: string;
    to_trade_id: string;
    link_type: string;
    trade_activity_id: string;
}

export interface TradeLinkChange {
    write: TradeLinkWrite;
    precondition: Precondition;
}

export interface TradeLinkRemoval {
    key: TradeLinkKey;
    precondition: Precondition;
}

export interface TradeLinkLookup {
    key: TradeLinkKey;
    trade_link: TradeLink | null;
}

export interface TradeLinkEvent {
    event_id: string;
    key: TradeLinkKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TradeLinkVersionKey {
    trade_link: TradeLinkKey;
    version: number;
}

export interface TradeLinkVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTradeLinksRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListTradeLinksResponse {
    result: Result;
    links: TradeLink[];
    total: number;
}

export interface GetTradeLinkRequest {
    key: TradeLinkKey;
}

export interface GetTradeLinkResponse {
    result: Result;
    trade_link: TradeLink | null;
}

export interface GetManyTradeLinksRequest {
    keys: TradeLinkKey[];
}

export interface GetManyTradeLinksResponse {
    result: Result;
    entries: TradeLinkLookup[];
}

export interface PutTradeLinkRequest {
    change: TradeLinkChange;
    intent: ChangeIntent;
}

export interface PutTradeLinkResponse {
    result: Result;
    trade_link: TradeLink | null;
}

export interface PutManyTradeLinksRequest {
    changes: TradeLinkChange[];
    intent: ChangeIntent;
}

export interface PutManyTradeLinksResponse {
    result: Result;
    links: TradeLink[];
}

export interface DeleteTradeLinkRequest {
    removal: TradeLinkRemoval;
    intent: ChangeIntent;
}

export interface DeleteTradeLinkResponse {
    result: Result;
}

export interface DeleteManyTradeLinksRequest {
    removals: TradeLinkRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTradeLinksResponse {
    result: Result;
}

export interface ListTradeLinkVersionsRequest {
    key: TradeLinkKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TradeLinkVersionsFilter | null;
}

export interface ListTradeLinkVersionsResponse {
    result: Result;
    versions: TradeLink[];
    total: number;
}

export interface GetTradeLinkVersionRequest {
    key: TradeLinkVersionKey;
}

export interface GetTradeLinkVersionResponse {
    result: Result;
    version: TradeLink | null;
}

export const subjects = {
    list_trade_links_request: 'trading.v1.trade_links.list',
    get_trade_link_request: 'trading.v1.trade_links.get',
    get_many_trade_links_request: 'trading.v1.trade_links.get_many',
    put_trade_link_request: 'trading.v1.trade_links.put',
    put_many_trade_links_request: 'trading.v1.trade_links.put_many',
    delete_trade_link_request: 'trading.v1.trade_links.delete',
    delete_many_trade_links_request: 'trading.v1.trade_links.delete_many',
    list_trade_link_versions_request: 'trading.v1.trade_links_versions.list',
    get_trade_link_version_request: 'trading.v1.trade_links_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_trade_links_request: true,
    get_trade_link_request: true,
    get_many_trade_links_request: true,
    put_trade_link_request: true,
    put_many_trade_links_request: true,
    delete_trade_link_request: true,
    delete_many_trade_links_request: true,
    list_trade_link_versions_request: true,
    get_trade_link_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.trade_links_events.created',
    updated: 'trading.v1.trade_links_events.updated',
    deleted: 'trading.v1.trade_links_events.deleted',
} as const;
