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
import type { TradeProduct } from '../domain/trade_product.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface TradeProductKey {
    trade_id: string;
}

export interface TradeProductWrite {
    trade_id: string;
    trade_activity_id: string;
    product_code: string;
    booking_form: string;
}

export interface TradeProductChange {
    write: TradeProductWrite;
    precondition: Precondition;
}

export interface TradeProductRemoval {
    key: TradeProductKey;
    precondition: Precondition;
}

export interface TradeProductLookup {
    key: TradeProductKey;
    trade_product: TradeProduct | null;
}

export interface TradeProductsFilter {
    trade_id: string | null;
    trade_id_one_of: string[] | null;
}

export interface TradeProductEvent {
    event_id: string;
    key: TradeProductKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TradeProductVersionKey {
    trade_product: TradeProductKey;
    version: number;
}

export interface TradeProductVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTradeProductsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: TradeProductsFilter | null;
    as_of: string | null;
}

export interface ListTradeProductsResponse {
    result: Result;
    products: TradeProduct[];
    total: number;
}

export interface GetTradeProductRequest {
    key: TradeProductKey;
}

export interface GetTradeProductResponse {
    result: Result;
    trade_product: TradeProduct | null;
}

export interface GetManyTradeProductsRequest {
    keys: TradeProductKey[];
}

export interface GetManyTradeProductsResponse {
    result: Result;
    entries: TradeProductLookup[];
}

export interface PutTradeProductRequest {
    change: TradeProductChange;
    intent: ChangeIntent;
}

export interface PutTradeProductResponse {
    result: Result;
    trade_product: TradeProduct | null;
}

export interface PutManyTradeProductsRequest {
    changes: TradeProductChange[];
    intent: ChangeIntent;
}

export interface PutManyTradeProductsResponse {
    result: Result;
    products: TradeProduct[];
}

export interface DeleteTradeProductRequest {
    removal: TradeProductRemoval;
    intent: ChangeIntent;
}

export interface DeleteTradeProductResponse {
    result: Result;
}

export interface DeleteManyTradeProductsRequest {
    removals: TradeProductRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTradeProductsResponse {
    result: Result;
}

export interface ListByTradeIdTradeProductsRequest {
    trade_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: TradeProductsFilter | null;
}

export interface ListByTradeIdTradeProductsResponse {
    result: Result;
    products: TradeProduct[];
    total: number;
}

export interface ListTradeProductVersionsRequest {
    key: TradeProductKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TradeProductVersionsFilter | null;
}

export interface ListTradeProductVersionsResponse {
    result: Result;
    versions: TradeProduct[];
    total: number;
}

export interface GetTradeProductVersionRequest {
    key: TradeProductVersionKey;
}

export interface GetTradeProductVersionResponse {
    result: Result;
    version: TradeProduct | null;
}

export const subjects = {
    list_trade_products_request: 'trading.v1.trade_products.list',
    get_trade_product_request: 'trading.v1.trade_products.get',
    get_many_trade_products_request: 'trading.v1.trade_products.get_many',
    put_trade_product_request: 'trading.v1.trade_products.put',
    put_many_trade_products_request: 'trading.v1.trade_products.put_many',
    delete_trade_product_request: 'trading.v1.trade_products.delete',
    delete_many_trade_products_request: 'trading.v1.trade_products.delete_many',
    list_by_trade_id_trade_products_request: 'trading.v1.trade_products.list_by_trade_id',
    list_trade_product_versions_request: 'trading.v1.trade_products_versions.list',
    get_trade_product_version_request: 'trading.v1.trade_products_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_trade_products_request: true,
    get_trade_product_request: true,
    get_many_trade_products_request: true,
    put_trade_product_request: true,
    put_many_trade_products_request: true,
    delete_trade_product_request: true,
    delete_many_trade_products_request: true,
    list_by_trade_id_trade_products_request: true,
    list_trade_product_versions_request: true,
    get_trade_product_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.trade_products.created',
    updated: 'trading.v1.events.trade_products.updated',
    deleted: 'trading.v1.events.trade_products.deleted',
} as const;
