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
import type { MarketSeriesAssetClass } from '../domain/market_series_asset_class.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface MarketSeriesAssetClassKey {
    market_series_id: string;
    asset_class_code: string;
}

export interface MarketSeriesAssetClassWrite {
    market_series_id: string;
    asset_class_code: string;
}

export interface MarketSeriesAssetClassChange {
    write: MarketSeriesAssetClassWrite;
    precondition: Precondition;
}

export interface MarketSeriesAssetClassRemoval {
    key: MarketSeriesAssetClassKey;
    precondition: Precondition;
}

export interface MarketSeriesAssetClassLookup {
    key: MarketSeriesAssetClassKey;
    market_series_asset_class: MarketSeriesAssetClass | null;
}

export interface MarketSeriesAssetClassesFilter {
    market_series_id: string | null;
    market_series_id_one_of: string[] | null;
}

export interface ListMarketSeriesAssetClassesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: MarketSeriesAssetClassesFilter | null;
}

export interface ListMarketSeriesAssetClassesResponse {
    result: Result;
    market_series_asset_classes: MarketSeriesAssetClass[];
    total: number;
}

export interface GetMarketSeriesAssetClassRequest {
    key: MarketSeriesAssetClassKey;
}

export interface GetMarketSeriesAssetClassResponse {
    result: Result;
    market_series_asset_class: MarketSeriesAssetClass | null;
}

export interface GetManyMarketSeriesAssetClassesRequest {
    keys: MarketSeriesAssetClassKey[];
}

export interface GetManyMarketSeriesAssetClassesResponse {
    result: Result;
    entries: MarketSeriesAssetClassLookup[];
}

export interface PutMarketSeriesAssetClassRequest {
    change: MarketSeriesAssetClassChange;
    intent: ChangeIntent;
}

export interface PutMarketSeriesAssetClassResponse {
    result: Result;
    market_series_asset_class: MarketSeriesAssetClass | null;
}

export interface PutManyMarketSeriesAssetClassesRequest {
    changes: MarketSeriesAssetClassChange[];
    intent: ChangeIntent;
}

export interface PutManyMarketSeriesAssetClassesResponse {
    result: Result;
    market_series_asset_classes: MarketSeriesAssetClass[];
}

export interface DeleteMarketSeriesAssetClassRequest {
    removal: MarketSeriesAssetClassRemoval;
    intent: ChangeIntent;
}

export interface DeleteMarketSeriesAssetClassResponse {
    result: Result;
}

export interface DeleteManyMarketSeriesAssetClassesRequest {
    removals: MarketSeriesAssetClassRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyMarketSeriesAssetClassesResponse {
    result: Result;
}

export interface ListByMarketSeriesIdMarketSeriesAssetClassesRequest {
    market_series_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: MarketSeriesAssetClassesFilter | null;
}

export interface ListByMarketSeriesIdMarketSeriesAssetClassesResponse {
    result: Result;
    market_series_asset_classes: MarketSeriesAssetClass[];
    total: number;
}

export const subjects = {
    list_market_series_asset_classes_request: 'marketdata.v1.market_series_asset_classes.list',
    get_market_series_asset_class_request: 'marketdata.v1.market_series_asset_classes.get',
    get_many_market_series_asset_classes_request:
        'marketdata.v1.market_series_asset_classes.get_many',
    put_market_series_asset_class_request: 'marketdata.v1.market_series_asset_classes.put',
    put_many_market_series_asset_classes_request:
        'marketdata.v1.market_series_asset_classes.put_many',
    delete_market_series_asset_class_request: 'marketdata.v1.market_series_asset_classes.delete',
    delete_many_market_series_asset_classes_request:
        'marketdata.v1.market_series_asset_classes.delete_many',
    list_by_market_series_id_market_series_asset_classes_request:
        'marketdata.v1.market_series_asset_classes.list_by_market_series_id',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_market_series_asset_classes_request: true,
    get_market_series_asset_class_request: true,
    get_many_market_series_asset_classes_request: true,
    put_market_series_asset_class_request: true,
    put_many_market_series_asset_classes_request: true,
    delete_market_series_asset_class_request: true,
    delete_many_market_series_asset_classes_request: true,
    list_by_market_series_id_market_series_asset_classes_request: true,
} as const;
