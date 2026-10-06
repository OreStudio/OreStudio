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
import type { MarketSeries } from '../domain/market_series.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface MarketSeriesKey {
    id: string;
}

export interface MarketSeriesWrite {
    id: string;
    party_id: string;
    oresmd_uri: string;
    series_subclass: string;
    producer_kind: string;
    derivation_kind: string;
    derivation_config_id: string;
    derivation_config_version: number;
}

export interface MarketSeriesChange {
    write: MarketSeriesWrite;
    precondition: Precondition;
}

export interface MarketSeriesRemoval {
    key: MarketSeriesKey;
    precondition: Precondition;
}

export interface MarketSeriesLookup {
    key: MarketSeriesKey;
    market_series: MarketSeries | null;
}

export interface MarketSeriesFilter {
    id_one_of: string[] | null;
}

export interface MarketSeriesEvent {
    event_id: string;
    key: MarketSeriesKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface MarketSeriesVersionKey {
    market_series: MarketSeriesKey;
    version: number;
}

export interface MarketSeriesVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListMarketSeriesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: MarketSeriesFilter | null;
    as_of: string | null;
}

export interface ListMarketSeriesResponse {
    result: Result;
    market_series: MarketSeries[];
    total: number;
}

export interface GetMarketSeriesRequest {
    key: MarketSeriesKey;
}

export interface GetMarketSeriesResponse {
    result: Result;
    market_series: MarketSeries | null;
}

export interface GetManyMarketSeriesRequest {
    keys: MarketSeriesKey[];
}

export interface GetManyMarketSeriesResponse {
    result: Result;
    entries: MarketSeriesLookup[];
}

export interface PutMarketSeriesRequest {
    change: MarketSeriesChange;
    intent: ChangeIntent;
}

export interface PutMarketSeriesResponse {
    result: Result;
    market_series: MarketSeries | null;
}

export interface PutManyMarketSeriesRequest {
    changes: MarketSeriesChange[];
    intent: ChangeIntent;
}

export interface PutManyMarketSeriesResponse {
    result: Result;
    market_series: MarketSeries[];
}

export interface DeleteMarketSeriesRequest {
    removal: MarketSeriesRemoval;
    intent: ChangeIntent;
}

export interface DeleteMarketSeriesResponse {
    result: Result;
}

export interface DeleteManyMarketSeriesRequest {
    removals: MarketSeriesRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyMarketSeriesResponse {
    result: Result;
}

export interface ListMarketSeriesVersionsRequest {
    key: MarketSeriesKey;
    offset: number;
    limit: number;
    order: Order;
    filter: MarketSeriesVersionsFilter | null;
}

export interface ListMarketSeriesVersionsResponse {
    result: Result;
    versions: MarketSeries[];
    total: number;
}

export interface GetMarketSeriesVersionRequest {
    key: MarketSeriesVersionKey;
}

export interface GetMarketSeriesVersionResponse {
    result: Result;
    version: MarketSeries | null;
}

export const subjects = {
    list_market_series_request: 'marketdata.v1.market_series.list',
    get_market_series_request: 'marketdata.v1.market_series.get',
    get_many_market_series_request: 'marketdata.v1.market_series.get_many',
    put_market_series_request: 'marketdata.v1.market_series.put',
    put_many_market_series_request: 'marketdata.v1.market_series.put_many',
    delete_market_series_request: 'marketdata.v1.market_series.delete',
    delete_many_market_series_request: 'marketdata.v1.market_series.delete_many',
    list_market_series_versions_request: 'marketdata.v1.market_series_versions.list',
    get_market_series_version_request: 'marketdata.v1.market_series_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_market_series_request: true,
    get_market_series_request: true,
    get_many_market_series_request: true,
    put_market_series_request: true,
    put_many_market_series_request: true,
    delete_market_series_request: true,
    delete_many_market_series_request: true,
    list_market_series_versions_request: true,
    get_market_series_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.market_series_events.created',
    updated: 'marketdata.v1.market_series_events.updated',
    deleted: 'marketdata.v1.market_series_events.deleted',
} as const;
