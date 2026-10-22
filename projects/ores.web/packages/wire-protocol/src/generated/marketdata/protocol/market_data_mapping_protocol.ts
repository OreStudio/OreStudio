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
import type { MarketDataMapping } from '../domain/market_data_mapping.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface MarketDataMappingKey {
    oresmd_uri: string;
}

export interface MarketDataMappingWrite {
    id: string;
    vendor_instrument_id: string;
    effective_from: string;
    oresmd_uri: string;
    effective_to: string | null;
}

export interface MarketDataMappingChange {
    write: MarketDataMappingWrite;
    precondition: Precondition;
}

export interface MarketDataMappingRemoval {
    key: MarketDataMappingKey;
    precondition: Precondition;
}

export interface MarketDataMappingLookup {
    key: MarketDataMappingKey;
    market_data_mapping: MarketDataMapping | null;
}

export interface MarketDataMappingsFilter {
    vendor_instrument_id: string | null;
    oresmd_uri: string | null;
    id_one_of: string[] | null;
    vendor_instrument_id_one_of: string[] | null;
    oresmd_uri_one_of: string[] | null;
    search: string | null;
}

export interface MarketDataMappingEvent {
    event_id: string;
    key: MarketDataMappingKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface MarketDataMappingVersionKey {
    market_data_mapping: MarketDataMappingKey;
    version: number;
}

export interface MarketDataMappingVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListMarketDataMappingsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: MarketDataMappingsFilter | null;
    as_of: string | null;
}

export interface ListMarketDataMappingsResponse {
    result: Result;
    mappings: MarketDataMapping[];
    total: number;
}

export interface GetMarketDataMappingRequest {
    key: MarketDataMappingKey;
}

export interface GetMarketDataMappingResponse {
    result: Result;
    market_data_mapping: MarketDataMapping | null;
}

export interface GetManyMarketDataMappingsRequest {
    keys: MarketDataMappingKey[];
}

export interface GetManyMarketDataMappingsResponse {
    result: Result;
    entries: MarketDataMappingLookup[];
}

export interface PutMarketDataMappingRequest {
    change: MarketDataMappingChange;
    intent: ChangeIntent;
}

export interface PutMarketDataMappingResponse {
    result: Result;
    market_data_mapping: MarketDataMapping | null;
}

export interface PutManyMarketDataMappingsRequest {
    changes: MarketDataMappingChange[];
    intent: ChangeIntent;
}

export interface PutManyMarketDataMappingsResponse {
    result: Result;
    mappings: MarketDataMapping[];
}

export interface DeleteMarketDataMappingRequest {
    removal: MarketDataMappingRemoval;
    intent: ChangeIntent;
}

export interface DeleteMarketDataMappingResponse {
    result: Result;
}

export interface DeleteManyMarketDataMappingsRequest {
    removals: MarketDataMappingRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyMarketDataMappingsResponse {
    result: Result;
}

export interface ListMarketDataMappingVersionsRequest {
    key: MarketDataMappingKey;
    offset: number;
    limit: number;
    order: Order;
    filter: MarketDataMappingVersionsFilter | null;
}

export interface ListMarketDataMappingVersionsResponse {
    result: Result;
    versions: MarketDataMapping[];
    total: number;
}

export interface GetMarketDataMappingVersionRequest {
    key: MarketDataMappingVersionKey;
}

export interface GetMarketDataMappingVersionResponse {
    result: Result;
    version: MarketDataMapping | null;
}

export const subjects = {
    list_market_data_mappings_request: 'marketdata.v1.market_data_mappings.list',
    get_market_data_mapping_request: 'marketdata.v1.market_data_mappings.get',
    get_many_market_data_mappings_request: 'marketdata.v1.market_data_mappings.get_many',
    put_market_data_mapping_request: 'marketdata.v1.market_data_mappings.put',
    put_many_market_data_mappings_request: 'marketdata.v1.market_data_mappings.put_many',
    delete_market_data_mapping_request: 'marketdata.v1.market_data_mappings.delete',
    delete_many_market_data_mappings_request: 'marketdata.v1.market_data_mappings.delete_many',
    list_market_data_mapping_versions_request: 'marketdata.v1.market_data_mappings_versions.list',
    get_market_data_mapping_version_request: 'marketdata.v1.market_data_mappings_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_market_data_mappings_request: true,
    get_market_data_mapping_request: true,
    get_many_market_data_mappings_request: true,
    put_market_data_mapping_request: true,
    put_many_market_data_mappings_request: true,
    delete_market_data_mapping_request: true,
    delete_many_market_data_mappings_request: true,
    list_market_data_mapping_versions_request: true,
    get_market_data_mapping_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.market_data_mappings_events.created',
    updated: 'marketdata.v1.market_data_mappings_events.updated',
    deleted: 'marketdata.v1.market_data_mappings_events.deleted',
} as const;
