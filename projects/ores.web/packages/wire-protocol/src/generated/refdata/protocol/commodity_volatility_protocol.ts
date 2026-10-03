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
import type { CommodityVolatility } from '../domain/commodity_volatility.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CommodityVolatilityKey {
    id: string;
}

export interface CommodityVolatilityWrite {
    id: string;
    curve_definition_id: string;
    currency: string;
    instrument_type: string | null;
    calendar_spread_offset: number | null;
    calendar_spread_underlying_name: string | null;
    day_counter: string | null;
    calendar: string | null;
    future_conventions: string | null;
    option_expiry_roll_days: string | null;
    price_curve_id: string | null;
    yield_curve_id: string | null;
    quote_suffix: string | null;
    prefer_out_of_the_money: string | null;
    has_volatility_config: boolean;
}

export interface CommodityVolatilityChange {
    write: CommodityVolatilityWrite;
    precondition: Precondition;
}

export interface CommodityVolatilityRemoval {
    key: CommodityVolatilityKey;
    precondition: Precondition;
}

export interface CommodityVolatilityLookup {
    key: CommodityVolatilityKey;
    commodity_volatility: CommodityVolatility | null;
}

export interface CommodityVolatilityEvent {
    event_id: string;
    key: CommodityVolatilityKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CommodityVolatilityVersionKey {
    commodity_volatility: CommodityVolatilityKey;
    version: number;
}

export interface CommodityVolatilityVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCommodityVolatilitiesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCommodityVolatilitiesResponse {
    result: Result;
    commodity_volatilities: CommodityVolatility[];
    total: number;
}

export interface GetCommodityVolatilityRequest {
    key: CommodityVolatilityKey;
}

export interface GetCommodityVolatilityResponse {
    result: Result;
    commodity_volatility: CommodityVolatility | null;
}

export interface GetManyCommodityVolatilitiesRequest {
    keys: CommodityVolatilityKey[];
}

export interface GetManyCommodityVolatilitiesResponse {
    result: Result;
    entries: CommodityVolatilityLookup[];
}

export interface PutCommodityVolatilityRequest {
    change: CommodityVolatilityChange;
    intent: ChangeIntent;
}

export interface PutCommodityVolatilityResponse {
    result: Result;
    commodity_volatility: CommodityVolatility | null;
}

export interface PutManyCommodityVolatilitiesRequest {
    changes: CommodityVolatilityChange[];
    intent: ChangeIntent;
}

export interface PutManyCommodityVolatilitiesResponse {
    result: Result;
    commodity_volatilities: CommodityVolatility[];
}

export interface DeleteCommodityVolatilityRequest {
    removal: CommodityVolatilityRemoval;
    intent: ChangeIntent;
}

export interface DeleteCommodityVolatilityResponse {
    result: Result;
}

export interface DeleteManyCommodityVolatilitiesRequest {
    removals: CommodityVolatilityRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCommodityVolatilitiesResponse {
    result: Result;
}

export interface ListCommodityVolatilityVersionsRequest {
    key: CommodityVolatilityKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityVolatilityVersionsFilter | null;
}

export interface ListCommodityVolatilityVersionsResponse {
    result: Result;
    versions: CommodityVolatility[];
    total: number;
}

export interface GetCommodityVolatilityVersionRequest {
    key: CommodityVolatilityVersionKey;
}

export interface GetCommodityVolatilityVersionResponse {
    result: Result;
    version: CommodityVolatility | null;
}

export const subjects = {
    list_commodity_volatilities_request: 'refdata.v1.commodity_volatilities.list',
    get_commodity_volatility_request: 'refdata.v1.commodity_volatilities.get',
    get_many_commodity_volatilities_request: 'refdata.v1.commodity_volatilities.get_many',
    put_commodity_volatility_request: 'refdata.v1.commodity_volatilities.put',
    put_many_commodity_volatilities_request: 'refdata.v1.commodity_volatilities.put_many',
    delete_commodity_volatility_request: 'refdata.v1.commodity_volatilities.delete',
    delete_many_commodity_volatilities_request: 'refdata.v1.commodity_volatilities.delete_many',
    list_commodity_volatility_versions_request: 'refdata.v1.commodity_volatilities_versions.list',
    get_commodity_volatility_version_request: 'refdata.v1.commodity_volatilities_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_commodity_volatilities_request: true,
    get_commodity_volatility_request: true,
    get_many_commodity_volatilities_request: true,
    put_commodity_volatility_request: true,
    put_many_commodity_volatilities_request: true,
    delete_commodity_volatility_request: true,
    delete_many_commodity_volatilities_request: true,
    list_commodity_volatility_versions_request: true,
    get_commodity_volatility_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.commodity_volatilities_events.created',
    updated: 'refdata.v1.commodity_volatilities_events.updated',
    deleted: 'refdata.v1.commodity_volatilities_events.deleted',
} as const;
