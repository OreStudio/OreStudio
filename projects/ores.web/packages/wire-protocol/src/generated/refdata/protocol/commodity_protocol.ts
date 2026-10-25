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
import type { Commodity } from '../domain/commodity.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CommodityKey {
    code: string;
}

export interface CommodityWrite {
    code: string;
    name: string;
    description: string;
    unit_code: string;
    quotation_currency_code: string;
    display_order: number;
}

export interface CommodityChange {
    write: CommodityWrite;
    precondition: Precondition;
}

export interface CommodityRemoval {
    key: CommodityKey;
    precondition: Precondition;
}

export interface CommodityLookup {
    key: CommodityKey;
    commodity: Commodity | null;
}

export interface CommoditiesFilter {
    code_one_of: string[] | null;
}

export interface CommodityEvent {
    event_id: string;
    key: CommodityKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CommodityVersionKey {
    commodity: CommodityKey;
    version: number;
}

export interface CommodityVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCommoditiesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CommoditiesFilter | null;
    as_of: string | null;
}

export interface ListCommoditiesResponse {
    result: Result;
    commodities: Commodity[];
    total: number;
}

export interface GetCommodityRequest {
    key: CommodityKey;
}

export interface GetCommodityResponse {
    result: Result;
    commodity: Commodity | null;
}

export interface GetManyCommoditiesRequest {
    keys: CommodityKey[];
}

export interface GetManyCommoditiesResponse {
    result: Result;
    entries: CommodityLookup[];
}

export interface PutCommodityRequest {
    change: CommodityChange;
    intent: ChangeIntent;
}

export interface PutCommodityResponse {
    result: Result;
    commodity: Commodity | null;
}

export interface PutManyCommoditiesRequest {
    changes: CommodityChange[];
    intent: ChangeIntent;
}

export interface PutManyCommoditiesResponse {
    result: Result;
    commodities: Commodity[];
}

export interface DeleteCommodityRequest {
    removal: CommodityRemoval;
    intent: ChangeIntent;
}

export interface DeleteCommodityResponse {
    result: Result;
}

export interface DeleteManyCommoditiesRequest {
    removals: CommodityRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCommoditiesResponse {
    result: Result;
}

export interface ListCommodityVersionsRequest {
    key: CommodityKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityVersionsFilter | null;
}

export interface ListCommodityVersionsResponse {
    result: Result;
    versions: Commodity[];
    total: number;
}

export interface GetCommodityVersionRequest {
    key: CommodityVersionKey;
}

export interface GetCommodityVersionResponse {
    result: Result;
    version: Commodity | null;
}

export const subjects = {
    list_commodities_request: 'refdata.v1.commodities.list',
    get_commodity_request: 'refdata.v1.commodities.get',
    get_many_commodities_request: 'refdata.v1.commodities.get_many',
    put_commodity_request: 'refdata.v1.commodities.put',
    put_many_commodities_request: 'refdata.v1.commodities.put_many',
    delete_commodity_request: 'refdata.v1.commodities.delete',
    delete_many_commodities_request: 'refdata.v1.commodities.delete_many',
    list_commodity_versions_request: 'refdata.v1.commodities_versions.list',
    get_commodity_version_request: 'refdata.v1.commodities_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_commodities_request: true,
    get_commodity_request: true,
    get_many_commodities_request: true,
    put_commodity_request: true,
    put_many_commodities_request: true,
    delete_commodity_request: true,
    delete_many_commodities_request: true,
    list_commodity_versions_request: true,
    get_commodity_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.commodities_events.created',
    updated: 'refdata.v1.commodities_events.updated',
    deleted: 'refdata.v1.commodities_events.deleted',
} as const;
