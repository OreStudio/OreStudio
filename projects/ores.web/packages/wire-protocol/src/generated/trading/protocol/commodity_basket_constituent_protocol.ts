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
import type { CommodityBasketConstituent } from '../domain/commodity_basket_constituent.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CommodityBasketConstituentKey {
    trade_id: string;
    sequence_number: number;
}

export interface CommodityBasketConstituentWrite {
    trade_id: string;
    sequence_number: number;
    underlying_code: string;
    weight: string | null;
}

export interface CommodityBasketConstituentChange {
    write: CommodityBasketConstituentWrite;
    precondition: Precondition;
}

export interface CommodityBasketConstituentRemoval {
    key: CommodityBasketConstituentKey;
    precondition: Precondition;
}

export interface CommodityBasketConstituentLookup {
    key: CommodityBasketConstituentKey;
    commodity_basket_constituent: CommodityBasketConstituent | null;
}

export interface CommodityBasketConstituentEvent {
    event_id: string;
    key: CommodityBasketConstituentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CommodityBasketConstituentVersionKey {
    commodity_basket_constituent: CommodityBasketConstituentKey;
    version: number;
}

export interface CommodityBasketConstituentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCommodityBasketConstituentsRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListCommodityBasketConstituentsResponse {
    result: Result;
    commodity_basket_constituents: CommodityBasketConstituent[];
    total: number;
}

export interface GetCommodityBasketConstituentRequest {
    key: CommodityBasketConstituentKey;
}

export interface GetCommodityBasketConstituentResponse {
    result: Result;
    commodity_basket_constituent: CommodityBasketConstituent | null;
}

export interface GetManyCommodityBasketConstituentsRequest {
    keys: CommodityBasketConstituentKey[];
}

export interface GetManyCommodityBasketConstituentsResponse {
    result: Result;
    entries: CommodityBasketConstituentLookup[];
}

export interface PutCommodityBasketConstituentRequest {
    change: CommodityBasketConstituentChange;
    intent: ChangeIntent;
}

export interface PutCommodityBasketConstituentResponse {
    result: Result;
    commodity_basket_constituent: CommodityBasketConstituent | null;
}

export interface PutManyCommodityBasketConstituentsRequest {
    changes: CommodityBasketConstituentChange[];
    intent: ChangeIntent;
}

export interface PutManyCommodityBasketConstituentsResponse {
    result: Result;
    commodity_basket_constituents: CommodityBasketConstituent[];
}

export interface DeleteCommodityBasketConstituentRequest {
    removal: CommodityBasketConstituentRemoval;
    intent: ChangeIntent;
}

export interface DeleteCommodityBasketConstituentResponse {
    result: Result;
}

export interface DeleteManyCommodityBasketConstituentsRequest {
    removals: CommodityBasketConstituentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCommodityBasketConstituentsResponse {
    result: Result;
}

export interface ListCommodityBasketConstituentVersionsRequest {
    key: CommodityBasketConstituentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityBasketConstituentVersionsFilter | null;
}

export interface ListCommodityBasketConstituentVersionsResponse {
    result: Result;
    versions: CommodityBasketConstituent[];
    total: number;
}

export interface GetCommodityBasketConstituentVersionRequest {
    key: CommodityBasketConstituentVersionKey;
}

export interface GetCommodityBasketConstituentVersionResponse {
    result: Result;
    version: CommodityBasketConstituent | null;
}

export const subjects = {
    list_commodity_basket_constituents_request: 'trading.v1.commodity_basket_constituents.list',
    get_commodity_basket_constituent_request: 'trading.v1.commodity_basket_constituents.get',
    get_many_commodity_basket_constituents_request:
        'trading.v1.commodity_basket_constituents.get_many',
    put_commodity_basket_constituent_request: 'trading.v1.commodity_basket_constituents.put',
    put_many_commodity_basket_constituents_request:
        'trading.v1.commodity_basket_constituents.put_many',
    delete_commodity_basket_constituent_request: 'trading.v1.commodity_basket_constituents.delete',
    delete_many_commodity_basket_constituents_request:
        'trading.v1.commodity_basket_constituents.delete_many',
    list_commodity_basket_constituent_versions_request:
        'trading.v1.commodity_basket_constituents_versions.list',
    get_commodity_basket_constituent_version_request:
        'trading.v1.commodity_basket_constituents_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_commodity_basket_constituents_request: true,
    get_commodity_basket_constituent_request: true,
    get_many_commodity_basket_constituents_request: true,
    put_commodity_basket_constituent_request: true,
    put_many_commodity_basket_constituents_request: true,
    delete_commodity_basket_constituent_request: true,
    delete_many_commodity_basket_constituents_request: true,
    list_commodity_basket_constituent_versions_request: true,
    get_commodity_basket_constituent_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.commodity_basket_constituents_events.created',
    updated: 'trading.v1.commodity_basket_constituents_events.updated',
    deleted: 'trading.v1.commodity_basket_constituents_events.deleted',
} as const;
