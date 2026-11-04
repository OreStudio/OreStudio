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
import type { CommodityCurveCrossCurrency } from '../domain/commodity_curve_cross_currency.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CommodityCurveCrossCurrencyKey {
    id: string;
}

export interface CommodityCurveCrossCurrencyWrite {
    id: string;
    curve_definition_id: string;
    base_price_curve: string | null;
    base_yield_curve: string | null;
    yield_curve: string | null;
}

export interface CommodityCurveCrossCurrencyChange {
    write: CommodityCurveCrossCurrencyWrite;
    precondition: Precondition;
}

export interface CommodityCurveCrossCurrencyRemoval {
    key: CommodityCurveCrossCurrencyKey;
    precondition: Precondition;
}

export interface CommodityCurveCrossCurrencyLookup {
    key: CommodityCurveCrossCurrencyKey;
    commodity_curve_cross_currency: CommodityCurveCrossCurrency | null;
}

export interface CommodityCurveCrossCurrenciesFilter {
    id_one_of: string[] | null;
}

export interface CommodityCurveCrossCurrencyEvent {
    event_id: string;
    key: CommodityCurveCrossCurrencyKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CommodityCurveCrossCurrencyVersionKey {
    commodity_curve_cross_currency: CommodityCurveCrossCurrencyKey;
    version: number;
}

export interface CommodityCurveCrossCurrencyVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCommodityCurveCrossCurrenciesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityCurveCrossCurrenciesFilter | null;
    as_of: string | null;
}

export interface ListCommodityCurveCrossCurrenciesResponse {
    result: Result;
    cross_currencies: CommodityCurveCrossCurrency[];
    total: number;
}

export interface GetCommodityCurveCrossCurrencyRequest {
    key: CommodityCurveCrossCurrencyKey;
}

export interface GetCommodityCurveCrossCurrencyResponse {
    result: Result;
    commodity_curve_cross_currency: CommodityCurveCrossCurrency | null;
}

export interface GetManyCommodityCurveCrossCurrenciesRequest {
    keys: CommodityCurveCrossCurrencyKey[];
}

export interface GetManyCommodityCurveCrossCurrenciesResponse {
    result: Result;
    entries: CommodityCurveCrossCurrencyLookup[];
}

export interface PutCommodityCurveCrossCurrencyRequest {
    change: CommodityCurveCrossCurrencyChange;
    intent: ChangeIntent;
}

export interface PutCommodityCurveCrossCurrencyResponse {
    result: Result;
    commodity_curve_cross_currency: CommodityCurveCrossCurrency | null;
}

export interface PutManyCommodityCurveCrossCurrenciesRequest {
    changes: CommodityCurveCrossCurrencyChange[];
    intent: ChangeIntent;
}

export interface PutManyCommodityCurveCrossCurrenciesResponse {
    result: Result;
    cross_currencies: CommodityCurveCrossCurrency[];
}

export interface DeleteCommodityCurveCrossCurrencyRequest {
    removal: CommodityCurveCrossCurrencyRemoval;
    intent: ChangeIntent;
}

export interface DeleteCommodityCurveCrossCurrencyResponse {
    result: Result;
}

export interface DeleteManyCommodityCurveCrossCurrenciesRequest {
    removals: CommodityCurveCrossCurrencyRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCommodityCurveCrossCurrenciesResponse {
    result: Result;
}

export interface ListCommodityCurveCrossCurrencyVersionsRequest {
    key: CommodityCurveCrossCurrencyKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityCurveCrossCurrencyVersionsFilter | null;
}

export interface ListCommodityCurveCrossCurrencyVersionsResponse {
    result: Result;
    versions: CommodityCurveCrossCurrency[];
    total: number;
}

export interface GetCommodityCurveCrossCurrencyVersionRequest {
    key: CommodityCurveCrossCurrencyVersionKey;
}

export interface GetCommodityCurveCrossCurrencyVersionResponse {
    result: Result;
    version: CommodityCurveCrossCurrency | null;
}

export const subjects = {
    list_commodity_curve_cross_currencies_request:
        'refdata.v1.commodity_curve_cross_currencies.list',
    get_commodity_curve_cross_currency_request: 'refdata.v1.commodity_curve_cross_currencies.get',
    get_many_commodity_curve_cross_currencies_request:
        'refdata.v1.commodity_curve_cross_currencies.get_many',
    put_commodity_curve_cross_currency_request: 'refdata.v1.commodity_curve_cross_currencies.put',
    put_many_commodity_curve_cross_currencies_request:
        'refdata.v1.commodity_curve_cross_currencies.put_many',
    delete_commodity_curve_cross_currency_request:
        'refdata.v1.commodity_curve_cross_currencies.delete',
    delete_many_commodity_curve_cross_currencies_request:
        'refdata.v1.commodity_curve_cross_currencies.delete_many',
    list_commodity_curve_cross_currency_versions_request:
        'refdata.v1.commodity_curve_cross_currencies_versions.list',
    get_commodity_curve_cross_currency_version_request:
        'refdata.v1.commodity_curve_cross_currencies_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_commodity_curve_cross_currencies_request: true,
    get_commodity_curve_cross_currency_request: true,
    get_many_commodity_curve_cross_currencies_request: true,
    put_commodity_curve_cross_currency_request: true,
    put_many_commodity_curve_cross_currencies_request: true,
    delete_commodity_curve_cross_currency_request: true,
    delete_many_commodity_curve_cross_currencies_request: true,
    list_commodity_curve_cross_currency_versions_request: true,
    get_commodity_curve_cross_currency_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.events.commodity_curve_cross_currencies.created',
    updated: 'refdata.v1.events.commodity_curve_cross_currencies.updated',
    deleted: 'refdata.v1.events.commodity_curve_cross_currencies.deleted',
} as const;
