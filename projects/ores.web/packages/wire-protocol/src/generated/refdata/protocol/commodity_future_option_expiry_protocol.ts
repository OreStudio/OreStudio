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
import type { CommodityFutureOptionExpiry } from '../domain/commodity_future_option_expiry.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CommodityFutureOptionExpiryKey {
    id: string;
}

export interface CommodityFutureOptionExpiryWrite {
    id: string;
    convention_id: string;
    option_expiry_month_lag: number | null;
    option_contract_frequency: string | null;
    option_expiry_offset: number | null;
    option_calendar_days_before: number | null;
    option_min_business_days_before: number | null;
    option_expiry_day: number | null;
    option_nth_nth: number | null;
    option_nth_weekday: string | null;
    option_expiry_last_weekday_of_month: string | null;
    option_expiry_weekly_day_of_the_week: string | null;
    option_business_day_convention: string | null;
    option_underlying_future_convention: string | null;
    option_continuation_mappings: string | null;
}

export interface CommodityFutureOptionExpiryChange {
    write: CommodityFutureOptionExpiryWrite;
    precondition: Precondition;
}

export interface CommodityFutureOptionExpiryRemoval {
    key: CommodityFutureOptionExpiryKey;
    precondition: Precondition;
}

export interface CommodityFutureOptionExpiryLookup {
    key: CommodityFutureOptionExpiryKey;
    commodity_future_option_expiry: CommodityFutureOptionExpiry | null;
}

export interface CommodityFutureOptionExpiriesFilter {
    convention_id: string | null;
    id_one_of: string[] | null;
    convention_id_one_of: string[] | null;
}

export interface CommodityFutureOptionExpiryEvent {
    event_id: string;
    key: CommodityFutureOptionExpiryKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CommodityFutureOptionExpiryVersionKey {
    commodity_future_option_expiry: CommodityFutureOptionExpiryKey;
    version: number;
}

export interface CommodityFutureOptionExpiryVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCommodityFutureOptionExpiriesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityFutureOptionExpiriesFilter | null;
    as_of: string | null;
}

export interface ListCommodityFutureOptionExpiriesResponse {
    result: Result;
    commodity_future_option_expiries: CommodityFutureOptionExpiry[];
    total: number;
}

export interface GetCommodityFutureOptionExpiryRequest {
    key: CommodityFutureOptionExpiryKey;
}

export interface GetCommodityFutureOptionExpiryResponse {
    result: Result;
    commodity_future_option_expiry: CommodityFutureOptionExpiry | null;
}

export interface GetManyCommodityFutureOptionExpiriesRequest {
    keys: CommodityFutureOptionExpiryKey[];
}

export interface GetManyCommodityFutureOptionExpiriesResponse {
    result: Result;
    entries: CommodityFutureOptionExpiryLookup[];
}

export interface PutCommodityFutureOptionExpiryRequest {
    change: CommodityFutureOptionExpiryChange;
    intent: ChangeIntent;
}

export interface PutCommodityFutureOptionExpiryResponse {
    result: Result;
    commodity_future_option_expiry: CommodityFutureOptionExpiry | null;
}

export interface PutManyCommodityFutureOptionExpiriesRequest {
    changes: CommodityFutureOptionExpiryChange[];
    intent: ChangeIntent;
}

export interface PutManyCommodityFutureOptionExpiriesResponse {
    result: Result;
    commodity_future_option_expiries: CommodityFutureOptionExpiry[];
}

export interface DeleteCommodityFutureOptionExpiryRequest {
    removal: CommodityFutureOptionExpiryRemoval;
    intent: ChangeIntent;
}

export interface DeleteCommodityFutureOptionExpiryResponse {
    result: Result;
}

export interface DeleteManyCommodityFutureOptionExpiriesRequest {
    removals: CommodityFutureOptionExpiryRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCommodityFutureOptionExpiriesResponse {
    result: Result;
}

export interface ListByConventionIdCommodityFutureOptionExpiriesRequest {
    convention_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityFutureOptionExpiriesFilter | null;
}

export interface ListByConventionIdCommodityFutureOptionExpiriesResponse {
    result: Result;
    commodity_future_option_expiries: CommodityFutureOptionExpiry[];
    total: number;
}

export interface ListCommodityFutureOptionExpiryVersionsRequest {
    key: CommodityFutureOptionExpiryKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityFutureOptionExpiryVersionsFilter | null;
}

export interface ListCommodityFutureOptionExpiryVersionsResponse {
    result: Result;
    versions: CommodityFutureOptionExpiry[];
    total: number;
}

export interface GetCommodityFutureOptionExpiryVersionRequest {
    key: CommodityFutureOptionExpiryVersionKey;
}

export interface GetCommodityFutureOptionExpiryVersionResponse {
    result: Result;
    version: CommodityFutureOptionExpiry | null;
}

export const subjects = {
    list_commodity_future_option_expiries_request:
        'refdata.v1.commodity_future_option_expiries.list',
    get_commodity_future_option_expiry_request: 'refdata.v1.commodity_future_option_expiries.get',
    get_many_commodity_future_option_expiries_request:
        'refdata.v1.commodity_future_option_expiries.get_many',
    put_commodity_future_option_expiry_request: 'refdata.v1.commodity_future_option_expiries.put',
    put_many_commodity_future_option_expiries_request:
        'refdata.v1.commodity_future_option_expiries.put_many',
    delete_commodity_future_option_expiry_request:
        'refdata.v1.commodity_future_option_expiries.delete',
    delete_many_commodity_future_option_expiries_request:
        'refdata.v1.commodity_future_option_expiries.delete_many',
    list_by_convention_id_commodity_future_option_expiries_request:
        'refdata.v1.commodity_future_option_expiries.list_by_convention_id',
    list_commodity_future_option_expiry_versions_request:
        'refdata.v1.commodity_future_option_expiries_versions.list',
    get_commodity_future_option_expiry_version_request:
        'refdata.v1.commodity_future_option_expiries_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_commodity_future_option_expiries_request: true,
    get_commodity_future_option_expiry_request: true,
    get_many_commodity_future_option_expiries_request: true,
    put_commodity_future_option_expiry_request: true,
    put_many_commodity_future_option_expiries_request: true,
    delete_commodity_future_option_expiry_request: true,
    delete_many_commodity_future_option_expiries_request: true,
    list_by_convention_id_commodity_future_option_expiries_request: true,
    list_commodity_future_option_expiry_versions_request: true,
    get_commodity_future_option_expiry_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.commodity_future_option_expiries_events.created',
    updated: 'refdata.v1.commodity_future_option_expiries_events.updated',
    deleted: 'refdata.v1.commodity_future_option_expiries_events.deleted',
} as const;
