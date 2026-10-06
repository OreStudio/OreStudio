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
import type { CommodityFutureConvention } from '../domain/commodity_future_convention.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CommodityFutureConventionKey {
    id: string;
}

export interface CommodityFutureConventionWrite {
    id: string;
    contract_frequency: string;
    calendar: string;
    expiry_calendar: string | null;
    expiry_month_lag: number | null;
    one_contract_month: string | null;
    offset_days: number | null;
    business_day_convention: string | null;
    adjust_before_offset: boolean | null;
    is_averaging: boolean | null;
    valid_contract_months: string | null;
    anchor_day_of_month: number | null;
    anchor_calendar_days_before: number | null;
    anchor_business_days_after: number | null;
    anchor_nth_nth: number | null;
    anchor_nth_weekday: string | null;
    anchor_last_weekday: string | null;
    anchor_weekly_day_of_the_week: string | null;
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
    hours_per_day: number | null;
    off_peak_index: string | null;
    peak_index: string | null;
    off_peak_hours: number | null;
    peak_calendar: string | null;
    index_name: string | null;
    savings_time: string | null;
    delivery_location: string | null;
    balance_of_the_month: boolean | null;
    balance_of_the_month_pricing_calendar: string | null;
    option_underlying_future_convention: string | null;
    averaging_commodity_name: string | null;
    averaging_period: string | null;
    averaging_pricing_calendar: string | null;
    averaging_conventions: string | null;
    averaging_use_business_days: boolean | null;
    averaging_delivery_roll_days: number | null;
    averaging_future_month_offset: number | null;
    averaging_daily_expiry_offset: number | null;
    prohibited_expiries: string | null;
    future_continuation_mappings: string | null;
    option_continuation_mappings: string | null;
}

export interface CommodityFutureConventionChange {
    write: CommodityFutureConventionWrite;
    precondition: Precondition;
}

export interface CommodityFutureConventionRemoval {
    key: CommodityFutureConventionKey;
    precondition: Precondition;
}

export interface CommodityFutureConventionLookup {
    key: CommodityFutureConventionKey;
    commodity_future_convention: CommodityFutureConvention | null;
}

export interface CommodityFutureConventionsFilter {
    id_one_of: string[] | null;
}

export interface CommodityFutureConventionEvent {
    event_id: string;
    key: CommodityFutureConventionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CommodityFutureConventionVersionKey {
    commodity_future_convention: CommodityFutureConventionKey;
    version: number;
}

export interface CommodityFutureConventionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCommodityFutureConventionsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityFutureConventionsFilter | null;
    as_of: string | null;
}

export interface ListCommodityFutureConventionsResponse {
    result: Result;
    commodity_future_conventions: CommodityFutureConvention[];
    total: number;
}

export interface GetCommodityFutureConventionRequest {
    key: CommodityFutureConventionKey;
}

export interface GetCommodityFutureConventionResponse {
    result: Result;
    commodity_future_convention: CommodityFutureConvention | null;
}

export interface GetManyCommodityFutureConventionsRequest {
    keys: CommodityFutureConventionKey[];
}

export interface GetManyCommodityFutureConventionsResponse {
    result: Result;
    entries: CommodityFutureConventionLookup[];
}

export interface PutCommodityFutureConventionRequest {
    change: CommodityFutureConventionChange;
    intent: ChangeIntent;
}

export interface PutCommodityFutureConventionResponse {
    result: Result;
    commodity_future_convention: CommodityFutureConvention | null;
}

export interface PutManyCommodityFutureConventionsRequest {
    changes: CommodityFutureConventionChange[];
    intent: ChangeIntent;
}

export interface PutManyCommodityFutureConventionsResponse {
    result: Result;
    commodity_future_conventions: CommodityFutureConvention[];
}

export interface DeleteCommodityFutureConventionRequest {
    removal: CommodityFutureConventionRemoval;
    intent: ChangeIntent;
}

export interface DeleteCommodityFutureConventionResponse {
    result: Result;
}

export interface DeleteManyCommodityFutureConventionsRequest {
    removals: CommodityFutureConventionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCommodityFutureConventionsResponse {
    result: Result;
}

export interface ListCommodityFutureConventionVersionsRequest {
    key: CommodityFutureConventionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityFutureConventionVersionsFilter | null;
}

export interface ListCommodityFutureConventionVersionsResponse {
    result: Result;
    versions: CommodityFutureConvention[];
    total: number;
}

export interface GetCommodityFutureConventionVersionRequest {
    key: CommodityFutureConventionVersionKey;
}

export interface GetCommodityFutureConventionVersionResponse {
    result: Result;
    version: CommodityFutureConvention | null;
}

export const subjects = {
    list_commodity_future_conventions_request: 'refdata.v1.commodity_future_conventions.list',
    get_commodity_future_convention_request: 'refdata.v1.commodity_future_conventions.get',
    get_many_commodity_future_conventions_request:
        'refdata.v1.commodity_future_conventions.get_many',
    put_commodity_future_convention_request: 'refdata.v1.commodity_future_conventions.put',
    put_many_commodity_future_conventions_request:
        'refdata.v1.commodity_future_conventions.put_many',
    delete_commodity_future_convention_request: 'refdata.v1.commodity_future_conventions.delete',
    delete_many_commodity_future_conventions_request:
        'refdata.v1.commodity_future_conventions.delete_many',
    list_commodity_future_convention_versions_request:
        'refdata.v1.commodity_future_conventions_versions.list',
    get_commodity_future_convention_version_request:
        'refdata.v1.commodity_future_conventions_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_commodity_future_conventions_request: true,
    get_commodity_future_convention_request: true,
    get_many_commodity_future_conventions_request: true,
    put_commodity_future_convention_request: true,
    put_many_commodity_future_conventions_request: true,
    delete_commodity_future_convention_request: true,
    delete_many_commodity_future_conventions_request: true,
    list_commodity_future_convention_versions_request: true,
    get_commodity_future_convention_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.commodity_future_conventions_events.created',
    updated: 'refdata.v1.commodity_future_conventions_events.updated',
    deleted: 'refdata.v1.commodity_future_conventions_events.deleted',
} as const;
