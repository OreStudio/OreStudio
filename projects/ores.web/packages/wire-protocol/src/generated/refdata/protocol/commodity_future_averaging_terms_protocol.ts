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
import type { CommodityFutureAveragingTerms } from '../domain/commodity_future_averaging_terms.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CommodityFutureAveragingTermsKey {
    id: string;
}

export interface CommodityFutureAveragingTermsWrite {
    id: string;
    convention_id: string;
    averaging_commodity_name: string;
    averaging_period: string;
    averaging_pricing_calendar: string;
    averaging_conventions: string;
    averaging_use_business_days: boolean | null;
    averaging_delivery_roll_days: number | null;
    averaging_future_month_offset: number | null;
    averaging_daily_expiry_offset: number | null;
}

export interface CommodityFutureAveragingTermsChange {
    write: CommodityFutureAveragingTermsWrite;
    precondition: Precondition;
}

export interface CommodityFutureAveragingTermsRemoval {
    key: CommodityFutureAveragingTermsKey;
    precondition: Precondition;
}

export interface CommodityFutureAveragingTermsLookup {
    key: CommodityFutureAveragingTermsKey;
    commodity_future_averaging_terms: CommodityFutureAveragingTerms | null;
}

export interface CommodityFutureAveragingTermsFilter {
    convention_id: string | null;
    id_one_of: string[] | null;
    convention_id_one_of: string[] | null;
}

export interface CommodityFutureAveragingTermsEvent {
    event_id: string;
    key: CommodityFutureAveragingTermsKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CommodityFutureAveragingTermsVersionKey {
    commodity_future_averaging_terms: CommodityFutureAveragingTermsKey;
    version: number;
}

export interface CommodityFutureAveragingTermsVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCommodityFutureAveragingTermsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityFutureAveragingTermsFilter | null;
    as_of: string | null;
}

export interface ListCommodityFutureAveragingTermsResponse {
    result: Result;
    commodity_future_averaging_terms: CommodityFutureAveragingTerms[];
    total: number;
}

export interface GetCommodityFutureAveragingTermsRequest {
    key: CommodityFutureAveragingTermsKey;
}

export interface GetCommodityFutureAveragingTermsResponse {
    result: Result;
    commodity_future_averaging_terms: CommodityFutureAveragingTerms | null;
}

export interface GetManyCommodityFutureAveragingTermsRequest {
    keys: CommodityFutureAveragingTermsKey[];
}

export interface GetManyCommodityFutureAveragingTermsResponse {
    result: Result;
    entries: CommodityFutureAveragingTermsLookup[];
}

export interface PutCommodityFutureAveragingTermsRequest {
    change: CommodityFutureAveragingTermsChange;
    intent: ChangeIntent;
}

export interface PutCommodityFutureAveragingTermsResponse {
    result: Result;
    commodity_future_averaging_terms: CommodityFutureAveragingTerms | null;
}

export interface PutManyCommodityFutureAveragingTermsRequest {
    changes: CommodityFutureAveragingTermsChange[];
    intent: ChangeIntent;
}

export interface PutManyCommodityFutureAveragingTermsResponse {
    result: Result;
    commodity_future_averaging_terms: CommodityFutureAveragingTerms[];
}

export interface DeleteCommodityFutureAveragingTermsRequest {
    removal: CommodityFutureAveragingTermsRemoval;
    intent: ChangeIntent;
}

export interface DeleteCommodityFutureAveragingTermsResponse {
    result: Result;
}

export interface DeleteManyCommodityFutureAveragingTermsRequest {
    removals: CommodityFutureAveragingTermsRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCommodityFutureAveragingTermsResponse {
    result: Result;
}

export interface ListByConventionIdCommodityFutureAveragingTermsRequest {
    convention_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityFutureAveragingTermsFilter | null;
}

export interface ListByConventionIdCommodityFutureAveragingTermsResponse {
    result: Result;
    commodity_future_averaging_terms: CommodityFutureAveragingTerms[];
    total: number;
}

export interface ListCommodityFutureAveragingTermsVersionsRequest {
    key: CommodityFutureAveragingTermsKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityFutureAveragingTermsVersionsFilter | null;
}

export interface ListCommodityFutureAveragingTermsVersionsResponse {
    result: Result;
    versions: CommodityFutureAveragingTerms[];
    total: number;
}

export interface GetCommodityFutureAveragingTermsVersionRequest {
    key: CommodityFutureAveragingTermsVersionKey;
}

export interface GetCommodityFutureAveragingTermsVersionResponse {
    result: Result;
    version: CommodityFutureAveragingTerms | null;
}

export const subjects = {
    list_commodity_future_averaging_terms_request:
        'refdata.v1.commodity_future_averaging_terms.list',
    get_commodity_future_averaging_terms_request: 'refdata.v1.commodity_future_averaging_terms.get',
    get_many_commodity_future_averaging_terms_request:
        'refdata.v1.commodity_future_averaging_terms.get_many',
    put_commodity_future_averaging_terms_request: 'refdata.v1.commodity_future_averaging_terms.put',
    put_many_commodity_future_averaging_terms_request:
        'refdata.v1.commodity_future_averaging_terms.put_many',
    delete_commodity_future_averaging_terms_request:
        'refdata.v1.commodity_future_averaging_terms.delete',
    delete_many_commodity_future_averaging_terms_request:
        'refdata.v1.commodity_future_averaging_terms.delete_many',
    list_by_convention_id_commodity_future_averaging_terms_request:
        'refdata.v1.commodity_future_averaging_terms.list_by_convention_id',
    list_commodity_future_averaging_terms_versions_request:
        'refdata.v1.commodity_future_averaging_terms_versions.list',
    get_commodity_future_averaging_terms_version_request:
        'refdata.v1.commodity_future_averaging_terms_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_commodity_future_averaging_terms_request: true,
    get_commodity_future_averaging_terms_request: true,
    get_many_commodity_future_averaging_terms_request: true,
    put_commodity_future_averaging_terms_request: true,
    put_many_commodity_future_averaging_terms_request: true,
    delete_commodity_future_averaging_terms_request: true,
    delete_many_commodity_future_averaging_terms_request: true,
    list_by_convention_id_commodity_future_averaging_terms_request: true,
    list_commodity_future_averaging_terms_versions_request: true,
    get_commodity_future_averaging_terms_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.events.commodity_future_averaging_terms.created',
    updated: 'refdata.v1.events.commodity_future_averaging_terms.updated',
    deleted: 'refdata.v1.events.commodity_future_averaging_terms.deleted',
} as const;
