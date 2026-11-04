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
import type { CommodityFutureContractTerms } from '../domain/commodity_future_contract_terms.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CommodityFutureContractTermsKey {
    id: string;
}

export interface CommodityFutureContractTermsWrite {
    id: string;
    convention_id: string;
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
    hours_per_day: number | null;
    index_name: string | null;
    savings_time: string | null;
    delivery_location: string | null;
    balance_of_the_month: boolean | null;
    balance_of_the_month_pricing_calendar: string | null;
    prohibited_expiries: string | null;
    future_continuation_mappings: string | null;
}

export interface CommodityFutureContractTermsChange {
    write: CommodityFutureContractTermsWrite;
    precondition: Precondition;
}

export interface CommodityFutureContractTermsRemoval {
    key: CommodityFutureContractTermsKey;
    precondition: Precondition;
}

export interface CommodityFutureContractTermsLookup {
    key: CommodityFutureContractTermsKey;
    commodity_future_contract_terms: CommodityFutureContractTerms | null;
}

export interface CommodityFutureContractTermsFilter {
    convention_id: string | null;
    id_one_of: string[] | null;
    convention_id_one_of: string[] | null;
}

export interface CommodityFutureContractTermsEvent {
    event_id: string;
    key: CommodityFutureContractTermsKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CommodityFutureContractTermsVersionKey {
    commodity_future_contract_terms: CommodityFutureContractTermsKey;
    version: number;
}

export interface CommodityFutureContractTermsVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCommodityFutureContractTermsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityFutureContractTermsFilter | null;
    as_of: string | null;
}

export interface ListCommodityFutureContractTermsResponse {
    result: Result;
    commodity_future_contract_terms: CommodityFutureContractTerms[];
    total: number;
}

export interface GetCommodityFutureContractTermsRequest {
    key: CommodityFutureContractTermsKey;
}

export interface GetCommodityFutureContractTermsResponse {
    result: Result;
    commodity_future_contract_terms: CommodityFutureContractTerms | null;
}

export interface GetManyCommodityFutureContractTermsRequest {
    keys: CommodityFutureContractTermsKey[];
}

export interface GetManyCommodityFutureContractTermsResponse {
    result: Result;
    entries: CommodityFutureContractTermsLookup[];
}

export interface PutCommodityFutureContractTermsRequest {
    change: CommodityFutureContractTermsChange;
    intent: ChangeIntent;
}

export interface PutCommodityFutureContractTermsResponse {
    result: Result;
    commodity_future_contract_terms: CommodityFutureContractTerms | null;
}

export interface PutManyCommodityFutureContractTermsRequest {
    changes: CommodityFutureContractTermsChange[];
    intent: ChangeIntent;
}

export interface PutManyCommodityFutureContractTermsResponse {
    result: Result;
    commodity_future_contract_terms: CommodityFutureContractTerms[];
}

export interface DeleteCommodityFutureContractTermsRequest {
    removal: CommodityFutureContractTermsRemoval;
    intent: ChangeIntent;
}

export interface DeleteCommodityFutureContractTermsResponse {
    result: Result;
}

export interface DeleteManyCommodityFutureContractTermsRequest {
    removals: CommodityFutureContractTermsRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCommodityFutureContractTermsResponse {
    result: Result;
}

export interface ListByConventionIdCommodityFutureContractTermsRequest {
    convention_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityFutureContractTermsFilter | null;
}

export interface ListByConventionIdCommodityFutureContractTermsResponse {
    result: Result;
    commodity_future_contract_terms: CommodityFutureContractTerms[];
    total: number;
}

export interface ListCommodityFutureContractTermsVersionsRequest {
    key: CommodityFutureContractTermsKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CommodityFutureContractTermsVersionsFilter | null;
}

export interface ListCommodityFutureContractTermsVersionsResponse {
    result: Result;
    versions: CommodityFutureContractTerms[];
    total: number;
}

export interface GetCommodityFutureContractTermsVersionRequest {
    key: CommodityFutureContractTermsVersionKey;
}

export interface GetCommodityFutureContractTermsVersionResponse {
    result: Result;
    version: CommodityFutureContractTerms | null;
}

export const subjects = {
    list_commodity_future_contract_terms_request: 'refdata.v1.commodity_future_contract_terms.list',
    get_commodity_future_contract_terms_request: 'refdata.v1.commodity_future_contract_terms.get',
    get_many_commodity_future_contract_terms_request:
        'refdata.v1.commodity_future_contract_terms.get_many',
    put_commodity_future_contract_terms_request: 'refdata.v1.commodity_future_contract_terms.put',
    put_many_commodity_future_contract_terms_request:
        'refdata.v1.commodity_future_contract_terms.put_many',
    delete_commodity_future_contract_terms_request:
        'refdata.v1.commodity_future_contract_terms.delete',
    delete_many_commodity_future_contract_terms_request:
        'refdata.v1.commodity_future_contract_terms.delete_many',
    list_by_convention_id_commodity_future_contract_terms_request:
        'refdata.v1.commodity_future_contract_terms.list_by_convention_id',
    list_commodity_future_contract_terms_versions_request:
        'refdata.v1.commodity_future_contract_terms_versions.list',
    get_commodity_future_contract_terms_version_request:
        'refdata.v1.commodity_future_contract_terms_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_commodity_future_contract_terms_request: true,
    get_commodity_future_contract_terms_request: true,
    get_many_commodity_future_contract_terms_request: true,
    put_commodity_future_contract_terms_request: true,
    put_many_commodity_future_contract_terms_request: true,
    delete_commodity_future_contract_terms_request: true,
    delete_many_commodity_future_contract_terms_request: true,
    list_by_convention_id_commodity_future_contract_terms_request: true,
    list_commodity_future_contract_terms_versions_request: true,
    get_commodity_future_contract_terms_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.events.commodity_future_contract_terms.created',
    updated: 'refdata.v1.events.commodity_future_contract_terms.updated',
    deleted: 'refdata.v1.events.commodity_future_contract_terms.deleted',
} as const;
