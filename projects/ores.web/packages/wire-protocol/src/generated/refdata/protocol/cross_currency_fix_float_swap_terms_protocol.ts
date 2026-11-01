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
import type { CrossCurrencyFixFloatSwapTerms } from '../domain/cross_currency_fix_float_swap_terms.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CrossCurrencyFixFloatSwapTermsKey {
    id: string;
}

export interface CrossCurrencyFixFloatSwapTermsWrite {
    id: string;
    convention_id: string;
    settlement_days: number;
    settlement_calendar: string;
    settlement_convention: string;
    eom: boolean | null;
    is_resettable: boolean | null;
}

export interface CrossCurrencyFixFloatSwapTermsChange {
    write: CrossCurrencyFixFloatSwapTermsWrite;
    precondition: Precondition;
}

export interface CrossCurrencyFixFloatSwapTermsRemoval {
    key: CrossCurrencyFixFloatSwapTermsKey;
    precondition: Precondition;
}

export interface CrossCurrencyFixFloatSwapTermsLookup {
    key: CrossCurrencyFixFloatSwapTermsKey;
    cross_currency_fix_float_swap_terms: CrossCurrencyFixFloatSwapTerms | null;
}

export interface CrossCurrencyFixFloatSwapTermsFilter {
    convention_id: string | null;
    id_one_of: string[] | null;
    convention_id_one_of: string[] | null;
}

export interface CrossCurrencyFixFloatSwapTermsEvent {
    event_id: string;
    key: CrossCurrencyFixFloatSwapTermsKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CrossCurrencyFixFloatSwapTermsVersionKey {
    cross_currency_fix_float_swap_terms: CrossCurrencyFixFloatSwapTermsKey;
    version: number;
}

export interface CrossCurrencyFixFloatSwapTermsVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCrossCurrencyFixFloatSwapTermsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CrossCurrencyFixFloatSwapTermsFilter | null;
    as_of: string | null;
}

export interface ListCrossCurrencyFixFloatSwapTermsResponse {
    result: Result;
    cross_currency_fix_float_swap_terms: CrossCurrencyFixFloatSwapTerms[];
    total: number;
}

export interface GetCrossCurrencyFixFloatSwapTermsRequest {
    key: CrossCurrencyFixFloatSwapTermsKey;
}

export interface GetCrossCurrencyFixFloatSwapTermsResponse {
    result: Result;
    cross_currency_fix_float_swap_terms: CrossCurrencyFixFloatSwapTerms | null;
}

export interface GetManyCrossCurrencyFixFloatSwapTermsRequest {
    keys: CrossCurrencyFixFloatSwapTermsKey[];
}

export interface GetManyCrossCurrencyFixFloatSwapTermsResponse {
    result: Result;
    entries: CrossCurrencyFixFloatSwapTermsLookup[];
}

export interface PutCrossCurrencyFixFloatSwapTermsRequest {
    change: CrossCurrencyFixFloatSwapTermsChange;
    intent: ChangeIntent;
}

export interface PutCrossCurrencyFixFloatSwapTermsResponse {
    result: Result;
    cross_currency_fix_float_swap_terms: CrossCurrencyFixFloatSwapTerms | null;
}

export interface PutManyCrossCurrencyFixFloatSwapTermsRequest {
    changes: CrossCurrencyFixFloatSwapTermsChange[];
    intent: ChangeIntent;
}

export interface PutManyCrossCurrencyFixFloatSwapTermsResponse {
    result: Result;
    cross_currency_fix_float_swap_terms: CrossCurrencyFixFloatSwapTerms[];
}

export interface DeleteCrossCurrencyFixFloatSwapTermsRequest {
    removal: CrossCurrencyFixFloatSwapTermsRemoval;
    intent: ChangeIntent;
}

export interface DeleteCrossCurrencyFixFloatSwapTermsResponse {
    result: Result;
}

export interface DeleteManyCrossCurrencyFixFloatSwapTermsRequest {
    removals: CrossCurrencyFixFloatSwapTermsRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCrossCurrencyFixFloatSwapTermsResponse {
    result: Result;
}

export interface ListByConventionIdCrossCurrencyFixFloatSwapTermsRequest {
    convention_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CrossCurrencyFixFloatSwapTermsFilter | null;
}

export interface ListByConventionIdCrossCurrencyFixFloatSwapTermsResponse {
    result: Result;
    cross_currency_fix_float_swap_terms: CrossCurrencyFixFloatSwapTerms[];
    total: number;
}

export interface ListCrossCurrencyFixFloatSwapTermsVersionsRequest {
    key: CrossCurrencyFixFloatSwapTermsKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CrossCurrencyFixFloatSwapTermsVersionsFilter | null;
}

export interface ListCrossCurrencyFixFloatSwapTermsVersionsResponse {
    result: Result;
    versions: CrossCurrencyFixFloatSwapTerms[];
    total: number;
}

export interface GetCrossCurrencyFixFloatSwapTermsVersionRequest {
    key: CrossCurrencyFixFloatSwapTermsVersionKey;
}

export interface GetCrossCurrencyFixFloatSwapTermsVersionResponse {
    result: Result;
    version: CrossCurrencyFixFloatSwapTerms | null;
}

export const subjects = {
    list_cross_currency_fix_float_swap_terms_request:
        'refdata.v1.cross_currency_fix_float_swap_terms.list',
    get_cross_currency_fix_float_swap_terms_request:
        'refdata.v1.cross_currency_fix_float_swap_terms.get',
    get_many_cross_currency_fix_float_swap_terms_request:
        'refdata.v1.cross_currency_fix_float_swap_terms.get_many',
    put_cross_currency_fix_float_swap_terms_request:
        'refdata.v1.cross_currency_fix_float_swap_terms.put',
    put_many_cross_currency_fix_float_swap_terms_request:
        'refdata.v1.cross_currency_fix_float_swap_terms.put_many',
    delete_cross_currency_fix_float_swap_terms_request:
        'refdata.v1.cross_currency_fix_float_swap_terms.delete',
    delete_many_cross_currency_fix_float_swap_terms_request:
        'refdata.v1.cross_currency_fix_float_swap_terms.delete_many',
    list_by_convention_id_cross_currency_fix_float_swap_terms_request:
        'refdata.v1.cross_currency_fix_float_swap_terms.list_by_convention_id',
    list_cross_currency_fix_float_swap_terms_versions_request:
        'refdata.v1.cross_currency_fix_float_swap_terms_versions.list',
    get_cross_currency_fix_float_swap_terms_version_request:
        'refdata.v1.cross_currency_fix_float_swap_terms_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_cross_currency_fix_float_swap_terms_request: true,
    get_cross_currency_fix_float_swap_terms_request: true,
    get_many_cross_currency_fix_float_swap_terms_request: true,
    put_cross_currency_fix_float_swap_terms_request: true,
    put_many_cross_currency_fix_float_swap_terms_request: true,
    delete_cross_currency_fix_float_swap_terms_request: true,
    delete_many_cross_currency_fix_float_swap_terms_request: true,
    list_by_convention_id_cross_currency_fix_float_swap_terms_request: true,
    list_cross_currency_fix_float_swap_terms_versions_request: true,
    get_cross_currency_fix_float_swap_terms_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.cross_currency_fix_float_swap_terms_events.created',
    updated: 'refdata.v1.cross_currency_fix_float_swap_terms_events.updated',
    deleted: 'refdata.v1.cross_currency_fix_float_swap_terms_events.deleted',
} as const;
