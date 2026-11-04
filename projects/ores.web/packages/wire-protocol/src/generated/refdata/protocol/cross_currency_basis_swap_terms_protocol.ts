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
import type { CrossCurrencyBasisSwapTerms } from '../domain/cross_currency_basis_swap_terms.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CrossCurrencyBasisSwapTermsKey {
    id: string;
}

export interface CrossCurrencyBasisSwapTermsWrite {
    id: string;
    convention_id: string;
    settlement_days: number;
    settlement_calendar: string | null;
    roll_convention: string;
    eom: boolean | null;
    is_resettable: boolean | null;
}

export interface CrossCurrencyBasisSwapTermsChange {
    write: CrossCurrencyBasisSwapTermsWrite;
    precondition: Precondition;
}

export interface CrossCurrencyBasisSwapTermsRemoval {
    key: CrossCurrencyBasisSwapTermsKey;
    precondition: Precondition;
}

export interface CrossCurrencyBasisSwapTermsLookup {
    key: CrossCurrencyBasisSwapTermsKey;
    cross_currency_basis_swap_terms: CrossCurrencyBasisSwapTerms | null;
}

export interface CrossCurrencyBasisSwapTermsFilter {
    convention_id: string | null;
    id_one_of: string[] | null;
    convention_id_one_of: string[] | null;
}

export interface CrossCurrencyBasisSwapTermsEvent {
    event_id: string;
    key: CrossCurrencyBasisSwapTermsKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CrossCurrencyBasisSwapTermsVersionKey {
    cross_currency_basis_swap_terms: CrossCurrencyBasisSwapTermsKey;
    version: number;
}

export interface CrossCurrencyBasisSwapTermsVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCrossCurrencyBasisSwapTermsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CrossCurrencyBasisSwapTermsFilter | null;
    as_of: string | null;
}

export interface ListCrossCurrencyBasisSwapTermsResponse {
    result: Result;
    cross_currency_basis_swap_terms: CrossCurrencyBasisSwapTerms[];
    total: number;
}

export interface GetCrossCurrencyBasisSwapTermsRequest {
    key: CrossCurrencyBasisSwapTermsKey;
}

export interface GetCrossCurrencyBasisSwapTermsResponse {
    result: Result;
    cross_currency_basis_swap_terms: CrossCurrencyBasisSwapTerms | null;
}

export interface GetManyCrossCurrencyBasisSwapTermsRequest {
    keys: CrossCurrencyBasisSwapTermsKey[];
}

export interface GetManyCrossCurrencyBasisSwapTermsResponse {
    result: Result;
    entries: CrossCurrencyBasisSwapTermsLookup[];
}

export interface PutCrossCurrencyBasisSwapTermsRequest {
    change: CrossCurrencyBasisSwapTermsChange;
    intent: ChangeIntent;
}

export interface PutCrossCurrencyBasisSwapTermsResponse {
    result: Result;
    cross_currency_basis_swap_terms: CrossCurrencyBasisSwapTerms | null;
}

export interface PutManyCrossCurrencyBasisSwapTermsRequest {
    changes: CrossCurrencyBasisSwapTermsChange[];
    intent: ChangeIntent;
}

export interface PutManyCrossCurrencyBasisSwapTermsResponse {
    result: Result;
    cross_currency_basis_swap_terms: CrossCurrencyBasisSwapTerms[];
}

export interface DeleteCrossCurrencyBasisSwapTermsRequest {
    removal: CrossCurrencyBasisSwapTermsRemoval;
    intent: ChangeIntent;
}

export interface DeleteCrossCurrencyBasisSwapTermsResponse {
    result: Result;
}

export interface DeleteManyCrossCurrencyBasisSwapTermsRequest {
    removals: CrossCurrencyBasisSwapTermsRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCrossCurrencyBasisSwapTermsResponse {
    result: Result;
}

export interface ListByConventionIdCrossCurrencyBasisSwapTermsRequest {
    convention_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CrossCurrencyBasisSwapTermsFilter | null;
}

export interface ListByConventionIdCrossCurrencyBasisSwapTermsResponse {
    result: Result;
    cross_currency_basis_swap_terms: CrossCurrencyBasisSwapTerms[];
    total: number;
}

export interface ListCrossCurrencyBasisSwapTermsVersionsRequest {
    key: CrossCurrencyBasisSwapTermsKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CrossCurrencyBasisSwapTermsVersionsFilter | null;
}

export interface ListCrossCurrencyBasisSwapTermsVersionsResponse {
    result: Result;
    versions: CrossCurrencyBasisSwapTerms[];
    total: number;
}

export interface GetCrossCurrencyBasisSwapTermsVersionRequest {
    key: CrossCurrencyBasisSwapTermsVersionKey;
}

export interface GetCrossCurrencyBasisSwapTermsVersionResponse {
    result: Result;
    version: CrossCurrencyBasisSwapTerms | null;
}

export const subjects = {
    list_cross_currency_basis_swap_terms_request: 'refdata.v1.cross_currency_basis_swap_terms.list',
    get_cross_currency_basis_swap_terms_request: 'refdata.v1.cross_currency_basis_swap_terms.get',
    get_many_cross_currency_basis_swap_terms_request:
        'refdata.v1.cross_currency_basis_swap_terms.get_many',
    put_cross_currency_basis_swap_terms_request: 'refdata.v1.cross_currency_basis_swap_terms.put',
    put_many_cross_currency_basis_swap_terms_request:
        'refdata.v1.cross_currency_basis_swap_terms.put_many',
    delete_cross_currency_basis_swap_terms_request:
        'refdata.v1.cross_currency_basis_swap_terms.delete',
    delete_many_cross_currency_basis_swap_terms_request:
        'refdata.v1.cross_currency_basis_swap_terms.delete_many',
    list_by_convention_id_cross_currency_basis_swap_terms_request:
        'refdata.v1.cross_currency_basis_swap_terms.list_by_convention_id',
    list_cross_currency_basis_swap_terms_versions_request:
        'refdata.v1.cross_currency_basis_swap_terms_versions.list',
    get_cross_currency_basis_swap_terms_version_request:
        'refdata.v1.cross_currency_basis_swap_terms_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_cross_currency_basis_swap_terms_request: true,
    get_cross_currency_basis_swap_terms_request: true,
    get_many_cross_currency_basis_swap_terms_request: true,
    put_cross_currency_basis_swap_terms_request: true,
    put_many_cross_currency_basis_swap_terms_request: true,
    delete_cross_currency_basis_swap_terms_request: true,
    delete_many_cross_currency_basis_swap_terms_request: true,
    list_by_convention_id_cross_currency_basis_swap_terms_request: true,
    list_cross_currency_basis_swap_terms_versions_request: true,
    get_cross_currency_basis_swap_terms_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.events.cross_currency_basis_swap_terms.created',
    updated: 'refdata.v1.events.cross_currency_basis_swap_terms.updated',
    deleted: 'refdata.v1.events.cross_currency_basis_swap_terms.deleted',
} as const;
