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
import type { CsaEligibleCurrency } from '../domain/csa_eligible_currency.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CsaEligibleCurrencyKey {
    currency_code: string;
}

export interface CsaEligibleCurrencyWrite {
    id: string;
    csa_id: string;
    currency_code: string;
    position: number;
}

export interface CsaEligibleCurrencyChange {
    write: CsaEligibleCurrencyWrite;
    precondition: Precondition;
}

export interface CsaEligibleCurrencyRemoval {
    key: CsaEligibleCurrencyKey;
    precondition: Precondition;
}

export interface CsaEligibleCurrencyLookup {
    key: CsaEligibleCurrencyKey;
    csa_eligible_currency: CsaEligibleCurrency | null;
}

export interface CsaEligibleCurrenciesFilter {
    csa_id: string | null;
    id_one_of: string[] | null;
    csa_id_one_of: string[] | null;
}

export interface CsaEligibleCurrencyEvent {
    event_id: string;
    key: CsaEligibleCurrencyKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CsaEligibleCurrencyVersionKey {
    csa_eligible_currency: CsaEligibleCurrencyKey;
    version: number;
}

export interface CsaEligibleCurrencyVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCsaEligibleCurrenciesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CsaEligibleCurrenciesFilter | null;
    as_of: string | null;
}

export interface ListCsaEligibleCurrenciesResponse {
    result: Result;
    csa_eligible_currencies: CsaEligibleCurrency[];
    total: number;
}

export interface GetCsaEligibleCurrencyRequest {
    key: CsaEligibleCurrencyKey;
}

export interface GetCsaEligibleCurrencyResponse {
    result: Result;
    csa_eligible_currency: CsaEligibleCurrency | null;
}

export interface GetManyCsaEligibleCurrenciesRequest {
    keys: CsaEligibleCurrencyKey[];
}

export interface GetManyCsaEligibleCurrenciesResponse {
    result: Result;
    entries: CsaEligibleCurrencyLookup[];
}

export interface PutCsaEligibleCurrencyRequest {
    change: CsaEligibleCurrencyChange;
    intent: ChangeIntent;
}

export interface PutCsaEligibleCurrencyResponse {
    result: Result;
    csa_eligible_currency: CsaEligibleCurrency | null;
}

export interface PutManyCsaEligibleCurrenciesRequest {
    changes: CsaEligibleCurrencyChange[];
    intent: ChangeIntent;
}

export interface PutManyCsaEligibleCurrenciesResponse {
    result: Result;
    csa_eligible_currencies: CsaEligibleCurrency[];
}

export interface DeleteCsaEligibleCurrencyRequest {
    removal: CsaEligibleCurrencyRemoval;
    intent: ChangeIntent;
}

export interface DeleteCsaEligibleCurrencyResponse {
    result: Result;
}

export interface DeleteManyCsaEligibleCurrenciesRequest {
    removals: CsaEligibleCurrencyRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCsaEligibleCurrenciesResponse {
    result: Result;
}

export interface ListByCsaIdCsaEligibleCurrenciesRequest {
    csa_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CsaEligibleCurrenciesFilter | null;
}

export interface ListByCsaIdCsaEligibleCurrenciesResponse {
    result: Result;
    csa_eligible_currencies: CsaEligibleCurrency[];
    total: number;
}

export interface ListCsaEligibleCurrencyVersionsRequest {
    key: CsaEligibleCurrencyKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CsaEligibleCurrencyVersionsFilter | null;
}

export interface ListCsaEligibleCurrencyVersionsResponse {
    result: Result;
    versions: CsaEligibleCurrency[];
    total: number;
}

export interface GetCsaEligibleCurrencyVersionRequest {
    key: CsaEligibleCurrencyVersionKey;
}

export interface GetCsaEligibleCurrencyVersionResponse {
    result: Result;
    version: CsaEligibleCurrency | null;
}

export const subjects = {
    list_csa_eligible_currencies_request: 'refdata.v1.csa_eligible_currencies.list',
    get_csa_eligible_currency_request: 'refdata.v1.csa_eligible_currencies.get',
    get_many_csa_eligible_currencies_request: 'refdata.v1.csa_eligible_currencies.get_many',
    put_csa_eligible_currency_request: 'refdata.v1.csa_eligible_currencies.put',
    put_many_csa_eligible_currencies_request: 'refdata.v1.csa_eligible_currencies.put_many',
    delete_csa_eligible_currency_request: 'refdata.v1.csa_eligible_currencies.delete',
    delete_many_csa_eligible_currencies_request: 'refdata.v1.csa_eligible_currencies.delete_many',
    list_by_csa_id_csa_eligible_currencies_request:
        'refdata.v1.csa_eligible_currencies.list_by_csa_id',
    list_csa_eligible_currency_versions_request: 'refdata.v1.csa_eligible_currencies_versions.list',
    get_csa_eligible_currency_version_request: 'refdata.v1.csa_eligible_currencies_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_csa_eligible_currencies_request: true,
    get_csa_eligible_currency_request: true,
    get_many_csa_eligible_currencies_request: true,
    put_csa_eligible_currency_request: true,
    put_many_csa_eligible_currencies_request: true,
    delete_csa_eligible_currency_request: true,
    delete_many_csa_eligible_currencies_request: true,
    list_by_csa_id_csa_eligible_currencies_request: true,
    list_csa_eligible_currency_versions_request: true,
    get_csa_eligible_currency_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.csa_eligible_currencies_events.created',
    updated: 'refdata.v1.csa_eligible_currencies_events.updated',
    deleted: 'refdata.v1.csa_eligible_currencies_events.deleted',
} as const;
