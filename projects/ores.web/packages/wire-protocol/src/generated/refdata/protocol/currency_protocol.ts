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
import type { Currency } from '../domain/currency.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurrencyKey {
    iso_code: string;
}

export interface CurrencyWrite {
    iso_code: string;
    name: string;
    numeric_code: string;
    symbol: string;
    fraction_symbol: string;
    fractions_per_unit: number;
    rounding_type: string;
    rounding_precision: number;
    format: string;
    monetary_nature: string;
    market_tier: string;
    image_id: string | null;
    spot_days: number;
    day_basis: string;
    base_precedence: number;
}

export interface CurrencyChange {
    write: CurrencyWrite;
    precondition: Precondition;
}

export interface CurrencyRemoval {
    key: CurrencyKey;
    precondition: Precondition;
}

export interface CurrencyLookup {
    key: CurrencyKey;
    currency: Currency | null;
}

export interface CurrencyEvent {
    event_id: string;
    key: CurrencyKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurrencyVersionKey {
    currency: CurrencyKey;
    version: number;
}

export interface CurrencyVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurrenciesRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListCurrenciesResponse {
    result: Result;
    currencies: Currency[];
    total: number;
}

export interface GetCurrencyRequest {
    key: CurrencyKey;
}

export interface GetCurrencyResponse {
    result: Result;
    currency: Currency | null;
}

export interface GetManyCurrenciesRequest {
    keys: CurrencyKey[];
}

export interface GetManyCurrenciesResponse {
    result: Result;
    entries: CurrencyLookup[];
}

export interface PutCurrencyRequest {
    change: CurrencyChange;
    intent: ChangeIntent;
}

export interface PutCurrencyResponse {
    result: Result;
    currency: Currency;
}

export interface PutManyCurrenciesRequest {
    changes: CurrencyChange[];
    intent: ChangeIntent;
}

export interface PutManyCurrenciesResponse {
    result: Result;
    currencies: Currency[];
}

export interface DeleteCurrencyRequest {
    removal: CurrencyRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurrencyResponse {
    result: Result;
}

export interface DeleteManyCurrenciesRequest {
    removals: CurrencyRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurrenciesResponse {
    result: Result;
}

export interface ListCurrencyVersionsRequest {
    key: CurrencyKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurrencyVersionsFilter | null;
}

export interface ListCurrencyVersionsResponse {
    result: Result;
    versions: Currency[];
    total: number;
}

export interface GetCurrencyVersionRequest {
    key: CurrencyVersionKey;
}

export interface GetCurrencyVersionResponse {
    result: Result;
    version: Currency;
}

export const subjects = {
    list_currencies_request: "refdata.v1.currencies.list",
    get_currency_request: "refdata.v1.currencies.get",
    get_many_currencies_request: "refdata.v1.currencies.get_many",
    put_currency_request: "refdata.v1.currencies.put",
    put_many_currencies_request: "refdata.v1.currencies.put_many",
    delete_currency_request: "refdata.v1.currencies.delete",
    delete_many_currencies_request: "refdata.v1.currencies.delete_many",
    list_currency_versions_request: "refdata.v1.currencies_versions.list",
    get_currency_version_request: "refdata.v1.currencies_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_currencies_request: true,
    get_currency_request: true,
    get_many_currencies_request: true,
    put_currency_request: true,
    put_many_currencies_request: true,
    delete_currency_request: true,
    delete_many_currencies_request: true,
    list_currency_versions_request: true,
    get_currency_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.currencies_events.created",
    updated: "refdata.v1.currencies_events.updated",
    deleted: "refdata.v1.currencies_events.deleted",
} as const;
