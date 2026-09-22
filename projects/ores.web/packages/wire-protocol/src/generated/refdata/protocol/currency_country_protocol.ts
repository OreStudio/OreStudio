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
import type { CurrencyCountry } from '../domain/currency_country.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CurrencyCountryKey {
    currency_iso_code: string;
    country_alpha2_code: string;
}

export interface CurrencyCountryWrite {
    currency_iso_code: string;
    country_alpha2_code: string;
}

export interface CurrencyCountryChange {
    write: CurrencyCountryWrite;
    precondition: Precondition;
}

export interface CurrencyCountryRemoval {
    key: CurrencyCountryKey;
    precondition: Precondition;
}

export interface CurrencyCountryLookup {
    key: CurrencyCountryKey;
    currency_country: CurrencyCountry | null;
}

export interface CurrencyCountriesFilter {
    currency_iso_code: string | null;
}

export interface CurrencyCountryEvent {
    event_id: string;
    key: CurrencyCountryKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListCurrencyCountriesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CurrencyCountriesFilter | null;
}

export interface ListCurrencyCountriesResponse {
    result: Result;
    currency_countries: CurrencyCountry[];
    total: number;
}

export interface GetCurrencyCountryRequest {
    key: CurrencyCountryKey;
}

export interface GetCurrencyCountryResponse {
    result: Result;
    currency_country: CurrencyCountry | null;
}

export interface GetManyCurrencyCountriesRequest {
    keys: CurrencyCountryKey[];
}

export interface GetManyCurrencyCountriesResponse {
    result: Result;
    entries: CurrencyCountryLookup[];
}

export interface PutCurrencyCountryRequest {
    change: CurrencyCountryChange;
    intent: ChangeIntent;
}

export interface PutCurrencyCountryResponse {
    result: Result;
    currency_country: CurrencyCountry;
}

export interface PutManyCurrencyCountriesRequest {
    changes: CurrencyCountryChange[];
    intent: ChangeIntent;
}

export interface PutManyCurrencyCountriesResponse {
    result: Result;
    currency_countries: CurrencyCountry[];
}

export interface DeleteCurrencyCountryRequest {
    removal: CurrencyCountryRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurrencyCountryResponse {
    result: Result;
}

export interface DeleteManyCurrencyCountriesRequest {
    removals: CurrencyCountryRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurrencyCountriesResponse {
    result: Result;
}

export interface ListByCurrencyIsoCodeCurrencyCountriesRequest {
    currency_iso_code: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CurrencyCountriesFilter | null;
}

export interface ListByCurrencyIsoCodeCurrencyCountriesResponse {
    result: Result;
    currency_countries: CurrencyCountry[];
    total: number;
}

export const subjects = {
    list_currency_countries_request: "refdata.v1.currency_countries.list",
    get_currency_country_request: "refdata.v1.currency_countries.get",
    get_many_currency_countries_request: "refdata.v1.currency_countries.get_many",
    put_currency_country_request: "refdata.v1.currency_countries.put",
    put_many_currency_countries_request: "refdata.v1.currency_countries.put_many",
    delete_currency_country_request: "refdata.v1.currency_countries.delete",
    delete_many_currency_countries_request: "refdata.v1.currency_countries.delete_many",
    list_by_currency_iso_code_currency_countries_request: "refdata.v1.currency_countries.list_by_currency_iso_code",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_currency_countries_request: true,
    get_currency_country_request: true,
    get_many_currency_countries_request: true,
    put_currency_country_request: true,
    put_many_currency_countries_request: true,
    delete_currency_country_request: true,
    delete_many_currency_countries_request: true,
    list_by_currency_iso_code_currency_countries_request: true,
} as const;
