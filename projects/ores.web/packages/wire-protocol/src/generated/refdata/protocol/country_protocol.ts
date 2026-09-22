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
import type { Country } from '../domain/country.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CountryKey {
    alpha2_code: string;
}

export interface CountryWrite {
    alpha2_code: string;
    alpha3_code: string;
    numeric_code: string;
    name: string;
    official_name: string;
    image_id: string | null;
    coding_scheme_code: string | null;
}

export interface CountryChange {
    write: CountryWrite;
    precondition: Precondition;
}

export interface CountryRemoval {
    key: CountryKey;
    precondition: Precondition;
}

export interface CountryLookup {
    key: CountryKey;
    country: Country | null;
}

export interface CountryEvent {
    event_id: string;
    key: CountryKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CountryVersionKey {
    country: CountryKey;
    version: number;
}

export interface CountryVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCountriesRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListCountriesResponse {
    result: Result;
    countries: Country[];
    total: number;
}

export interface GetCountryRequest {
    key: CountryKey;
}

export interface GetCountryResponse {
    result: Result;
    country: Country | null;
}

export interface GetManyCountriesRequest {
    keys: CountryKey[];
}

export interface GetManyCountriesResponse {
    result: Result;
    entries: CountryLookup[];
}

export interface PutCountryRequest {
    change: CountryChange;
    intent: ChangeIntent;
}

export interface PutCountryResponse {
    result: Result;
    country: Country;
}

export interface PutManyCountriesRequest {
    changes: CountryChange[];
    intent: ChangeIntent;
}

export interface PutManyCountriesResponse {
    result: Result;
    countries: Country[];
}

export interface DeleteCountryRequest {
    removal: CountryRemoval;
    intent: ChangeIntent;
}

export interface DeleteCountryResponse {
    result: Result;
}

export interface DeleteManyCountriesRequest {
    removals: CountryRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCountriesResponse {
    result: Result;
}

export interface ListCountryVersionsRequest {
    key: CountryKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CountryVersionsFilter | null;
}

export interface ListCountryVersionsResponse {
    result: Result;
    versions: Country[];
    total: number;
}

export interface GetCountryVersionRequest {
    key: CountryVersionKey;
}

export interface GetCountryVersionResponse {
    result: Result;
    version: Country;
}

export const subjects = {
    list_countries_request: "refdata.v1.countries.list",
    get_country_request: "refdata.v1.countries.get",
    get_many_countries_request: "refdata.v1.countries.get_many",
    put_country_request: "refdata.v1.countries.put",
    put_many_countries_request: "refdata.v1.countries.put_many",
    delete_country_request: "refdata.v1.countries.delete",
    delete_many_countries_request: "refdata.v1.countries.delete_many",
    list_country_versions_request: "refdata.v1.countries_versions.list",
    get_country_version_request: "refdata.v1.countries_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_countries_request: true,
    get_country_request: true,
    get_many_countries_request: true,
    put_country_request: true,
    put_many_countries_request: true,
    delete_country_request: true,
    delete_many_countries_request: true,
    list_country_versions_request: true,
    get_country_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.countries_events.created",
    updated: "refdata.v1.countries_events.updated",
    deleted: "refdata.v1.countries_events.deleted",
} as const;
