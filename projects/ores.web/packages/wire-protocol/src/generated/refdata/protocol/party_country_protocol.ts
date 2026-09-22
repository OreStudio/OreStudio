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
import type { PartyCountry } from '../domain/party_country.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface PartyCountryKey {
    party_id: string;
    country_alpha2_code: string;
}

export interface PartyCountryWrite {
    party_id: string;
    country_alpha2_code: string;
}

export interface PartyCountryChange {
    write: PartyCountryWrite;
    precondition: Precondition;
}

export interface PartyCountryRemoval {
    key: PartyCountryKey;
    precondition: Precondition;
}

export interface PartyCountryLookup {
    key: PartyCountryKey;
    party_country: PartyCountry | null;
}

export interface PartyCountriesFilter {
    party_id: string | null;
}

export interface PartyCountryEvent {
    event_id: string;
    key: PartyCountryKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListPartyCountriesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: PartyCountriesFilter | null;
}

export interface ListPartyCountriesResponse {
    result: Result;
    party_countries: PartyCountry[];
    total: number;
}

export interface GetPartyCountryRequest {
    key: PartyCountryKey;
}

export interface GetPartyCountryResponse {
    result: Result;
    party_country: PartyCountry | null;
}

export interface GetManyPartyCountriesRequest {
    keys: PartyCountryKey[];
}

export interface GetManyPartyCountriesResponse {
    result: Result;
    entries: PartyCountryLookup[];
}

export interface PutPartyCountryRequest {
    change: PartyCountryChange;
    intent: ChangeIntent;
}

export interface PutPartyCountryResponse {
    result: Result;
    party_country: PartyCountry;
}

export interface PutManyPartyCountriesRequest {
    changes: PartyCountryChange[];
    intent: ChangeIntent;
}

export interface PutManyPartyCountriesResponse {
    result: Result;
    party_countries: PartyCountry[];
}

export interface DeletePartyCountryRequest {
    removal: PartyCountryRemoval;
    intent: ChangeIntent;
}

export interface DeletePartyCountryResponse {
    result: Result;
}

export interface DeleteManyPartyCountriesRequest {
    removals: PartyCountryRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyPartyCountriesResponse {
    result: Result;
}

export interface ListByPartyIdPartyCountriesRequest {
    party_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: PartyCountriesFilter | null;
}

export interface ListByPartyIdPartyCountriesResponse {
    result: Result;
    party_countries: PartyCountry[];
    total: number;
}

export const subjects = {
    list_party_countries_request: "refdata.v1.party_countries.list",
    get_party_country_request: "refdata.v1.party_countries.get",
    get_many_party_countries_request: "refdata.v1.party_countries.get_many",
    put_party_country_request: "refdata.v1.party_countries.put",
    put_many_party_countries_request: "refdata.v1.party_countries.put_many",
    delete_party_country_request: "refdata.v1.party_countries.delete",
    delete_many_party_countries_request: "refdata.v1.party_countries.delete_many",
    list_by_party_id_party_countries_request: "refdata.v1.party_countries.list_by_party_id",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_party_countries_request: true,
    get_party_country_request: true,
    get_many_party_countries_request: true,
    put_party_country_request: true,
    put_many_party_countries_request: true,
    delete_party_country_request: true,
    delete_many_party_countries_request: true,
    list_by_party_id_party_countries_request: true,
} as const;
