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

export interface GetPartyCountriesRequest {
    offset: number;
    limit: number;
}

export interface GetPartyCountriesResponse {
    party_countries: PartyCountry[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface GetPartyCountriesByPartyRequest {
    party_id: string;
    offset: number;
    limit: number;
}

export interface GetPartyCountriesByPartyResponse {
    party_countries: PartyCountryView[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SavePartyCountryRequest {
    party_countries: PartyCountry[];
}

export interface SavePartyCountryResponse {
    success: boolean;
    message: string;
}

export interface DeletePartyCountryRequest {
    party_ids: string[];
    country_alpha2_codes: string[];
}

export interface DeletePartyCountryResponse {
    success: boolean;
    message: string;
}

export interface CountPartyCountriesByPartyRequest {
    party_id: string;
}

export interface CountPartyCountriesByPartyResponse {
    total_available_count: number;
}

export interface CountPartyCountriesByCountryRequest {
    country_alpha2_code: string;
}

export interface CountPartyCountriesByCountryResponse {
    total_available_count: number;
}

export interface PartyCountryView {
    party_country: PartyCountry;
}

export const subjects = {
    get_party_countries_request: "refdata.v1.party_countries.list",
    get_party_countries_by_party_request: "refdata.v1.party_countries.list_by_party_id",
    save_party_country_request: "refdata.v1.party_countries.save",
    delete_party_country_request: "refdata.v1.party_countries.delete",
    count_party_countries_by_party_request: "refdata.v1.party_countries.count_by_party_id",
    count_party_countries_by_country_request: "refdata.v1.party_countries.count_by_country_alpha2_code",
} as const;
