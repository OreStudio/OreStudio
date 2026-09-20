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

export interface GetCurrencyCountriesRequest {
    offset: number;
    limit: number;
}

export interface GetCurrencyCountriesResponse {
    currency_countries: CurrencyCountry[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface GetCurrencyCountriesByCurrencyRequest {
    currency_iso_code: string;
    offset: number;
    limit: number;
}

export interface GetCurrencyCountriesByCurrencyResponse {
    currency_countries: CurrencyCountryView[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveCurrencyCountryRequest {
    currency_countries: CurrencyCountry[];
}

export interface SaveCurrencyCountryResponse {
    success: boolean;
    message: string;
}

export interface DeleteCurrencyCountryRequest {
    currency_iso_codes: string[];
    country_alpha2_codes: string[];
}

export interface DeleteCurrencyCountryResponse {
    success: boolean;
    message: string;
}

export interface CountCurrencyCountriesByCurrencyRequest {
    currency_iso_code: string;
}

export interface CountCurrencyCountriesByCurrencyResponse {
    total_available_count: number;
}

export interface CountCurrencyCountriesByCountryRequest {
    country_alpha2_code: string;
}

export interface CountCurrencyCountriesByCountryResponse {
    total_available_count: number;
}

export interface CurrencyCountryView {
    currency_country: CurrencyCountry;
}

export const subjects = {
    get_currency_countries_request: "refdata.v1.currency_countries.list",
    get_currency_countries_by_currency_request: "refdata.v1.currency_countries.list_by_currency_iso_code",
    save_currency_country_request: "refdata.v1.currency_countries.save",
    delete_currency_country_request: "refdata.v1.currency_countries.delete",
    count_currency_countries_by_currency_request: "refdata.v1.currency_countries.count_by_currency_iso_code",
    count_currency_countries_by_country_request: "refdata.v1.currency_countries.count_by_country_alpha2_code",
} as const;
