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
import type { PartyCurrency } from '../domain/party_currency.js';

export interface GetPartyCurrenciesRequest {
    offset: number;
    limit: number;
}

export interface GetPartyCurrenciesResponse {
    party_currencies: PartyCurrency[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface GetPartyCurrenciesByPartyRequest {
    party_id: string;
    offset: number;
    limit: number;
}

export interface GetPartyCurrenciesByPartyResponse {
    party_currencies: PartyCurrencyView[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SavePartyCurrencyRequest {
    party_currencies: PartyCurrency[];
}

export interface SavePartyCurrencyResponse {
    success: boolean;
    message: string;
}

export interface DeletePartyCurrencyRequest {
    party_ids: string[];
    currency_iso_codes: string[];
}

export interface DeletePartyCurrencyResponse {
    success: boolean;
    message: string;
}

export interface CountPartyCurrenciesByPartyRequest {
    party_id: string;
}

export interface CountPartyCurrenciesByPartyResponse {
    total_available_count: number;
}

export interface CountPartyCurrenciesByCurrencyRequest {
    currency_iso_code: string;
}

export interface CountPartyCurrenciesByCurrencyResponse {
    total_available_count: number;
}

export interface PartyCurrencyView {
    party_currency: PartyCurrency;
}

export const subjects = {
    get_party_currencies_request: "refdata.v1.party_currencies.list",
    get_party_currencies_by_party_request: "refdata.v1.party_currencies.list_by_party_id",
    save_party_currency_request: "refdata.v1.party_currencies.save",
    delete_party_currency_request: "refdata.v1.party_currencies.delete",
    count_party_currencies_by_party_request: "refdata.v1.party_currencies.count_by_party_id",
    count_party_currencies_by_currency_request: "refdata.v1.party_currencies.count_by_currency_iso_code",
} as const;
