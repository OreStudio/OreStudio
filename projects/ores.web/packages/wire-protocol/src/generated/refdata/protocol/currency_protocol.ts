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

export interface GetCurrenciesRequest {
    offset: number;
    limit: number;
    // Empty = current/latest. Note: when as_of is set, results are not
    // paginated by offset/limit -- all matching rows are returned.
    as_of: string;
}

export interface GetCurrenciesResponse {
    currencies: Currency[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveCurrencyRequest {
    data: Currency;
}

export interface SaveCurrencyResponse {
    success: boolean;
    message: string;
}

export interface DeleteCurrencyRequest {
    iso_codes: string[];
}

export interface DeleteCurrencyResponse {
    success: boolean;
    message: string;
}

export interface GetCurrencyHistoryRequest {
    iso_code: string;
}

export interface GetCurrencyHistoryResponse {
    history: Currency[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_currencies_request: "refdata.v1.currencies.list",
    save_currency_request: "refdata.v1.currencies.save",
    delete_currency_request: "refdata.v1.currencies.delete",
    get_currency_history_request: "refdata.v1.currencies.history",
} as const;
