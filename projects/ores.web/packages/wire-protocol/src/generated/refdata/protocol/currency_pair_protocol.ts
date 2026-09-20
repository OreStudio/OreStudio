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
import type { CurrencyPair } from '../domain/currency_pair.js';

export interface GetCurrencyPairsRequest {
    offset: number;
    limit: number;
}

export interface GetCurrencyPairsResponse {
    pairs: CurrencyPair[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveCurrencyPairRequest {
    data: CurrencyPair;
}

export interface SaveCurrencyPairResponse {
    success: boolean;
    message: string;
}

export interface DeleteCurrencyPairRequest {
    pair_codes: string[];
}

export interface DeleteCurrencyPairResponse {
    success: boolean;
    message: string;
}

export interface GetCurrencyPairHistoryRequest {
    pair_code: string;
}

export interface GetCurrencyPairHistoryResponse {
    history: CurrencyPair[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_currency_pairs_request: "refdata.v1.currency_pairs.list",
    save_currency_pair_request: "refdata.v1.currency_pairs.save",
    delete_currency_pair_request: "refdata.v1.currency_pairs.delete",
    get_currency_pair_history_request: "refdata.v1.currency_pairs.history",
} as const;
