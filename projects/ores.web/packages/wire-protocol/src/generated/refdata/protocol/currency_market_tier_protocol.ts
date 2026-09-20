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
import type { CurrencyMarketTier } from '../domain/currency_market_tier.js';

export interface GetCurrencyMarketTiersRequest {
    offset: number;
    limit: number;
}

export interface GetCurrencyMarketTiersResponse {
    types: CurrencyMarketTier[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveCurrencyMarketTierRequest {
    data: CurrencyMarketTier;
}

export interface SaveCurrencyMarketTierResponse {
    success: boolean;
    message: string;
}

export interface DeleteCurrencyMarketTierRequest {
    codes: string[];
}

export interface DeleteCurrencyMarketTierResponse {
    success: boolean;
    message: string;
}

export interface GetCurrencyMarketTierHistoryRequest {
    code: string;
}

export interface GetCurrencyMarketTierHistoryResponse {
    history: CurrencyMarketTier[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_currency_market_tiers_request: "refdata.v1.currency_market_tiers.list",
    save_currency_market_tier_request: "refdata.v1.currency_market_tiers.save",
    delete_currency_market_tier_request: "refdata.v1.currency_market_tiers.delete",
    get_currency_market_tier_history_request: "refdata.v1.currency_market_tiers.history",
} as const;
