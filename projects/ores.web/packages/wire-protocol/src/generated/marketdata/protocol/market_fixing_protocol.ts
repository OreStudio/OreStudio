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
import type { MarketFixing } from '../domain/market_fixing.js';

export interface GetMarketFixingsRequest {
    offset: number;
    limit: number;
}

export interface GetMarketFixingsResponse {
    market_fixings: MarketFixing[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveMarketFixingRequest {
    data: MarketFixing;
}

export interface SaveMarketFixingResponse {
    success: boolean;
    message: string;
}

export interface DeleteMarketFixingRequest {
    ids: string[];
}

export interface DeleteMarketFixingResponse {
    success: boolean;
    message: string;
}

export interface GetMarketFixingHistoryRequest {
    id: string;
}

export interface GetMarketFixingHistoryResponse {
    history: MarketFixing[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_market_fixings_request: "marketdata.v1.market_fixings.list",
    save_market_fixing_request: "marketdata.v1.market_fixings.save",
    delete_market_fixing_request: "marketdata.v1.market_fixings.delete",
    get_market_fixing_history_request: "marketdata.v1.market_fixings.history",
} as const;
