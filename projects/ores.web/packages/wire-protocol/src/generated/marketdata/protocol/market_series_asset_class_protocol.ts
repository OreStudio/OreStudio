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
import type { MarketSeriesAssetClass } from '../domain/market_series_asset_class.js';

export interface GetMarketSeriesAssetClassesRequest {
    offset: number;
    limit: number;
}

export interface GetMarketSeriesAssetClassesResponse {
    market_series_asset_classes: MarketSeriesAssetClass[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveMarketSeriesAssetClassRequest {
    market_series_asset_classes: MarketSeriesAssetClass[];
}

export interface SaveMarketSeriesAssetClassResponse {
    success: boolean;
    message: string;
}

export interface DeleteMarketSeriesAssetClassRequest {
    market_series_ids: string[];
    asset_class_codes: string[];
}

export interface DeleteMarketSeriesAssetClassResponse {
    success: boolean;
    message: string;
}

export interface CountMarketSeriesAssetClassesBySeriesRequest {
    market_series_id: string;
}

export interface CountMarketSeriesAssetClassesBySeriesResponse {
    total_available_count: number;
}

export interface CountMarketSeriesAssetClassesByAssetClassRequest {
    asset_class_code: string;
}

export interface CountMarketSeriesAssetClassesByAssetClassResponse {
    total_available_count: number;
}

export interface MarketSeriesAssetClassView {
    market_series_asset_class: MarketSeriesAssetClass;
}

export const subjects = {
    get_market_series_asset_classes_request: "marketdata.v1.market_series_asset_classes.list",
    save_market_series_asset_class_request: "marketdata.v1.market_series_asset_classes.save",
    delete_market_series_asset_class_request: "marketdata.v1.market_series_asset_classes.delete",
    count_market_series_asset_classes_by_series_request: "marketdata.v1.market_series_asset_classes.count_by_market_series_id",
    count_market_series_asset_classes_by_asset_class_request: "marketdata.v1.market_series_asset_classes.count_by_asset_class_code",
} as const;
