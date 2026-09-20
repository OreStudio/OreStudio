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
import type { MarketObservation } from '../domain/market_observation.js';

export interface GetMarketObservationsRequest {
    offset: number;
    limit: number;
}

export interface GetMarketObservationsResponse {
    market_observations: MarketObservation[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveMarketObservationRequest {
    data: MarketObservation;
}

export interface SaveMarketObservationResponse {
    success: boolean;
    message: string;
}

export interface DeleteMarketObservationRequest {
    ids: string[];
}

export interface DeleteMarketObservationResponse {
    success: boolean;
    message: string;
}

export interface GetMarketObservationHistoryRequest {
    id: string;
}

export interface GetMarketObservationHistoryResponse {
    history: MarketObservation[];
    success: boolean;
    message: string;
}

export interface GetMarketObservationsBySeriesIdRequest {
    series_id: string;
    offset: number;
    limit: number;
}

export interface GetMarketObservationsBySeriesIdResponse {
    market_observations: MarketObservation[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export const subjects = {
    get_market_observations_request: "marketdata.v1.market_observations.list",
    save_market_observation_request: "marketdata.v1.market_observations.save",
    delete_market_observation_request: "marketdata.v1.market_observations.delete",
    get_market_observation_history_request: "marketdata.v1.market_observations.history",
    get_market_observations_by_series_id_request: "marketdata.v1.market_observations.list_by_series_id",
} as const;
