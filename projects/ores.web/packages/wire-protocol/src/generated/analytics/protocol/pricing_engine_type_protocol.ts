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
import type { PricingEngineType } from '../domain/pricing_engine_type.js';

export interface GetPricingEngineTypesRequest {
    offset: number;
    limit: number;
}

export interface GetPricingEngineTypesResponse {
    types: PricingEngineType[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SavePricingEngineTypeRequest {
    data: PricingEngineType;
}

export interface SavePricingEngineTypeResponse {
    success: boolean;
    message: string;
}

export interface DeletePricingEngineTypeRequest {
    codes: string[];
}

export interface DeletePricingEngineTypeResponse {
    success: boolean;
    message: string;
}

export interface GetPricingEngineTypeHistoryRequest {
    code: string;
}

export interface GetPricingEngineTypeHistoryResponse {
    history: PricingEngineType[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_pricing_engine_types_request: "analytics.v1.pricing_engine_types.list",
    save_pricing_engine_type_request: "analytics.v1.pricing_engine_types.save",
    delete_pricing_engine_type_request: "analytics.v1.pricing_engine_types.delete",
    get_pricing_engine_type_history_request: "analytics.v1.pricing_engine_types.history",
} as const;
