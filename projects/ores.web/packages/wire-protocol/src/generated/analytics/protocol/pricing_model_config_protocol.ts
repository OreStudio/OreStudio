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
import type { PricingModelConfig } from '../domain/pricing_model_config.js';

export interface GetPricingModelConfigsRequest {
    offset: number;
    limit: number;
}

export interface GetPricingModelConfigsResponse {
    configs: PricingModelConfig[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SavePricingModelConfigRequest {
    data: PricingModelConfig;
}

export interface SavePricingModelConfigResponse {
    success: boolean;
    message: string;
}

export interface DeletePricingModelConfigRequest {
    ids: string[];
}

export interface DeletePricingModelConfigResponse {
    success: boolean;
    message: string;
}

export interface GetPricingModelConfigHistoryRequest {
    id: string;
}

export interface GetPricingModelConfigHistoryResponse {
    history: PricingModelConfig[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_pricing_model_configs_request: "analytics.v1.pricing_model_configs.list",
    save_pricing_model_config_request: "analytics.v1.pricing_model_configs.save",
    delete_pricing_model_config_request: "analytics.v1.pricing_model_configs.delete",
    get_pricing_model_config_history_request: "analytics.v1.pricing_model_configs.history",
} as const;
