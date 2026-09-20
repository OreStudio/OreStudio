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
import type { SyntheticFxSpotConfig } from '../domain/synthetic_fx_spot_config.js';

export interface GetSyntheticFxSpotConfigsRequest {
    offset: number;
    limit: number;
}

export interface GetSyntheticFxSpotConfigsResponse {
    configs: SyntheticFxSpotConfig[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveSyntheticFxSpotConfigRequest {
    data: SyntheticFxSpotConfig;
}

export interface SaveSyntheticFxSpotConfigResponse {
    success: boolean;
    message: string;
}

export interface DeleteSyntheticFxSpotConfigRequest {
    ids: string[];
}

export interface DeleteSyntheticFxSpotConfigResponse {
    success: boolean;
    message: string;
}

export interface GetSyntheticFxSpotConfigHistoryRequest {
    id: string;
}

export interface GetSyntheticFxSpotConfigHistoryResponse {
    history: SyntheticFxSpotConfig[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_synthetic_fx_spot_configs_request: "dq.v1.synthetic_fx_spot_configs.list",
    save_synthetic_fx_spot_config_request: "dq.v1.synthetic_fx_spot_configs.save",
    delete_synthetic_fx_spot_config_request: "dq.v1.synthetic_fx_spot_configs.delete",
    get_synthetic_fx_spot_config_history_request: "dq.v1.synthetic_fx_spot_configs.history",
} as const;
