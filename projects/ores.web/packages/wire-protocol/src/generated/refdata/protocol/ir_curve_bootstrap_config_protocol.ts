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
import type { IrCurveBootstrapConfig } from '../domain/ir_curve_bootstrap_config.js';

export interface GetIrCurveBootstrapConfigsRequest {
    offset: number;
    limit: number;
}

export interface GetIrCurveBootstrapConfigsResponse {
    bootstrap_configs: IrCurveBootstrapConfig[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveIrCurveBootstrapConfigRequest {
    data: IrCurveBootstrapConfig;
}

export interface SaveIrCurveBootstrapConfigResponse {
    success: boolean;
    message: string;
}

export interface DeleteIrCurveBootstrapConfigRequest {
    ids: string[];
}

export interface DeleteIrCurveBootstrapConfigResponse {
    success: boolean;
    message: string;
}

export interface GetIrCurveBootstrapConfigHistoryRequest {
    id: string;
}

export interface GetIrCurveBootstrapConfigHistoryResponse {
    history: IrCurveBootstrapConfig[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_ir_curve_bootstrap_configs_request: "refdata.v1.ir_curve_bootstrap_configs.list",
    save_ir_curve_bootstrap_config_request: "refdata.v1.ir_curve_bootstrap_configs.save",
    delete_ir_curve_bootstrap_config_request: "refdata.v1.ir_curve_bootstrap_configs.delete",
    get_ir_curve_bootstrap_config_history_request: "refdata.v1.ir_curve_bootstrap_configs.history",
} as const;
