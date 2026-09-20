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
import type { IrCurveBootstrapPillar } from '../domain/ir_curve_bootstrap_pillar.js';

export interface GetIrCurveBootstrapPillarsRequest {
    offset: number;
    limit: number;
}

export interface GetIrCurveBootstrapPillarsResponse {
    pillars: IrCurveBootstrapPillar[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveIrCurveBootstrapPillarRequest {
    data: IrCurveBootstrapPillar;
}

export interface SaveIrCurveBootstrapPillarResponse {
    success: boolean;
    message: string;
}

export interface DeleteIrCurveBootstrapPillarRequest {
    ids: string[];
}

export interface DeleteIrCurveBootstrapPillarResponse {
    success: boolean;
    message: string;
}

export interface GetIrCurveBootstrapPillarHistoryRequest {
    id: string;
}

export interface GetIrCurveBootstrapPillarHistoryResponse {
    history: IrCurveBootstrapPillar[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_ir_curve_bootstrap_pillars_request: "refdata.v1.ir_curve_bootstrap_pillars.list",
    save_ir_curve_bootstrap_pillar_request: "refdata.v1.ir_curve_bootstrap_pillars.save",
    delete_ir_curve_bootstrap_pillar_request: "refdata.v1.ir_curve_bootstrap_pillars.delete",
    get_ir_curve_bootstrap_pillar_history_request: "refdata.v1.ir_curve_bootstrap_pillars.history",
} as const;
