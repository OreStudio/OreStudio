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
import type { TenorUnit } from '../domain/tenor_unit.js';

export interface GetTenorUnitsRequest {
    offset: number;
    limit: number;
}

export interface GetTenorUnitsResponse {
    units: TenorUnit[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveTenorUnitRequest {
    data: TenorUnit;
}

export interface SaveTenorUnitResponse {
    success: boolean;
    message: string;
}

export interface DeleteTenorUnitRequest {
    codes: string[];
}

export interface DeleteTenorUnitResponse {
    success: boolean;
    message: string;
}

export interface GetTenorUnitHistoryRequest {
    code: string;
}

export interface GetTenorUnitHistoryResponse {
    history: TenorUnit[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_tenor_units_request: "refdata.v1.tenor_units.list",
    save_tenor_unit_request: "refdata.v1.tenor_units.save",
    delete_tenor_unit_request: "refdata.v1.tenor_units.delete",
    get_tenor_unit_history_request: "refdata.v1.tenor_units.history",
} as const;
