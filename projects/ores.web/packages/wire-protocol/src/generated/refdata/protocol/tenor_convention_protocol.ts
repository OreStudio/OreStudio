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
import type { TenorConvention } from '../domain/tenor_convention.js';

export interface GetTenorConventionsRequest {
    offset: number;
    limit: number;
}

export interface GetTenorConventionsResponse {
    conventions: TenorConvention[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveTenorConventionRequest {
    data: TenorConvention;
}

export interface SaveTenorConventionResponse {
    success: boolean;
    message: string;
}

export interface DeleteTenorConventionRequest {
    codes: string[];
}

export interface DeleteTenorConventionResponse {
    success: boolean;
    message: string;
}

export interface GetTenorConventionHistoryRequest {
    code: string;
}

export interface GetTenorConventionHistoryResponse {
    history: TenorConvention[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_tenor_conventions_request: "refdata.v1.tenor_conventions.list",
    save_tenor_convention_request: "refdata.v1.tenor_conventions.save",
    delete_tenor_convention_request: "refdata.v1.tenor_conventions.delete",
    get_tenor_convention_history_request: "refdata.v1.tenor_conventions.history",
} as const;
