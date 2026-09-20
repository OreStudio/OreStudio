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
import type { IborIndexConvention } from '../domain/ibor_index_convention.js';

export interface GetIborIndexConventionsRequest {
    offset: number;
    limit: number;
}

export interface GetIborIndexConventionsResponse {
    ibor_index_conventions: IborIndexConvention[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveIborIndexConventionRequest {
    data: IborIndexConvention;
}

export interface SaveIborIndexConventionResponse {
    success: boolean;
    message: string;
}

export interface DeleteIborIndexConventionRequest {
    ids: string[];
}

export interface DeleteIborIndexConventionResponse {
    success: boolean;
    message: string;
}

export interface GetIborIndexConventionHistoryRequest {
    id: string;
}

export interface GetIborIndexConventionHistoryResponse {
    history: IborIndexConvention[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_ibor_index_conventions_request: "refdata.v1.ibor_index_conventions.list",
    save_ibor_index_convention_request: "refdata.v1.ibor_index_conventions.save",
    delete_ibor_index_convention_request: "refdata.v1.ibor_index_conventions.delete",
    get_ibor_index_convention_history_request: "refdata.v1.ibor_index_conventions.history",
} as const;
