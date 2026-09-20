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
import type { RoundingType } from '../domain/rounding_type.js';

export interface GetRoundingTypesRequest {
    offset: number;
    limit: number;
}

export interface GetRoundingTypesResponse {
    types: RoundingType[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveRoundingTypeRequest {
    data: RoundingType;
}

export interface SaveRoundingTypeResponse {
    success: boolean;
    message: string;
}

export interface DeleteRoundingTypeRequest {
    codes: string[];
}

export interface DeleteRoundingTypeResponse {
    success: boolean;
    message: string;
}

export interface GetRoundingTypeHistoryRequest {
    code: string;
}

export interface GetRoundingTypeHistoryResponse {
    history: RoundingType[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_rounding_types_request: "refdata.v1.rounding_types.list",
    save_rounding_type_request: "refdata.v1.rounding_types.save",
    delete_rounding_type_request: "refdata.v1.rounding_types.delete",
    get_rounding_type_history_request: "refdata.v1.rounding_types.history",
} as const;
