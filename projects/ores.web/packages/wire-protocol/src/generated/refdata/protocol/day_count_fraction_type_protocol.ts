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
import type { DayCountFractionType } from '../domain/day_count_fraction_type.js';

export interface GetDayCountFractionTypesRequest {
    offset: number;
    limit: number;
}

export interface GetDayCountFractionTypesResponse {
    types: DayCountFractionType[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveDayCountFractionTypeRequest {
    data: DayCountFractionType;
}

export interface SaveDayCountFractionTypeResponse {
    success: boolean;
    message: string;
}

export interface DeleteDayCountFractionTypeRequest {
    codes: string[];
}

export interface DeleteDayCountFractionTypeResponse {
    success: boolean;
    message: string;
}

export interface GetDayCountFractionTypeHistoryRequest {
    code: string;
}

export interface GetDayCountFractionTypeHistoryResponse {
    history: DayCountFractionType[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_day_count_fraction_types_request: "refdata.v1.day_count_fraction_types.list",
    save_day_count_fraction_type_request: "refdata.v1.day_count_fraction_types.save",
    delete_day_count_fraction_type_request: "refdata.v1.day_count_fraction_types.delete",
    get_day_count_fraction_type_history_request: "refdata.v1.day_count_fraction_types.history",
} as const;
