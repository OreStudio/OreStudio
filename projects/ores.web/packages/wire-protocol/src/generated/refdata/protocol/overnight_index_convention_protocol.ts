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
import type { OvernightIndexConvention } from '../domain/overnight_index_convention.js';

export interface GetOvernightIndexConventionsRequest {
    offset: number;
    limit: number;
}

export interface GetOvernightIndexConventionsResponse {
    overnight_index_conventions: OvernightIndexConvention[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveOvernightIndexConventionRequest {
    data: OvernightIndexConvention;
}

export interface SaveOvernightIndexConventionResponse {
    success: boolean;
    message: string;
}

export interface DeleteOvernightIndexConventionRequest {
    ids: string[];
}

export interface DeleteOvernightIndexConventionResponse {
    success: boolean;
    message: string;
}

export interface GetOvernightIndexConventionHistoryRequest {
    id: string;
}

export interface GetOvernightIndexConventionHistoryResponse {
    history: OvernightIndexConvention[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_overnight_index_conventions_request: "refdata.v1.overnight_index_conventions.list",
    save_overnight_index_convention_request: "refdata.v1.overnight_index_conventions.save",
    delete_overnight_index_convention_request: "refdata.v1.overnight_index_conventions.delete",
    get_overnight_index_convention_history_request: "refdata.v1.overnight_index_conventions.history",
} as const;
