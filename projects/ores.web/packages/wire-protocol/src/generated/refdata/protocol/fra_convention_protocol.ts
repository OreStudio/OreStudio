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
import type { FraConvention } from '../domain/fra_convention.js';

export interface GetFraConventionsRequest {
    offset: number;
    limit: number;
}

export interface GetFraConventionsResponse {
    fra_conventions: FraConvention[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveFraConventionRequest {
    data: FraConvention;
}

export interface SaveFraConventionResponse {
    success: boolean;
    message: string;
}

export interface DeleteFraConventionRequest {
    ids: string[];
}

export interface DeleteFraConventionResponse {
    success: boolean;
    message: string;
}

export interface GetFraConventionHistoryRequest {
    id: string;
}

export interface GetFraConventionHistoryResponse {
    history: FraConvention[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_fra_conventions_request: "refdata.v1.fra_conventions.list",
    save_fra_convention_request: "refdata.v1.fra_conventions.save",
    delete_fra_convention_request: "refdata.v1.fra_conventions.delete",
    get_fra_convention_history_request: "refdata.v1.fra_conventions.history",
} as const;
