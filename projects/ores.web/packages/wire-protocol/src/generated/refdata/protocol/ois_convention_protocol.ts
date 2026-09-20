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
import type { OisConvention } from '../domain/ois_convention.js';

export interface GetOisConventionsRequest {
    offset: number;
    limit: number;
}

export interface GetOisConventionsResponse {
    ois_conventions: OisConvention[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveOisConventionRequest {
    data: OisConvention;
}

export interface SaveOisConventionResponse {
    success: boolean;
    message: string;
}

export interface DeleteOisConventionRequest {
    ids: string[];
}

export interface DeleteOisConventionResponse {
    success: boolean;
    message: string;
}

export interface GetOisConventionHistoryRequest {
    id: string;
}

export interface GetOisConventionHistoryResponse {
    history: OisConvention[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_ois_conventions_request: "refdata.v1.ois_conventions.list",
    save_ois_convention_request: "refdata.v1.ois_conventions.save",
    delete_ois_convention_request: "refdata.v1.ois_conventions.delete",
    get_ois_convention_history_request: "refdata.v1.ois_conventions.history",
} as const;
