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
import type { ZeroConvention } from '../domain/zero_convention.js';

export interface GetZeroConventionsRequest {
    offset: number;
    limit: number;
}

export interface GetZeroConventionsResponse {
    zero_conventions: ZeroConvention[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveZeroConventionRequest {
    data: ZeroConvention;
}

export interface SaveZeroConventionResponse {
    success: boolean;
    message: string;
}

export interface DeleteZeroConventionRequest {
    ids: string[];
}

export interface DeleteZeroConventionResponse {
    success: boolean;
    message: string;
}

export interface GetZeroConventionHistoryRequest {
    id: string;
}

export interface GetZeroConventionHistoryResponse {
    history: ZeroConvention[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_zero_conventions_request: "refdata.v1.zero_conventions.list",
    save_zero_convention_request: "refdata.v1.zero_conventions.save",
    delete_zero_convention_request: "refdata.v1.zero_conventions.delete",
    get_zero_convention_history_request: "refdata.v1.zero_conventions.history",
} as const;
