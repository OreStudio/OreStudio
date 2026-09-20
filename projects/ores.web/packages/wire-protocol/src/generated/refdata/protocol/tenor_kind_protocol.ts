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
import type { TenorKind } from '../domain/tenor_kind.js';

export interface GetTenorKindsRequest {
    offset: number;
    limit: number;
}

export interface GetTenorKindsResponse {
    kinds: TenorKind[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveTenorKindRequest {
    data: TenorKind;
}

export interface SaveTenorKindResponse {
    success: boolean;
    message: string;
}

export interface DeleteTenorKindRequest {
    codes: string[];
}

export interface DeleteTenorKindResponse {
    success: boolean;
    message: string;
}

export interface GetTenorKindHistoryRequest {
    code: string;
}

export interface GetTenorKindHistoryResponse {
    history: TenorKind[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_tenor_kinds_request: "refdata.v1.tenor_kinds.list",
    save_tenor_kind_request: "refdata.v1.tenor_kinds.save",
    delete_tenor_kind_request: "refdata.v1.tenor_kinds.delete",
    get_tenor_kind_history_request: "refdata.v1.tenor_kinds.history",
} as const;
