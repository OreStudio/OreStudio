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
import type { RegulatoryBookType } from '../domain/regulatory_book_type.js';

export interface GetRegulatoryBookTypesRequest {
    offset: number;
    limit: number;
    // Empty = current/latest. Note: when as_of is set, results are not
    // paginated by offset/limit -- all matching rows are returned.
    as_of: string;
}

export interface GetRegulatoryBookTypesResponse {
    types: RegulatoryBookType[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveRegulatoryBookTypeRequest {
    data: RegulatoryBookType;
}

export interface SaveRegulatoryBookTypeResponse {
    success: boolean;
    message: string;
}

export interface DeleteRegulatoryBookTypeRequest {
    codes: string[];
}

export interface DeleteRegulatoryBookTypeResponse {
    success: boolean;
    message: string;
}

export interface GetRegulatoryBookTypeHistoryRequest {
    code: string;
}

export interface GetRegulatoryBookTypeHistoryResponse {
    history: RegulatoryBookType[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_regulatory_book_types_request: "refdata.v1.regulatory_book_types.list",
    save_regulatory_book_type_request: "refdata.v1.regulatory_book_types.save",
    delete_regulatory_book_type_request: "refdata.v1.regulatory_book_types.delete",
    get_regulatory_book_type_history_request: "refdata.v1.regulatory_book_types.history",
} as const;
