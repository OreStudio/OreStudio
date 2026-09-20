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
import type { ContactType } from '../domain/contact_type.js';

export interface GetContactTypesRequest {
    offset: number;
    limit: number;
}

export interface GetContactTypesResponse {
    types: ContactType[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveContactTypeRequest {
    data: ContactType;
}

export interface SaveContactTypeResponse {
    success: boolean;
    message: string;
}

export interface DeleteContactTypeRequest {
    codes: string[];
}

export interface DeleteContactTypeResponse {
    success: boolean;
    message: string;
}

export interface GetContactTypeHistoryRequest {
    code: string;
}

export interface GetContactTypeHistoryResponse {
    history: ContactType[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_contact_types_request: "refdata.v1.contact_types.list",
    save_contact_type_request: "refdata.v1.contact_types.save",
    delete_contact_type_request: "refdata.v1.contact_types.delete",
    get_contact_type_history_request: "refdata.v1.contact_types.history",
} as const;
