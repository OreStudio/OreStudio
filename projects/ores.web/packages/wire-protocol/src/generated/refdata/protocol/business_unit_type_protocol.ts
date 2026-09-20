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
import type { BusinessUnitType } from '../domain/business_unit_type.js';

export interface GetBusinessUnitTypesRequest {
    offset: number;
    limit: number;
}

export interface GetBusinessUnitTypesResponse {
    types: BusinessUnitType[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveBusinessUnitTypeRequest {
    data: BusinessUnitType;
}

export interface SaveBusinessUnitTypeResponse {
    success: boolean;
    message: string;
}

export interface DeleteBusinessUnitTypeRequest {
    ids: string[];
}

export interface DeleteBusinessUnitTypeResponse {
    success: boolean;
    message: string;
}

export interface GetBusinessUnitTypeHistoryRequest {
    id: string;
}

export interface GetBusinessUnitTypeHistoryResponse {
    history: BusinessUnitType[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_business_unit_types_request: "refdata.v1.business_unit_types.list",
    save_business_unit_type_request: "refdata.v1.business_unit_types.save",
    delete_business_unit_type_request: "refdata.v1.business_unit_types.delete",
    get_business_unit_type_history_request: "refdata.v1.business_unit_types.history",
} as const;
