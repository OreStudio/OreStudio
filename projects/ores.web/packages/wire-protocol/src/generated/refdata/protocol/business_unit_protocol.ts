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
import type { BusinessUnit } from '../domain/business_unit.js';
import type { HierarchyNode } from '../../../utility/hierarchy.js';

export interface GetBusinessUnitsRequest {
    offset: number;
    limit: number;
}

export interface GetBusinessUnitsResponse {
    business_units: BusinessUnit[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveBusinessUnitRequest {
    data: BusinessUnit;
}

export interface SaveBusinessUnitResponse {
    success: boolean;
    message: string;
}

export interface DeleteBusinessUnitRequest {
    ids: string[];
}

export interface DeleteBusinessUnitResponse {
    success: boolean;
    message: string;
}

export interface GetBusinessUnitHistoryRequest {
    id: string;
}

export interface GetBusinessUnitHistoryResponse {
    history: BusinessUnit[];
    success: boolean;
    message: string;
}

export interface GetBusinessUnitHierarchyRequest {
    root_id: string;
    from_root: boolean;
}

export interface GetBusinessUnitHierarchyResponse {
    success: boolean;
    message: string;
    roots: HierarchyNode[];
}

export const subjects = {
    get_business_units_request: "refdata.v1.business_units.list",
    save_business_unit_request: "refdata.v1.business_units.save",
    delete_business_unit_request: "refdata.v1.business_units.delete",
    get_business_unit_history_request: "refdata.v1.business_units.history",
    get_business_unit_hierarchy_request: "refdata.v1.business_units.hierarchy",
} as const;
