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
/**
 * @brief An asset class entry from the refdata catalogue.
 *
 * Carries the code, human-readable description and coding scheme so
 * callers can build display labels and filter values without
 * hard-coding them.
 */
export interface AssetClassInfo {
    code: string;
    description: string;
    coding_scheme_code: string;
}

/**
 * @brief Request published asset class entries from the refdata service.
 *
 * An optional coding_scheme_code filter narrows the result to a single
 * coding scheme. If empty, all schemes are returned.
 */
export interface GetAssetClassesRequest {
    coding_scheme_code: string;
    offset: number;
    limit: number;
}

export interface GetAssetClassesResponse {
    asset_classes: AssetClassInfo[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export const subjects = {
    get_asset_classes_request: "refdata.v1.asset-classes.list",
} as const;
