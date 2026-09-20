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
import type { AssetClassCode } from '../domain/asset_class_code.js';

export interface GetAssetClassCodesRequest {
    offset: number;
    limit: number;
}

export interface GetAssetClassCodesResponse {
    asset_classes: AssetClassCode[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveAssetClassCodeRequest {
    data: AssetClassCode;
}

export interface SaveAssetClassCodeResponse {
    success: boolean;
    message: string;
}

export interface DeleteAssetClassCodeRequest {
    codes: string[];
}

export interface DeleteAssetClassCodeResponse {
    success: boolean;
    message: string;
}

export interface GetAssetClassCodeHistoryRequest {
    code: string;
}

export interface GetAssetClassCodeHistoryResponse {
    history: AssetClassCode[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_asset_class_codes_request: "refdata.v1.asset_class_codes.list",
    save_asset_class_code_request: "refdata.v1.asset_class_codes.save",
    delete_asset_class_code_request: "refdata.v1.asset_class_codes.delete",
    get_asset_class_code_history_request: "refdata.v1.asset_class_codes.history",
} as const;
