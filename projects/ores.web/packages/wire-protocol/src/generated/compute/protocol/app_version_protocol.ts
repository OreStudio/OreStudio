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
import type { AppVersion } from '../domain/app_version.js';

export interface GetAppVersionsRequest {
    offset: number;
    limit: number;
}

export interface GetAppVersionsResponse {
    app_versions: AppVersion[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveAppVersionRequest {
    data: AppVersion;
}

export interface SaveAppVersionResponse {
    success: boolean;
    message: string;
}

export interface DeleteAppVersionRequest {
    ids: string[];
}

export interface DeleteAppVersionResponse {
    success: boolean;
    message: string;
}

export interface GetAppVersionHistoryRequest {
    id: string;
}

export interface GetAppVersionHistoryResponse {
    history: AppVersion[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_app_versions_request: "compute.v1.app_versions.list",
    save_app_version_request: "compute.v1.app_versions.save",
    delete_app_version_request: "compute.v1.app_versions.delete",
    get_app_version_history_request: "compute.v1.app_versions.history",
} as const;
