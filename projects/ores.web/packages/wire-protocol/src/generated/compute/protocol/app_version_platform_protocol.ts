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
import type { AppVersionPlatform } from '../domain/app_version_platform.js';

export interface GetAppVersionPlatformsRequest {
    offset: number;
    limit: number;
}

export interface GetAppVersionPlatformsResponse {
    app_version_platforms: AppVersionPlatform[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface GetAppVersionPlatformsByAppVersionRequest {
    app_version_id: string;
    offset: number;
    limit: number;
}

export interface GetAppVersionPlatformsByAppVersionResponse {
    app_version_platforms: AppVersionPlatformView[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveAppVersionPlatformRequest {
    app_version_platforms: AppVersionPlatform[];
}

export interface SaveAppVersionPlatformResponse {
    success: boolean;
    message: string;
}

export interface DeleteAppVersionPlatformRequest {
    app_version_ids: string[];
    platform_ids: string[];
}

export interface DeleteAppVersionPlatformResponse {
    success: boolean;
    message: string;
}

export interface ReplaceAppVersionPlatformsByAppVersionRequest {
    app_version_id: string;
    app_version_platforms: AppVersionPlatform[];
    modified_by: string;
    performed_by: string;
    change_reason_code: string;
    change_commentary: string;
}

export interface ReplaceAppVersionPlatformsByAppVersionResponse {
    success: boolean;
    message: string;
}

export interface CountAppVersionPlatformsByAppVersionRequest {
    app_version_id: string;
}

export interface CountAppVersionPlatformsByAppVersionResponse {
    total_available_count: number;
}

export interface CountAppVersionPlatformsByPlatformRequest {
    platform_id: string;
}

export interface CountAppVersionPlatformsByPlatformResponse {
    total_available_count: number;
}

export interface AppVersionPlatformView {
    app_version_platform: AppVersionPlatform;
    platform_code: string;
}

export const subjects = {
    get_app_version_platforms_request: "compute.v1.app_version_platforms.list",
    get_app_version_platforms_by_app_version_request: "compute.v1.app_version_platforms.list_by_app_version_id",
    save_app_version_platform_request: "compute.v1.app_version_platforms.save",
    delete_app_version_platform_request: "compute.v1.app_version_platforms.delete",
    replace_app_version_platforms_by_app_version_request: "compute.v1.app_version_platforms.replace_by_app_version_id",
    count_app_version_platforms_by_app_version_request: "compute.v1.app_version_platforms.count_by_app_version_id",
    count_app_version_platforms_by_platform_request: "compute.v1.app_version_platforms.count_by_platform_id",
} as const;
