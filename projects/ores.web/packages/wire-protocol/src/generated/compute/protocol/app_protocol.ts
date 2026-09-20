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
import type { App } from '../domain/app.js';

export interface GetAppsRequest {
    offset: number;
    limit: number;
}

export interface GetAppsResponse {
    apps: App[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveAppRequest {
    data: App;
}

export interface SaveAppResponse {
    success: boolean;
    message: string;
}

export interface DeleteAppRequest {
    ids: string[];
}

export interface DeleteAppResponse {
    success: boolean;
    message: string;
}

export interface GetAppHistoryRequest {
    id: string;
}

export interface GetAppHistoryResponse {
    history: App[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_apps_request: "compute.v1.apps.list",
    save_app_request: "compute.v1.apps.save",
    delete_app_request: "compute.v1.apps.delete",
    get_app_history_request: "compute.v1.apps.history",
} as const;
