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
import type { Permission } from '../domain/permission.js';

export interface GetPermissionsRequest {
    offset: number;
    limit: number;
}

export interface GetPermissionsResponse {
    permissions: Permission[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SavePermissionRequest {
    data: Permission;
}

export interface SavePermissionResponse {
    success: boolean;
    message: string;
}

export interface DeletePermissionRequest {
    ids: string[];
}

export interface DeletePermissionResponse {
    success: boolean;
    message: string;
}

export interface GetPermissionHistoryRequest {
    id: string;
}

export interface GetPermissionHistoryResponse {
    history: Permission[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_permissions_request: "iam.v1.permissions.list",
    save_permission_request: "iam.v1.permissions.save",
    delete_permission_request: "iam.v1.permissions.delete",
    get_permission_history_request: "iam.v1.permissions.history",
} as const;
