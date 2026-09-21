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
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface PermissionKey {
    id: string;
}

export interface PermissionWrite {
    id: string;
    code: string;
    description: string;
}

export interface PermissionChange {
    write: PermissionWrite;
    precondition: Precondition;
}

export interface PermissionRemoval {
    key: PermissionKey;
    precondition: Precondition;
}

export interface PermissionLookup {
    key: PermissionKey;
    permission: Permission | null;
}

export interface PermissionEvent {
    event_id: string;
    key: PermissionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListPermissionsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListPermissionsResponse {
    result: Result;
    permissions: Permission[];
    total: number;
}

export interface GetPermissionRequest {
    key: PermissionKey;
}

export interface GetPermissionResponse {
    result: Result;
    permission: Permission | null;
}

export interface GetManyPermissionsRequest {
    keys: PermissionKey[];
}

export interface GetManyPermissionsResponse {
    result: Result;
    entries: PermissionLookup[];
}

export interface PutPermissionRequest {
    change: PermissionChange;
    intent: ChangeIntent;
}

export interface PutPermissionResponse {
    result: Result;
    permission: Permission;
}

export interface PutManyPermissionsRequest {
    changes: PermissionChange[];
    intent: ChangeIntent;
}

export interface PutManyPermissionsResponse {
    result: Result;
    permissions: Permission[];
}

export interface DeletePermissionRequest {
    removal: PermissionRemoval;
    intent: ChangeIntent;
}

export interface DeletePermissionResponse {
    result: Result;
}

export interface DeleteManyPermissionsRequest {
    removals: PermissionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyPermissionsResponse {
    result: Result;
}

export const subjects = {
    list_permissions_request: "iam.v1.permissions.list",
    get_permission_request: "iam.v1.permissions.get",
    get_many_permissions_request: "iam.v1.permissions.get_many",
    put_permission_request: "iam.v1.permissions.put",
    put_many_permissions_request: "iam.v1.permissions.put_many",
    delete_permission_request: "iam.v1.permissions.delete",
    delete_many_permissions_request: "iam.v1.permissions.delete_many",
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "iam.v1.permissions_events.created",
    updated: "iam.v1.permissions_events.updated",
    deleted: "iam.v1.permissions_events.deleted",
} as const;
