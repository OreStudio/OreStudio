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
import type { RolePermission } from '../domain/role_permission.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface RolePermissionKey {
    role_id: string;
    permission_id: string;
}

export interface RolePermissionWrite {
    role_id: string;
    permission_id: string;
}

export interface RolePermissionChange {
    write: RolePermissionWrite;
    precondition: Precondition;
}

export interface RolePermissionRemoval {
    key: RolePermissionKey;
    precondition: Precondition;
}

export interface RolePermissionLookup {
    key: RolePermissionKey;
    role_permission: RolePermission | null;
}

export interface RolePermissionsFilter {
    role_id: string | null;
    permission_id: string | null;
    role_id_one_of: string[] | null;
    permission_id_one_of: string[] | null;
}

export interface ListRolePermissionsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: RolePermissionsFilter | null;
}

export interface ListRolePermissionsResponse {
    result: Result;
    role_permissions: RolePermission[];
    total: number;
}

export interface GetRolePermissionRequest {
    key: RolePermissionKey;
}

export interface GetRolePermissionResponse {
    result: Result;
    role_permission: RolePermission | null;
}

export interface GetManyRolePermissionsRequest {
    keys: RolePermissionKey[];
}

export interface GetManyRolePermissionsResponse {
    result: Result;
    entries: RolePermissionLookup[];
}

export interface PutRolePermissionRequest {
    change: RolePermissionChange;
    intent: ChangeIntent;
}

export interface PutRolePermissionResponse {
    result: Result;
    role_permission: RolePermission | null;
}

export interface PutManyRolePermissionsRequest {
    changes: RolePermissionChange[];
    intent: ChangeIntent;
}

export interface PutManyRolePermissionsResponse {
    result: Result;
    role_permissions: RolePermission[];
}

export interface DeleteRolePermissionRequest {
    removal: RolePermissionRemoval;
    intent: ChangeIntent;
}

export interface DeleteRolePermissionResponse {
    result: Result;
}

export interface DeleteManyRolePermissionsRequest {
    removals: RolePermissionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyRolePermissionsResponse {
    result: Result;
}

export interface ListByRoleIdRolePermissionsRequest {
    role_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: RolePermissionsFilter | null;
}

export interface ListByRoleIdRolePermissionsResponse {
    result: Result;
    role_permissions: RolePermission[];
    total: number;
}

export interface ListByPermissionIdRolePermissionsRequest {
    permission_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: RolePermissionsFilter | null;
}

export interface ListByPermissionIdRolePermissionsResponse {
    result: Result;
    role_permissions: RolePermission[];
    total: number;
}

export const subjects = {
    list_role_permissions_request: 'iam.v1.role_permissions.list',
    get_role_permission_request: 'iam.v1.role_permissions.get',
    get_many_role_permissions_request: 'iam.v1.role_permissions.get_many',
    put_role_permission_request: 'iam.v1.role_permissions.put',
    put_many_role_permissions_request: 'iam.v1.role_permissions.put_many',
    delete_role_permission_request: 'iam.v1.role_permissions.delete',
    delete_many_role_permissions_request: 'iam.v1.role_permissions.delete_many',
    list_by_role_id_role_permissions_request: 'iam.v1.role_permissions.list_by_role_id',
    list_by_permission_id_role_permissions_request: 'iam.v1.role_permissions.list_by_permission_id',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_role_permissions_request: true,
    get_role_permission_request: true,
    get_many_role_permissions_request: true,
    put_role_permission_request: true,
    put_many_role_permissions_request: true,
    delete_role_permission_request: true,
    delete_many_role_permissions_request: true,
    list_by_role_id_role_permissions_request: true,
    list_by_permission_id_role_permissions_request: true,
} as const;
