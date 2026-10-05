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
import type { RoleGrantRequestRole } from '../domain/role_grant_request_role.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface RoleGrantRequestRoleKey {
    request_id: string;
    role_id: string;
}

export interface RoleGrantRequestRoleWrite {
    request_id: string;
    role_id: string;
    applied_at: string | null;
}

export interface RoleGrantRequestRoleChange {
    write: RoleGrantRequestRoleWrite;
    precondition: Precondition;
}

export interface RoleGrantRequestRoleRemoval {
    key: RoleGrantRequestRoleKey;
    precondition: Precondition;
}

export interface RoleGrantRequestRoleLookup {
    key: RoleGrantRequestRoleKey;
    role_grant_request_role: RoleGrantRequestRole | null;
}

export interface RoleGrantRequestRolesFilter {
    request_id: string | null;
    request_id_one_of: string[] | null;
}

export interface ListRoleGrantRequestRolesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: RoleGrantRequestRolesFilter | null;
}

export interface ListRoleGrantRequestRolesResponse {
    result: Result;
    role_grant_request_roles: RoleGrantRequestRole[];
    total: number;
}

export interface GetRoleGrantRequestRoleRequest {
    key: RoleGrantRequestRoleKey;
}

export interface GetRoleGrantRequestRoleResponse {
    result: Result;
    role_grant_request_role: RoleGrantRequestRole | null;
}

export interface GetManyRoleGrantRequestRolesRequest {
    keys: RoleGrantRequestRoleKey[];
}

export interface GetManyRoleGrantRequestRolesResponse {
    result: Result;
    entries: RoleGrantRequestRoleLookup[];
}

export interface PutRoleGrantRequestRoleRequest {
    change: RoleGrantRequestRoleChange;
    intent: ChangeIntent;
}

export interface PutRoleGrantRequestRoleResponse {
    result: Result;
    role_grant_request_role: RoleGrantRequestRole | null;
}

export interface PutManyRoleGrantRequestRolesRequest {
    changes: RoleGrantRequestRoleChange[];
    intent: ChangeIntent;
}

export interface PutManyRoleGrantRequestRolesResponse {
    result: Result;
    role_grant_request_roles: RoleGrantRequestRole[];
}

export interface DeleteRoleGrantRequestRoleRequest {
    removal: RoleGrantRequestRoleRemoval;
    intent: ChangeIntent;
}

export interface DeleteRoleGrantRequestRoleResponse {
    result: Result;
}

export interface DeleteManyRoleGrantRequestRolesRequest {
    removals: RoleGrantRequestRoleRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyRoleGrantRequestRolesResponse {
    result: Result;
}

export interface ListByRequestIdRoleGrantRequestRolesRequest {
    request_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: RoleGrantRequestRolesFilter | null;
}

export interface ListByRequestIdRoleGrantRequestRolesResponse {
    result: Result;
    role_grant_request_roles: RoleGrantRequestRole[];
    total: number;
}

export const subjects = {
    list_role_grant_request_roles_request: 'iam.v1.role_grant_request_roles.list',
    get_role_grant_request_role_request: 'iam.v1.role_grant_request_roles.get',
    get_many_role_grant_request_roles_request: 'iam.v1.role_grant_request_roles.get_many',
    put_role_grant_request_role_request: 'iam.v1.role_grant_request_roles.put',
    put_many_role_grant_request_roles_request: 'iam.v1.role_grant_request_roles.put_many',
    delete_role_grant_request_role_request: 'iam.v1.role_grant_request_roles.delete',
    delete_many_role_grant_request_roles_request: 'iam.v1.role_grant_request_roles.delete_many',
    list_by_request_id_role_grant_request_roles_request:
        'iam.v1.role_grant_request_roles.list_by_request_id',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_role_grant_request_roles_request: true,
    get_role_grant_request_role_request: true,
    get_many_role_grant_request_roles_request: true,
    put_role_grant_request_role_request: true,
    put_many_role_grant_request_roles_request: true,
    delete_role_grant_request_role_request: true,
    delete_many_role_grant_request_roles_request: true,
    list_by_request_id_role_grant_request_roles_request: true,
} as const;
