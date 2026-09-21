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
import type { Role } from '../domain/role.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface RoleKey {
    id: string;
}

export interface RoleWrite {
    id: string;
    name: string;
    description: string;
}

export interface RoleChange {
    write: RoleWrite;
    precondition: Precondition;
}

export interface RoleRemoval {
    key: RoleKey;
    precondition: Precondition;
}

export interface RoleLookup {
    key: RoleKey;
    role: Role | null;
}

export interface RoleVersionKey {
    role: RoleKey;
    version: number;
}

export interface RoleVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListRolesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListRolesResponse {
    result: Result;
    roles: Role[];
    total: number;
}

export interface GetRoleRequest {
    key: RoleKey;
}

export interface GetRoleResponse {
    result: Result;
    role: Role | null;
}

export interface GetManyRolesRequest {
    keys: RoleKey[];
}

export interface GetManyRolesResponse {
    result: Result;
    entries: RoleLookup[];
}

export interface PutRoleRequest {
    change: RoleChange;
    intent: ChangeIntent;
}

export interface PutRoleResponse {
    result: Result;
    role: Role;
}

export interface PutManyRolesRequest {
    changes: RoleChange[];
    intent: ChangeIntent;
}

export interface PutManyRolesResponse {
    result: Result;
    roles: Role[];
}

export interface DeleteRoleRequest {
    removal: RoleRemoval;
    intent: ChangeIntent;
}

export interface DeleteRoleResponse {
    result: Result;
}

export interface DeleteManyRolesRequest {
    removals: RoleRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyRolesResponse {
    result: Result;
}

export interface ListRoleVersionsRequest {
    key: RoleKey;
    offset: number;
    limit: number;
    order: Order;
    filter: RoleVersionsFilter | null;
}

export interface ListRoleVersionsResponse {
    result: Result;
    versions: Role[];
    total: number;
}

export interface GetRoleVersionRequest {
    key: RoleVersionKey;
}

export interface GetRoleVersionResponse {
    result: Result;
    version: Role;
}

export const subjects = {
    list_roles_request: "iam.v1.roles.list",
    get_role_request: "iam.v1.roles.get",
    get_many_roles_request: "iam.v1.roles.get_many",
    put_role_request: "iam.v1.roles.put",
    put_many_roles_request: "iam.v1.roles.put_many",
    delete_role_request: "iam.v1.roles.delete",
    delete_many_roles_request: "iam.v1.roles.delete_many",
    list_role_versions_request: "iam.v1.roles_versions.list",
    get_role_version_request: "iam.v1.roles_versions.get",
} as const;
