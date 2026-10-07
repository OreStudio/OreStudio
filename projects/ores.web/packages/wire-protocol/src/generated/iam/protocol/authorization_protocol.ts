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
import type { Result } from '../../../utility/protocol.js';

export interface AssignRoleRequest {
    account_id: string;
    role_id: string;
    change_reason_code: string;
    change_commentary: string;
}

export interface AssignRoleResponse {
    success: boolean;
    error_message: string;
}

export interface AssignRoleByNameResponse {
    success: boolean;
    error_message: string;
}

export interface AssignRoleByNameRequest {
    principal: string;
    role_name: string;
}

export interface RevokeRoleRequest {
    account_id: string;
    role_id: string;
}

export interface RevokeRoleResponse {
    success: boolean;
    error_message: string;
}

export interface RevokeRoleByNameResponse {
    success: boolean;
    error_message: string;
}

export interface RevokeRoleByNameRequest {
    principal: string;
    role_name: string;
}

export interface GetAccountRolesRequest {
    account_id: string;
}

export interface GetMyRolesRequest {}

/**
 * @brief One role an account holds, with the permissions it grants and the
 * record of its assignment.
 *
 * The element both access reads answer with, so the member's screen and the
 * administrator's render one type. The permissions are attributed to the
 * role rather than flattened into one list, and the tail is the junction
 * row's own record of who granted the role, when and why.
 */
export interface AccountRoleAccess {
    role: Role;
    permission_codes: string[];
    assigned_by: string;
    assigned_at: string;
    change_reason_code: string;
    change_commentary: string;
}

export interface GetAccountRolesResponse {
    result: Result;
    roles: AccountRoleAccess[];
}

export interface GetRolePermissionsRequest {
    role_id: string;
}

export interface GetRolePermissionsResponse {
    result: Result;
    permission_codes: string[];
}

/**
 * @brief Replaces the permissions a role bundles.
 *
 * The codes are the whole bundle the role should carry, not an increment: a
 * code the role bundles and this request omits is removed, and a code both
 * name stays. The write answers with the bundle as stored, so the caller
 * reads back what it wrote rather than assuming the two agree.
 *
 * The change reason and commentary are the junction row's own record of who
 * changed the bundle, when and why; the actor is stamped from the request.
 */
export interface PutRolePermissionsRequest {
    role_id: string;
    permission_codes: string[];
    change_reason_code: string;
    change_commentary: string;
}

export interface SuggestRoleCommandsRequest {
    username: string;
    tenant_id: string;
    hostname: string;
}

export interface SuggestRoleCommandsResponse {
    commands: string[];
}

export const subjects = {
    assign_role_request: 'iam.v1.ops.assign_role',
    assign_role_by_name_request: 'iam.v1.ops.assign_role_by_name',
    revoke_role_request: 'iam.v1.ops.revoke_role',
    revoke_role_by_name_request: 'iam.v1.ops.revoke_role_by_name',
    get_account_roles_request: 'iam.v1.ops.get_account_roles',
    get_my_roles_request: 'iam.v1.ops.get_my_roles',
    get_role_permissions_request: 'iam.v1.ops.get_role_permissions',
    put_role_permissions_request: 'iam.v1.roles_permissions.put',
    suggest_role_commands_request: 'iam.v1.ops.suggest_role_commands',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    assign_role_request: true,
    assign_role_by_name_request: true,
    revoke_role_request: true,
    revoke_role_by_name_request: true,
    get_account_roles_request: true,
    get_my_roles_request: true,
    get_role_permissions_request: true,
    put_role_permissions_request: true,
    suggest_role_commands_request: true,
} as const;
