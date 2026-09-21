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

export interface AssignRoleRequest {
    account_id: string;
    role_id: string;
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

export interface GetAccountRolesResponse {
    roles: Role[];
}

export interface GetAccountPermissionsRequest {
    account_id: string;
}

export interface GetAccountPermissionsResponse {
    permission_codes: string[];
}

export interface GetRolePermissionsRequest {
    role_id: string;
}

export interface GetRolePermissionsResponse {
    permission_codes: string[];
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
    assign_role_request: "iam.v1.roles.assign",
    assign_role_by_name_request: "iam.v1.roles.assign-by-name",
    revoke_role_request: "iam.v1.roles.revoke",
    revoke_role_by_name_request: "iam.v1.roles.revoke-by-name",
    get_account_roles_request: "iam.v1.roles.by-account",
    get_account_permissions_request: "iam.v1.roles.permissions-by-account",
    get_role_permissions_request: "iam.v1.roles.permissions",
    suggest_role_commands_request: "iam.v1.roles.suggest-commands",
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
    get_account_permissions_request: true,
    get_role_permissions_request: true,
    suggest_role_commands_request: true,
} as const;
