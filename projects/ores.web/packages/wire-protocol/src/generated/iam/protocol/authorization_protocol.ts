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
 * @brief One resource of the permission catalogue and what an account holds of it.
 *
 * The unit of a page. A screen draws a resource as a row with its actions as
 * columns, so a page holds whole rows and never half of one. =actions= are the
 * actions the catalogue defines for the resource, =held= the ones the account's
 * roles grant, and =roles= the roles that grant any of them, so a screen can
 * say why without reading the roles again.
 */
export interface PermissionResourceRow {
    component: string;
    resource: string;
    actions: string[];
    held: string[];
    roles: string[];
}

/**
 * @brief One area (a component) in which an account holds something, and how
 * many of its resources they hold.
 *
 * The areas a screen offers to choose from. An area the account holds nothing
 * in is not listed, so there is nothing to choose that would show an empty page.
 */
export interface PermissionAreaCount {
    component: string;
    resources: number;
}

/**
 * @brief One page of what an account's roles let it do, by resource.
 *
 * The caller needs iam::roles:read, as reading the account's roles does. The
 * page is the server's: =offset= and =limit= bound the rows, =area= narrows
 * them to one component, and =search= to a resource or component name. An empty
 * =area= means the first area the account holds something in, so a page is
 * always of one area.
 */
export interface ListAccountPermissionsRequest {
    account_id: string;
    area: string;
    search: string;
    offset: number;
    limit: number;
}

/**
 * @brief One page of what the caller's own roles let them do, by resource.
 *
 * The session names the account, so the request names none and the read needs
 * no permission: it is a self read on the allow-list of Authorised reads. The
 * paging fields are those of iam.v1.ops.list_account_permissions.
 */
export interface ListMyPermissionsRequest {
    area: string;
    search: string;
    offset: number;
    limit: number;
}

export interface PermissionPageResponse {
    result: Result;
    /**
     * @brief The area the rows belong to: the one asked for, or the first the
     * account holds something in when none was.
     */
    area: string;
    rows: PermissionResourceRow[];
    /**
     * @brief How many rows match the area and the search, not how many this page
     * holds.
     */
    total_count: number;
    areas: PermissionAreaCount[];
    /**
     * @brief Whether the chosen area is granted whole, by =component::*= or by
     * everything, so a screen can say so and not tick every row.
     */
    area_whole: boolean;
    /**
     * @brief Whether the roles grant everything (=*=).
     */
    everything: boolean;
}

/**
 * @brief One page of the permission catalogue against what one role grants.
 *
 * It serves the role editor, which must offer every permission to tick, not
 * only the ones the role holds, so =include_unheld= asks for the rows the role
 * does not grant too. The paging fields are those of
 * iam.v1.ops.list_account_permissions. The caller needs iam::roles:read.
 */
export interface ListRolePermissionsRequest {
    role_id: string;
    area: string;
    search: string;
    include_unheld: boolean;
    offset: number;
    limit: number;
}

/**
 * @brief One role as the roles list draws it.
 *
 * The permissions are counted, not listed: the list says how much a role lets
 * people do, and the role's own page pages what it lets them do. A service
 * role's count is zero, because it is not given to people and is not changed
 * from a screen.
 */
export interface RolePageRow {
    id: string;
    version: number;
    name: string;
    description: string;
    service: boolean;
    registration_default: boolean;
    requestable: boolean;
    permission_count: number;
    everything: boolean;
}

/**
 * @brief One page of the tenant's roles.
 *
 * =search= matches a role's name or description, =area= keeps the roles that
 * grant something in that area, and =include_service= brings in the platform's
 * own service roles, which are left out otherwise. A =role_id= names one role
 * and ignores the rest, so the role's own page reads its header the same way.
 * The caller needs iam::roles:read.
 */
export interface ListRolesPageRequest {
    role_id: string;
    search: string;
    area: string;
    include_service: boolean;
    offset: number;
    limit: number;
}

export interface RolePageResponse {
    result: Result;
    roles: RolePageRow[];
    total_count: number;
    /**
     * @brief How many service roles were left out, so the list can say so.
     */
    service_hidden: number;
    /**
     * @brief The areas some listed role grants something in, with how many roles do.
     * An area no role grants is not offered, so choosing one never shows an empty list.
     */
    areas: PermissionAreaCount[];
}

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
    list_account_permissions_request: 'iam.v1.ops.list_account_permissions',
    list_my_permissions_request: 'iam.v1.ops.list_my_permissions',
    list_role_permissions_request: 'iam.v1.ops.list_role_permissions',
    list_roles_page_request: 'iam.v1.ops.list_roles_page',
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
    list_account_permissions_request: true,
    list_my_permissions_request: true,
    list_role_permissions_request: true,
    list_roles_page_request: true,
    get_role_permissions_request: true,
    put_role_permissions_request: true,
    suggest_role_commands_request: true,
} as const;
