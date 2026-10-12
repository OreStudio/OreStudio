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
import type { AccountRole } from '../domain/account_role.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface AccountRoleKey {
    account_id: string;
    role_id: string;
}

export interface AccountRoleWrite {
    account_id: string;
    role_id: string;
}

export interface AccountRoleChange {
    write: AccountRoleWrite;
    precondition: Precondition;
}

export interface AccountRoleRemoval {
    key: AccountRoleKey;
    precondition: Precondition;
}

export interface AccountRoleLookup {
    key: AccountRoleKey;
    account_role: AccountRole | null;
}

export interface AccountRolesFilter {
    account_id: string | null;
    role_id: string | null;
    account_id_one_of: string[] | null;
    role_id_one_of: string[] | null;
}

export interface ListAccountRolesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: AccountRolesFilter | null;
}

export interface ListAccountRolesResponse {
    result: Result;
    account_roles: AccountRole[];
    total: number;
}

export interface GetAccountRoleRequest {
    key: AccountRoleKey;
}

export interface GetAccountRoleResponse {
    result: Result;
    account_role: AccountRole | null;
}

export interface GetManyAccountRolesRequest {
    keys: AccountRoleKey[];
}

export interface GetManyAccountRolesResponse {
    result: Result;
    entries: AccountRoleLookup[];
}

export interface PutAccountRoleRequest {
    change: AccountRoleChange;
    intent: ChangeIntent;
}

export interface PutAccountRoleResponse {
    result: Result;
    account_role: AccountRole | null;
}

export interface PutManyAccountRolesRequest {
    changes: AccountRoleChange[];
    intent: ChangeIntent;
}

export interface PutManyAccountRolesResponse {
    result: Result;
    account_roles: AccountRole[];
}

export interface DeleteAccountRoleRequest {
    removal: AccountRoleRemoval;
    intent: ChangeIntent;
}

export interface DeleteAccountRoleResponse {
    result: Result;
}

export interface DeleteManyAccountRolesRequest {
    removals: AccountRoleRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyAccountRolesResponse {
    result: Result;
}

export interface ListByAccountIdAccountRolesRequest {
    account_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: AccountRolesFilter | null;
}

export interface ListByAccountIdAccountRolesResponse {
    result: Result;
    account_roles: AccountRole[];
    total: number;
}

export interface ListByRoleIdAccountRolesRequest {
    role_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: AccountRolesFilter | null;
}

export interface ListByRoleIdAccountRolesResponse {
    result: Result;
    account_roles: AccountRole[];
    total: number;
}

export const subjects = {
    list_account_roles_request: 'iam.v1.account_roles.list',
    get_account_role_request: 'iam.v1.account_roles.get',
    get_many_account_roles_request: 'iam.v1.account_roles.get_many',
    put_account_role_request: 'iam.v1.account_roles.put',
    put_many_account_roles_request: 'iam.v1.account_roles.put_many',
    delete_account_role_request: 'iam.v1.account_roles.delete',
    delete_many_account_roles_request: 'iam.v1.account_roles.delete_many',
    list_by_account_id_account_roles_request: 'iam.v1.account_roles.list_by_account_id',
    list_by_role_id_account_roles_request: 'iam.v1.account_roles.list_by_role_id',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_account_roles_request: true,
    get_account_role_request: true,
    get_many_account_roles_request: true,
    put_account_role_request: true,
    put_many_account_roles_request: true,
    delete_account_role_request: true,
    delete_many_account_roles_request: true,
    list_by_account_id_account_roles_request: true,
    list_by_role_id_account_roles_request: true,
} as const;
