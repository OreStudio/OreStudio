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
 *
 */

import { z } from 'zod';
import type { AuthenticatedCaller } from './account-operations.js';
import {
    uuidSchema,
    type AccountAccess,
    type PermissionEntry,
    type RoleSummary,
} from './domain.js';
import { OperationFailedError } from './errors.js';
import {
    subjects as authorizationSubjects,
    type AssignRoleRequest,
    type GetAccountRolesRequest,
    type GetMyRolesRequest,
    type GetRolePermissionsRequest,
    type PutRolePermissionsRequest,
    type RevokeRoleRequest,
} from './generated/iam/protocol/authorization_protocol.js';
import {
    subjects as permissionSubjects,
    type ListPermissionsRequest,
} from './generated/iam/protocol/permission_protocol.js';
import {
    subjects as roleSubjects,
    type DeleteRoleRequest,
    type ListRolesRequest,
    type PutRoleRequest,
} from './generated/iam/protocol/role_protocol.js';
import { resultEnvelopeSchema } from './operations.js';

export const ACCESS_SUBJECTS = {
    mine: authorizationSubjects.get_my_roles_request,
    byAccount: authorizationSubjects.get_account_roles_request,
    rolePermissions: authorizationSubjects.get_role_permissions_request,
    putRolePermissions: authorizationSubjects.put_role_permissions_request,
    assign: authorizationSubjects.assign_role_request,
    revoke: authorizationSubjects.revoke_role_request,
    listRoles: roleSubjects.list_roles_request,
    putRole: roleSubjects.put_role_request,
    deleteRole: roleSubjects.delete_role_request,
    listPermissions: permissionSubjects.list_permissions_request,
} as const;

/** The platform defines fewer permissions than this, so one page reads them all. */
const PERMISSION_PAGE = 1000;
const ROLE_PAGE = 1000;

const wireRoleSchema = z.object({
    id: uuidSchema,
    version: z.int().nonnegative().default(0),
    name: z.string(),
    description: z.string().default(''),
    is_registration_default: z.boolean().default(false),
    /** Absent from a peer that predates the flag, and then not on offer. */
    is_requestable: z.boolean().default(false),
});

const accessReplySchema = z.object({
    result: resultEnvelopeSchema,
    roles: z
        .array(
            z.object({
                role: wireRoleSchema,
                permission_codes: z.array(z.string()).default([]),
                assigned_by: z.string().default(''),
                assigned_at: z.string().default(''),
                change_reason_code: z.string().default(''),
                change_commentary: z.string().default(''),
            }),
        )
        .default([]),
});

const codesReplySchema = z.object({
    result: resultEnvelopeSchema,
    permission_codes: z.array(z.string()).default([]),
});

const rolesReplySchema = z.object({
    result: resultEnvelopeSchema,
    roles: z.array(wireRoleSchema).default([]),
});

const permissionsReplySchema = z.object({
    result: resultEnvelopeSchema,
    permissions: z
        .array(z.object({ code: z.string(), description: z.string().default('') }))
        .default([]),
});

const resultReplySchema = z.object({ result: resultEnvelopeSchema });

const flagReplySchema = z.object({
    success: z.boolean().default(false),
    error_message: z.string().default(''),
});

/**
 * The outcome of a write a person asked for: done, or refused with the
 * server's words.
 *
 * Giving and taking away a role answer `success` and `error_message`, the
 * older reply shape; the role and bundle writes answer a `result`. Both are
 * read into this one shape so a screen handles a refusal one way.
 */
export type AccessWrite =
    { readonly done: true } | { readonly done: false; readonly message: string };

function ok(subject: string, result: z.infer<typeof resultEnvelopeSchema>): void {
    if (result.outcome !== 'ok') {
        throw new OperationFailedError(subject, result.message);
    }
}

function toAccess(reply: z.infer<typeof accessReplySchema>): AccountAccess {
    return {
        roles: reply.roles.map((held) => ({
            roleId: held.role.id,
            name: held.role.name,
            description: held.role.description,
            permissionCodes: held.permission_codes,
            givenBy: held.assigned_by,
            givenAt: held.assigned_at,
            reasonCode: held.change_reason_code,
            commentary: held.change_commentary,
        })),
    };
}

/** The roles the caller holds. The account is the caller, so it names nobody else. */
export async function readMyAccess(caller: AuthenticatedCaller): Promise<AccountAccess> {
    const request: GetMyRolesRequest = {};
    const reply = await caller.callAuthenticated(ACCESS_SUBJECTS.mine, request, accessReplySchema);
    ok(ACCESS_SUBJECTS.mine, reply.result);
    return toAccess(reply);
}

/** The roles one account holds. The server allows this to a holder of `iam::roles:read`. */
export async function readAccountAccess(
    caller: AuthenticatedCaller,
    accountId: string,
): Promise<AccountAccess> {
    const request: GetAccountRolesRequest = { account_id: accountId };
    const reply = await caller.callAuthenticated(
        ACCESS_SUBJECTS.byAccount,
        request,
        accessReplySchema,
    );
    ok(ACCESS_SUBJECTS.byAccount, reply.result);
    return toAccess(reply);
}

async function readRolePermissions(caller: AuthenticatedCaller, roleId: string): Promise<string[]> {
    const request: GetRolePermissionsRequest = { role_id: roleId };
    const reply = await caller.callAuthenticated(
        ACCESS_SUBJECTS.rolePermissions,
        request,
        codesReplySchema,
    );
    ok(ACCESS_SUBJECTS.rolePermissions, reply.result);
    return reply.permission_codes;
}

/**
 * The tenant's roles, each with the permissions it grants.
 *
 * A service role's permissions are not read: no screen shows them, and the
 * seed holds nineteen of them.
 */
export async function readRoles(caller: AuthenticatedCaller): Promise<RoleSummary[]> {
    const request: ListRolesRequest = {
        offset: 0,
        limit: ROLE_PAGE,
        order: { field: '', descending: false },
        as_of: null,
        filter: null,
    };
    const reply = await caller.callAuthenticated(
        ACCESS_SUBJECTS.listRoles,
        request,
        rolesReplySchema,
    );
    ok(ACCESS_SUBJECTS.listRoles, reply.result);
    return Promise.all(
        reply.roles.map(async (role) => {
            const service = role.name.endsWith('Service');
            return {
                id: role.id,
                version: role.version,
                name: role.name,
                description: role.description,
                service,
                registrationDefault: role.is_registration_default,
                requestable: role.is_requestable,
                permissionCodes: service ? [] : await readRolePermissions(caller, role.id),
            };
        }),
    );
}

/** Every permission the platform defines, in code order. */
export async function readPermissionCatalogue(
    caller: AuthenticatedCaller,
): Promise<PermissionEntry[]> {
    const request: ListPermissionsRequest = {
        offset: 0,
        limit: PERMISSION_PAGE,
        order: { field: '', descending: false },
        as_of: null,
        filter: null,
    };
    const reply = await caller.callAuthenticated(
        ACCESS_SUBJECTS.listPermissions,
        request,
        permissionsReplySchema,
    );
    ok(ACCESS_SUBJECTS.listPermissions, reply.result);
    return reply.permissions
        .map((permission) => ({ code: permission.code, description: permission.description }))
        .sort((a, b) => a.code.localeCompare(b.code));
}

/**
 * Replaces the permissions a role grants with exactly these, and answers with
 * the bundle as stored.
 */
export async function saveRolePermissions(
    caller: AuthenticatedCaller,
    roleId: string,
    codes: readonly string[],
    note: string,
): Promise<string[]> {
    const request: PutRolePermissionsRequest = {
        role_id: roleId,
        permission_codes: [...codes],
        change_reason_code: '',
        change_commentary: note,
    };
    const reply = await caller.callAuthenticated(
        ACCESS_SUBJECTS.putRolePermissions,
        request,
        codesReplySchema,
    );
    ok(ACCESS_SUBJECTS.putRolePermissions, reply.result);
    return reply.permission_codes;
}

/** Gives a role to an account, for a reason from the access category. */
export async function giveRole(
    caller: AuthenticatedCaller,
    input: {
        readonly accountId: string;
        readonly roleId: string;
        readonly reasonCode: string;
        readonly note: string;
    },
): Promise<AccessWrite> {
    const request: AssignRoleRequest = {
        account_id: input.accountId,
        role_id: input.roleId,
        change_reason_code: input.reasonCode,
        change_commentary: input.note,
    };
    const reply = await caller.callAuthenticated(ACCESS_SUBJECTS.assign, request, flagReplySchema);
    return reply.success ? { done: true } : { done: false, message: reply.error_message };
}

/** Takes a role away from an account. The server refuses one's own account. */
export async function takeRoleAway(
    caller: AuthenticatedCaller,
    input: { readonly accountId: string; readonly roleId: string },
): Promise<AccessWrite> {
    const request: RevokeRoleRequest = { account_id: input.accountId, role_id: input.roleId };
    const reply = await caller.callAuthenticated(ACCESS_SUBJECTS.revoke, request, flagReplySchema);
    return reply.success ? { done: true } : { done: false, message: reply.error_message };
}

/**
 * Writes a role's name, description and whether a member may ask for it: a
 * new role when no id is given, or a new version of the one named.
 */
export async function saveRole(
    caller: AuthenticatedCaller,
    input: {
        readonly id: string;
        readonly version: number | null;
        readonly name: string;
        readonly description: string;
        readonly registrationDefault: boolean;
        readonly requestable: boolean;
    },
): Promise<AccessWrite> {
    const request: PutRoleRequest = {
        change: {
            write: {
                id: input.id,
                name: input.name,
                description: input.description,
                is_registration_default: input.registrationDefault,
                is_requestable: input.requestable,
            },
            precondition:
                input.version === null
                    ? { kind: 'must_not_exist', version: null }
                    : { kind: 'must_match_version', version: input.version },
        },
        intent: { reason_code: '', commentary: '' },
    };
    const reply = await caller.callAuthenticated(
        ACCESS_SUBJECTS.putRole,
        request,
        resultReplySchema,
    );
    return reply.result.outcome === 'ok'
        ? { done: true }
        : { done: false, message: reply.result.message };
}

/** Deletes a role by its name. */
export async function deleteRole(caller: AuthenticatedCaller, name: string): Promise<AccessWrite> {
    const request: DeleteRoleRequest = {
        removal: { key: { name }, precondition: { kind: 'any', version: null } },
        intent: { reason_code: '', commentary: '' },
    };
    const reply = await caller.callAuthenticated(
        ACCESS_SUBJECTS.deleteRole,
        request,
        resultReplySchema,
    );
    return reply.result.outcome === 'ok'
        ? { done: true }
        : { done: false, message: reply.result.message };
}
