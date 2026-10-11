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

/**
 * The ways a person could come to hold a permission.
 *
 * A screen they may not use should say what to do about it. A permission cannot
 * be asked for, a role can, and only a role the tenant lets members ask for. So
 * the answer is the requestable roles that grant the permission, less the ones
 * the person already holds.
 */

/** The shape of a role this needs; the full summary has more. */
export interface GrantingRole {
    readonly id: string;
    readonly name: string;
    readonly description: string;
    readonly requestable: boolean;
    readonly service: boolean;
    readonly permissionCodes: readonly string[];
}

/** A permission code as the platform spells it: `area::resource:action`. */
export const PERMISSION_CODE = /^[a-z][a-z0-9_]*::[a-z][a-z0-9_]*:[a-z][a-z0-9_]*$/;

/** Whether a role grants a permission, counting everything and an area's wildcard. */
export function grants(role: GrantingRole, code: string): boolean {
    const area = `${code.split('::')[0] ?? ''}::*`;
    return role.permissionCodes.some((held) => held === '*' || held === area || held === code);
}

/** The roles a person can ask for that would give them the permission. */
export function waysToHold(
    code: string,
    roles: readonly GrantingRole[],
    heldRoleIds: ReadonlySet<string>,
): readonly { readonly id: string; readonly name: string; readonly description: string }[] {
    return roles
        .filter(
            (role) =>
                role.requestable &&
                !role.service &&
                !heldRoleIds.has(role.id) &&
                grants(role, code),
        )
        .map(({ id, name, description }) => ({ id, name, description }));
}
