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
 * The permissions a screen or a form can need.
 *
 * Named once, here, so a wrong code is a compile error and not a screen that
 * quietly asks for something nobody holds. A screen or a form declares what it
 * needs from this list, and the menu, the route guard and the buttons that open
 * a form read that one declaration.
 */
export const PERMISSION = {
    accountsRead: 'iam::accounts:read',
    accountsUpdate: 'iam::accounts:update',
    accountsReset: 'iam::accounts:reset',
    contactsRead: 'iam::account_contact_informations:read',
    contactsWrite: 'iam::account_contact_informations:write',
    organisationRead: 'iam::organisation:read',
    rolesRead: 'iam::roles:read',
    rolesAssign: 'iam::roles:assign',
    sessionsRead: 'iam::sessions:read',
} as const;

export type Permission = (typeof PERMISSION)[keyof typeof PERMISSION];

/**
 * What a screen or a form needs before it can work.
 *
 * Every permission in `all`, and at least one in `any` when `any` is given. A
 * declaration with neither needs nothing.
 */
export interface Needs {
    readonly all?: readonly Permission[];
    readonly any?: readonly Permission[];
}

/** Whether what is held meets what is needed. */
export function meets(held: (code: Permission) => boolean, needs: Needs): boolean {
    const every = (needs.all ?? []).every(held);
    const some = needs.any === undefined || needs.any.length === 0 || needs.any.some(held);
    return every && some;
}
