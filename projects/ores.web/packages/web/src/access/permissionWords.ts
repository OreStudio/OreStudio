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

import { PERMISSION, type Permission } from './permissions.js';

/**
 * What a permission lets a person do, in the words a person uses.
 *
 * A reader is told "you need to be able to see the accounts of your tenant" and
 * never `iam::accounts:read`. The table is typed by {@link Permission}, so a
 * permission added without words does not compile.
 */
const WORDS = {
    [PERMISSION.accountsRead]: 'permissionWords.accountsRead',
    [PERMISSION.accountsUpdate]: 'permissionWords.accountsUpdate',
    [PERMISSION.accountsReset]: 'permissionWords.accountsReset',
    [PERMISSION.contactsRead]: 'permissionWords.contactsRead',
    [PERMISSION.contactsWrite]: 'permissionWords.contactsWrite',
    [PERMISSION.organisationRead]: 'permissionWords.organisationRead',
    [PERMISSION.rolesRead]: 'permissionWords.rolesRead',
    [PERMISSION.rolesAssign]: 'permissionWords.rolesAssign',
    [PERMISSION.sessionsRead]: 'permissionWords.sessionsRead',
} as const satisfies Record<Permission, string>;

/** The translation key for what a permission allows. */
export function wordsFor(permission: Permission): string {
    return WORDS[permission];
}
