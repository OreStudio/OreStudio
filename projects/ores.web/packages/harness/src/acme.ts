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
 * The seeded Acme Corporation tenant the harness signs in to. The password is
 * the shared one every seeded account carries; compass open-as holds the same
 * facts in compass_open_as.py, and a change there changes here.
 */
export const TENANT_HOSTNAME = 'acme_corporation';
export const PASSWORD = 'Secure-Password-123';

export interface Person {
    readonly username: string;
    readonly role: 'administrator' | 'member';
    /** Signs in to the system tenant, whose principals carry no tenant hostname. */
    readonly system?: true;
}

/** The tenant administrator, who may read and write every reference data kind. */
export const TENANT_ADMIN: Person = { username: 'tenant_admin', role: 'administrator' };

/** A seeded member of the London office, who reads reference data and writes none. */
export const ADRIAN_VANCE: Person = { username: 'adrian.vance', role: 'member' };

/** The system administrator, who runs the installation's feeds and sets its market. */
export const SUPER_ADMIN: Person = { username: 'super_admin', role: 'administrator', system: true };

export function principal(person: Person): string {
    return person.system ? person.username : `${person.username}@${TENANT_HOSTNAME}`;
}
