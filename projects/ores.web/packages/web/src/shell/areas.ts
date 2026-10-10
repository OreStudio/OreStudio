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

import type { SessionMode } from '@ores/wire-protocol/browser';

/** One entry of the shell's menu: a screen, named for what a person does there. */
export interface MenuItem {
    readonly nameKey: string;
    readonly to: string;
    /** Shown only to a person who holds this permission, or any of these. */
    readonly permission?: string | readonly string[];
}

/**
 * The screens each mode's menu holds.
 *
 * The menu is the client's structure; the server answers what the session may
 * do, not where a screen goes. A screen that belongs to another mode is absent
 * rather than disabled, because it is not a door this person may open later.
 * A person's own screens, such as their password and sign-ins, are reached
 * from Home rather than from the menu.
 */
export const SHELL_MENUS: Readonly<Record<SessionMode, readonly MenuItem[]>> = {
    'system-administration': [
        { nameKey: 'shell.menu.home', to: '/' },
        { nameKey: 'shell.menu.accounts', to: '/people' },
        { nameKey: 'shell.menu.tenants', to: '/tenants' },
        { nameKey: 'shell.menu.operations', to: '/operations' },
    ],
    'tenant-administration': [
        { nameKey: 'shell.menu.home', to: '/' },
        { nameKey: 'shell.menu.parties', to: '/parties' },
        { nameKey: 'shell.menu.organisation', to: '/organisation' },
        { nameKey: 'shell.menu.requests', to: '/requests', permission: 'iam::roles:assign' },
        { nameKey: 'shell.menu.roles', to: '/roles' },
        { nameKey: 'shell.menu.refdata', to: '/refdata' },
        { nameKey: 'shell.menu.rescue', to: '/rescue' },
        { nameKey: 'shell.menu.audit', to: '/audit' },
    ],
    application: [
        { nameKey: 'shell.menu.home', to: '/' },
        {
            nameKey: 'shell.menu.organisation',
            to: '/organisation',
            permission: ['iam::accounts:read', 'iam::organisation:read'],
        },
        { nameKey: 'shell.menu.requests', to: '/requests', permission: 'iam::roles:assign' },
        { nameKey: 'shell.menu.refdata', to: '/refdata' },
        { nameKey: 'shell.menu.access', to: '/access' },
    ],
};

export function menuFor(mode: SessionMode): readonly MenuItem[] {
    return SHELL_MENUS[mode];
}

export function modeKey(mode: SessionMode): string {
    return `shell.mode.${mode}`;
}

/** Whether a menu item is offered to somebody who holds what `holds` answers for. */
export function offered(item: MenuItem, holds: (code: string) => boolean): boolean {
    if (item.permission === undefined) {
        return true;
    }
    return typeof item.permission === 'string'
        ? holds(item.permission)
        : item.permission.some(holds);
}
