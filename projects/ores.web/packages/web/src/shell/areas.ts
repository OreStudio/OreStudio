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
import { DEPLOYMENT_READ } from '@ores/contracts';
import { PATHS } from './paths.js';

/** One entry of the shell's menu: an area, named for what a person does there. */
export interface MenuItem {
    readonly nameKey: string;
    readonly to: string;
    /** Drawn only for a person who holds this permission, or any of these. */
    readonly permission?: string | readonly string[];
}

/**
 * The areas of the menu, in one order for every person.
 *
 * An area is drawn for a person who holds the read that one of its screens
 * needs. An area that needs no read is drawn for everybody. A person's own
 * screens, such as their password and sign-ins, are in the account menu.
 */
export const MENU: readonly MenuItem[] = [
    { nameKey: 'shell.menu.home', to: '/' },
    { nameKey: 'shell.menu.refdata', to: PATHS.refdata },
    {
        nameKey: 'shell.menu.organisation',
        to: PATHS.organisation,
        permission: [
            'iam::accounts:read',
            'iam::organisation:read',
            'iam::roles:read',
            'iam::accounts:lock',
        ],
    },
    {
        nameKey: 'shell.menu.marketdata',
        to: '/market-data',
        permission: [
            'synthetic::fx_spot_generation_configs:read',
            'synthetic::ir_curve_generation_configs:read',
            'synthetic::market_data_providers:read',
            'marketdata::curve_snapshots:read',
        ],
    },
    { nameKey: 'shell.menu.requests', to: '/requests' },
    { nameKey: 'shell.menu.operations', to: PATHS.operations },
    { nameKey: 'shell.menu.development', to: '/development' },
    { nameKey: 'shell.menu.tenants', to: '/tenants', permission: DEPLOYMENT_READ },
];

export function modeKey(mode: SessionMode): string {
    return `shell.mode.${mode}`;
}

/** Whether a screen is drawn for somebody who holds what `holds` answers for. */
export function offered(
    item: Pick<MenuItem, 'permission'>,
    holds: (code: string) => boolean,
): boolean {
    if (item.permission === undefined) {
        return true;
    }
    return typeof item.permission === 'string'
        ? holds(item.permission)
        : item.permission.some(holds);
}
