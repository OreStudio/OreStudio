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

import { PERMISSION, type Needs } from './permissions.js';

/**
 * What each screen needs, declared once.
 *
 * The menu, the home page, the route guard and the tiles that lead to a screen
 * all read this, so a screen is offered exactly to the people who can use it
 * and a link that is drawn is a link that works.
 */
export const NEEDS = {
    /** The whole directory of the tenant. */
    people: { all: [PERMISSION.accountsRead] },
    /** The people of the parties the reader works in. */
    staffOfMyParties: { all: [PERMISSION.organisationRead] },
    /** Either list, whichever the reader can use. */
    staff: { any: [PERMISSION.accountsRead, PERMISSION.organisationRead] },
} as const satisfies Record<string, Needs>;
