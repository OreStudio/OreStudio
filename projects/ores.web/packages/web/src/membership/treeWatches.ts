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

import type { WatchedEntity } from '../events/EntityEvents.js';

/**
 * What the reporting tree and the staff list are read from: the people, and the
 * parties they belong to. A reporting line has no event of its own, so a change
 * to one is heard only when it also changes the person.
 */
export const TREE_WATCHES: readonly WatchedEntity[] = [
    { component: 'iam', entity: 'accounts' },
    { component: 'refdata', entity: 'parties' },
];
