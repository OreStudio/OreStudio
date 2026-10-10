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

import { describe, expect, it } from 'vitest';
import { missingOf } from './NotAvailable.js';
import { holdsFrom } from './holds.js';
import { PERMISSION } from './permissions.js';
import { NEEDS } from './screens.js';

describe('what a reader lacks of what a screen needs', () => {
    const nothing = holdsFrom(new Set());
    const reader = holdsFrom(new Set([PERMISSION.rolesRead]));

    it('lists every missing permission of all, in the order declared', () => {
        const needs = {
            all: [PERMISSION.accountsRead, PERMISSION.rolesRead, PERMISSION.rolesAssign],
        };
        expect(missingOf(needs, reader)).toEqual([PERMISSION.accountsRead, PERMISSION.rolesAssign]);
    });

    it('lists the choices of any when the reader holds none of them', () => {
        expect(missingOf(NEEDS.staff, nothing)).toEqual([
            PERMISSION.accountsRead,
            PERMISSION.organisationRead,
        ]);
    });

    it('lists nothing when one of any is held', () => {
        const member = holdsFrom(new Set([PERMISSION.organisationRead]));
        expect(missingOf(NEEDS.staff, member)).toEqual([]);
    });
});
