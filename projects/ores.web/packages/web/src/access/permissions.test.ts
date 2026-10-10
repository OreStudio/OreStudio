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
import { holdsFrom } from './holds.js';
import { meets, PERMISSION } from './permissions.js';
import { NEEDS } from './screens.js';
import { offered, SHELL_MENUS } from '../shell/areas.js';

const member = holdsFrom(new Set([PERMISSION.organisationRead]));
const administrator = holdsFrom(new Set([PERMISSION.accountsRead, PERMISSION.organisationRead]));
const everything = holdsFrom(new Set(['*']));
const nobody = holdsFrom(new Set());

describe('what a screen needs', () => {
    it('needs every permission in all, and one of any', () => {
        expect(meets(administrator, { all: [PERMISSION.accountsRead] })).toBe(true);
        expect(meets(member, { all: [PERMISSION.accountsRead] })).toBe(false);
        expect(meets(member, { any: [PERMISSION.accountsRead, PERMISSION.organisationRead] })).toBe(
            true,
        );
        expect(meets(nobody, { any: [PERMISSION.accountsRead] })).toBe(false);
        expect(meets(nobody, {})).toBe(true);
    });

    it('counts everything and an area wildcard as holding a permission', () => {
        expect(meets(everything, NEEDS.people)).toBe(true);
        expect(meets(holdsFrom(new Set(['iam::*'])), NEEDS.people)).toBe(true);
    });

    it('gives a member the staff of their parties and an administrator the directory', () => {
        expect(meets(member, NEEDS.people)).toBe(false);
        expect(meets(member, NEEDS.staffOfMyParties)).toBe(true);
        expect(meets(administrator, NEEDS.people)).toBe(true);
    });

    it('offers the organisation in the menu to those who can use one of its lists', () => {
        const item = SHELL_MENUS.application.find((entry) => entry.to === '/organisation');
        expect(item).toBeDefined();
        expect(offered(item!, member)).toBe(true);
        expect(offered(item!, nobody)).toBe(false);
    });
});
