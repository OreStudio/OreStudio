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
import { mayConcern } from './recordKey.js';

describe('whether a change concerns a record', () => {
    const diane = { username: 'diane.kay' };

    it('may concern any record when it names none', () => {
        expect(mayConcern(undefined, diane)).toBe(true);
    });

    it('concerns the record it names', () => {
        expect(mayConcern([{ username: 'hazel.jennings' }, diane], diane)).toBe(true);
    });

    it('does not concern a record it does not name', () => {
        expect(mayConcern([{ username: 'hazel.jennings' }], diane)).toBe(false);
    });

    it("needs every column of the record's key to match", () => {
        const key = { tenant: 't1', code: 'USD' };
        expect(mayConcern([{ tenant: 't1', code: 'EUR' }], key)).toBe(false);
        expect(mayConcern([{ tenant: 't1', code: 'USD', extra: 'x' }], key)).toBe(true);
    });

    it('concerns no record when it names an empty list', () => {
        expect(mayConcern([], diane)).toBe(false);
    });
});
