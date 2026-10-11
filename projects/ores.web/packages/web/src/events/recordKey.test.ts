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
import { NOTHING_HEARD, hear, mayConcern, whereChanged } from './recordKey.js';

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

    it('may concern a record when the key it names is of another shape', () => {
        expect(mayConcern([{ id: '42' }], diane)).toBe(true);
        expect(mayConcern([{ tenant: 't1' }], { tenant: 't1', code: 'USD' })).toBe(true);
    });

    it('concerns no record when it names an empty list', () => {
        expect(mayConcern([], diane)).toBe(false);
    });
});

describe('what a screen has heard', () => {
    const rows = [{ u: 'a' }, { u: 'b' }, { u: 'c' }];
    const recordOf = (row: { u: string }) => ({ username: row.u });

    it('joins the records of several changes without repeating one', () => {
        const heard = hear(hear(NOTHING_HEARD, [{ username: 'a' }]), [
            { username: 'a' },
            { username: 'b' },
        ]);
        expect(heard).toEqual({ keys: [{ username: 'a' }, { username: 'b' }] });
    });

    it('cannot tell which once a change names none', () => {
        const heard = hear(hear(NOTHING_HEARD, [{ username: 'a' }]), undefined);
        expect(heard).toBe('unknown');
        expect(hear(heard, [{ username: 'a' }])).toBe('unknown');
    });

    it('cannot tell which once there are too many to keep', () => {
        const many = Array.from({ length: 101 }, (_, i) => ({ username: `u${i}` }));
        expect(hear(NOTHING_HEARD, many)).toBe('unknown');
    });

    it('points at the rows on the page that a change named', () => {
        const where = whereChanged({ keys: [{ username: 'b' }] }, rows, recordOf);
        expect([...where.rows]).toEqual([1]);
        expect(where.elsewhere).toBe(false);
    });

    it('says something changed elsewhere when a named record is off the page', () => {
        const where = whereChanged({ keys: [{ username: 'b' }, { username: 'z' }] }, rows, recordOf);
        expect([...where.rows]).toEqual([1]);
        expect(where.elsewhere).toBe(true);
    });

    it('says something changed elsewhere when the changes were too many to name', () => {
        const where = whereChanged('unknown', rows, recordOf);
        expect(where.rows.size).toBe(0);
        expect(where.elsewhere).toBe(true);
    });

    it('says something changed elsewhere when the list cannot tell its rows apart', () => {
        const where = whereChanged({ keys: [{ username: 'a' }] }, rows, undefined);
        expect(where.rows.size).toBe(0);
        expect(where.elsewhere).toBe(true);
    });

    it('says nothing when nothing was heard', () => {
        const where = whereChanged(NOTHING_HEARD, rows, recordOf);
        expect(where.rows.size).toBe(0);
        expect(where.elsewhere).toBe(false);
    });
});
