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
import { arcsOf, bookSharesOf, pathOf, problemsOf, type BookRow } from './Donut.js';

describe('arcsOf', () => {
    it('sweeps the whole circle across the parts', () => {
        const arcs = arcsOf([
            { name: 'One', value: 3 },
            { name: 'Two', value: 1 },
        ]);

        expect(arcs[0]?.from).toBe(0);
        expect(arcs[0]?.to).toBe(270);
        expect(arcs[1]?.from).toBe(270);
        expect(arcs[1]?.to).toBe(360);
    });

    it('states no arcs when the parts sum to nothing', () => {
        expect(arcsOf([{ name: 'Nothing', value: 0 }])).toEqual([]);
    });

    it('states a part as its share of the whole it is told', () => {
        const arcs = arcsOf([{ name: 'Part', value: 5 }], 10);

        expect(arcs[0]?.to).toBe(180);
    });
});

describe('bookSharesOf', () => {
    const book = (key: string, name: string, total: string, currency = 'GBP'): BookRow => ({
        key,
        name,
        is_group: false,
        is_removed: false,
        totals: [{ currency, total }],
    });

    it('states each book by its name', () => {
        expect(bookSharesOf([book('b1', 'One', '100'), book('b2', '', '50')])).toEqual([
            { name: 'One', value: 100 },
            { name: 'b2', value: 50 },
        ]);
    });

    it('draws nothing for one book, two currencies or a loss', () => {
        expect(bookSharesOf([book('b1', 'One', '100')])).toBeNull();
        expect(bookSharesOf([book('b1', 'One', '100'), book('b2', 'Two', '50', 'USD')])).toBeNull();
        expect(bookSharesOf([book('b1', 'One', '100'), book('b2', 'Two', '-50')])).toBeNull();
    });
});

describe('problemsOf', () => {
    it('counts each scope, and the unscoped ones apart', () => {
        expect(
            problemsOf(
                [{ scope: 'Curve' }, { scope: 'Curve' }, { scope: 'Trade' }, { scope: '' }],
                'None',
            ),
        ).toEqual([
            { name: 'Curve', value: 2 },
            { name: 'Trade', value: 1 },
            { name: 'None', value: 1 },
        ]);
    });
});

describe('problemsOf, the crowded ring', () => {
    it('adds the scopes past the colours into one part', () => {
        const scopes = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'].map((scope) => ({ scope }));
        const parts = problemsOf(scopes, 'None', 'Other');

        expect(parts).toHaveLength(6);
        expect(parts[5]?.name).toBe('Other');
        expect(parts[5]?.value).toBe(3);
        expect(parts.reduce((sum, part) => sum + part.value, 0)).toBe(8);
    });

    it('states nothing for no problems', () => {
        expect(problemsOf([], 'None')).toEqual([]);
    });
});

describe('pathOf, a whole in one part', () => {
    it('draws a ring with a hole rather than a filled disc', () => {
        const path = pathOf({ name: 'All', value: 1, from: 0, to: 360 }, 60, 60, 56, 34);

        expect(path.match(/M /g)).toHaveLength(2);
    });
});

describe('bookSharesOf, the rows it must not count', () => {
    const book = (over: Partial<BookRow>): BookRow => ({
        key: 'b1',
        name: 'One',
        is_group: false,
        is_removed: false,
        totals: [{ currency: 'GBP', total: '100' }],
        ...over,
    });

    it('leaves out a group row, a removed row and a book with no figure', () => {
        const shares = bookSharesOf([
            book({ key: 'b1', name: 'One' }),
            book({ key: 'b2', name: 'Two' }),
            book({ key: 'p1', name: 'Portfolio', is_group: true }),
            book({ key: 'b3', name: 'Gone', is_removed: true }),
            book({ key: 'b4', name: 'Empty', totals: [] }),
        ]);

        expect(shares?.map((share) => share.name)).toEqual(['One', 'Two', 'Empty']);
    });

    it('draws nothing for more books than a ring has colours', () => {
        const many = Array.from({ length: 7 }, (_, index) =>
            book({ key: `b${index}`, name: `Book ${index}` }),
        );

        expect(bookSharesOf(many)).toBeNull();
    });
});
