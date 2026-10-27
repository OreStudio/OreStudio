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
import { arcsOf } from './Donut.js';

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
