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
import { renderToStaticMarkup } from 'react-dom/server';
import { PieChart, shareOf, sliceAngles, slicePath } from './PieChart.js';

/**
 * The pie: the angles the slices cover, and the paths that draw them.
 *
 * The chart is drawn from the values as they arrive, so what is checked is that
 * the angles describe the whole circle once and that a slice of nothing is not
 * drawn at all.
 */

const SLICES = [
    { key: 'a', label: 'Alpha', value: 50 },
    { key: 'b', label: 'Beta', value: 30 },
    { key: 'c', label: 'Gamma', value: 20 },
];

describe('the angles of a pie', () => {
    it('covers the whole circle once, in the order the slices came', () => {
        const angles = sliceAngles(SLICES);
        expect(angles).toHaveLength(3);
        expect(angles[0]?.[0]).toBeCloseTo(-Math.PI / 2, 9);
        expect(angles[2]?.[1]).toBeCloseTo(-Math.PI / 2 + 2 * Math.PI, 9);
        expect(angles[0]?.[1]).toBeCloseTo(angles[1]?.[0] ?? 0, 9);
    });

    it('states no angles when every slice is nothing', () => {
        expect(sliceAngles([{ key: 'a', label: 'A', value: 0 }])).toEqual([]);
        expect(sliceAngles([])).toEqual([]);
    });

    it('reads a share of the whole, and nothing when the whole is empty', () => {
        expect(shareOf(30, 100)).toBeCloseTo(0.3, 9);
        expect(shareOf(30, 0)).toBe(0);
    });
});

describe('the path of a slice', () => {
    it('draws an arc from the centre for a slice of the circle', () => {
        const path = slicePath(0, Math.PI / 2);
        expect(path.startsWith('M 110 110')).toBe(true);
        expect(path).toContain('A');
        expect(path.endsWith('Z')).toBe(true);
    });

    it('draws a circle for a slice that holds everything', () => {
        // An arc cannot close on itself, so the whole circle takes its own path.
        expect(slicePath(0, 2 * Math.PI)).toContain('A 106 106 0 1 1');
    });

    it('draws nothing for a slice with no width', () => {
        expect(slicePath(1, 1)).toBe('');
        expect(slicePath(2, 1)).toBe('');
    });
});

describe('the chart', () => {
    it('states each slice in the legend with its figure and its share', () => {
        const html = renderToStaticMarkup(
            <PieChart slices={SLICES} format={(value) => `${value} B`} caption="A pie" />,
        );
        expect(html).toContain('A pie');
        expect(html.match(/<path/g) ?? []).toHaveLength(3);
        expect(html).toContain('Alpha');
        expect(html).toContain('50 B');
        expect(html).toContain('50%');
    });

    it('draws nothing when there is nothing to draw', () => {
        const html = renderToStaticMarkup(<PieChart slices={[]} format={String} caption="A pie" />);
        expect(html).toBe('');
    });
});
