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

import { renderToStaticMarkup } from 'react-dom/server';
import { describe, expect, it } from 'vitest';
import { MAX_SLICES, SharePie, percent, pieSlices, slicePath } from './SharePie.js';

const item = (key: string, value: number) => ({ key, label: key, value });

describe('pieSlices', () => {
    it('sorts largest first and shares the whole', () => {
        const slices = pieSlices([item('a', 1), item('b', 3)], 'Others');

        expect(slices.map((slice) => slice.key)).toEqual(['b', 'a']);
        expect(slices.map((slice) => slice.fraction)).toEqual([0.75, 0.25]);
    });

    it('leaves out a part with no share, and draws nothing for an empty whole', () => {
        expect(pieSlices([item('a', 0), item('b', 2)], 'Others').map((s) => s.key)).toEqual(['b']);
        expect(pieSlices([item('a', 0)], 'Others')).toEqual([]);
    });

    it('folds the tail into one slice so the total stays whole', () => {
        const items = Array.from({ length: MAX_SLICES + 3 }, (_, i) => item(`p${i}`, i + 1));
        const slices = pieSlices(items, 'Others');

        expect(slices).toHaveLength(MAX_SLICES);
        expect(slices.at(-1)?.label).toBe('Others');
        const sum = slices.reduce((total, slice) => total + slice.fraction, 0);
        expect(sum).toBeCloseTo(1, 10);
    });
});

describe('pieSlices palette', () => {
    it('folds a sixth part into Others so no named slice shares the Others colour', () => {
        const items = Array.from({ length: 6 }, (_, i) => item(`p${i}`, 10 - i));
        const slices = pieSlices(items, 'Others');
        const swatches = slices.map((slice) => slice.swatch);

        expect(slices.at(-1)?.key).toBe('other');
        expect(new Set(swatches).size).toBe(swatches.length);
    });

    it('uses no status colour for a slice', () => {
        const items = Array.from({ length: 6 }, (_, i) => item(`p${i}`, 10 - i));

        for (const slice of pieSlices(items, 'Others')) {
            expect(slice.swatch).not.toMatch(/text-(up|down|warn)/);
        }
    });
});

describe('slicePath and percent', () => {
    it('marks a slice over half as a large arc', () => {
        expect(slicePath(0, 0.75)).toContain(' 0 1 1 ');
        expect(slicePath(0, 0.25)).toContain(' 0 0 1 ');
    });

    it('floors a small share at under one percent', () => {
        expect(percent(0.004)).toBe('<1%');
        expect(percent(0.5)).toBe('50%');
    });
});

describe('SharePie', () => {
    it('draws one circle for a single part', () => {
        const html = renderToStaticMarkup(
            <SharePie items={[item('a', 5)]} otherLabel="Others" label="Share" />,
        );

        expect(html).toContain('<circle');
        expect(html).toContain('100%');
    });

    it('draws nothing when no part has a share', () => {
        const html = renderToStaticMarkup(
            <SharePie items={[item('a', 0)]} otherLabel="Others" label="Share" />,
        );

        expect(html).toBe('');
    });
});
