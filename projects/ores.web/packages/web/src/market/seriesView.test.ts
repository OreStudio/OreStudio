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
import { instantLabel, latestFirst, pointsOf } from './seriesView.js';

const obs = (at: string, value: string) => ({ at, value, source: 'feed' });

describe('pointsOf', () => {
    it('reads the instant and the value, and keeps the order', () => {
        const points = pointsOf([
            obs('2026-01-01 10:00:00', '1.25'),
            obs('2026-01-01 11:00:00', '1.5'),
        ]);
        expect(points.map((p) => p.value)).toEqual([1.25, 1.5]);
        expect(points[0]!.at).toBeLessThan(points[1]!.at);
    });

    it('leaves out a value that is not a number, rather than draw it as zero', () => {
        const points = pointsOf([
            obs('2026-01-01 10:00:00', ''),
            obs('2026-01-01 11:00:00', 'n/a'),
        ]);
        expect(points).toEqual([]);
    });

    it('leaves out an instant that is not a date', () => {
        expect(pointsOf([obs('yesterday', '1')])).toEqual([]);
    });
});

describe('latestFirst', () => {
    it('takes the newest rows, newest first', () => {
        const rows = ['a', 'b', 'c'].map((value, i) => obs(`2026-01-0${i + 1} 00:00:00`, value));
        expect(latestFirst(rows, 2).map((r) => r.value)).toEqual(['c', 'b']);
    });
});

describe('instantLabel', () => {
    it('shows the day and the minute', () => {
        expect(instantLabel(Date.parse('2026-01-02T03:04:05Z'))).toBe('2026-01-02 03:04');
    });
});
