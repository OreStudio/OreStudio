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
import type { Charts } from '@ores/contracts';
import { CHART, barsOf, burnupLine, recentVelocity, sprintLabel } from './charts.js';

const charts = (over: Partial<Charts> = {}): Charts => ({
    velocity: [],
    burnup: [],
    scope: 0,
    perDay: [],
    byState: [],
    ...over,
});

describe('barsOf', () => {
    it('scales the bars to the tallest, and an all-zero chart is flat', () => {
        const [low, high] = barsOf([
            { label: 'a', value: 1 },
            { label: 'b', value: 2 },
        ]);
        expect(high?.height).toBeCloseTo((low?.height ?? 0) * 2);
        expect(barsOf([{ label: 'a', value: 0 }])[0]?.height).toBe(0);
    });

    it('keeps every bar inside the chart', () => {
        for (const bar of barsOf(
            Array.from({ length: 8 }, (_, i) => ({ label: `${i}`, value: i })),
        )) {
            expect(bar.x).toBeGreaterThanOrEqual(CHART.left);
            expect(bar.x + bar.width).toBeLessThanOrEqual(CHART.width - CHART.right);
            expect(bar.y).toBeGreaterThanOrEqual(CHART.top);
        }
    });

    it('draws nothing for no data', () => {
        expect(barsOf([])).toEqual([]);
    });
});

describe('sprintLabel and recentVelocity', () => {
    it('shortens the sprint name', () => {
        expect(sprintLabel('sprint_27')).toBe('s27');
    });

    it('keeps the last eight sprints', () => {
        const velocity = Array.from({ length: 10 }, (_, i) => ({ sprint: `sprint_${i}`, done: i }));
        expect(recentVelocity(charts({ velocity })).map((point) => point.sprint)[0]).toBe(
            'sprint_2',
        );
    });
});

describe('burnupLine', () => {
    it('has no path without a done task, but keeps the scope line', () => {
        const line = burnupLine(charts({ scope: 3 }));
        expect(line.path).toBe('');
        expect(line.scopeY).not.toBeNull();
    });

    it('steps up at each date, from the first date to the last', () => {
        const line = burnupLine(
            charts({
                scope: 4,
                burnup: [
                    { date: '2026-10-02', done: 2 },
                    { date: '2026-10-04', done: 3 },
                ],
            }),
        );
        expect(line.first).toBe('2026-10-02');
        expect(line.last).toBe('2026-10-04');
        expect(line.path.startsWith('M ')).toBe(true);
        expect(line.path.match(/L/g)).toHaveLength(4);
        expect(line.max).toBe(4);
    });

    it('draws one date without dividing by zero', () => {
        const line = burnupLine(charts({ burnup: [{ date: '2026-10-02', done: 1 }] }));
        expect(line.path).not.toContain('NaN');
    });

    it('puts the scope line above a total that is below the scope', () => {
        const line = burnupLine(charts({ scope: 4, burnup: [{ date: '2026-10-02', done: 1 }] }));
        expect(line.scopeY).toBe(CHART.top);
    });
});
