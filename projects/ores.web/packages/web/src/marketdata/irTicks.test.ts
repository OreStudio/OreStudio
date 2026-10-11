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
import { curvesCsv, curvesOf, pillarsOf, slopeOf, type PillarTick } from './irTicks.js';

const tick = (at: string, value: number): PillarTick => ({ at, value, uri: '' });

describe('the pillars of a curve', () => {
    it('are the template entries in sequence order', () => {
        const pillars = pillarsOf([
            { sequence_index: 1, start_tenor_code: 'SPOT', end_tenor_code: '5Y' },
            { sequence_index: 0, start_tenor_code: 'SPOT', end_tenor_code: '3M' },
        ]);
        expect(pillars.map((pillar) => pillar.end)).toEqual(['3M', '5Y']);
    });
});

describe('the curves in a run of ticks', () => {
    it('are the ticks that share a time, one for each pillar', () => {
        const curves = curvesOf(
            [tick('t1', 0.01), tick('t1', 0.02), tick('t2', 0.011), tick('t2', 0.021)],
            2,
        );
        expect(curves).toEqual([
            { at: 't1', rates: [0.01, 0.02] },
            { at: 't2', rates: [0.011, 0.021] },
        ]);
    });

    it('drop a batch with a pillar missing, which the buffer heard only in part', () => {
        const curves = curvesOf(
            [tick('t0', 0.02), tick('t1', 0.01), tick('t1', 0.02), tick('t2', 0.011)],
            2,
        );
        expect(curves.map((curve) => curve.at)).toEqual(['t1']);
    });

    it('are none when the template has no pillar', () => {
        expect(curvesOf([tick('t1', 0.01)], 0)).toEqual([]);
    });
});

describe('the slope of a curve', () => {
    it('is the long end over the short end in basis points', () => {
        expect(slopeOf({ at: 't', rates: [0.01, 0.015, 0.03] })).toBeCloseTo(200, 6);
        expect(slopeOf({ at: 't', rates: [] })).toBe(0);
    });
});

describe('the CSV of the curves', () => {
    it('has a column for each pillar and a row for each curve', () => {
        const csv = curvesCsv(
            'usd.sofr',
            [
                { start: 'SPOT', end: '3M' },
                { start: 'SPOT', end: '5Y' },
            ],
            [{ at: 't1', rates: [0.01, 0.02] }],
        );
        expect(csv).toBe('source,time,SPOT-3M,SPOT-5Y\nusd.sofr,t1,0.01,0.02\n');
    });
});
