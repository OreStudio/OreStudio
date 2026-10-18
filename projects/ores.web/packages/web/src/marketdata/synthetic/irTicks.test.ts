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
import { wireTimestamp, type WireTimestamp } from '@ores/wire-protocol/browser';
import { curvesCsv, curvesOf, pillarsOf, slopeOf, type PillarTick } from './irTicks.js';
import { curveStyle } from './IrTicksPanel.js';

/** The instants of a feed's batches, which the server writes in the one wire spelling. */
const T0 = wireTimestamp('2026-10-11 08:59:00Z');
const T1 = wireTimestamp('2026-10-11 09:00:00Z');
const T2 = wireTimestamp('2026-10-11 09:01:00Z');

const tick = (at: WireTimestamp, value: number): PillarTick => ({ at, value, uri: '' });

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
            [tick(T1, 0.01), tick(T1, 0.02), tick(T2, 0.011), tick(T2, 0.021)],
            2,
        );
        expect(curves).toEqual([
            { at: T1, rates: [0.01, 0.02] },
            { at: T2, rates: [0.011, 0.021] },
        ]);
    });

    it('drop a batch with a pillar missing, which the buffer heard only in part', () => {
        const curves = curvesOf(
            [tick(T0, 0.02), tick(T1, 0.01), tick(T1, 0.02), tick(T2, 0.011)],
            2,
        );
        expect(curves.map((curve) => curve.at)).toEqual([T1]);
    });

    it('are none when the template has no pillar', () => {
        expect(curvesOf([tick(T1, 0.01)], 0)).toEqual([]);
    });
});

describe('the slope of a curve', () => {
    it('is the long end over the short end in basis points', () => {
        expect(slopeOf({ at: T1, rates: [0.01, 0.015, 0.03] })).toBeCloseTo(200, 6);
        expect(slopeOf({ at: T1, rates: [] })).toBe(0);
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
            [{ at: T1, rates: [0.01, 0.02] }],
        );
        expect(csv).toBe('source,time,SPOT-3M,SPOT-5Y\nusd.sofr,2026-10-11 09:00:00Z,0.01,0.02\n');
    });
});

describe('how a curve of the history is drawn', () => {
    it('makes the newest curve bold and green, as the Qt chart does', () => {
        expect(curveStyle(4, 5)).toEqual({ colour: '#22C55E', width: 2.5, opacity: 1 });
    });

    it('fades the older curves with age, and no curve below 40 of 255', () => {
        const oldest = curveStyle(0, 5);
        const middle = curveStyle(2, 5);
        expect(oldest.colour).toBe('#8A9BAE');
        expect(oldest.width).toBe(1);
        expect(oldest.opacity).toBeCloseTo(40 / 255, 6);
        expect(middle.opacity).toBeCloseTo(((255 * 3) / 5 - 60) / 255, 6);
        expect(middle.opacity).toBeGreaterThan(oldest.opacity);
    });
});
