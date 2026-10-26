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
import {
    arrivalPositions,
    curvePath,
    multiplierText,
    rateCurves,
    stateBands,
} from './marketRaster.js';

const NOW = Date.parse('2026-10-26T12:00:00Z');

/** An instant, as a feed stamps its ticks, at a number of seconds before now. */
function at(secondsAgo: number): string {
    return new Date(NOW - secondsAgo * 1000).toISOString();
}

describe('a raster of tick arrivals', () => {
    it('places the newest tick at the right edge and the oldest at the left', () => {
        expect(arrivalPositions([at(60), at(0)], 60, NOW)).toEqual([0, 1]);
    });

    it('places a tick halfway through the window in the middle', () => {
        expect(arrivalPositions([at(30)], 60, NOW)).toEqual([0.5]);
    });

    it('drops a tick that has left the window', () => {
        expect(arrivalPositions([at(61), at(10)], 60, NOW)).toEqual([expect.closeTo(5 / 6, 5)]);
    });

    it('drops a tick stamped after the clock the picture is drawn at', () => {
        expect(arrivalPositions([at(-5), at(10)], 60, NOW)).toEqual([expect.closeTo(5 / 6, 5)]);
    });

    it('drops a tick whose time cannot be read', () => {
        expect(arrivalPositions(['not a time', at(10)], 60, NOW)).toEqual([
            expect.closeTo(5 / 6, 5),
        ]);
    });

    it('draws nothing for a window of no width', () => {
        expect(arrivalPositions([at(10)], 0, NOW)).toEqual([]);
    });
});

describe('the multiplier as a screen writes it', () => {
    it('writes a whole multiplier without decimals', () => {
        expect(multiplierText(2)).toBe('×2');
    });

    it('writes a fraction', () => {
        expect(multiplierText(0.5)).toBe('×0.5');
    });

    it('keeps two decimals and no more', () => {
        expect(multiplierText(1.23456)).toBe('×1.23');
    });
});

describe('the stretch a market held a state', () => {
    it('draws a band from the left edge to the first reading, then a band per reading', () => {
        expect(
            stateBands(
                [
                    { at: NOW - 60_000, state: 'calm' },
                    { at: NOW - 30_000, state: 'active' },
                ],
                60,
                NOW,
            ),
        ).toEqual([
            { from: 0, to: 0, state: 'calm' },
            { from: 0, to: 50, state: 'calm' },
            { from: 50, to: 100, state: 'active' },
        ]);
    });

    it('draws nothing before the tab has seen a state', () => {
        expect(stateBands([], 60, NOW)).toEqual([]);
    });

    it('draws nothing for a reading older than the window', () => {
        expect(stateBands([{ at: NOW - 120_000, state: 'active' }], 60, NOW)).toEqual([]);
    });
});

describe('how fast the market ticked', () => {
    /** One feed a class, ticking once a second for a window of ten seconds. */
    function everySecond(
        assetClass: string,
        running = true,
    ): {
        assetClass: string;
        running: boolean;
        ticks: string[];
    } {
        return {
            assetClass,
            running,
            ticks: Array.from({ length: 10 }, (_, second) => at(10 - second)),
        };
    }

    it('reads a feed that ticks once a second as one tick a second', () => {
        const chart = rateCurves([everySecond('fx')], 10, NOW, 10);
        expect(chart.curves).toEqual([{ assetClass: 'fx', points: new Array(10).fill(1) }]);
        expect(chart.peak).toBe(1);
    });

    it('averages a class over the feeds that run, so a bigger class is not busier', () => {
        const chart = rateCurves([everySecond('fx'), everySecond('fx')], 10, NOW, 10);
        expect(chart.peak).toBe(1);
    });

    it('draws no curve for a class whose feeds are all stopped', () => {
        expect(rateCurves([everySecond('fx', false)], 10, NOW, 10).curves).toEqual([]);
    });

    it('keeps a curve per asset class', () => {
        const chart = rateCurves([everySecond('fx'), everySecond('interest_rates')], 10, NOW, 10);
        expect(chart.curves.map((curve) => curve.assetClass).sort()).toEqual([
            'fx',
            'interest_rates',
        ]);
    });
});

describe('a rate curve as a path', () => {
    it('draws a flat line at the foot for a curve with no rate', () => {
        expect(curvePath([0, 0], 0)).toBe('M 25.00 1.000 L 75.00 1.000');
    });

    it('puts the peak at the top of the box', () => {
        expect(curvePath([0, 2], 2)).toBe('M 25.00 1.000 L 75.00 0.000');
    });

    it('draws nothing for no points', () => {
        expect(curvePath([], 1)).toBe('');
    });
});
