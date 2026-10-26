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
import { arrivalPositions, factorPath, multiplierText } from './marketRaster.js';

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

describe('the factor a market ran at', () => {
    it('draws a step, not a slope, between two readings', () => {
        // Calm at 0.5 from the left edge to halfway, then active at 2 from there.
        const path = factorPath(
            [
                { at: NOW - 60_000, multiplier: 0.5 },
                { at: NOW - 30_000, multiplier: 2 },
            ],
            60,
            NOW,
        );

        // Calm sits at the foot and active at the top, because a larger factor is higher on the picture.
        expect(path).toBe('M 0.00 1.000 H 50.00 V 0.000 H 100');
    });

    it('draws a flat line through the middle for a factor that never moved', () => {
        expect(
            factorPath(
                [
                    { at: NOW - 60_000, multiplier: 2 },
                    { at: NOW - 10_000, multiplier: 2 },
                ],
                60,
                NOW,
            ),
        ).toBe('M 0.00 0.500 H 83.33 H 100');
    });

    it('draws nothing for a read older than the window', () => {
        expect(factorPath([{ at: NOW - 120_000, multiplier: 2 }], 60, NOW)).toBe('');
    });

    it('draws nothing before the first read', () => {
        expect(factorPath([], 60, NOW)).toBe('');
    });
});
