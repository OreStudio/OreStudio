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
import {
    TIME_TICKS,
    VALUE_TICKS,
    TimeChart,
    chartTop,
    drawableWindow,
    instantOf,
    seriesPath,
    timeLabel,
    timeTicks,
    xOf,
    yOf,
} from './TimeChart.js';

/**
 * The chart of a size over time: the axes it states, and the points it refuses
 * to move.
 *
 * The window is the read's answer, so what is checked is that the picture is
 * drawn inside it: a point outside is not drawn, a window that cannot be drawn
 * draws nothing, and the labels state the instants the window holds.
 */

const WINDOW = { from: '2026-11-27 00:00:00.000000Z', to: '2026-11-28 00:00:00.000000Z' };

const POINTS = [
    { at: '2026-11-27 01:00:00.000000Z', value: 1000 },
    { at: '2026-11-27 13:00:00.000000Z', value: 2000 },
    { at: '2026-11-28 00:00:00.000000Z', value: 3000 },
];

describe('the window a series covers', () => {
    it('reads the instants the read stated', () => {
        expect(instantOf('2026-11-27 00:00:00.000000Z')).toBe(Date.UTC(2026, 10, 27, 0, 0, 0));
        expect(Number.isNaN(instantOf('last week'))).toBe(true);
    });

    it('is drawable only when the read stated two instants in order', () => {
        expect(drawableWindow(WINDOW)).toBe(true);
        expect(drawableWindow({ from: WINDOW.to, to: WINDOW.from })).toBe(false);
        expect(drawableWindow({ from: WINDOW.from, to: WINDOW.from })).toBe(false);
        expect(drawableWindow({ from: '', to: WINDOW.to })).toBe(false);
    });

    it('draws nothing at all for a window it cannot state', () => {
        const html = renderToStaticMarkup(
            <TimeChart
                window={{ from: '', to: '' }}
                points={POINTS}
                format={String}
                caption="nothing"
            />,
        );
        expect(html).toBe('');
    });
});

describe('the scale a chart states', () => {
    it('puts zero at the foot and the highest measurement at the head', () => {
        const top = chartTop(POINTS);
        expect(top).toBe(3000);
        expect(yOf(0, top)).toBeGreaterThan(yOf(top, top));
        expect(yOf(top, top)).toBeLessThan(yOf(0, top));
    });

    it('labels the value axis from zero to the top', () => {
        const html = renderToStaticMarkup(
            <TimeChart window={WINDOW} points={POINTS} format={String} caption="a store" />,
        );
        expect(VALUE_TICKS).toBe(5);
        for (const label of ['0', '750', '1500', '2250', '3000']) {
            expect(html).toContain(`>${label}</text>`);
        }
    });

    it('labels the time axis with the instants the window holds, in UTC', () => {
        expect(TIME_TICKS).toBe(4);
        expect(timeTicks(WINDOW).map(timeLabel)).toEqual([
            '11-27 00:00',
            '11-27 08:00',
            '11-27 16:00',
            '11-28 00:00',
        ]);
    });
});

describe('the points a chart draws', () => {
    it('places a mark inside the window and none outside it', () => {
        const from = instantOf(WINDOW.from);
        const to = instantOf(WINDOW.to);
        expect(xOf(from, { from, to })).toBeDefined();
        expect(xOf(instantOf('2026-11-27 01:00:00.000000Z'), { from, to })).not.toBeNull();
        expect(xOf(instantOf('2026-11-26 23:00:00.000000Z'), { from, to })).toBeNull();
        expect(xOf(instantOf('2026-11-28 01:00:00.000000Z'), { from, to })).toBeNull();
    });

    it('draws no line through fewer than two points', () => {
        const from = instantOf(WINDOW.from);
        const to = instantOf(WINDOW.to);
        expect(seriesPath([POINTS[0]!], { from, to }, 3000)).toBe('');
    });

    it('draws one mark per measurement inside the window', () => {
        const html = renderToStaticMarkup(
            <TimeChart window={WINDOW} points={POINTS} format={String} caption="a store" />,
        );
        expect(html.match(/<circle/g) ?? []).toHaveLength(3);
        expect(html).toContain('<path');
        expect(html).toContain('a store');
    });
});
