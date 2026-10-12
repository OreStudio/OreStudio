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
import {
    SeriesChart,
    axisLabel,
    chartMaximum,
    drawable,
    drawableRuns,
    layerPath,
    linePath,
    type ChartAxis,
    type ChartSeries,
} from './SeriesChart.js';

const AXIS: ChartAxis = { startTime: '2026-10-11 10:00:00Z', bucketSeconds: 60, count: 6 };

function series(values: readonly (number | null)[], key = 'a'): ChartSeries {
    return { key, label: key, values, tone: 'accent' };
}

describe('the buckets a chart can draw', () => {
    it('breaks a run at a bucket with no sample, and does not read it as zero', () => {
        const samples = [2, 1, 0, 0, 3, 1];
        const runs = drawableRuns(samples, [series([5, 6, 0, 0, 7, 8])], 6);

        expect(runs).toEqual([
            [0, 1],
            [4, 5],
        ]);
    });

    it('treats an unknown figure as a gap too', () => {
        const samples = [1, 1, 1, 1];
        const entries = [series([1, null, 3, 4])];

        expect(drawable(samples, entries, 1)).toBe(false);
        expect(drawableRuns(samples, entries, 4)).toEqual([
            [0, 0],
            [2, 3],
        ]);
    });

    it('finds no run in a range with no sample at all', () => {
        expect(drawableRuns([0, 0, 0], [series([0, 0, 0])], 3)).toEqual([]);
    });
});

describe('the height of the plot', () => {
    it('stacks the layers of an area, and takes the tallest line for a line chart', () => {
        const samples = [1, 1, 1];
        const entries = [series([1, 4, 2], 'a'), series([2, 1, 5], 'b')];

        expect(chartMaximum('stacked-area', samples, entries, 3)).toBe(7);
        expect(chartMaximum('lines', samples, entries, 3)).toBe(5);
    });

    it('ignores the figures in a bucket that cannot be drawn', () => {
        const samples = [1, 0, 1];
        const entries = [series([2, 900, 3])];

        expect(chartMaximum('stacked-bars', samples, entries, 3)).toBe(3);
    });
});

describe('the paths', () => {
    it('draws one bucket as a short stroke, not as a point with no width', () => {
        const path = linePath([2, 2], [0, 0, 5, 0], 4, 5);

        expect(path).toMatch(/^M [\d.]+ [\d.]+ L [\d.]+ [\d.]+$/);
        const [x1, , x2] = path.replace(/[ML]/g, '').trim().split(/\s+/).map(Number);
        expect(x2).toBeGreaterThan(x1 ?? 0);
    });

    it('closes a layer between its lower and upper edges', () => {
        const path = layerPath([0, 2], [0, 0, 0], [1, 2, 3], 3, 3);

        expect(path.startsWith('M ')).toBe(true);
        expect(path.endsWith('Z')).toBe(true);
        // Three points along the top, and the same three back along the bottom.
        expect(path.split(' L ')).toHaveLength(6);
    });
});

describe('the axis labels', () => {
    it('states the time for a short bucket and the date as well for a wide one', () => {
        expect(axisLabel(AXIS, 3)).toBe('10:03');
        expect(axisLabel({ ...AXIS, bucketSeconds: 1800 }, 2)).toBe('10-11 11:00');
    });

    it('states nothing for a start it cannot read', () => {
        expect(axisLabel({ ...AXIS, startTime: 'later' }, 1)).toBe('');
    });
});

describe('the chart', () => {
    it('marks each stretch with no sample and draws nothing across it', () => {
        const html = renderToStaticMarkup(
            <SeriesChart
                kind="lines"
                axis={AXIS}
                samples={[2, 1, 0, 0, 3, 1]}
                series={[series([5, 6, 0, 0, 7, 8])]}
                caption="Hosts"
            />,
        );

        // One band for the gap, and one line for each run on either side of it.
        expect(html.match(/data-gap/g)).toHaveLength(1);
        expect(html.match(/<path/g)).toHaveLength(2);
        expect(html).toContain('<title>Hosts</title>');
    });

    it('draws a bar only for a bucket with a sample', () => {
        const html = renderToStaticMarkup(
            <SeriesChart
                kind="stacked-bars"
                axis={AXIS}
                samples={[1, 0, 1, 1, 0, 1]}
                series={[series([3, 9, 4, 0, 9, 2])]}
                caption="Finished"
            />,
        );

        // Buckets 1 and 4 hold no sample. Bucket 3 holds one and a figure of zero,
        // which draws no height.
        const bars = html.match(/<rect(?![^>]*data-gap)/g) ?? [];
        expect(bars).toHaveLength(3);
        expect(html.match(/data-gap/g)).toHaveLength(2);
    });

    it('names each series in a legend', () => {
        const html = renderToStaticMarkup(
            <SeriesChart
                kind="stacked-area"
                axis={AXIS}
                samples={[1, 1, 1, 1, 1, 1]}
                series={[series([1, 1, 1, 1, 1, 1], 'unsent'), series([2, 2, 2, 2, 2, 2], 'busy')]}
                caption="Load"
            />,
        );

        expect(html).toContain('unsent');
        expect(html).toContain('busy');
    });
});
