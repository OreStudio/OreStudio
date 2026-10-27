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
 */

import { describe, expect, it } from 'vitest';
import type { TraceEvent, TraceFeed } from '../../api/client.js';
import {
    commandLog,
    exponentialQuantiles,
    feedFit,
    paceSteps,
    tauPositions,
    tauSpan,
    traceBands,
} from './marketTrace.js';

const NOW = Date.parse('2026-10-27T18:02:00Z');

function event(secondsAgo: number, over: Partial<TraceEvent> = {}): TraceEvent {
    return {
        kind: 'tick',
        sourceName: 'feed.a',
        at: new Date(NOW - secondsAgo * 1000).toISOString(),
        tau: 120 - secondsAgo,
        operationalGap: 1,
        wallGap: 1,
        compensatedGap: 1,
        heat: 0,
        multiplier: 0.5,
        pace: 0.5,
        state: 'calm',
        ...over,
    };
}

describe('the pace from the trace', () => {
    it('steps from one decision to the next, split into the regime and the heat', () => {
        const chart = paceSteps(
            [event(60, { multiplier: 0.5, pace: 0.6 }), event(30, { multiplier: 2, pace: 2.5 })],
            120,
            NOW,
        );
        expect(chart.steps).toEqual([
            { from: 50, to: 75, multiplier: 0.5, heat: expect.closeTo(0.1, 9) },
            { from: 75, to: 100, multiplier: 2, heat: expect.closeTo(0.5, 9) },
        ]);
        expect(chart.peak).toBe(2.5);
    });

    it('leaves out a decision older than the window', () => {
        expect(paceSteps([event(200)], 120, NOW).steps).toEqual([]);
    });
});

describe('the state bands from the trace', () => {
    it('joins decisions in one state and turns at a transition', () => {
        const bands = traceBands(
            [
                event(90),
                event(60),
                event(30, { kind: 'transition', state: 'active' }),
                event(15, { state: 'active' }),
            ],
            120,
            NOW,
        );
        expect(bands).toEqual([
            { from: 25, to: 75, state: 'calm' },
            { from: 75, to: 100, state: 'active' },
        ]);
    });
});

describe('the command log', () => {
    it('keeps pins and changes of the feeds, newest first, and no ticks or turns', () => {
        const log = commandLog([
            event(50, { kind: 'add', sourceName: 'feed.b' }),
            event(40),
            event(30, { kind: 'transition', sourceName: '' }),
            event(20, { kind: 'pin', sourceName: '' }),
        ]);
        expect(log.map((e) => e.kind)).toEqual(['pin', 'add']);
    });
});

describe('the raster in market time', () => {
    it('places a periodic feed’s ticks evenly when they are a unit of tau apart', () => {
        const events = [0, 1, 2, 3, 4].map((tau) => event(10 - tau, { tau }));
        const span = tauSpan(events);
        expect(span).toEqual({ from: 0, to: 4 });
        expect(tauPositions(events, 'feed.a', span ?? { from: 0, to: 1 })).toEqual([
            0, 0.25, 0.5, 0.75, 1,
        ]);
    });

    it('has no span before the trace moved', () => {
        expect(tauSpan([])).toBeUndefined();
        expect(tauSpan([event(1), event(1)])).toBeUndefined();
    });
});

describe('a feed’s fit to its law', () => {
    const feed: TraceFeed = {
        sourceName: 'feed.a',
        mode: 'poisson',
        rate: 2,
        sensitivity: 1.5,
        effectiveRate: 3,
        phase: 0,
        excess: 0,
    };

    it('expects the rate times the sensitivity times the tau the window spans', () => {
        const events = [
            event(9, { compensatedGap: 0.4 }),
            event(5, { compensatedGap: 2 }),
            event(1, { compensatedGap: 0.7 }),
        ];
        const fit = feedFit(events, feed, { from: 100, to: 110 });
        expect(fit.observed).toBe(3);
        expect(fit.expected).toBe(30);
        // The first tick's gap began before the window, so it is left out.
        expect(fit.gaps).toEqual([0.7, 2]);
    });

    it('puts each gap against the Exp(1) quantile of its rank', () => {
        const points = exponentialQuantiles([0.5, 1.5]);
        expect(points[0]?.expected).toBeCloseTo(-Math.log(0.75), 12);
        expect(points[1]?.expected).toBeCloseTo(-Math.log(0.25), 12);
        expect(points.map((p) => p.observed)).toEqual([0.5, 1.5]);
    });
});
