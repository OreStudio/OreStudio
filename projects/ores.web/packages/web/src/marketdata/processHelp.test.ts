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
import { runFeeds } from './feedRuns.js';
import { bandWidth, halfLifeMinutes, PROFILES, processTypeOf, profileOf } from './processHelp.js';
import { emaOf, instantOf, statsOf, ticksCsv } from './ticks.js';

describe('the quant help', () => {
    it('gives the half-life in minutes at the feed rate, and none when κ does not revert', () => {
        expect(halfLifeMinutes(Math.log(2), 60)).toBeCloseTo(1);
        expect(halfLifeMinutes(Math.log(2) / 10, 3600)).toBeCloseTo(10 / 60);
        expect(halfLifeMinutes(0, 3600)).toBeUndefined();
        expect(halfLifeMinutes(0.1, 0)).toBeUndefined();
    });

    it('gives the band a mean reverting price settles in as σ/√(2κ)', () => {
        expect(bandWidth(0.5, 0.01)).toBeCloseTo(0.01);
        expect(bandWidth(0, 0.01)).toBeUndefined();
    });

    it('names the profile a deviation is, and custom for any other', () => {
        expect(profileOf(String(PROFILES.normal))).toBe('normal');
        expect(profileOf('0')).toBe('flat');
        expect(profileOf('0.0123')).toBe('custom');
        expect(profileOf('')).toBe('custom');
    });

    it('reads an unknown engine as geometric, the default', () => {
        expect(processTypeOf('ornstein_uhlenbeck')).toBe('ornstein_uhlenbeck');
        expect(processTypeOf(undefined)).toBe('geometric');
    });

    it('reads a wire timestamp as an instant', () => {
        expect(instantOf('2026-10-11 09:00:00Z')).toBe(Date.UTC(2026, 9, 11, 9, 0, 0));
    });
});

const A = '11111111-1111-4111-8111-111111111111';
const B = '22222222-2222-4222-8222-222222222222';
const C = '33333333-3333-4333-8333-333333333333';

describe('the ticks', () => {
    const ticks = [
        { at: '2026-10-11 09:00:00Z', value: 1.1 },
        { at: '2026-10-11 09:01:00Z', value: 1.3 },
        { at: '2026-10-11 09:02:00Z', value: 1.2 },
    ];

    it('says the last tick, the move from the first, and the high and low', () => {
        const stats = statsOf(ticks);
        expect(stats?.last).toEqual(ticks[2]);
        expect(stats?.move).toBeCloseTo(0.1);
        expect(stats?.high).toBe(1.3);
        expect(stats?.low).toBe(1.1);
        expect(stats?.count).toBe(3);
        expect(statsOf([])).toBeUndefined();
    });

    it('averages each value 2/(period+1) of the way from the last average', () => {
        expect(emaOf([1, 3, 3], 3)).toEqual([1, 2, 2.5]);
        expect(emaOf([])).toEqual([]);
    });

    it('writes the ticks as CSV with the feed on every row', () => {
        expect(ticksCsv('synthetic.eurusd', ticks.slice(0, 2))).toBe(
            [
                'source_name,observation_time,value',
                'synthetic.eurusd,2026-10-11 09:00:00Z,1.1',
                'synthetic.eurusd,2026-10-11 09:01:00Z,1.3',
            ].join('\n'),
        );
    });
});

describe('starting and stopping many feeds', () => {
    it('calls for every feed, even after a refusal, and says why each refused one was refused, by its name', async () => {
        const called: string[] = [];
        const outcome = await runFeeds(
            'start',
            [
                { id: A, label: 'feed.a' },
                { id: B, label: 'feed.b' },
                { id: C, label: 'feed.c' },
            ],
            async (id) => {
                called.push(id);
                if (id === B) {
                    throw new Error(`Feed config is not enabled: ${B}`);
                }
            },
        );
        expect(called).toEqual([A, B, C]);
        expect(outcome).toEqual({
            action: 'start',
            done: 2,
            refused: [{ label: 'feed.b', message: 'Feed config is not enabled: feed.b' }],
        });
    });
});
