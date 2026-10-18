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
import type { RecordRow } from '../api/client.js';
import {
    formatText,
    formatValue,
    instantLabel,
    latestFirst,
    MAX_PLACES,
    placesOf,
    pointsOf,
    windowOf,
} from './seriesView.js';

const obs = (at: string, value: string) => ({
    at: wireTimestamp(at) as WireTimestamp,
    value,
    source: 'feed',
});

describe('pointsOf', () => {
    it('reads the instant and the value, and keeps the order', () => {
        const points = pointsOf([
            obs('2026-01-01 10:00:00Z', '1.25'),
            obs('2026-01-01 11:00:00Z', '1.5'),
        ]);
        expect(points.map((p) => p.value)).toEqual([1.25, 1.5]);
        expect(points[0]!.at).toBeLessThan(points[1]!.at);
    });

    it('leaves out a value that is not a number, rather than draw it as zero', () => {
        const points = pointsOf([
            obs('2026-01-01 10:00:00Z', ''),
            obs('2026-01-01 11:00:00Z', 'n/a'),
        ]);
        expect(points).toEqual([]);
    });

    it('reads the wire instant as UTC, so the viewer zone does not shift it', () => {
        const [point] = pointsOf([obs('2026-01-01 10:00:00Z', '1')]);
        expect(point!.at).toBe(Date.UTC(2026, 0, 1, 10, 0, 0));
    });

    it('refuses an instant that is not the wire shape rather than placing it', () => {
        const malformed = { at: 'yesterday', value: '1', source: 'feed' } as never;
        expect(() => pointsOf([malformed])).toThrow(TypeError);
    });
});

describe('latestFirst', () => {
    it('takes the newest rows, newest first', () => {
        const rows = ['a', 'b', 'c'].map((value, i) => obs(`2026-01-0${i + 1} 00:00:00Z`, value));
        expect(latestFirst(rows, 2).map((r) => r.value)).toEqual(['c', 'b']);
    });
});

describe('instantLabel', () => {
    it('shows the day and the minute', () => {
        expect(instantLabel(Date.parse('2026-01-02T03:04:05Z'))).toBe('2026-01-02 03:04');
    });
});

describe('windowOf', () => {
    it('reads the window an address names', () => {
        expect(windowOf('hour')).toBe('hour');
        expect(windowOf('year')).toBe('year');
        expect(windowOf('all')).toBe('all');
    });

    it('draws the page as it opens when the address names no window or one it does not offer', () => {
        expect(windowOf(null)).toBe('all');
        expect(windowOf('')).toBe('all');
        expect(windowOf('fortnight')).toBe('all');
    });
});

describe('placesOf', () => {
    const conventions = [
        { version: 1, pair_code: 'AUD/USD', decimal_places: 4 },
        { version: 1, pair_code: 'USD/JPY', decimal_places: 2 },
    ] as unknown as RecordRow[];
    const spot = (
        scope: string,
        ccy: string,
        quote_type = 'rate',
        instrument_type = 'fx_spot',
    ) => ({
        scope,
        instrument_type,
        quote_type,
        fields: [{ name: 'ccy', text: ccy }],
    });

    it('reads the places of the convention of the pair', () => {
        expect(placesOf(spot('AUD', 'USD'), conventions)).toBe(4);
        expect(placesOf(spot('USD', 'JPY'), conventions)).toBe(2);
    });

    it('keeps the plain format for a pair with no convention', () => {
        expect(placesOf(spot('USD', 'COP'), conventions)).toBeUndefined();
        expect(placesOf(spot('AUD', 'USD'), [])).toBeUndefined();
    });

    it('keeps the plain format for a series that is not an FX spot rate', () => {
        expect(placesOf(spot('AUD', 'USD', 'price'), conventions)).toBeUndefined();
        expect(placesOf(spot('AUD', 'USD', 'rate', 'fx_fwd'), conventions)).toBeUndefined();
    });

    it('accepts the largest number of places and rejects one more', () => {
        const at = (places: number) =>
            [
                { version: 1, pair_code: 'AUD/USD', decimal_places: places },
            ] as unknown as RecordRow[];
        expect(placesOf(spot('AUD', 'USD'), at(MAX_PLACES))).toBe(MAX_PLACES);
        expect(placesOf(spot('AUD', 'USD'), at(MAX_PLACES + 1))).toBeUndefined();
    });

    it('ignores a convention whose places are not a sane number', () => {
        const odd = [
            { version: 1, pair_code: 'AUD/USD', decimal_places: 99 },
        ] as unknown as RecordRow[];
        expect(placesOf(spot('AUD', 'USD'), odd)).toBeUndefined();
    });
});

describe('formatValue and formatText', () => {
    it('rounds to the places of the convention', () => {
        expect(formatValue(0.71955, 4)).toBe('0.7196');
        expect(formatValue(149.5, 2)).toBe('149.50');
        expect(formatText('0.7230', 4)).toBe('0.7230');
        expect(formatText('1.23456', 2)).toBe('1.23');
    });

    it('uses the plain format of up to seven digits with no places', () => {
        expect(formatValue(0.123456789, undefined)).toBe('0.1234568');
        expect(formatText('0.123456789', undefined)).toBe('0.123456789');
    });

    it('leaves a text that is not a number as it is', () => {
        expect(formatText('n/a', 4)).toBe('n/a');
        expect(formatText('', 4)).toBe('');
    });
});
