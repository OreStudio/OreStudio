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
import type { FxForwardLadder } from '../api/client.js';
import {
    asOfInstant,
    daysBetween,
    forwardForwardPoints,
    forwardForwardTenors,
    rowsFromStart,
    usdPairs,
} from './fxForwardLadder.js';

const row = (tenor: string, near: string, value: string, rate: number, swap: number) => ({
    tenor,
    near_date: near,
    value_date: value,
    base_discount: 1,
    quote_discount: 1,
    forward_rate: rate,
    forward_points: 0,
    swap_points: swap,
});

const source = {
    currency: 'EUR',
    curve_name: 'EUR-ESTR',
    output_series_id: '',
    publish_id: '',
    as_of: '',
    published_by: '',
};

const ladder: FxForwardLadder = {
    pair_code: 'EUR/USD',
    as_of: '',
    spot: 1.085,
    spot_observed_at: '',
    spot_date: '2026-11-11',
    spot_days: 2,
    pip_factor: 0.0001,
    decimal_places: 4,
    base_curve: source,
    quote_curve: source,
    rows: [
        row('ON', '2026-11-09', '2026-11-10', 1.08493, 0.67),
        row('TN', '2026-11-10', '2026-11-11', 1.085, 0.67),
        row('SP', '2026-11-11', '2026-11-11', 1.085, 0),
        row('SN', '2026-11-11', '2026-11-12', 1.08507, 0.67),
        row('1M', '2026-11-11', '2026-12-11', 1.08699, 19.9),
    ],
};

describe('the ladder start', () => {
    it('leaves every row alone from spot', () => {
        const rows = rowsFromStart(ladder, 'spot');
        expect(rows.every((r) => !r.moved)).toBe(true);
        expect(rows.at(-1)?.swap_points).toBe(19.9);
    });

    it('measures later tenors from today and keeps the short dates', () => {
        const rows = rowsFromStart(ladder, 'today');
        const month = rows.at(-1);
        expect(month?.moved).toBe(true);
        expect(month?.near_date).toBe('2026-11-09');
        expect(month?.swap_points).toBeCloseTo((1.08699 - (1.08493 - 0.67 * 0.0001)) / 0.0001, 8);
        expect(rows.find((r) => r.tenor === 'TN')?.moved).toBe(false);
        expect(rows.find((r) => r.tenor === 'TN')?.swap_points).toBe(0.67);
    });

    it('measures later tenors from tomorrow', () => {
        const month = rowsFromStart(ladder, 'tomorrow').at(-1);
        expect(month?.near_date).toBe('2026-11-10');
        expect(month?.swap_points).toBeCloseTo((1.08699 - 1.08493) / 0.0001, 8);
    });
});

describe('the forward-forward', () => {
    it('offers only the tenors that run from spot', () => {
        expect(forwardForwardTenors(rowsFromStart(ladder, 'spot')).map((r) => r.tenor)).toEqual([
            '1M',
        ]);
    });

    it('states the points between two tenors, and none backwards', () => {
        const near = row('3M', '', '2027-02-11', 1.09, 0);
        const far = row('6M', '', '2027-05-11', 1.095, 0);
        expect(forwardForwardPoints(near, far, 0.0001)).toBeCloseTo(50, 8);
        expect(forwardForwardPoints(far, near, 0.0001)).toBeUndefined();
    });
});

describe('the helpers', () => {
    it('keeps only USD pairs, in order', () => {
        expect(usdPairs(['USD/JPY', 'EUR/GBP', 'EUR/USD'])).toEqual(['EUR/USD', 'USD/JPY']);
    });

    it('reads an as-of to the end of its day', () => {
        expect(asOfInstant('2026-11-09')).toBe('2026-11-09 23:59:59Z');
    });

    it('counts calendar days', () => {
        expect(daysBetween('2027-02-11', '2027-05-11')).toBe(89);
    });
});
