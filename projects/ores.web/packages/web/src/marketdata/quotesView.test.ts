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
import type { Quote, QuoteFeed } from '../api/quotes.js';
import {
    ALL_VENDORS,
    allQuotesOf,
    bySeries,
    changeOf,
    feedKey,
    instrumentTypes,
    isStale,
    merged,
    shownQuotes,
} from './quotesView.js';

const GBPUSD = 'oresmd://fx/GBP?type=quote&instrument=fx_spot&quote=rate&ccy=USD';
const SOFR = (term: string): string =>
    `oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=3M&term=${term}`;
const SOFR_SERIES =
    'oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=3M';

const bloomberg: QuoteFeed = {
    provider: 'bloomberg',
    feed: 'b1',
    source: 'bloomberg-a',
    last_at: null,
};
const lseg: QuoteFeed = { provider: 'lseg', feed: 'l1', source: 'lseg-a', last_at: null };

function quote(
    feed: QuoteFeed,
    uri: string,
    value: string,
    at = '2026-11-19T12:00:00.000Z',
): Quote {
    const fx = uri.includes('fx_spot');
    return {
        provider: feed.provider,
        feed: feed.feed,
        source: feed.source,
        uri,
        value,
        previous: value,
        at,
        asset_class: fx ? 'fx' : 'interest_rates',
        instrument_type: fx ? 'fx_spot' : 'ir_swap',
        series_uri: fx ? GBPUSD : SOFR_SERIES,
        series: fx ? 'GBP USD' : 'USD 0D 3M',
        point: fx ? '' : (/term=(\w+)/.exec(uri)?.[1] ?? ''),
    };
}

const held = merged(new Map(), [
    quote(bloomberg, GBPUSD, '1.30810'),
    quote(lseg, GBPUSD, '1.30830'),
    quote(bloomberg, SOFR('5Y'), '4.1533'),
    quote(bloomberg, SOFR('10Y'), '4.2201'),
]);

const noFilter = { source: ALL_VENDORS, assetClass: '', instrumentType: '', find: '' };

describe('the quotes view', () => {
    it('puts a newer quote of a datum on a feed in place of the older one', () => {
        const next = merged(held, [
            { ...quote(bloomberg, GBPUSD, '1.30900'), previous: '1.30810' },
        ]);
        expect(next.size).toBe(4);
        const gbp = [...next.values()].find((q) => q.provider === 'bloomberg' && q.uri === GBPUSD);
        expect(gbp?.value).toBe('1.30900');
        expect(changeOf(gbp!)).toBeCloseTo(0.0009, 6);
    });

    it('shows only the feeds the session may watch', () => {
        const rows = shownQuotes(held, [bloomberg], noFilter);
        expect(rows.every((q) => q.provider === 'bloomberg')).toBe(true);
        expect(rows).toHaveLength(3);
    });

    it('shows one vendor when the rail picks it, and every vendor for all', () => {
        expect(
            shownQuotes(held, [bloomberg, lseg], { ...noFilter, source: feedKey(lseg) }),
        ).toHaveLength(1);
        expect(shownQuotes(held, [bloomberg, lseg], noFilter)).toHaveLength(4);
    });

    it('keeps the vendors of one datum together', () => {
        const rows = shownQuotes(held, [bloomberg, lseg], noFilter);
        const gbp = rows.map((q, i) => (q.uri === GBPUSD ? i : -1)).filter((i) => i >= 0);
        expect(gbp[1]! - gbp[0]!).toBe(1);
    });

    it('narrows by asset class, instrument type and text', () => {
        expect(
            shownQuotes(held, [bloomberg, lseg], { ...noFilter, assetClass: 'fx' }),
        ).toHaveLength(2);
        expect(
            shownQuotes(held, [bloomberg, lseg], { ...noFilter, instrumentType: 'ir_swap' }),
        ).toHaveLength(2);
        expect(shownQuotes(held, [bloomberg, lseg], { ...noFilter, find: '10y' })).toHaveLength(1);
        expect(instrumentTypes(held, 'interest_rates')).toEqual(['ir_swap']);
    });

    it('groups the points of a series under one row', () => {
        const groups = bySeries(shownQuotes(held, [bloomberg], noFilter));
        const sofr = groups.find((g) => g.quotes[0]?.series_uri === SOFR_SERIES);
        expect(sofr?.quotes).toHaveLength(2);
    });

    it('compares every vendor that may be watched on one datum, with the mid and the spread', () => {
        const all = allQuotesOf(held, [bloomberg, lseg], GBPUSD);
        expect(all.quotes.map((q) => q.provider)).toEqual(['bloomberg', 'lseg']);
        expect(all.mid).toBeCloseTo(1.3082, 6);
        expect(all.spread).toBeCloseTo(0.0002, 6);
        expect(allQuotesOf(held, [bloomberg], GBPUSD).quotes).toHaveLength(1);
    });

    it('calls a feed with no tick, or an old one, stale', () => {
        const now = Date.parse('2026-11-19T12:05:00.000Z');
        expect(isStale(null, now)).toBe(true);
        expect(isStale('2026-11-19T12:00:00.000Z', now)).toBe(true);
        expect(isStale('2026-11-19T12:04:30.000Z', now)).toBe(false);
        expect(isStale('2026-11-19 12:04:30.123456Z', now)).toBe(false);
    });
});
