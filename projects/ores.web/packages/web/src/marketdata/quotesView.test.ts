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
    agreementOf,
    boardsOf,
    byTenor,
    compare,
    CONSENSUS,
    differenceOf,
    differenceText,
    feedKey,
    gridOf,
    isStale,
    merged,
    onTimeAxis,
    shownSeries,
    tenorsOf,
    valueText,
    vendorsOf,
    type Board,
    type Series,
} from './quotesView.js';

const NOW = Date.parse('2026-11-19T12:00:30.000Z');
const AT = '2026-11-19 12:00:00.000000Z';
const OLD = '2026-11-19 10:00:00.000000Z';

const feed = (provider: string, precedence: number): QuoteFeed => ({
    provider,
    feed: `${provider}-1`,
    precedence,
    source: `${provider}-a`,
    last_at: AT,
});
const bloomberg = feed('bloomberg', 1);
const ice = feed('ice', 3);
const tradeweb = feed('tradeweb', 4);
const demo = feed('demo', 7);
const derived = feed('derived', 30);
const vendors = vendorsOf([demo, tradeweb, derived, ice, bloomberg]);
const all = new Set(vendors.map((v) => v.key));

function spot(f: QuoteFeed, pair: string, value: string, at = AT): Quote {
    const [base, quote] = pair.split('/') as [string, string];
    return {
        provider: f.provider,
        feed: f.feed,
        source: f.source,
        uri: `oresmd://fx/${base}?type=quote&instrument=fx_spot&quote=rate&ccy=${quote}`,
        value,
        previous: value,
        at,
        asset_class: 'fx',
        instrument_type: 'fx_spot',
        series_uri: `oresmd://fx/${base}?type=series&instrument=fx_spot&quote=rate&ccy=${quote}`,
        series: `${base} ${quote}`,
        point: '',
    };
}

function swap(f: QuoteFeed, term: string, value: string): Quote {
    return {
        provider: f.provider,
        feed: f.feed,
        source: f.source,
        uri: `oresmd://ir/USD?type=quote&instrument=ir_swap&tenor=1D&term=${term}`,
        value,
        previous: value,
        at: AT,
        asset_class: 'interest_rates',
        instrument_type: 'ir_swap',
        series_uri: 'oresmd://ir/USD?type=series&instrument=ir_swap&tenor=1D',
        series: 'USD 0D 1D',
        point: term,
    };
}

function vol(f: QuoteFeed, expiry: string, strike: string, value: string, at = AT): Quote {
    return {
        provider: f.provider,
        feed: f.feed,
        source: f.source,
        uri: `oresmd://fx/EUR?type=quote&instrument=fx_option&ccy=USD&expiry=${expiry}&strike=${strike}`,
        value,
        previous: value,
        at,
        asset_class: 'fx',
        instrument_type: 'fx_option',
        series_uri: 'oresmd://fx/EUR?type=series&instrument=fx_option&ccy=USD',
        series: 'EUR USD',
        point: `${expiry} ${strike}`,
    };
}

const held = merged(new Map(), [
    spot(bloomberg, 'EUR/USD', '1.10618'),
    spot(ice, 'EUR/USD', '1.10615'),
    spot(tradeweb, 'EUR/USD', '1.10620'),
    spot(demo, 'EUR/USD', '0.86679'),
    spot(bloomberg, 'USD/CHF', '0.77886'),
    spot(demo, 'USD/CHF', '164.37'),
    spot(derived, 'EUR/GBP', '0.79233', OLD),
    swap(bloomberg, '10Y', '0.04390'),
    swap(bloomberg, '1M', '0.03965'),
    swap(bloomberg, '1Y', '0.04091'),
    swap(bloomberg, '2M', '0.03980'),
    swap(bloomberg, '1W', '0.03950'),
    vol(bloomberg, '1M', '25RR', '-0.0042'),
    vol(bloomberg, '1M', 'ATM', '0.0723'),
    vol(bloomberg, '1W', 'ATM', '0.0717'),
    vol(ice, '1M', 'ATM', '0.0712'),
    vol(tradeweb, '1M', 'ATM', '0.0735'),
    vol(ice, '1W', 'ATM', '0.0710'),
]);

const boards = boardsOf(held, vendors);
const board = (key: string): Board => boards.find((b) => b.key === key)!;
const series = (key: string, name: string): Series =>
    board(key).series.find((s) => s.name === name)!;

describe('the vendors', () => {
    it('are listed in the order their providers are trusted', () => {
        expect(vendors.map((v) => v.provider)).toEqual([
            'bloomberg',
            'ice',
            'tradeweb',
            'demo',
            'derived',
        ]);
    });
});

describe('the boards', () => {
    it('are one asset class and one shape each: quotes, curves and surfaces', () => {
        expect(boards.map((b) => b.key)).toEqual([
            'fx.quote',
            'fx.surface',
            'interest_rates.curve',
        ]);
    });

    it('hold one row per series, each vendor beside the others on its point', () => {
        const eurusd = series('fx.quote', 'EUR/USD');
        expect(eurusd.points).toHaveLength(1);
        expect(eurusd.points[0]!.quotes.size).toBe(4);
    });

    it('sort a curve by tenor, not as text', () => {
        const curve = series('interest_rates.curve', 'USD 0D 1D');
        expect(curve.points.map((p) => p.label)).toEqual(['1W', '1M', '2M', '1Y', '10Y']);
        expect(['1Y', '18M', '1W', 'ON', '2Y'].sort(byTenor)).toEqual([
            'ON',
            '1W',
            '1Y',
            '18M',
            '2Y',
        ]);
        expect(tenorsOf([curve])).toEqual(['1W', '1M', '2M', '1Y', '10Y']);
    });

    it('lay a surface out by tenor down and strategy across, ATM first', () => {
        const grid = gridOf(series('fx.surface', 'EUR/USD'));
        expect(grid.rows).toEqual(['1W', '1M']);
        expect(grid.columns).toEqual(['ATM', '25RR']);
        expect(grid.at('1M', 'ATM')?.quotes.size).toBe(3);
        expect(grid.at('1W', '25RR')).toBeUndefined();
    });

    it('list the live series first, and leave out a vendor the person left out', () => {
        const names = (shown: ReadonlySet<string>): string[] =>
            shownSeries(board('fx.quote'), shown, '', 'live', NOW).map((s) => s.name);
        expect(names(all)).toEqual(['EUR/USD', 'USD/CHF', 'EUR/GBP']);
        const withoutDerived = new Set([...all].filter((k) => k !== feedKey(derived)));
        expect(names(withoutDerived)).toEqual(['EUR/USD', 'USD/CHF']);
        expect(
            shownSeries(board('fx.quote'), all, 'eur/gbp', 'live', NOW).map((s) => s.name),
        ).toEqual(['EUR/GBP']);
    });
});

describe('the agreement on a point', () => {
    const point = (name: string) => series('fx.quote', name).points[0]!;

    it('measures three or more vendors against their median, and leaves an outlier out of the mid', () => {
        const a = agreementOf('spot', point('EUR/USD'), vendors, all, NOW);
        expect(a.count).toBe(3);
        expect(a.mid).toBeCloseTo((1.10618 + 1.10615 + 1.1062) / 3, 8);
        expect(a.spread).toBeCloseTo(0.00005, 8);
    });

    it('trusts the highest-ranked vendor when only two disagree', () => {
        const a = agreementOf('spot', point('USD/CHF'), vendors, all, NOW);
        expect(a.reference).toBe(0.77886);
        expect(a.mid).toBe(0.77886);
    });

    it('leaves a stale vendor out', () => {
        expect(agreementOf('spot', point('EUR/GBP'), vendors, all, NOW).mid).toBeNull();
        expect(isStale(OLD, NOW)).toBe(true);
    });
});

describe('a comparison of two sources', () => {
    const atm = gridOf(series('fx.surface', 'EUR/USD')).at('1M', 'ATM')!;

    it('gives A − B for two vendors', () => {
        const c = compare('vol', atm, feedKey(tradeweb), feedKey(bloomberg), vendors, all, NOW);
        expect(c.difference).toBeCloseTo(0.0012, 8);
    });

    it('measures a vendor against the consensus of the others, never against itself', () => {
        const c = compare('vol', atm, feedKey(tradeweb), CONSENSUS, vendors, all, NOW);
        expect(c.b).toBeCloseTo((0.0723 + 0.0712) / 2, 8);
        expect(c.difference).toBeCloseTo(0.0735 - (0.0723 + 0.0712) / 2, 8);
    });
});

describe('the units', () => {
    it('show a price to its pair decimals, a rate in % and a vol in vol points', () => {
        expect(valueText('spot', 1.106183)).toBe('1.10618');
        expect(valueText('spot', 154.2117)).toBe('154.212');
        expect(valueText('rate', 0.0439)).toBe('4.390%');
        expect(valueText('vol', 0.0723)).toBe('7.23');
        expect(valueText('vol', null)).toBe('—');
    });

    it('show a change in pips, basis points or vol points, and zero as zero', () => {
        expect(differenceText(differenceOf('spot', 0.00012, 1.1))).toBe('+1.2');
        expect(differenceText(differenceOf('spot', 0.12, 154.2))).toBe('+12.0');
        expect(differenceText(differenceOf('rate', -0.00031, 0.04))).toBe('-3.1');
        expect(differenceText(differenceOf('vol', 0.0012, 0.07))).toBe('+0.12');
        expect(differenceText(differenceOf('spot', -0.000001, 1.1))).toBe('0');
    });
});

describe('the time axis', () => {
    it('holds each vendor at its last value, with a gap before its first tick', () => {
        const { xs, lines } = onTimeAxis(
            [
                [
                    { at: 0, value: 1 },
                    { at: 25, value: 2 },
                ],
                [{ at: 60, value: 5 }],
            ],
            0,
            100,
            5,
        );
        expect(xs).toEqual([0, 25, 50, 75, 100]);
        expect(lines[0]).toEqual([1, 2, 2, 2, 2]);
        expect(lines[1]).toEqual([null, null, null, 5, 5]);
    });
});
