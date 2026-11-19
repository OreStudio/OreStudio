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

import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import type { ReactNode } from 'react';
import { renderToStaticMarkup } from 'react-dom/server';
import { describe, expect, it } from 'vitest';
import type { Quote, QuoteFeed } from '../api/quotes.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { COMPARE, GridBoard, MID, QuoteBoard, type Watch } from './QuoteBoards.js';
import {
    boardsOf,
    CONSENSUS,
    currencyGrids,
    feedKey,
    gridOf,
    merged,
    vendorsOf,
    type Grid,
} from './quotesView.js';

const NOW = Date.parse('2026-11-19T12:00:30.000Z');
const AT = '2026-11-19 12:00:00.000000Z';

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
const vendors = vendorsOf([demo, tradeweb, ice, bloomberg]);
const watch: Watch = {
    vendors,
    shown: new Set(vendors.map((v) => v.key)),
    now: NOW,
    colourOf: () => 'var(--color-accent)',
};

function vol(f: QuoteFeed, expiry: string, strike: string, value: string): Quote {
    return {
        provider: f.provider,
        feed: f.feed,
        source: f.source,
        uri: `oresmd://fx/EUR?type=quote&instrument=fx_option&ccy=USD&expiry=${expiry}&strike=${strike}`,
        value,
        previous: value,
        at: AT,
        asset_class: 'fx',
        instrument_type: 'fx_option',
        series_uri: 'oresmd://fx/EUR?type=series&instrument=fx_option&ccy=USD',
        series: 'EUR USD',
        point: `${expiry} ${strike}`,
    };
}

function spot(f: QuoteFeed, value: string): Quote {
    return {
        provider: f.provider,
        feed: f.feed,
        source: f.source,
        uri: 'oresmd://fx/EUR?type=quote&instrument=fx_spot&ccy=USD',
        value,
        previous: value,
        at: AT,
        asset_class: 'fx',
        instrument_type: 'fx_spot',
        series_uri: 'oresmd://fx/EUR?type=series&instrument=fx_spot&ccy=USD',
        series: 'EUR USD',
        point: '',
    };
}

function swap(f: QuoteFeed, ccy: string, index: string, term: string, value: string): Quote {
    return {
        provider: f.provider,
        feed: f.feed,
        source: f.source,
        uri: `oresmd://ir/${ccy}?type=quote&instrument=ir_swap&tenor=${index}&term=${term}`,
        value,
        previous: value,
        at: AT,
        asset_class: 'interest_rates',
        instrument_type: 'ir_swap',
        series_uri: `oresmd://ir/${ccy}?type=series&instrument=ir_swap&tenor=${index}`,
        series: `${ccy} 0D ${index}`,
        point: term,
    };
}

const boards = boardsOf(
    merged(new Map(), [
        spot(bloomberg, '1.10618'),
        spot(ice, '1.10615'),
        spot(demo, '0.86679'),
        vol(bloomberg, '1M', '25RR', '-0.0042'),
        vol(bloomberg, '1M', 'ATM', '0.0723'),
        vol(bloomberg, '1W', 'ATM', '0.0717'),
        vol(ice, '1M', 'ATM', '0.0712'),
        vol(ice, '1M', '25RR', '-0.0102'),
        vol(tradeweb, '1M', 'ATM', '0.0735'),
        vol(tradeweb, '1M', '25RR', '-0.0045'),
        swap(bloomberg, 'USD', '1D', '1Y', '0.0409'),
        swap(tradeweb, 'USD', '1D', '1Y', '0.0411'),
        swap(bloomberg, 'USD', '3M', '1Y', '0.0402'),
        swap(bloomberg, 'USD', '1D', '6M', '0.0403'),
        swap(tradeweb, 'USD', '1D', '6M', '0.0404'),
        swap(bloomberg, 'EUR', '1D', '1Y', '0.0212'),
    ]),
    vendors,
);
const surfaces = boards.find((b) => b.key === 'fx.surface')!.series.map(gridOf);
const currencies = currencyGrids(boards.find((b) => b.key === 'interest_rates.curve')!.series);
const quotes = boards.find((b) => b.key === 'fx.quote')!.series;

function render(node: ReactNode): string {
    return renderToStaticMarkup(
        <QueryClientProvider client={new QueryClient()}>
            <TranslationProvider>{node}</TranslationProvider>
        </QueryClientProvider>,
    );
}

const text = (html: string): string => html.replace(/<[^>]+>/g, ' ').replace(/\s+/g, ' ');

describe('the quotes board', () => {
    it('puts each vendor in a column, in the order they are trusted, and warns of an outlier', () => {
        const html = text(
            render(<QuoteBoard series={quotes} watch={watch} picked="" onPick={() => undefined} />),
        );
        expect(html.indexOf('bloomberg')).toBeLessThan(html.indexOf('ice'));
        expect(html.indexOf('ice')).toBeLessThan(html.indexOf('demo'));
        expect(html).toContain('0.86679 ⚠');
        expect(html).toMatch(/1\.1061[67]/);
    });
});

function gridBoard(grids: readonly Grid[], mode: string, a: string, picked = ''): string {
    return text(
        render(
            <GridBoard
                grids={grids}
                label="Grids"
                watch={watch}
                picked={picked}
                onPick={() => undefined}
                mode={mode}
                onMode={() => undefined}
                point=""
                onPoint={() => undefined}
                comparison={{ a, b: CONSENSUS, row: '', column: '' }}
                onCompare={() => undefined}
            />,
        ),
    );
}

describe('the surface board', () => {
    it('lays the surface out tenor down and ATM first across, as the mid in vol points', () => {
        const html = gridBoard(surfaces, MID, feedKey(ice));
        expect(html).toMatch(/Tenor ATM 25RR 1W 7\.17 · 1M 7\.23/);
    });

    it('compares a vendor with the consensus of the others, cell by cell, and flags a gap past the limit', () => {
        const html = gridBoard(surfaces, COMPARE, feedKey(ice));
        expect(html).toContain('Consensus of the others');
        // ice's ATM against the mean of bloomberg and tradeweb: 7.12 − 7.29.
        expect(html).toContain('-0.17');
        // ice's 25RR against theirs: −1.02 − (−0.435), past the 0.4 vol point limit.
        expect(html).toMatch(/-0\.5[89] ⚠/);
        expect(html).toMatch(/Past the limit 1 \/ 2/);
    });
});

describe('the curves board', () => {
    it('lists the currencies, and shows only the curves of the picked currency, tenor down', () => {
        expect(currencies.map((g) => g.name)).toEqual(['EUR', 'USD']);
        const html = gridBoard(currencies, MID, feedKey(tradeweb), 'USD');
        expect(html).toMatch(/Tenor USD 0D 1D USD 0D 3M 6M 4\.035% · 1Y 4\.100% 4\.020%/);
        expect(html).not.toContain('EUR 0D 1D');
    });

    it('compares two providers tenor by tenor in basis points', () => {
        const html = gridBoard(currencies, COMPARE, feedKey(tradeweb), 'USD');
        // tradeweb against the consensus of the others, here bloomberg alone.
        expect(html).toMatch(/6M \+1\.0 · 1Y \+2\.0 ·/);
    });
});
