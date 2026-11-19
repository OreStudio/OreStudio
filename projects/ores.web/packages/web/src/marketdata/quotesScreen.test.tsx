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
import { COMPARE, MID, QuoteBoard, SurfaceBoard, type Watch } from './QuoteBoards.js';
import { boardsOf, CONSENSUS, feedKey, merged, vendorsOf } from './quotesView.js';

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
    ]),
    vendors,
);
const surfaces = boards.find((b) => b.key === 'fx.surface')!.series;
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

describe('the surface board', () => {
    const board = (mode: string, a = feedKey(ice)): string =>
        text(
            render(
                <SurfaceBoard
                    series={surfaces}
                    watch={watch}
                    picked=""
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

    it('lays the surface out tenor down and ATM first across, as the mid in vol points', () => {
        const html = board(MID);
        expect(html).toMatch(/Tenor ATM 25RR 1W 7\.17 · 1M 7\.23/);
    });

    it('compares a vendor with the consensus of the others, cell by cell, and flags a gap past the limit', () => {
        const html = board(COMPARE);
        expect(html).toContain('Consensus of the others');
        // ice's ATM against the mean of bloomberg and tradeweb: 7.12 − 7.29.
        expect(html).toContain('-0.17');
        // ice's 25RR against theirs: −1.02 − (−0.435), past the 0.4 vol point limit.
        expect(html).toMatch(/-0\.5[89] ⚠/);
        expect(html).toMatch(/Past the limit 1 \/ 2/);
    });
});
