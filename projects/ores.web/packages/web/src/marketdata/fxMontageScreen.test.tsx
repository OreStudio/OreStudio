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

import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import { afterEach, describe, expect, it, vi } from 'vitest';
import type { FxMontage } from '../api/client.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { chosenDayQuery, FxMontagePage, montageQuery } from './FxMontagePage.js';

/**
 * The montage screen, drawn from answers the reads actually fetched.
 *
 * The queries are built by the screen's own options and settled through a
 * stubbed `fetch`, then the page is drawn from the cache those reads filled. A
 * page that asked for a different key, or read a field under another name,
 * would draw nothing here even though a hand-seeded answer would have drawn it.
 */

const montage: FxMontage = {
    base_currency: 'EUR',
    as_of: '2026-11-13 23:59:59Z',
    change_since: '2026-11-13 00:00:00Z',
    pairs: [
        {
            pair_name: 'EUR/USD',
            base_currency: 'EUR',
            quote_currency: 'USD',
            spot_by_provider: [
                {
                    provider_code: 'derived',
                    value: 1.0848,
                    as_of: '2026-11-13 10:00:00Z',
                    age_seconds: 120,
                    stale: false,
                    series_id: 's-2',
                },
                {
                    provider_code: 'ore',
                    value: 1.0852,
                    as_of: '2026-11-13 10:00:00Z',
                    age_seconds: 60,
                    stale: false,
                    series_id: 's-1',
                },
            ],
            consensus: 1.085,
            change_pct: 0.12,
            day_high: 1.0891,
            day_low: 1.0802,
        },
        {
            pair_name: 'EUR/GBP',
            base_currency: 'EUR',
            quote_currency: 'GBP',
            spot_by_provider: [
                {
                    provider_code: 'ore',
                    value: 0.8421,
                    as_of: '2026-11-13 10:00:00Z',
                    age_seconds: 60,
                    stale: false,
                    series_id: 's-3',
                },
            ],
            consensus: 0.8421,
            change_pct: null,
            day_high: null,
            day_low: null,
        },
    ],
};

/** The observations each series answers with, in the wire's own spelling. */
const observations: Readonly<Record<string, readonly [string, string][]>> = {
    's-1': [
        ['2026-11-13 09:00:00Z', '1.0820'],
        ['2026-11-13 12:00:00Z', '1.0860'],
        ['2026-11-13 15:00:00Z', '1.0850'],
    ],
    's-3': [
        ['2026-11-13 09:00:00Z', '0.8410'],
        ['2026-11-13 15:00:00Z', '0.8421'],
    ],
};

/**
 * Answer both routes the screen reads, and record what it asked for.
 *
 * The two answers keep the shapes their BFF routes send, so the screen's own
 * schemas parse them: an observation is a wire timestamp and a value as text.
 */
function serve(): string[] {
    const asked: string[] = [];
    vi.stubGlobal('fetch', async (url: string | URL | Request) => {
        const address = String(url);
        asked.push(address);
        if (address.startsWith('/api/market/fx-montage')) {
            return new Response(JSON.stringify(montage), { status: 200 });
        }
        const id = /^\/api\/series\/([^/]+)\/observations/.exec(address)?.[1] ?? '';
        const rows = observations[id] ?? [];
        return new Response(
            JSON.stringify({
                observations: rows.map(([at, value]) => ({ at, value, source: 'ore' })),
                total: rows.length,
            }),
            { status: 200 },
        );
    });
    return asked;
}

/** Settle both reads through the client, then draw the page from what they wrote. */
async function settle(client: QueryClient, asOf: string, seriesId: string): Promise<void> {
    await client.fetchQuery(montageQuery('EUR', asOf));
    await client.fetchQuery(chosenDayQuery(seriesId, asOf));
}

function draw(address: string, client: QueryClient): string {
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[address]}>
                    <FxMontagePage />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

function emptyClient(): QueryClient {
    return new QueryClient({ defaultOptions: { queries: { retry: false } } });
}

afterEach(() => {
    vi.unstubAllGlobals();
});

describe('the FX montage page', () => {
    it('draws every cross of the base with its consensus and its range', async () => {
        serve();
        const client = emptyClient();
        await settle(client, '2026-11-13', 's-1');
        const page = draw('/market-data/fx-montage?base=EUR&asof=2026-11-13', client);
        for (const text of ['EUR/USD', '1.0850', '1.0802', '1.0891', 'EUR/GBP', '0.8421']) {
            expect(page).toContain(text);
        }
    });

    it('states the change and leaves an unmeasured one absent', async () => {
        serve();
        const client = emptyClient();
        await settle(client, '2026-11-13', 's-1');
        const page = draw('/market-data/fx-montage?base=EUR&asof=2026-11-13', client);
        expect(page).toContain('+0.12%');
        // The second row's window holds no price at its start.
        expect(page).toContain('—');
    });

    it('draws the day\u2019s range with the consensus on it', async () => {
        serve();
        const client = emptyClient();
        await settle(client, '2026-11-13', 's-1');
        const page = draw('/market-data/fx-montage?base=EUR&asof=2026-11-13', client);
        expect(page).toContain('Today from 1.0802 to 1.0891, last 1.085');
    });

    it('draws the chosen pair\u2019s day from the series read, and names its provider', async () => {
        serve();
        const client = emptyClient();
        await settle(client, '2026-11-13', 's-1');
        const page = draw('/market-data/fx-montage?base=EUR&asof=2026-11-13', client);
        expect(page).toContain('The EUR/USD day, from ore');
        expect(page).toContain('3 values, oldest first.');
    });

    it('reads the day the address names, ending at that day and nothing after it', async () => {
        const asked = serve();
        const client = emptyClient();
        await settle(client, '2026-11-13', 's-1');
        draw('/market-data/fx-montage?base=EUR&asof=2026-11-13', client);

        expect(asked).toContain(
            '/api/series/s-1/observations?limit=500&window=day&as_of=2026-11-13+23%3A59%3A59Z',
        );
    });

    it('draws the pair the address chooses, with its own provider and its own values', async () => {
        serve();
        const client = emptyClient();
        await settle(client, '2026-11-13', 's-3');
        const page = draw('/market-data/fx-montage?base=EUR&asof=2026-11-13&pair=EUR/GBP', client);
        expect(page).toContain('The EUR/GBP day, from ore');
        expect(page).toContain('2 values, oldest first.');
    });

    it('makes every pair a control that states which one the day belongs to', async () => {
        serve();
        const client = emptyClient();
        await settle(client, '2026-11-13', 's-1');
        const page = draw('/market-data/fx-montage?base=EUR&asof=2026-11-13&pair=EUR/GBP', client);
        const chosen = /<button[^>]*aria-pressed="true"[^>]*>EUR\/GBP<\/button>/;
        expect(chosen.test(page)).toBe(true);
        expect(/<button[^>]*aria-pressed="false"[^>]*>EUR\/USD<\/button>/.test(page)).toBe(true);
    });

    it('says which instant the change is measured from', async () => {
        serve();
        const client = emptyClient();
        await settle(client, '2026-11-13', 's-1');
        const page = draw('/market-data/fx-montage?base=EUR&asof=2026-11-13', client);
        expect(page).toContain('Change measured from');
    });

    it('sorts by move when asked', async () => {
        serve();
        const client = emptyClient();
        await settle(client, '2026-11-13', 's-1');
        const page = draw('/market-data/fx-montage?base=EUR&asof=2026-11-13&sort=move', client);
        expect(page.indexOf('EUR/USD')).toBeLessThan(page.indexOf('EUR/GBP'));
    });

    it('says nothing is quoted when the base has no crosses', async () => {
        vi.stubGlobal(
            'fetch',
            async () => new Response(JSON.stringify({ ...montage, pairs: [] }), { status: 200 }),
        );
        const client = emptyClient();
        await client.fetchQuery(montageQuery('EUR', '2026-11-13'));
        const page = draw('/market-data/fx-montage?base=EUR&asof=2026-11-13', client);
        expect(page).toContain('Nothing quoted.');
    });
});
