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
import type { FxMontage, FxMontagePair } from '../api/client.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { FxMontagePage } from './FxMontagePage.js';
import { chosenDayQuery, montageQuery } from './fxMontage.js';

/**
 * The montage screen, drawn from answers the reads actually fetched.
 *
 * The queries are the screen's own, settled through a stubbed `fetch`, and the
 * page is drawn from the cache those reads filled. A page that asked for another
 * key, or read a field under another name, would draw nothing here even though a
 * hand-seeded answer would have drawn it.
 */

const AS_OF = '2026-11-13';
const ADDRESS = `/market-data/fx-montage?base=EUR&asof=${AS_OF}`;

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
    's-2': [
        ['2026-11-13 09:00:00Z', '1.0840'],
        ['2026-11-13 15:00:00Z', '1.0848'],
    ],
    's-3': [
        ['2026-11-13 09:00:00Z', '0.8410'],
        ['2026-11-13 15:00:00Z', '0.8421'],
    ],
};

/** The answer a deployment gives when the person asks for one provider only. */
function onlyProviders(answer: FxMontage, codes: readonly string[]): FxMontage {
    const keep = (pair: FxMontagePair): FxMontagePair => ({
        ...pair,
        spot_by_provider: pair.spot_by_provider.filter((spot) =>
            codes.includes(spot.provider_code),
        ),
    });
    return { ...answer, pairs: answer.pairs.map(keep) };
}

/**
 * Answer both routes the screen reads, and record what it asked for.
 *
 * The answers keep the shapes the BFF routes send, so the screen's own schemas
 * parse them: an observation is a wire timestamp and a value as text.
 */
function serve(
    montageAnswer: FxMontage = montage,
    dayRows: Readonly<Record<string, readonly [string, string][]>> = observations,
): { readonly asked: string[]; readonly posted: readonly unknown[] } {
    const asked: string[] = [];
    const posted: unknown[] = [];
    vi.stubGlobal(
        'fetch',
        async (url: string | URL | Request, init?: { readonly body?: unknown }) => {
            const address = String(url);
            asked.push(address);
            if (address.startsWith('/api/market/fx-montage')) {
                posted.push(typeof init?.body === 'string' ? JSON.parse(init.body) : init?.body);
                return new Response(JSON.stringify(montageAnswer), { status: 200 });
            }
            const id = /^\/api\/series\/([^/]+)\/observations/.exec(address)?.[1] ?? '';
            const rows = dayRows[id] ?? [];
            return new Response(
                JSON.stringify({
                    observations: rows.map(([at, value]) => ({ at, value, source: 'ore' })),
                    total: rows.length,
                }),
                { status: 200 },
            );
        },
    );
    return { asked, posted };
}

/**
 * Settle the montage, and every series its answer names.
 *
 * Every candidate is settled because the page chooses which spot to draw from; a
 * page that chose another provider would find that provider's rows and say so in
 * its heading, which is what these tests read.
 */
async function settle(client: QueryClient, providers: readonly string[] = []): Promise<void> {
    await client.fetchQuery(montageQuery('EUR', AS_OF, providers));
    for (const id of ['s-1', 's-2', 's-3']) {
        await client.fetchQuery(chosenDayQuery(id, AS_OF));
    }
}

function emptyClient(): QueryClient {
    return new QueryClient({ defaultOptions: { queries: { retry: false } } });
}

function draw(client: QueryClient, address: string = ADDRESS): string {
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

/** Settle both reads from a fresh answer, then draw the page from what they wrote. */
async function drawnPage(
    address: string = ADDRESS,
    montageAnswer: FxMontage = montage,
    dayRows: Readonly<Record<string, readonly [string, string][]>> = observations,
): Promise<string> {
    serve(montageAnswer, dayRows);
    const client = emptyClient();
    await settle(client);
    return draw(client, address);
}

afterEach(() => {
    vi.unstubAllGlobals();
});

describe('the FX montage page', () => {
    it('draws every cross of the base with its consensus and its range', async () => {
        const page = await drawnPage();
        for (const text of ['EUR/USD', '1.0850', '1.0802', '1.0891', 'EUR/GBP', '0.8421']) {
            expect(page).toContain(text);
        }
    });

    it('states the change and leaves an unmeasured one absent', async () => {
        const page = await drawnPage();
        expect(page).toContain('+0.12%');
        // The second row's window holds no price at its start.
        expect(page).toContain('—');
    });

    it('draws the day\u2019s range with the consensus on it', async () => {
        const page = await drawnPage();
        expect(page).toContain('Today from 1.0802 to 1.0891, last 1.085');
    });

    it('draws every pair\u2019s day as a shape in its own row', async () => {
        const page = await drawnPage();
        expect(page).toContain('aria-label="EUR/USD day shape"');
        expect(page).toContain('aria-label="EUR/GBP day shape"');
    });

    it('draws the chosen pair\u2019s day from the freshest provider, and names it', async () => {
        const page = await drawnPage();
        expect(page).toContain('The EUR/USD day, from ore');
        expect(page).toContain('3 values, oldest first.');
    });

    it('states the provider\u2019s last, the day\u2019s ends and its change beside the path', async () => {
        const page = await drawnPage();
        expect(page).toContain('ore 1.0852 · high 1.0891 · low 1.0802 · change +0.12%');
    });

    it('states one value without drawing a line from it', async () => {
        const page = await drawnPage(ADDRESS, montage, {
            ...observations,
            's-1': [['2026-11-13 09:00:00Z', '1.0820']],
        });
        expect(page).toContain('The EUR/USD day, from ore');
        expect(page).toContain('1 value, oldest first.');
        expect(page).not.toContain('aria-label="The EUR/USD day, from ore"');
    });

    it('says so when the day holds no observation', async () => {
        const page = await drawnPage(ADDRESS, montage, { ...observations, 's-1': [] });
        expect(page).toContain('No observations for EUR/USD on this day.');
    });

    it('asks for the providers the address names, and for every one when it names none', async () => {
        const all = serve();
        const client = emptyClient();
        await client.fetchQuery(montageQuery('EUR', AS_OF, []));
        draw(client);
        expect(all.posted[0]).toMatchObject({ providers: [] });

        const { posted } = serve(onlyProviders(montage, ['ore']));
        const filtered = emptyClient();
        await filtered.fetchQuery(montageQuery('EUR', AS_OF, ['ore']));
        draw(filtered, `${ADDRESS}&provider=ore`);
        expect(posted[0]).toMatchObject({ providers: ['ore'] });
    });

    it('draws the columns of the providers it asked for, with the chosen one pressed', async () => {
        serve(onlyProviders(montage, ['ore']));
        const client = emptyClient();
        await settle(client, ['ore']);
        const page = draw(client, `${ADDRESS}&provider=ore`);

        expect(page).not.toContain('>derived<');
        expect(/<button[^>]*aria-pressed="true"[^>]*>ore<\/button>/.test(page)).toBe(true);
        expect(/<button[^>]*aria-pressed="false"[^>]*>All<\/button>/.test(page)).toBe(true);
    });

    it('shows every provider when the address names none', async () => {
        const page = await drawnPage();
        expect(/<button[^>]*aria-pressed="true"[^>]*>All<\/button>/.test(page)).toBe(true);
        expect(/<button[^>]*aria-pressed="false"[^>]*>derived<\/button>/.test(page)).toBe(true);
    });

    it('reads the day the address names, ending at that day and nothing after it', async () => {
        const { asked } = serve();
        const client = emptyClient();
        await client.fetchQuery(montageQuery('EUR', AS_OF, []));
        await client.fetchQuery(chosenDayQuery('s-1', AS_OF));
        draw(client);

        expect(asked).toContain(
            '/api/series/s-1/observations?limit=500&window=day&as_of=2026-11-13+23%3A59%3A59Z',
        );
    });

    it('draws the pair the address chooses, with its own provider and its own values', async () => {
        const page = await drawnPage(`${ADDRESS}&pair=EUR/GBP`);
        expect(page).toContain('The EUR/GBP day, from ore');
        expect(page).toContain('2 values, oldest first.');
    });

    it('makes every pair a control that states which one the day belongs to', async () => {
        const page = await drawnPage(`${ADDRESS}&pair=EUR/GBP`);
        const chosen = /<button[^>]*aria-pressed="true"[^>]*>EUR\/GBP<\/button>/;
        expect(chosen.test(page)).toBe(true);
        expect(/<button[^>]*aria-pressed="false"[^>]*>EUR\/USD<\/button>/.test(page)).toBe(true);
    });

    it('says which instant the change is measured from', async () => {
        const page = await drawnPage();
        expect(page).toContain('Change measured from');
    });

    it('sorts by move without changing which pair the day belongs to', async () => {
        const moved = await drawnPage(`${ADDRESS}&sort=move`);
        expect(moved.indexOf('EUR/USD')).toBeLessThan(moved.indexOf('EUR/GBP'));
        // The open pair is the answer's first, not the moved table's first.
        expect(moved).toContain('The EUR/USD day, from ore');
    });

    it('says nothing is quoted when the base has no crosses', async () => {
        const page = await drawnPage(ADDRESS, { ...montage, pairs: [] });
        expect(page).toContain('Nothing quoted.');
    });
});
