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
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter, Route, Routes } from 'react-router';
import type { AccountAccess } from '@ores/wire-protocol/browser';
import type { RecordRow, SyntheticResourceView } from '../api/client.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { FIRST_PAGE, pageKey } from '../refdata/RecordList.js';
import { completeFeed, FxFeedPage, FxFeedsPage } from './fxFeeds.js';
import { MarketDataPage } from './MarketDataPage.js';

const FEED_ID = '55555555-5555-4555-8555-555555555555';

function resource(key: string, name: string, search = false): SyntheticResourceView {
    return {
        key,
        entityType: `ores.synthetic.${name}`,
        keyField: 'id',
        writable: true,
        readOnlyBecause: null,
        search,
        sortable: search ? ['source_name'] : [],
        readPermission: `synthetic::${name}s:read`,
        writePermission: `synthetic::${name}s:write`,
        deletePermission: `synthetic::${name}s:delete`,
    };
}

const REGISTRY = [
    resource('fx-feeds', 'fx_spot_generation_config', true),
    resource('gmm-components', 'gmm_component'),
];

function access(codes: string[]): AccountAccess {
    return {
        roles: [
            {
                roleId: '33333333-3333-3333-3333-333333333333' as AccountAccess['roles'][number]['roleId'],
                name: 'Quant',
                description: '',
                permissionCodes: codes,
                givenBy: 'priya',
                givenAt: '2026-10-04',
                reasonCode: 'access.new_joiner',
                commentary: '',
            },
        ],
    };
}

const feed: RecordRow = {
    version: 3,
    id: FEED_ID,
    config_id: '88888888-8888-4888-8888-888888888888',
    base_currency_code: 'EUR',
    quote_currency_code: 'USD',
    source_name: 'synthetic.eurusd',
    ore_key: 'FX/RATE/EUR/USD',
    price_source: 'fixed',
    gmm_initial_price: 1.08,
    ticks_per_hour: 3600,
    process_type: 'geometric',
    enabled: false,
    auto_start: false,
    vintage_source: '',
    vintage_date: '',
    folder_id: null,
};

function component(index: number, weight: number): RecordRow {
    return {
        version: 1,
        id: `c${String(index)}`,
        fx_spot_config_id: FEED_ID,
        component_index: index,
        description: index === 0 ? 'calm' : 'stressed',
        mean: 0,
        stdev: 0.01 * (index + 1),
        weight,
    };
}

function render(path: string, seed: (client: QueryClient) => void): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['registry', 'synthetic'], REGISTRY);
    seed(client);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[path]}>
                    <Routes>
                        <Route path="/market-data" element={<MarketDataPage />} />
                        <Route path="/market-data/fx-feeds" element={<FxFeedsPage />} />
                        <Route path="/market-data/fx-feeds/:id" element={<FxFeedPage />} />
                    </Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the fields a feed derives', () => {
    it('derives the ORE key and a source name from the pair, and empties the vintage of a fixed price', () => {
        expect(
            completeFeed({
                base_currency_code: 'GBP',
                quote_currency_code: 'JPY',
                source_name: '',
                price_source: 'fixed',
                gmm_initial_price: 190,
                vintage_source: null,
                vintage_date: null,
            }),
        ).toMatchObject({
            ore_key: 'FX/RATE/GBP/JPY',
            source_name: 'synthetic.gbpjpy',
            gmm_initial_price: 190,
            vintage_source: '',
            vintage_date: '',
        });
    });

    it('keeps a source name that is given, and zeroes the price of a vintage feed', () => {
        expect(
            completeFeed({
                base_currency_code: 'EUR',
                quote_currency_code: 'USD',
                source_name: 'synthetic.themes.eurusd',
                price_source: 'vintage',
                gmm_initial_price: null,
                vintage_source: 'ore.reference',
                vintage_date: '2016-02-05',
            }),
        ).toMatchObject({
            source_name: 'synthetic.themes.eurusd',
            gmm_initial_price: 0,
            vintage_source: 'ore.reference',
            vintage_date: '2016-02-05',
        });
    });
});

describe('the market data area', () => {
    it('offers the FX price feeds to a person who reads them', () => {
        const html = render('/market-data', (client) => {
            client.setQueryData(['my-access'], access(['synthetic::*']));
        });
        expect(html).toContain('FX price feeds');
    });

    it('draws no card for a person who does not', () => {
        const html = render('/market-data', (client) => {
            client.setQueryData(['my-access'], access(['refdata::currencies:read']));
        });
        expect(html).not.toContain('FX price feeds');
    });
});

describe('the FX feed screens', () => {
    it('lists one page of feeds with the pair, the source name and the engine', () => {
        const html = render('/market-data/fx-feeds', (client) => {
            client.setQueryData(pageKey({ key: 'fx-feeds' }, FIRST_PAGE), {
                rows: [feed],
                total: 1,
            });
            client.setQueryData(['my-access'], access(['synthetic::*']));
        });
        expect(html).toContain('EUR/USD');
        expect(html).toContain('synthetic.eurusd');
        expect(html).toContain('Geometric');
        expect(html).toContain('Add feed');
        expect(html).toContain('Search…');
    });

    it('shows a feed with its fields and the ORE key, and Edit and Delete to a writer', () => {
        const html = render(`/market-data/fx-feeds/${FEED_ID}`, (client) => {
            client.setQueryData(['records', 'fx-feeds', 'key', FEED_ID], feed);
            client.setQueryData(['my-access'], access(['synthetic::*']));
        });
        expect(html).toContain('synthetic.eurusd');
        expect(html).toContain('FX/RATE/EUR/USD');
        expect(html).toContain('A fixed price');
        expect(html).toContain('>Mixture<');
        expect(html).toContain('Edit');
        expect(html).toContain('Delete');
    });

    it('offers a reader no change', () => {
        const html = render(`/market-data/fx-feeds/${FEED_ID}`, (client) => {
            client.setQueryData(['records', 'fx-feeds', 'key', FEED_ID], feed);
            client.setQueryData(
                ['my-access'],
                access(['synthetic::fx_spot_generation_configs:read']),
            );
        });
        expect(html).not.toContain('>Edit<');
        expect(html).toContain('Read only');
    });

    it('draws the mixture with its weight sum and the distribution it implies', () => {
        const html = render(`/market-data/fx-feeds/${FEED_ID}?tab=mixture`, (client) => {
            client.setQueryData(['records', 'fx-feeds', 'key', FEED_ID], feed);
            client.setQueryData(
                ['records', 'gmm-components', FEED_ID],
                [component(1, 0.1), component(0, 0.8)],
            );
            client.setQueryData(['my-access'], access(['synthetic::*']));
        });
        expect(html).toContain('value="calm"');
        expect(html.indexOf('value="calm"')).toBeLessThan(html.indexOf('value="stressed"'));
        expect(html).toContain('Weight sum 0.9000');
        expect(html).toContain('Return distribution');
        expect(html).toContain('<svg');
        expect(html).toContain('Add component');
        expect(html).not.toContain('Save mixture');
    });

    it('says what a feed with no mixture lacks', () => {
        const html = render(`/market-data/fx-feeds/${FEED_ID}?tab=mixture`, (client) => {
            client.setQueryData(['records', 'fx-feeds', 'key', FEED_ID], feed);
            client.setQueryData(['records', 'gmm-components', FEED_ID], []);
            client.setQueryData(['my-access'], access(['synthetic::*']));
        });
        expect(html).toContain('No component yet.');
        expect(html).toContain('The mixture needs at least one component.');
    });

    it('marks each feed running or stopped, and offers a writer start and stop for the chosen feeds', () => {
        const html = render('/market-data/fx-feeds', (client) => {
            client.setQueryData(pageKey({ key: 'fx-feeds' }, FIRST_PAGE), {
                rows: [feed, { ...feed, id: 'other', source_name: 'synthetic.gbpusd' }],
                total: 2,
            });
            client.setQueryData(['synthetic-running'], ['synthetic.eurusd']);
            client.setQueryData(['my-access'], access(['synthetic::*']));
        });
        expect(html).toContain('>Running<');
        expect(html).toContain('>Stopped<');
        expect(html).toContain('Choose synthetic.gbpusd');
        expect(html).toContain('Start chosen (0)');
        expect(html).toContain('Stop chosen (0)');
    });

    it('offers a reader the status but no start or stop', () => {
        const html = render('/market-data/fx-feeds', (client) => {
            client.setQueryData(pageKey({ key: 'fx-feeds' }, FIRST_PAGE), {
                rows: [feed],
                total: 1,
            });
            client.setQueryData(['synthetic-running'], []);
            client.setQueryData(
                ['my-access'],
                access(['synthetic::fx_spot_generation_configs:read']),
            );
        });
        expect(html).toContain('>Stopped<');
        expect(html).not.toContain('Start chosen');
        expect(html).not.toContain('aria-label="Choose ');
    });

    it('offers Stop on a running feed and Start on a stopped one', () => {
        const running = render(`/market-data/fx-feeds/${FEED_ID}`, (client) => {
            client.setQueryData(['records', 'fx-feeds', 'key', FEED_ID], feed);
            client.setQueryData(['synthetic-running'], ['synthetic.eurusd']);
            client.setQueryData(['my-access'], access(['synthetic::*']));
        });
        expect(running).toContain('>Running<');
        expect(running).toContain('>Stop<');
        const stopped = render(`/market-data/fx-feeds/${FEED_ID}`, (client) => {
            client.setQueryData(['records', 'fx-feeds', 'key', FEED_ID], feed);
            client.setQueryData(['synthetic-running'], []);
            client.setQueryData(['my-access'], access(['synthetic::*']));
        });
        expect(stopped).toContain('>Stopped<');
        expect(stopped).toContain('>Start<');
    });

    it('plots the last ticks with the last price and the move over them', () => {
        const html = render(`/market-data/fx-feeds/${FEED_ID}?tab=ticks`, (client) => {
            client.setQueryData(['records', 'fx-feeds', 'key', FEED_ID], feed);
            client.setQueryData(['synthetic-running'], ['synthetic.eurusd']);
            client.setQueryData(['feed-ticks', FEED_ID, 200], {
                sourceName: 'synthetic.eurusd',
                ticks: [
                    { at: '2026-10-11 09:00:00Z', value: 1.08 },
                    { at: '2026-10-11 09:01:00Z', value: 1.081 },
                    { at: '2026-10-11 09:02:00Z', value: 1.083 },
                ],
            });
            client.setQueryData(['my-access'], access(['synthetic::*']));
        });
        expect(html).toContain('Last 1.08300 at 09:02:00');
        expect(html).toContain('+0.00300 over 3 ticks');
        expect(html).toContain('<svg');
    });

    it('says a stopped feed with no ticks heard must be started', () => {
        const html = render(`/market-data/fx-feeds/${FEED_ID}?tab=ticks`, (client) => {
            client.setQueryData(['records', 'fx-feeds', 'key', FEED_ID], feed);
            client.setQueryData(['synthetic-running'], []);
            client.setQueryData(['feed-ticks', FEED_ID, 200], {
                sourceName: 'synthetic.eurusd',
                ticks: [],
            });
            client.setQueryData(['my-access'], access(['synthetic::*']));
        });
        expect(html).toContain('Start the feed to see its prices.');
    });

    it('explains the geometric engine and names the profile of each deviation', () => {
        const html = render(`/market-data/fx-feeds/${FEED_ID}?tab=mixture`, (client) => {
            client.setQueryData(['records', 'fx-feeds', 'key', FEED_ID], feed);
            client.setQueryData(
                ['records', 'gmm-components', FEED_ID],
                [{ ...component(0, 1), stdev: 0.001 }],
            );
            client.setQueryData(['my-access'], access(['synthetic::*']));
        });
        expect(html).toContain('so the price stays positive');
        expect(html).toContain('title="Drift (μ): the mean log-return per update.');
        expect(html).toContain('<option value="normal" selected="">Normal</option>');
    });

    it('warns that the arithmetic engine can take the price below zero', () => {
        const html = render(`/market-data/fx-feeds/${FEED_ID}?tab=mixture`, (client) => {
            client.setQueryData(['records', 'fx-feeds', 'key', FEED_ID], {
                ...feed,
                process_type: 'arithmetic',
            });
            client.setQueryData(['records', 'gmm-components', FEED_ID], [component(0, 1)]);
            client.setQueryData(['my-access'], access(['synthetic::*']));
        });
        expect(html).toContain('the price can go to zero or below');
        expect(html).toContain('in price units, not a percentage');
    });

    it('reads a mean reverting mixture as κ and σ, with its half-life and band', () => {
        const html = render(`/market-data/fx-feeds/${FEED_ID}?tab=mixture`, (client) => {
            client.setQueryData(['records', 'fx-feeds', 'key', FEED_ID], {
                ...feed,
                process_type: 'ornstein_uhlenbeck',
                ticks_per_hour: 60,
            });
            client.setQueryData(
                ['records', 'gmm-components', FEED_ID],
                [{ ...component(0, Math.log(2) / 30), stdev: 0.01 }],
            );
            client.setQueryData(['my-access'], access(['synthetic::*']));
        });
        expect(html).toContain('Reversion (κ)');
        expect(html).toContain('Half-life t½: 30.0 minutes (0.02 days)');
        expect(html).toContain('The mean reverting engine reads only the first component.');
        expect(html).not.toContain('Weight sum');
    });
});
