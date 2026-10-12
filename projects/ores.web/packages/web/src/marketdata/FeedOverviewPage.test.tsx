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
import type { FeedFolder } from '../api/client.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { FeedOverviewPage } from './FeedOverviewPage.js';
import { SyntheticPage } from './SyntheticPage.js';

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

function folder(over: Partial<FeedFolder>): FeedFolder {
    return {
        id: 'f',
        parent_id: 'r',
        name: 'Folder',
        kind: 'collection',
        scope: 'party',
        provider_code: '',
        binding_mode: 'sandboxed',
        fx_feeds: 0,
        ir_feeds: 0,
        running_feeds: 0,
        has_children: true,
        ...over,
    };
}

const root = folder({ id: 'r', parent_id: '', name: 'Synthetic', kind: 'root' });
const bloomberg = folder({
    id: 'b',
    name: 'Bloomberg',
    provider_code: 'bloomberg',
    binding_mode: 'bound',
    fx_feeds: 26,
    ir_feeds: 33,
    running_feeds: 20,
});
const ore = folder({ id: 'o', name: '2016 ORE Samples', fx_feeds: 13, ir_feeds: 4 });

function render(path: string, seed: (client: QueryClient) => void): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    seed(client);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[path]}>
                    <Routes>
                        <Route path="/market-data/synthetic" element={<SyntheticPage />} />
                        <Route
                            path="/market-data/synthetic/overview"
                            element={<FeedOverviewPage />}
                        />
                    </Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

const loaded = (client: QueryClient, collections: FeedFolder[], codes: string[]): void => {
    client.setQueryData(['my-access'], access(codes));
    client.setQueryData(['feed-folders', 'roots'], [root]);
    client.setQueryData(['feed-folders', 'collections', ['r']], collections);
};

describe('the Synthetic folder', () => {
    it('holds the feed overview and the two feed lists, under the trail Home, Market data, Synthetic', () => {
        const html = render('/market-data/synthetic', (client) => {
            client.setQueryData(['my-access'], access(['synthetic::*']));
        });
        expect(html).toContain('Feed overview');
        expect(html).toContain('FX price feeds');
        expect(html).toContain('IR curve feeds');
        expect(html).toContain('href="/market-data/synthetic/overview"');
    });

    it('draws only the screens a person may read', () => {
        const html = render('/market-data/synthetic', (client) => {
            client.setQueryData(
                ['my-access'],
                access(['synthetic::fx_spot_generation_configs:read']),
            );
        });
        expect(html).toContain('FX price feeds');
        expect(html).not.toContain('IR curve feeds');
    });
});

describe('the feed overview', () => {
    it('shows the scopes with what runs under each, and chooses the scope that holds feeds', () => {
        const html = render('/market-data/synthetic/overview', (client) =>
            loaded(client, [bloomberg, ore], ['synthetic::*']),
        );
        expect(html).toContain('System feeds');
        expect(html).toContain('Test feeds');
        expect(html).toContain('>Empty<');
        expect(html).toContain('39 FX · 37 IR');
    });

    it('lists the providers of the chosen scope with the synthetic mark', () => {
        const html = render('/market-data/synthetic/overview', (client) =>
            loaded(client, [bloomberg, ore], ['synthetic::*']),
        );
        expect(html).toContain('Bloomberg (synthetic)');
        expect(html).toContain('Generic (synthetic)');
        expect(html).toContain('26 FX · 33 IR');
        expect(html).toContain('20 of 59 running');
    });

    it('offers Start and Stop to a person who may run feeds, and not to one who may only read', () => {
        const may = render('/market-data/synthetic/overview', (client) =>
            loaded(client, [bloomberg], ['synthetic::*']),
        );
        expect(may).toContain('>Start<');
        expect(may).toContain('>Stop<');
        const reads = render('/market-data/synthetic/overview', (client) =>
            loaded(client, [bloomberg], ['synthetic::fx_spot_generation_configs:read']),
        );
        expect(reads).not.toContain('>Start<');
        expect(reads).not.toContain('>Stop<');
    });

    it('never lists a feed, and says where the feeds are', () => {
        const html = render('/market-data/synthetic/overview', (client) =>
            loaded(client, [bloomberg], ['synthetic::*']),
        );
        expect(html).not.toContain('synthetic.bloomberg.');
        expect(html).toContain('Choose a provider to see its feed kinds.');
    });

    it('says there are no feeds when the party holds none', () => {
        const html = render('/market-data/synthetic/overview', (client) =>
            loaded(client, [], ['synthetic::*']),
        );
        expect(html).toContain('There are no feeds yet.');
    });
});
