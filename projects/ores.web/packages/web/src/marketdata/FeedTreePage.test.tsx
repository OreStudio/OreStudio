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
import { FeedTreePage } from './FeedTreePage.js';
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
                        <Route path="/market-data/synthetic/tree" element={<FeedTreePage />} />
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

const FOLDER_ROWS = [
    { id: 'r', parent_id: '', name: 'Synthetic', kind: 'root', version: 1 },
    { id: 'b', parent_id: 'r', name: 'Bloomberg', kind: 'collection', version: 1 },
    { id: 'a', parent_id: 'b', name: 'FX', kind: 'asset_class', version: 1 },
    { id: 'i', parent_id: 'a', name: 'Spot', kind: 'instrument_type', version: 1 },
];

const leaf = folder({
    id: 'i',
    parent_id: 'a',
    name: 'Spot',
    kind: 'instrument_type',
    fx_feeds: 26,
    running_feeds: 2,
    has_children: false,
});

const FEED = {
    id: 'x1',
    kind: 'fx_spot',
    label: 'EUR/USD',
    source_name: 'synthetic.bloomberg.eurusd',
    oresmd_uri: 'oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD',
    folder_id: 'i',
    enabled: true,
};

describe('the feed tree', () => {
    it('is a card of the Synthetic folder', () => {
        const html = render('/market-data/synthetic', (client) => {
            client.setQueryData(['my-access'], access(['synthetic::*']));
        });
        expect(html).toContain('Feed tree');
        expect(html).toContain('href="/market-data/synthetic/tree"');
    });

    it('shows the two scopes with what runs under each, and chooses the one that holds feeds', () => {
        const html = render('/market-data/synthetic/tree', (client) =>
            loaded(client, [bloomberg, ore], ['synthetic::*']),
        );
        expect(html).toContain('System feeds');
        expect(html).toContain('Test feeds');
        expect(html).toContain('20/76');
        expect(html).toContain('0/0');
    });

    it('lists the collections under the chosen scope with Start and Stop', () => {
        const html = render('/market-data/synthetic/tree', (client) =>
            loaded(client, [bloomberg, ore], ['synthetic::*']),
        );
        expect(html).toContain('Bloomberg');
        expect(html).toContain('2016 ORE Samples');
        expect(html).toContain('>Start<');
        expect(html).toContain('>Stop<');
    });

    it('offers no Start or Stop to a person who may only read', () => {
        const html = render('/market-data/synthetic/tree', (client) =>
            loaded(client, [bloomberg], ['synthetic::fx_spot_generation_configs:read']),
        );
        expect(html).not.toContain('>Start<');
        expect(html).not.toContain('>Stop<');
    });

    it('opens a collection by its address, with its provider and how it is bound', () => {
        const html = render('/market-data/synthetic/tree?node=folder%3Ab', (client) =>
            loaded(client, [bloomberg, ore], ['synthetic::*']),
        );
        expect(html).toContain('>bloomberg<');
        expect(html).toContain('>Bound<');
    });

    it('lists the feeds of the folder that holds them, each with a link to its page', () => {
        const html = render('/market-data/synthetic/tree?node=folder%3Ai', (client) => {
            loaded(client, [bloomberg], ['synthetic::*']);
            client.setQueryData(['records', 'folders'], FOLDER_ROWS);
            client.setQueryData(['feed-folders', 'siblings', 'a'], [leaf]);
            client.setQueryData(['feed-folders', 'feeds', 'i', '', 0], {
                total: 26,
                feeds: [FEED],
            });
        });
        expect(html).toContain('EUR/USD');
        expect(html).toContain('synthetic.bloomberg.eurusd');
        expect(html).toContain('href="/market-data/synthetic/fx-feeds/x1"');
        expect(html).toContain('1 to 26 of 26');
    });

    it('says there are no feeds when the party holds none', () => {
        const html = render('/market-data/synthetic/tree', (client) =>
            loaded(client, [], ['synthetic::*']),
        );
        expect(html).toContain('There are no feeds yet.');
    });
});
