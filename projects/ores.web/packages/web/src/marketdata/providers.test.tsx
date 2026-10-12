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
import { MarketDataPage } from './MarketDataPage.js';
import { ProvidersPage } from './providers.js';

function resource(key: string, name: string): SyntheticResourceView {
    return {
        key,
        entityType: `ores.synthetic.${name}`,
        keyField: key === 'providers' ? 'code' : 'id',
        writable: false,
        readOnlyBecause: 'Read only.',
        search: false,
        sortable: [],
        readPermission: `synthetic::${name}s:read`,
        writePermission: `synthetic::${name}s:write`,
        deletePermission: `synthetic::${name}s:delete`,
    };
}

const REGISTRY = [
    resource('providers', 'market_data_provider'),
    resource('collections', 'market_data_generation_config'),
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

const PROVIDERS: RecordRow[] = [
    {
        version: 1,
        code: 'bloomberg',
        name: 'Bloomberg',
        kind: 'data_vendor',
        description: 'Data vendor publishing quotes across asset classes.',
        display_order: 1,
    },
    {
        version: 1,
        code: 'cme',
        name: 'CME Group',
        kind: 'exchange',
        description: 'Derivatives exchange.',
        display_order: 2,
    },
];

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
                        <Route path="/market-data/providers" element={<ProvidersPage />} />
                    </Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the market data providers screen', () => {
    it('offers the providers to a person who reads them', () => {
        const html = render('/market-data', (client) => {
            client.setQueryData(['my-access'], access(['synthetic::market_data_providers:read']));
        });
        expect(html).toContain('Market data providers');
        expect(html).toContain('href="/market-data/providers"');
    });

    it('draws no card for a person who does not', () => {
        const html = render('/market-data', (client) => {
            client.setQueryData(['my-access'], access(['refdata::currencies:read']));
        });
        expect(html).not.toContain('Market data providers');
    });

    it('names each provider with the synthetic mark, its code and its kind', () => {
        const html = render('/market-data/providers', (client) => {
            client.setQueryData(pageKey({ key: 'providers' }, FIRST_PAGE), {
                rows: PROVIDERS,
                total: 2,
            });
            client.setQueryData(['my-access'], access(['synthetic::market_data_providers:read']));
        });
        expect(html).toContain('Bloomberg (synthetic)');
        expect(html).toContain('CME Group (synthetic)');
        expect(html).toContain('>bloomberg<');
        expect(html).toContain('Data vendor');
        expect(html).toContain('>Exchange<');
    });

    it('counts the collections that stand for each provider', () => {
        const html = render('/market-data/providers', (client) => {
            client.setQueryData(pageKey({ key: 'providers' }, FIRST_PAGE), {
                rows: PROVIDERS,
                total: 2,
            });
            client.setQueryData(
                ['records', 'collections'],
                [
                    { id: 'c1', provider_code: 'bloomberg', version: 1 },
                    { id: 'c2', provider_code: 'bloomberg', version: 1 },
                    { id: 'c3', provider_code: null, version: 1 },
                ],
            );
            client.setQueryData(['my-access'], access(['synthetic::market_data_providers:read']));
        });
        expect(html).toContain('>2<');
        expect(html).toContain('>0<');
    });
});
