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
import type { RecordRow, SyntheticResourceView } from '../../api/client.js';
import { TranslationProvider } from '../../i18n/Provider.js';
import { VolContributorPage, surfacesOf } from './volContributors.js';

const BLOOMBERG = 'c3f0d994-efcd-88d6-456e-ad7f8837aa51';
const EURUSD = '77777777-7777-4777-8777-777777777777';
const GBPUSD = '66666666-6666-4666-8666-666666666666';

function resource(key: string, name: string): SyntheticResourceView {
    return {
        key,
        entityType: `ores.synthetic.${name}`,
        keyField: 'id',
        writable: true,
        readOnlyBecause: null,
        search: false,
        sortable: [],
        readPermission: `synthetic::${name}s:read`,
        writePermission: `synthetic::${name}s:write`,
        deletePermission: `synthetic::${name}s:delete`,
    };
}

const access: AccountAccess = {
    roles: [
        {
            roleId: '33333333-3333-3333-3333-333333333333' as AccountAccess['roles'][number]['roleId'],
            name: 'Quant',
            description: '',
            permissionCodes: ['synthetic::*'],
            givenBy: 'priya',
            givenAt: '2026-10-04',
            reasonCode: 'access.new_joiner',
            commentary: '',
        },
    ],
};

const contributor: RecordRow = {
    id: BLOOMBERG,
    version: 2,
    provider_code: 'bloomberg',
    noise: 0.002,
    wing_widening: 1,
    expiry_widening: 0.5,
    quote_step: 0.0005,
    missing_probability: 0.02,
    stale_probability: 0.05,
    outlier_probability: 0.001,
    outlier_size: 0.01,
    bias: 0,
};

const links: RecordRow[] = [
    { id: 'l1', feed_id: EURUSD, contributor_id: BLOOMBERG },
    { id: 'l2', feed_id: GBPUSD, contributor_id: 'someone-else' },
];

const surfaces: RecordRow[] = [
    { id: GBPUSD, source_name: 'synthetic.bloomberg.vol.gbpusd' },
    { id: EURUSD, source_name: 'synthetic.bloomberg.vol.eurusd' },
];

function render(path: string): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(
        ['registry', 'synthetic'],
        [
            resource('fx-vol-contributors', 'fx_vol_contributor'),
            resource('fx-vol-views', 'fx_vol_view'),
            resource('fx-vol-surface-feeds', 'fx_vol_surface_feed'),
        ],
    );
    client.setQueryData(['records', 'fx-vol-contributors', 'key', BLOOMBERG], contributor);
    client.setQueryData(['records', 'fx-vol-views'], links);
    client.setQueryData(['records', 'fx-vol-surface-feeds'], surfaces);
    client.setQueryData(['my-access'], access);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[path]}>
                    <Routes>
                        <Route
                            path="/market-data/synthetic/vol-contributors/:id"
                            element={<VolContributorPage />}
                        />
                    </Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('a vol contributor page', () => {
    it('shows the noise profile with one Edit, as a record page does', () => {
        const html = render(`/market-data/synthetic/vol-contributors/${BLOOMBERG}`);
        expect(html).toContain('Noise profile: bloomberg');
        expect(html).toContain('>Edit<');
        for (const tab of ['Details', 'Surfaces', 'History']) {
            expect(html).toContain(`>${tab}<`);
        }
    });

    it('links each surface the contributor quotes and none it does not', () => {
        const html = render(`/market-data/synthetic/vol-contributors/${BLOOMBERG}?tab=surfaces`);
        expect(html).toContain(`href="/market-data/synthetic/vol/${EURUSD}"`);
        expect(html).not.toContain(GBPUSD);
    });

    it('lists the surfaces a contributor quotes in source name order', () => {
        const both = [...links, { id: 'l3', feed_id: GBPUSD, contributor_id: BLOOMBERG }];
        expect(surfacesOf(BLOOMBERG, both, surfaces).map((row) => row['id'])).toEqual([
            EURUSD,
            GBPUSD,
        ]);
    });
});
