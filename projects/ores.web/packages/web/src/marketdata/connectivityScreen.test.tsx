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
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import { describe, expect, it } from 'vitest';
import type { Connectivity } from '../api/client.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { ConnectivityPage } from './ConnectivityPage.js';

const CATALOGUE: Connectivity = {
    publishers: [
        {
            code: 'ecb',
            name: 'European Central Bank',
            kind: 'central_bank',
            description: '',
            terms_url: 'https://ecb.example/terms',
            terms_credit: 'Source: ECB',
            terms_accepted: true,
            terms_accepted_by: 'admin',
            terms_accepted_at: '2026-10-30 10:00:00Z',
        },
    ],
    suppliers: [
        {
            code: 'frankfurter',
            name: 'Frankfurter',
            kind: 'aggregator',
            description: '',
            contract_reference: 'C-1',
            support: 'support@frankfurter.example',
        },
    ],
    adapters: [
        {
            code: 'frankfurter',
            name: 'Frankfurter',
            module_version: '0.0.29',
            description: '',
            process: 'ores.marketdata.official',
            asset_classes: ['fx'],
            services: ['end_of_day'],
            schema: [
                {
                    name: 'url',
                    type: 'url',
                    required: false,
                    default: 'https://api.frankfurter.dev',
                },
            ],
        },
    ],
    sessions: [
        {
            code: 'frankfurter-eu',
            name: 'Frankfurter EU',
            supplier_code: 'frankfurter',
            adapter_code: 'frankfurter',
            environment: 'production',
            endpoint: 'https://api.frankfurter.dev',
            credential_ref: 'none',
            settings: { poll_minutes: 60 },
            enabled: true,
            description: '',
        },
    ],
    feeds: [
        {
            id: 'f0000000-0000-0000-0000-000000000001',
            name: 'ecb-frankfurter',
            publisher_code: 'ecb',
            session_code: 'frankfurter-eu',
            service: 'end_of_day',
            provider_key: 'ECB',
            is_default: true,
            description: '',
            scopes: [
                {
                    asset_class: 'fx',
                    item: 'oresmd://fx/EUR?type=fixing&index=fx&source=ECB&ccy=USD',
                    vendor_instrument_id: null,
                },
            ],
            routes: [
                {
                    party_id: '55555555-5555-5555-5555-555555555555',
                    publisher_code: 'ecb',
                    asset_class: 'fx',
                },
            ],
        },
    ],
};

function render(): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['connectivity'], CATALOGUE);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter>
                    <ConnectivityPage />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the connectivity screen', () => {
    it('draws the delivery tree and the publisher tree', () => {
        const html = render();
        expect(html).toContain('Connectivity');
        expect(html).toContain('Frankfurter');
        expect(html).toContain('frankfurter-eu');
        expect(html).toContain('ecb-frankfurter');
        expect(html).toContain('European Central Bank');
    });

    it('opens with the counts of what connectivity holds', () => {
        const html = render();
        expect(html).toContain('Delivery overview');
        expect(html).toContain('Adapters catalogue');
    });
});
