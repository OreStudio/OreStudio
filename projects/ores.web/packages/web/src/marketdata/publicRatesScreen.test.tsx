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
import type { PublicRatesFixings, PublicRatesProvider } from '../api/client.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { PublicRatesPage } from './PublicRatesPage.js';

const TODAY = new Date().toISOString().slice(0, 10);
const YESTERDAY = new Date(Date.now() - 86_400_000).toISOString().slice(0, 10);

function provider(
    code: string,
    name: string,
    pivot: string,
    quotes: readonly string[],
    accepted: boolean,
): PublicRatesProvider {
    return {
        code,
        version: 3,
        name,
        kind: 'central_bank',
        description: '',
        terms_url: `https://${code}.example/terms`,
        terms_credit: `Source: ${name}`,
        allows_storage: true,
        terms_accepted: accepted,
        terms_accepted_by: accepted ? 'super_admin' : '',
        terms_accepted_at: accepted ? '2026-10-28 09:00:00Z' : null,
        feeds: [
            {
                id: `${code}-feed`,
                name: `${code}-frankfurter`,
                service: 'end_of_day',
                provider_key: code.toUpperCase(),
                is_default: true,
                session_code: 'frankfurter-eu',
                supplier_code: 'frankfurter',
                supplier_name: 'Frankfurter',
                asset_classes: ['fx'],
            },
        ],
        series: quotes.map((quote) => ({
            id: `${code}-${quote}`,
            oresmd_uri: `oresmd://fx/${pivot}?type=fixing&index=fx&source=${code.toUpperCase()}&ccy=${quote}`,
            base: pivot,
            quote,
        })),
    };
}

const PROVIDERS = [
    provider('ecb', 'European Central Bank', 'EUR', ['USD', 'GBP'], true),
    provider('boc', 'Bank of Canada', 'CAD', ['EUR', 'USD'], false),
];

const FIXINGS: PublicRatesFixings = {
    'ecb-USD': [
        { date: YESTERDAY, value: '1.1600' },
        { date: TODAY, value: '1.1500' },
    ],
};

const COVERAGE = {
    'ecb-USD': { first: YESTERDAY, last: TODAY, count: 2 },
    'ecb-GBP': { first: YESTERDAY, last: TODAY, count: 2 },
    'boc-EUR': { first: '2026-09-01', last: '2026-09-02', count: 2 },
    'boc-USD': { first: '2026-09-01', last: '2026-09-02', count: 2 },
};

function render(permissions: readonly string[]): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['my-access'], {
        roles: [
            {
                roleId: 'r',
                name: 'System administrator',
                description: '',
                permissionCodes: [...permissions],
                givenBy: 'system',
                givenAt: '2026-10-28 09:00:00Z',
                reasonCode: 'access.initial',
                commentary: '',
            },
        ],
    });
    client.setQueryData(['public-rates'], PROVIDERS);
    client.setQueryData(['public-rates-coverage'], COVERAGE);
    client.setQueryData(['public-rates-fixings', 'ecb-USD'], { 'ecb-USD': FIXINGS['ecb-USD'] });
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter>
                    <PublicRatesPage />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the public rates screen', () => {
    it('lists each publisher with its terms state, its feed and its freshness', () => {
        const html = render(['marketdata::providers:read']);
        expect(html).toContain('Public rates');
        expect(html).toContain('European Central Bank');
        expect(html).toContain('By super_admin on 2026-10-28');
        expect(html).toContain('Not accepted');
        expect(html).toContain('ecb-frankfurter');
        expect(html).toContain('Frankfurter');
        expect(html).toContain('Fresh');
        expect(html).toContain('Late');
    });

    it('offers Accept terms only to a person who may accept them', () => {
        expect(render(['marketdata::providers:read'])).not.toContain('Accept terms');
        expect(
            render(['marketdata::providers:read', 'marketdata::providers:accept_terms']),
        ).toContain('Accept terms');
    });

    it('lists the first publisher’s latest fixings, newest first', () => {
        const html = render(['marketdata::providers:read']);
        expect(html).toContain('The rates of European Central Bank');
        expect(html.indexOf('1.1500')).toBeLessThan(html.indexOf('1.1600'));
    });

    it('shows the tabs of the screen', () => {
        const html = render(['marketdata::providers:read']);
        expect(html).toContain('Availability — who holds this underlying');
        expect(html).toContain('Compare — how far apart they are');
        expect(html).toContain('Fetch — ask for data now');
    });
});
