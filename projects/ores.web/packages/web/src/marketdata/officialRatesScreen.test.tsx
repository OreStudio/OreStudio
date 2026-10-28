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
import type { OfficialFixings, OfficialProvider } from '../api/client.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { OfficialRatesPage } from './OfficialRatesPage.js';

function provider(
    code: string,
    name: string,
    pivot: string,
    quotes: readonly string[],
    accepted: boolean,
): OfficialProvider {
    return {
        code,
        version: 3,
        name,
        description: '',
        terms_url: `https://${code}.example/terms`,
        terms_credit: `Source: ${name}`,
        allows_storage: true,
        terms_accepted: accepted,
        terms_accepted_by: accepted ? 'super_admin' : '',
        terms_accepted_at: accepted ? '2026-10-28 09:00:00Z' : null,
        sources: [{ id: `${code}-source`, name: `${code}-frankfurter`, kind: 'vendor' }],
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

const FIXINGS: OfficialFixings = {
    'ecb-USD': [
        { date: '2026-10-26', value: '1.1600' },
        { date: '2026-10-27', value: '1.1500' },
    ],
    'boc-USD': [
        { date: '2026-10-26', value: '1.3900' },
        { date: '2026-10-27', value: '1.4000' },
    ],
    'boc-EUR': [
        { date: '2026-10-26', value: '1.6000' },
        { date: '2026-10-27', value: '1.6100' },
    ],
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
    client.setQueryData(['official-rates'], PROVIDERS);
    client.setQueryData(['official-fixings', 'ecb-USD'], { 'ecb-USD': FIXINGS['ecb-USD'] });
    client.setQueryData(['official-fixings', 'ecb-USD', 'boc-USD', 'boc-EUR'], FIXINGS);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter>
                    <OfficialRatesPage />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the official rates screen', () => {
    it('lists each provider with its terms state and its source', () => {
        const html = render(['marketdata::providers:read']);
        expect(html).toContain('European Central Bank');
        expect(html).toContain('By super_admin on 2026-10-28');
        expect(html).toContain('Not accepted');
        expect(html).toContain('boc-frankfurter');
    });

    it('offers Accept terms only to a person who may accept them', () => {
        expect(render(['marketdata::providers:read'])).not.toContain('Accept terms');
        expect(
            render(['marketdata::providers:read', 'marketdata::providers:accept_terms']),
        ).toContain('Accept terms');
    });

    it('lists the first provider’s latest fixings, newest first', () => {
        const html = render(['marketdata::providers:read']);
        expect(html).toContain('The rates of European Central Bank');
        expect(html.indexOf('1.1500')).toBeLessThan(html.indexOf('1.1600'));
    });

    it('compares one pair across providers and names a crossed value as such', () => {
        const html = render(['marketdata::providers:read']);
        expect(html).toContain('Published');
        expect(html).toContain('Crossed');
        expect(html).toContain((1.4 / 1.61).toPrecision(6));
    });
});
