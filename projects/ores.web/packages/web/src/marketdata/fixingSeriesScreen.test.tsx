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
import type { FixingSeries } from '../api/client.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { FixingSeriesPage } from './FixingSeriesPage.js';

const URI = 'oresmd://fx/EUR?type=fixing&index=fx&source=ECB&ccy=USD';

const SERIES: FixingSeries = {
    series: { id: 's1', oresmd_uri: URI, provider_code: 'ecb', version: 1 },
    publisher: { code: 'ecb', name: 'European Central Bank' },
    values: {
        first: '2026-10-29',
        last: '2026-10-30',
        count: 2,
        rows: [
            { date: '2026-10-29', value: '1.16' },
            { date: '2026-10-30', value: '1.15' },
        ],
    },
    feeds: [
        {
            id: 'f1',
            name: 'ecb-frankfurter',
            service: 'end_of_day',
            session_code: 'frankfurter-eu',
            environment: 'production',
            supplier_name: 'Frankfurter',
        },
    ],
};

function render(uri: string): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['fixing-series', uri], SERIES);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter
                    initialEntries={[`/market-data/fixings?uri=${encodeURIComponent(uri)}`]}
                >
                    <FixingSeriesPage />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the fixing series screen', () => {
    it('draws the stored values and the feed that carried them', () => {
        const html = render(URI);

        expect(html).toContain('Fixing series');
        expect(html).toContain('European Central Bank');
        expect(html).toContain('2026-10-30');
        expect(html).toContain('1.1500'.slice(0, 4));
        expect(html).toContain('ecb-frankfurter');
        expect(html).toContain('Frankfurter');
    });
});
