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
import { describe, expect, it } from 'vitest';
import type { InstrumentCatalogue } from '../api/client.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { InstrumentFinderPage } from './InstrumentFinderPage.js';

const catalogue: InstrumentCatalogue = {
    total: 3,
    truncated: false,
    instruments: [
        {
            instrument_id: 'a0000000-0000-0000-0000-000000000001',
            kind: 'curve',
            name: 'USD-SOFR',
            currency: 'USD',
            base_currency: '',
            quote_currency: '',
            instrument_type: 'discount',
            quote_type: 'rate',
            forward_start: '',
            axes: [{ field: 'term', values: ['ON', '1M', '3M'] }],
            sources: [
                {
                    provider_code: 'ore',
                    provider_kind: 'derived',
                    series_id: 's-1',
                    as_of: '2026-11-13 10:00:00Z',
                    age_seconds: 900,
                    stale: false,
                },
            ],
            party_id: 'p-1',
            stale: false,
            as_of: '2026-11-13 10:00:00Z',
        },
        {
            instrument_id: 'a0000000-0000-0000-0000-000000000002',
            kind: 'pair',
            name: 'EUR/USD',
            currency: '',
            base_currency: 'EUR',
            quote_currency: 'USD',
            instrument_type: 'fx_spot',
            quote_type: 'rate',
            forward_start: '',
            axes: [],
            sources: [
                {
                    provider_code: 'derived',
                    provider_kind: 'derived',
                    series_id: 's-2',
                    as_of: '2026-11-13 10:00:00Z',
                    age_seconds: 60,
                    stale: false,
                },
            ],
            party_id: 'p-1',
            stale: false,
            as_of: '2026-11-13 10:00:00Z',
        },
        {
            instrument_id: 'a0000000-0000-0000-0000-000000000003',
            kind: 'surface',
            name: 'EURUSD_SSVI',
            currency: 'EUR',
            base_currency: 'EUR',
            quote_currency: 'USD',
            instrument_type: 'fx_vol_surface',
            quote_type: 'rate_lnvol',
            forward_start: '',
            axes: [
                { field: 'expiry', values: ['1W', '1M'] },
                { field: 'strike', values: ['ATM', 'DEL/Spot/Call/25'] },
            ],
            sources: [
                {
                    provider_code: 'ore',
                    provider_kind: 'derived',
                    series_id: 's-3',
                    as_of: '2026-11-13 10:00:00Z',
                    age_seconds: 3600,
                    stale: true,
                },
            ],
            party_id: 'p-1',
            stale: true,
            as_of: '2026-11-13 10:00:00Z',
        },
    ],
};

function draw(address: string, set: InstrumentCatalogue = catalogue): string {
    const client = new QueryClient();
    client.setQueryData(['market-instruments', '', '', ''], set);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[address]}>
                    <InstrumentFinderPage />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the instrument finder', () => {
    it('draws one row per instrument, with its kind, sources and values', () => {
        const page = draw('/market-data/instruments');
        for (const text of ['Curve', 'USD-SOFR', 'ON, 1M, 3M', 'ore', 'Pair', 'EUR/USD']) {
            expect(page).toContain(text);
        }
    });

    it('draws a surface as one instrument with two axes of values', () => {
        const page = draw('/market-data/instruments');
        expect(page).toContain('Surface');
        expect(page).toContain('1W, 1M, ATM, DEL/Spot/Call/25');
    });

    it('opens a pair at the FX montage of its base, as the journey says', () => {
        const page = draw('/market-data/instruments');
        expect(page).toContain('href="/market-data/fx-montage?base=EUR"');
    });

    it('opens a curve at the curve inspector and leaves a surface as text', () => {
        const page = draw('/market-data/instruments');
        expect(page).toContain('href="/market-data/curve-analytics?curve=USD-SOFR"');
        // A surface has no page, so its name is not a link.
        expect(page).not.toContain('href="/market-data/curve-analytics?curve=EURUSD_SSVI"');
    });

    it('says nothing matched when the catalogue answers nothing', () => {
        const page = draw('/market-data/instruments', {
            total: 0,
            truncated: false,
            instruments: [],
        });
        expect(page).toContain('No instrument matches.');
    });
});
