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
import type { RateLadder } from '../api/client.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { RateLadderPage } from './RateLadderPage.js';

const ladder: RateLadder = {
    currency: 'USD',
    instrument_type: 'discount',
    forward_start: '',
    as_of: '2026-11-13 23:59:59Z',
    provider_count: 3,
    tenor_convention: 'RATES_SPOT_FORWARD',
    day_count_convention: 'A365F',
    rungs: [
        {
            tenor: '1M',
            maturity_date: '2026-12-14',
            year_fraction: 0.0833,
            quotes: [
                {
                    provider_code: 'ore',
                    value: 0.0432,
                    as_of: '2026-11-13 10:00:00Z',
                    age_seconds: 60,
                    stale: false,
                    series_id: 's-1',
                },
                {
                    provider_code: 'bloomberg',
                    value: 0.0435,
                    as_of: '2026-11-13 10:00:00Z',
                    age_seconds: 90,
                    stale: false,
                    series_id: 's-2',
                },
                {
                    provider_code: 'derived',
                    value: 0.043,
                    as_of: '2026-11-13 10:00:00Z',
                    age_seconds: 120,
                    stale: false,
                    series_id: 's-3',
                },
            ],
            consensus: { value: 0.0432, method: 'mean', providers_used: 3 },
        },
        {
            tenor: '3M',
            maturity_date: '2027-02-15',
            year_fraction: 0.25,
            quotes: [
                {
                    provider_code: 'ore',
                    value: 0.041,
                    as_of: '2026-11-13 10:00:00Z',
                    age_seconds: 60,
                    stale: false,
                    series_id: 's-4',
                },
            ],
            consensus: { value: 0.041, method: 'mean', providers_used: 1 },
        },
    ],
};

function draw(address: string, set: RateLadder = ladder): string {
    const client = new QueryClient();
    client.setQueryData(['rate-ladder', 'USD', '2026-11-13', ''], set);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[address]}>
                    <RateLadderPage />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the rate ladder page', () => {
    it('draws each rung with the date the server dated it at', () => {
        const page = draw('/market-data/rate-ladder?ccy=USD&asof=2026-11-13');
        for (const text of [
            '1M',
            '2026-12-14',
            '3M',
            '2027-02-15',
            'RATES_SPOT_FORWARD',
            'A365F',
        ]) {
            expect(page).toContain(text);
        }
    });

    it('draws the consensus in percent and the best quote beside its distance', () => {
        const page = draw('/market-data/rate-ladder?ccy=USD&asof=2026-11-13');
        // 0.0432 as a percentage, and the best quote 0.0003 above it, in bp.
        expect(page).toContain('4.3200');
        expect(page).toContain('+3.0 bp');
    });

    it('names the providers that quote a rung', () => {
        const page = draw('/market-data/rate-ladder?ccy=USD&asof=2026-11-13');
        expect(page).toContain('ore, bloomberg, derived');
    });

    it('marks a rung fewer providers quote than the desk holds', () => {
        const page = draw('/market-data/rate-ladder?ccy=USD&asof=2026-11-13');
        expect(page).toContain('Thin');
        expect(page).toContain('Where the market is thin');
        expect(page).toContain('3M: quoted by ore; the consensus rests on mean.');
    });

    it('says so when the party holds no ladder for the currency', () => {
        const page = draw('/market-data/rate-ladder?ccy=USD&asof=2026-11-13', {
            ...ladder,
            rungs: [],
        });
        expect(page).toContain('The party holds no ladder for this currency.');
    });
});
