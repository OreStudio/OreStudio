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
import type { FxMontage } from '../api/client.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { FxMontagePage } from './FxMontagePage.js';

const montage: FxMontage = {
    base_currency: 'EUR',
    as_of: '2026-11-13 23:59:59Z',
    change_since: '2026-11-13 00:00:00Z',
    pairs: [
        {
            pair_name: 'EUR/USD',
            base_currency: 'EUR',
            quote_currency: 'USD',
            spot_by_provider: [
                {
                    provider_code: 'ore',
                    value: 1.0852,
                    as_of: '2026-11-13 10:00:00Z',
                    age_seconds: 60,
                    stale: false,
                    series_id: 's-1',
                },
                {
                    provider_code: 'derived',
                    value: 1.0848,
                    as_of: '2026-11-13 10:00:00Z',
                    age_seconds: 120,
                    stale: false,
                    series_id: 's-2',
                },
            ],
            consensus: 1.085,
            change_pct: 0.12,
            day_high: 1.0891,
            day_low: 1.0802,
        },
        {
            pair_name: 'EUR/GBP',
            base_currency: 'EUR',
            quote_currency: 'GBP',
            spot_by_provider: [
                {
                    provider_code: 'ore',
                    value: 0.8421,
                    as_of: '2026-11-13 10:00:00Z',
                    age_seconds: 60,
                    stale: false,
                    series_id: 's-3',
                },
            ],
            consensus: 0.8421,
            change_pct: null,
            day_high: null,
            day_low: null,
        },
    ],
};

function draw(address: string, set: FxMontage = montage): string {
    const client = new QueryClient();
    client.setQueryData(['fx-montage', 'EUR', '2026-11-13'], set);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[address]}>
                    <FxMontagePage />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the FX montage page', () => {
    it('draws every cross of the base with its consensus and its range', () => {
        const page = draw('/market-data/fx-montage?base=EUR&asof=2026-11-13');
        for (const text of ['EUR/USD', '1.0850', '1.0802', '1.0891', 'EUR/GBP', '0.8421']) {
            expect(page).toContain(text);
        }
    });

    it('states the change and leaves an unmeasured one absent', () => {
        const page = draw('/market-data/fx-montage?base=EUR&asof=2026-11-13');
        expect(page).toContain('+0.12%');
        // The second row's window holds no price at its start.
        expect(page).toContain('—');
    });

    it('says which instant the change is measured from', () => {
        const page = draw('/market-data/fx-montage?base=EUR&asof=2026-11-13');
        expect(page).toContain('Change measured from');
    });

    it('sorts by move when asked', () => {
        const page = draw('/market-data/fx-montage?base=EUR&asof=2026-11-13&sort=move');
        expect(page.indexOf('EUR/USD')).toBeLessThan(page.indexOf('EUR/GBP'));
    });

    it('says nothing is quoted when the base has no crosses', () => {
        const page = draw('/market-data/fx-montage?base=EUR&asof=2026-11-13', {
            ...montage,
            pairs: [],
        });
        expect(page).toContain('Nothing quoted.');
    });
});
