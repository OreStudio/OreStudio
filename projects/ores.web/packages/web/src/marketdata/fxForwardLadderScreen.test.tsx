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
import type { FxForwardLadder } from '../api/client.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { FxForwardLadderPage } from './FxForwardLadderPage.js';

const source = (currency: string, name: string) => ({
    currency,
    curve_name: name,
    output_series_id: '',
    publish_id: '',
    as_of: '2026-11-09 16:00:00Z',
    published_by: 'a.silva',
});

const row = (tenor: string, near: string, value: string, rate: number, swap: number) => ({
    tenor,
    near_date: near,
    value_date: value,
    base_discount: 0.99,
    quote_discount: 0.98,
    forward_rate: rate,
    forward_points: 19.9,
    swap_points: swap,
});

const ladder: FxForwardLadder = {
    pair_code: 'EUR/USD',
    as_of: '2026-11-09 23:59:59Z',
    spot: 1.085,
    spot_observed_at: '2026-11-09 16:00:02Z',
    spot_date: '2026-11-11',
    spot_days: 2,
    pip_factor: 0.0001,
    decimal_places: 4,
    base_curve: source('EUR', 'EUR-ESTR'),
    quote_curve: source('USD', 'USD-SOFR-FOMC'),
    rows: [
        row('ON', '2026-11-09', '2026-11-10', 1.08493, 0.67),
        row('1M', '2026-11-11', '2026-12-11', 1.08699, 19.9),
    ],
};

function draw(address: string): string {
    const client = new QueryClient();
    client.setQueryData(['currency-pairs', 'usd'], ['EUR/USD']);
    client.setQueryData(['fx-forward-ladder', 'EUR/USD', '2026-11-09'], ladder);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[address]}>
                    <FxForwardLadderPage />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the FX forward ladder page', () => {
    it('shows every measure of a tenor, beside its inputs', () => {
        const page = draw('/market-data/fx-forwards?pair=EUR/USD&asof=2026-11-09');
        for (const text of [
            'Forward rate',
            'Forward points',
            'Swap points',
            'EUR discount factor',
            'USD discount factor',
            'EUR-ESTR',
            'USD-SOFR-FOMC',
            'no cross-currency basis',
            '1.08699',
        ]) {
            expect(page).toContain(text);
        }
    });

    it('marks the rows a start from today moves', () => {
        const page = draw('/market-data/fx-forwards?pair=EUR/USD&asof=2026-11-09&start=today');
        expect(page).toContain('from today');
    });

    it('marks no row from spot', () => {
        const page = draw('/market-data/fx-forwards?pair=EUR/USD&asof=2026-11-09');
        expect(page).not.toContain('from today');
    });
});
