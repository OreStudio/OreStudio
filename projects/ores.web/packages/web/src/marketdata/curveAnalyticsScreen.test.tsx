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
import type { CurveAnalytics } from '../api/client.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { CurveAnalyticsPage } from './CurveAnalyticsPage.js';

const CONFIG_ID = 'c0000000-0000-0000-0000-000000000001';

const analytics: CurveAnalytics = {
    instrument_id: CONFIG_ID,
    name: 'USD-SOFR',
    as_of: '2026-11-13 23:59:59Z',
    stale: false,
    conventions: {
        index_name: 'USD-SOFR',
        day_count: 'A365F',
        compounding: 'continuous',
        interpolation: 'flat_forward_then_log_linear',
        calendar: 'UnitedStates.Settlement',
        spot_lag_days: 2,
        pillar_count: 1,
    },
    market_pillars: [
        {
            tenor: '1M',
            maturity_date: '2026-12-14',
            year_fraction: 0.0833,
            rate: 0.0432,
            provider_code: 'ore',
            series_id: 's-1',
            as_of: '2026-11-13 10:00:00Z',
        },
    ],
    bootstrapped: [
        {
            tenor: '1M',
            maturity_date: '2026-12-14',
            year_fraction: 0.0833,
            discount_factor: 0.9964,
            zero_rate: 0.0432,
            implied_forward: 0.0432,
            shift_bps: 1.25,
        },
    ],
};

function draw(address: string, set: CurveAnalytics = analytics): string {
    const client = new QueryClient();
    client.setQueryData(
        ['records', 'ir-curve-bootstrap-configs'],
        [{ id: CONFIG_ID, name: 'USD-SOFR' }],
    );
    client.setQueryData(
        ['curve-analytics', CONFIG_ID, address.includes('asof=2026-11-13') ? '2026-11-13' : ''],
        set,
    );
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[address]}>
                    <CurveAnalyticsPage />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the curve analytics page', () => {
    it('states the conventions the curve was built under', () => {
        const page = draw(`/market-data/curve-analytics?curve=${CONFIG_ID}&asof=2026-11-13`);
        for (const text of [
            'USD-SOFR',
            'A365F',
            'continuous',
            'flat_forward_then_log_linear',
            'UnitedStates.Settlement',
        ]) {
            expect(page).toContain(text);
        }
    });

    it('pairs the market pillar with what the bootstrap made of it', () => {
        const page = draw(`/market-data/curve-analytics?curve=${CONFIG_ID}&asof=2026-11-13`);
        expect(page).toContain('Market pillars');
        expect(page).toContain('Bootstrapped curve');
        expect(page).toContain('2026-12-14');
        // The market rate and the zero rate the curve derives from it, in percent.
        expect(page).toContain('4.3200');
        expect(page).toContain('0.996400');
    });

    it('states the shift in basis points as the curve less the market', () => {
        const page = draw(`/market-data/curve-analytics?curve=${CONFIG_ID}&asof=2026-11-13`);
        expect(page).toContain('+1.3');
    });

    it('warns when the newest quote behind the curve is old', () => {
        const page = draw(`/market-data/curve-analytics?curve=${CONFIG_ID}&asof=2026-11-13`, {
            ...analytics,
            stale: true,
        });
        expect(page).toContain('older than the server treats as current');
    });

    it('says when the party holds no curve configuration', () => {
        const client = new QueryClient();
        client.setQueryData(['records', 'ir-curve-bootstrap-configs'], []);
        const page = renderToStaticMarkup(
            <QueryClientProvider client={client}>
                <TranslationProvider>
                    <MemoryRouter initialEntries={['/market-data/curve-analytics']}>
                        <CurveAnalyticsPage />
                    </MemoryRouter>
                </TranslationProvider>
            </QueryClientProvider>,
        );
        expect(page).toContain('The party holds no curve configuration.');
    });
});
