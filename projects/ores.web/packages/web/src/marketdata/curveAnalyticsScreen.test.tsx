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
const PROJECTION_ID = 'c0000000-0000-0000-0000-000000000002';

const analytics: CurveAnalytics = {
    instrument_id: CONFIG_ID,
    name: 'USD-SOFR',
    as_of: '2026-11-13 23:59:59Z',
    compare_at: '',
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
        {
            tenor: '3M',
            maturity_date: '2027-02-13',
            year_fraction: 0.2521,
            rate: 0.0418,
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
            prior_zero_rate: null,
            change_bps: null,
        },
        {
            tenor: '3M',
            maturity_date: '2027-02-13',
            year_fraction: 0.2521,
            discount_factor: 0.9895,
            zero_rate: 0.0419,
            implied_forward: 0.0414,
            shift_bps: 0.9,
            prior_zero_rate: null,
            change_bps: null,
        },
    ],
};

/** A term curve, drawn a little above the overnight curve it discounts off. */
const libor: CurveAnalytics = {
    instrument_id: PROJECTION_ID,
    name: 'USD-LIBOR-1M',
    as_of: '2026-11-13 23:59:59Z',
    stale: false,
    compare_at: '',
    conventions: {
        index_name: 'USD-LIBOR-1M',
        day_count: 'A360F',
        compounding: 'simple',
        interpolation: 'flat_forward_then_log_linear',
        calendar: 'UnitedKingdom.London',
        spot_lag_days: 2,
        pillar_count: 2,
    },
    market_pillars: [
        {
            tenor: '1M',
            maturity_date: '2026-12-14',
            year_fraction: 0.0833,
            rate: 0.0462,
            provider_code: 'ice',
            series_id: 's-2',
            as_of: '2026-11-13 10:00:00Z',
        },
        {
            tenor: '3M',
            maturity_date: '2027-02-13',
            year_fraction: 0.2521,
            rate: 0.0448,
            provider_code: 'ice',
            series_id: 's-2',
            as_of: '2026-11-13 10:00:00Z',
        },
    ],
    bootstrapped: [
        {
            tenor: '1M',
            maturity_date: '2026-12-14',
            year_fraction: 0.0833,
            discount_factor: 0.9962,
            zero_rate: 0.0462,
            implied_forward: 0.046,
            shift_bps: 2,
            prior_zero_rate: null,
            change_bps: null,
        },
        {
            tenor: '3M',
            maturity_date: '2027-02-13',
            year_fraction: 0.2521,
            discount_factor: 0.9887,
            zero_rate: 0.0451,
            implied_forward: 0.044,
            shift_bps: 1.9,
            prior_zero_rate: null,
            change_bps: null,
        },
    ],
};

/** The funding curve the term curve above names as the one it discounts off. */
const funding = { ...analytics, name: 'USD-SOFR-FOMC', instrument_id: CONFIG_ID };

const pairOfConfigs = [
    {
        id: CONFIG_ID,
        name: 'USD-SOFR-FOMC',
        curve_family_role: 'FUNDING',
        index_tenor: '1D',
    },
    {
        id: PROJECTION_ID,
        name: 'USD-LIBOR-1M',
        curve_family_role: 'PROJECTION',
        index_tenor: '1M',
        discount_curve_config_id: CONFIG_ID,
    },
];

function draw(
    address: string,
    set: CurveAnalytics = analytics,
    configs: readonly Record<string, unknown>[] = [{ id: CONFIG_ID, name: 'USD-SOFR' }],
    beside?: CurveAnalytics,
): string {
    const client = new QueryClient();
    client.setQueryData(['records', 'ir-curve-bootstrap-configs'], configs);
    const asOf = address.includes('asof=2026-11-13') ? '2026-11-13' : '';
    const compare = address.includes('compare=') ? '2026-11-12' : '';
    client.setQueryData(['curve-analytics', set.instrument_id, asOf, compare], set);
    if (beside !== undefined) {
        // The second curve is read at one instant, so it carries no prior set.
        client.setQueryData(['curve-analytics', beside.instrument_id, asOf, ''], beside);
    }
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

    it('names each curve by its role and index tenor, so a LIBOR tenor reads apart from its overnight', () => {
        const page = draw(
            `/market-data/curve-analytics?curve=${CONFIG_ID}&asof=2026-11-13`,
            analytics,
            [
                {
                    id: CONFIG_ID,
                    name: 'USD-SOFR-FOMC',
                    curve_family_role: 'FUNDING',
                    index_tenor: '1D',
                },
                {
                    id: PROJECTION_ID,
                    name: 'USD-LIBOR-1M',
                    curve_family_role: 'PROJECTION',
                    index_tenor: '1M',
                },
            ],
        );
        expect(page).toContain('USD-SOFR-FOMC · Funding · 1D');
        expect(page).toContain('USD-LIBOR-1M · Projection · 1M');
    });

    it('draws a projection beside the funding curve it discounts off', () => {
        const page = draw(
            `/market-data/curve-analytics?curve=${PROJECTION_ID}&asof=2026-11-13`,
            libor,
            pairOfConfigs,
            funding,
        );
        // The pair is read without being asked for, because the projection names it.
        expect(page).toContain('USD-LIBOR-1M is drawn solid and USD-SOFR-FOMC dashed beside it');
        expect(page).toContain('Pillar by pillar');
        // Either curve can be read alone, as the prototype offers.
        expect(page).toContain('Both curves');
        expect(page).toContain('USD-SOFR-FOMC alone');
    });

    it('states the gap between the two curves at each pillar, in basis points', () => {
        const page = draw(
            `/market-data/curve-analytics?curve=${PROJECTION_ID}&asof=2026-11-13`,
            libor,
            pairOfConfigs,
            funding,
        );
        // The term curve opens on its forward rates: 4.60% against the overnight 4.32%.
        expect(page).toContain('4.6000');
        expect(page).toContain('4.3200');
        expect(page).toContain('+28.0');
        expect(page).toContain('+26.0');
    });

    it('draws one curve alone when the reader asks for no second curve', () => {
        const page = draw(
            `/market-data/curve-analytics?curve=${PROJECTION_ID}&asof=2026-11-13&against=none`,
            libor,
            pairOfConfigs,
            funding,
        );
        expect(page).not.toContain('Pillar by pillar');
        expect(page).toContain('The bootstrapped zero rate');
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

    it('draws the curve and switches the metric it draws', () => {
        const zero = draw(`/market-data/curve-analytics?curve=${CONFIG_ID}&asof=2026-11-13`);
        expect(zero).toContain('The bootstrapped zero rate');
        expect(zero).toContain('<svg');
        const discount = draw(
            `/market-data/curve-analytics?curve=${CONFIG_ID}&asof=2026-11-13&m=discount`,
        );
        expect(discount).toContain('The discount factor the curve holds at each pillar.');
        expect(discount).not.toContain('The bootstrapped zero rate');
    });

    it('draws the prior close and the change when the answer carries one', () => {
        const page = draw(
            `/market-data/curve-analytics?curve=${CONFIG_ID}&asof=2026-11-13&compare=2026-11-12`,
            {
                ...analytics,
                compare_at: '2026-11-12 23:59:59Z',
                bootstrapped: analytics.bootstrapped.map((point, index) => ({
                    ...point,
                    prior_zero_rate: index === 0 ? 0.0439 : 0.0421,
                    change_bps: index === 0 ? -0.7 : -0.2,
                })),
            },
        );
        expect(page).toContain('Prior zero (%)');
        expect(page).toContain('Change (bp)');
        expect(page).toContain('Measured from the published set as of 2026-11-12 23:59:59Z.');
        expect(page).toContain('4.3900');
        expect(page).toContain('-0.7');
    });

    it('says no set was current rather than drawing a zero', () => {
        const page = draw(
            `/market-data/curve-analytics?curve=${CONFIG_ID}&asof=2026-11-13&compare=2026-11-12`,
            { ...analytics, compare_at: '' },
        );
        expect(page).toContain('No published set was current at that instant');
        expect(page).not.toContain('Prior zero (%)');
    });

    it('draws no prior column when no comparison was asked for', () => {
        const page = draw(`/market-data/curve-analytics?curve=${CONFIG_ID}&asof=2026-11-13`);
        expect(page).not.toContain('Prior zero (%)');
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
