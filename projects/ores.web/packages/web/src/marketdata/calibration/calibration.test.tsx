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
import { describe, expect, it } from 'vitest';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter, Route, Routes } from 'react-router';
import type { CalibratedFeed, Calibration } from '../../api/client.js';
import { TranslationProvider } from '../../i18n/Provider.js';
import { moveProvider, needsAttention, outcomeOf, visibleFeeds } from './calibration.js';
import { CalibrationFeedPage, CalibrationPage, CalibrationPolicyPage } from './CalibrationPages.js';

const EURUSD = '11111111-1111-4111-8111-111111111111';
const AUDUSD = '22222222-2222-4222-8222-222222222222';
const GBPUSD = '33333333-3333-4333-8333-333333333333';

const start = {
    from_fixing: false,
    item: '',
    provider_code: '',
    vendor_id: '',
    value: '',
    inverted: false,
    fixing_date: null,
    received_at: null,
    window_days: 0,
    used: 0,
    hourly_spread: null,
    note: '',
    dispersion: [],
};

const eurusd: CalibratedFeed = {
    config_id: EURUSD,
    source_name: 'synthetic.eurusd',
    base_currency_code: 'EUR',
    quote_currency_code: 'USD',
    explicit_items: false,
    window_days_override: null,
    refused: false,
    start: {
        ...start,
        from_fixing: true,
        item: 'oresmd://fx/EUR?type=fixing&index=fx&source=ECB&ccy=USD',
        provider_code: 'ecb',
        value: '1.0841',
        fixing_date: '2026-10-30',
        window_days: 90,
        used: 63,
        hourly_spread: 0.0004,
        daily_spread: 0.0042,
    },
    candidates: [
        {
            item: 'oresmd://fx/EUR?type=fixing&index=fx&source=ECB&ccy=USD',
            provider_code: 'ecb',
            inverted: false,
            rank: 1,
            state: 'chosen',
            reason: 'the first candidate with a fixing',
        },
        {
            item: 'oresmd://fx/USD?type=fixing&index=fx&source=IMF&ccy=EUR',
            provider_code: 'imf',
            inverted: true,
            rank: 2,
            state: 'not_reached',
            reason: 'a candidate ranked higher was chosen',
        },
    ],
};

const audusd: CalibratedFeed = {
    ...eurusd,
    config_id: AUDUSD,
    source_name: 'synthetic.audusd',
    base_currency_code: 'AUD',
    start: { ...start, note: 'The config names no fixing item, and no series covers its pair.' },
    candidates: [],
};

const gbpusd: CalibratedFeed = {
    ...eurusd,
    config_id: GBPUSD,
    source_name: 'synthetic.gbpusd',
    base_currency_code: 'GBP',
    explicit_items: true,
};

function calibration(over: Partial<Calibration> = {}): Calibration {
    return {
        assetClass: 'fx',
        policy: {
            from_policy_row: false,
            window_days: 90,
            allow_inverse: true,
            fallback_code: 'keep_fixed',
            provider_order: [],
            version: null,
        },
        policyWritable: true,
        feeds: [eurusd, audusd, gbpusd],
        coverage: { total: 3, fromFixing: 2, keptFixed: 1, refused: 0, overridden: 1 },
        ...over,
    };
}

function render(path: string, data: Calibration): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['calibration', 'fx'], data);
    client.setQueryData(['public-rates'], []);
    client.setQueryData(['reference-data-reasons', 'amend'], []);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[path]}>
                    <Routes>
                        <Route
                            path="/market-data/calibration/:assetClass"
                            element={<CalibrationPage />}
                        />
                        <Route
                            path="/market-data/calibration/:assetClass/feeds/:configId"
                            element={<CalibrationFeedPage />}
                        />
                        <Route
                            path="/market-data/calibration/:assetClass/policy"
                            element={<CalibrationPolicyPage />}
                        />
                    </Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('how a feed resolved', () => {
    it('names the outcome and whether the feed needs attention', () => {
        expect(outcomeOf(eurusd)).toBe('fixing');
        expect(outcomeOf(audusd)).toBe('fixed');
        expect(outcomeOf({ ...audusd, refused: true })).toBe('refused');
        expect(needsAttention(eurusd)).toBe(false);
        expect(needsAttention(audusd)).toBe(true);
        expect(needsAttention(gbpusd)).toBe(true);
    });

    it('filters to the feeds that need attention or override the policy', () => {
        const feeds = [eurusd, audusd, gbpusd];
        expect(visibleFeeds(feeds, 'all')).toHaveLength(3);
        expect(visibleFeeds(feeds, 'attention').map((f) => f.config_id)).toEqual([AUDUSD, GBPUSD]);
        expect(visibleFeeds(feeds, 'overridden').map((f) => f.config_id)).toEqual([GBPUSD]);
    });

    it('moves a provider within the order and never past its ends', () => {
        expect(moveProvider(['ecb', 'imf', 'boc'], 'boc', -1)).toEqual(['ecb', 'boc', 'imf']);
        expect(moveProvider(['ecb', 'imf'], 'ecb', -1)).toEqual(['ecb', 'imf']);
        expect(moveProvider(['ecb', 'imf'], 'zzz', 1)).toEqual(['ecb', 'imf']);
    });
});

describe('the calibration list', () => {
    it('states automatic calibration and lists every feed with its start', () => {
        const html = render('/market-data/calibration/fx', calibration());
        expect(html).toContain(
            'Automatic: 90-day window, inverse allowed, every provider, by code',
        );
        expect(html).toContain('href="/market-data/calibration/fx/policy"');
        expect(html).toContain('EUR/USD');
        expect(html).toContain('1.0841');
        expect(html).toContain('AUD/USD');
        expect(html).toContain('kept a level');
        expect(html).toContain(`href="/market-data/calibration/fx/feeds/${EURUSD}"`);
        expect(html).not.toMatch(/vintage/i);
    });

    it('shows only the overridden feeds under that filter', () => {
        const html = render('/market-data/calibration/fx?filter=overridden', calibration());
        expect(html).toContain('GBP/USD');
        expect(html).not.toContain('AUD/USD');
    });
});

describe('a feed calibration record', () => {
    it('shows the start, the candidates with their reasons and the measures', () => {
        const html = render(`/market-data/calibration/fx/feeds/${EURUSD}`, calibration());
        expect(html).toContain('Feed record');
        expect(html).toContain('starts here');
        expect(html).toContain('not reached');
        expect(html).toContain('a candidate ranked higher was chosen');
        expect(html).toContain('1/x');
        expect(html).toContain('Realised volatility');
        expect(html).toContain('This feed follows its class policy.');
        expect(html).not.toMatch(/vintage/i);
    });

    it('says the feed overrides the policy and offers the way back', () => {
        const html = render(`/market-data/calibration/fx/feeds/${GBPUSD}`, calibration());
        expect(html).toContain('This feed names its own items, so it overrides the class policy.');
        expect(html).toContain('Clear the items and follow the policy');
        expect(html).toContain('href="/market-data/calibration/fx/policy"');
    });

    it('explains the flow from the policy to the process', () => {
        const html = render(`/market-data/calibration/fx/feeds/${EURUSD}?tab=flow`, calibration());
        expect(html).toContain('Starts from the ecb fixing of 1.0841 on 2026-10-30.');
        expect(html).toContain('The measured volatility scales the mixture&#x27;s deviations.');
    });
});

describe('the calibration policy', () => {
    it('keeps the screen to the system administrator', () => {
        const html = render('/market-data/calibration/fx/policy', calibration());
        expect(html).toContain('Calibration is a system control');
        expect(html).not.toContain('Save the policy');
    });

    it('offers the save and the way back to automatic to the system tenant', () => {
        const html = render(
            '/market-data/calibration/fx/policy',
            calibration({
                policyWritable: true,
                policy: {
                    from_policy_row: true,
                    window_days: 60,
                    allow_inverse: false,
                    fallback_code: 'refuse',
                    provider_order: ['ecb', 'imf'],
                    version: 2,
                },
            }),
        );
        expect(html).toContain('Save the policy');
        expect(html).toContain('Return to automatic');
        expect(html).toContain('Move imf up');
    });
});
