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
import type { FeedStart, RecordRow } from '../../api/client.js';
import { TranslationProvider } from '../../i18n/Provider.js';
import { FeedRunTab } from './feedStart.js';

const CALIBRATION = 'b608b4be-6a2d-56d1-a144-09cb668ba393';

const START: FeedStart = {
    from_fixing: true,
    item: 'oresmd://fx/EUR?type=fixing&index=fx&source=ECB&ccy=USD',
    provider_code: 'ecb',
    vendor_id: 'EUR',
    value: '1.15',
    inverted: false,
    fixing_date: '2026-10-27',
    received_at: '2026-10-27 16:05:00.000000Z',
    window_days: 400,
    used: 261,
    hourly_spread: 0.0004,
    note: '',
    dispersion: [],
    variance: {
        model: 'gjr_garch',
        calibration_id: CALIBRATION,
        calibration_version: 2,
        fitted: true,
        alpha: 0.0113,
        gamma: 0.023,
        beta: 0.9323,
        persistence: 0.9551,
        initial_ratio: 0.92,
        returns: 279,
        note: '',
    },
};

function feed(over: Record<string, unknown> = {}): RecordRow {
    return {
        id: 'feed-1',
        asset_class_code: 'fx',
        variance_model: 'gjr_garch',
        variance_calibration_id: CALIBRATION,
        gmm_initial_price: 0.7196,
        ...over,
    } as unknown as RecordRow;
}

function render(start: FeedStart | null, row: RecordRow = feed()): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <MemoryRouter>
                <TranslationProvider>
                    <FeedRunTab feed={row} start={start} />
                </TranslationProvider>
            </MemoryRouter>
        </QueryClientProvider>,
    );
}

const CALIBRATION_LINK = `href="/market-data/calibration/fx/feeds/feed-1?tab=variance" data-discover="true"`;

describe('a feed’s run tab', () => {
    it('shows a start from a fixing as facts: level, provider, date and item', () => {
        const html = render(START);
        expect(html).toContain('From a fixing');
        expect(html).toContain('1.15');
        expect(html).toContain('ecb');
        expect(html).toContain('2026-10-27');
        expect(html).toContain('Direct');
        expect(html).toContain('oresmd://fx/EUR?type=fixing&amp;index=fx');
        expect(html).toContain('261');
        expect(html).toContain('400 days');
    });

    it('shows an inverted start at its level in market convention, before the raw fixing', () => {
        const html = render({ ...START, value: '1.4239', inverted: true });
        const level = (1 / 1.4239).toPrecision(6);
        expect(html).toContain(level);
        expect(html).toContain('Inverted');
        expect(html.indexOf(level)).toBeLessThan(html.indexOf('1.4239'));
    });

    it('marks a run that kept its configured level and gives the reason', () => {
        const html = render({
            ...START,
            from_fixing: false,
            value: '',
            item: '',
            provider_code: '',
            fixing_date: null,
            received_at: null,
            used: 0,
            window_days: 0,
            note: 'The config names no fixing item.',
        });
        expect(html).toContain('Configured level');
        expect(html).toContain('0.7196');
        expect(html).toContain('The config names no fixing item.');
        expect(html).not.toContain('Fixing window');
        expect(html).not.toContain('Fixing date');
    });

    it('shows the level a kept run started at, not the config’s later level', () => {
        const html = render(
            {
                ...START,
                from_fixing: false,
                item: '',
                provider_code: '',
                value: '0.7196',
                fixing_date: null,
                received_at: null,
                note: '',
            },
            feed({ gmm_initial_price: 0.7022965095863474 }),
        );
        expect(html).toContain('0.7196');
        expect(html).not.toContain('0.7022965');
    });

    it('shows the GJR-GARCH shape with its Greek letters, and links the calibration by its id', () => {
        const html = render(START);
        expect(html).toMatch(/<th[^>]*>Alpha \(α\)<\/th>/);
        expect(html).not.toMatch(/uppercase[^>]*>Alpha/);
        expect(html).toContain('0.0113');
        expect(html).toContain('0.9551');
        expect(html).toContain('Fitted, 279 returns');
        expect(html).toContain(`${CALIBRATION_LINK}>${CALIBRATION}</a>`);
        expect(html).not.toContain('Restart the feed');
    });

    it('warns when the config names a newer calibration than the run uses', () => {
        const html = render(START, feed({ variance_calibration_id: 'newer-calibration' }));
        expect(html).toContain('Named by the config');
        expect(html).toContain(`${CALIBRATION_LINK}>newer-calibration</a>`);
        expect(html).toContain('Restart the feed to run it.');
    });

    it('shows the window measures as tiles, used or recorded only', () => {
        const html = render({
            ...START,
            daily_spread: 0.0042,
            annual_drift: -0.031,
            jumps: 1,
            dispersion: [{ provider_code: 'imf', item: 'x', basis_points: 3.25 }],
        });
        expect(html).toContain('Daily volatility');
        expect(html).toContain('0.42%');
        expect(html).toContain('Sets the spread');
        expect(html).toContain('-3.10%');
        expect(html).toContain('Recorded only');
        expect(html).toContain('Distance to imf');
        expect(html).toContain('3.3 bp');
    });

    it('shows a stopped feed’s configured calibration without a start', () => {
        const html = render(null);
        expect(html).toContain('The feed is not running.');
        expect(html).toContain('GJR-GARCH');
        expect(html).toContain(`${CALIBRATION_LINK}>${CALIBRATION}</a>`);
        expect(html).not.toContain('Start level');
    });

    it('shows a constant variance and no calibration for a feed without GJR-GARCH', () => {
        const html = render(
            { ...START, variance: null },
            feed({ variance_model: 'constant', variance_calibration_id: null }),
        );
        expect(html).toContain('Constant');
        expect(html).not.toContain('Open on the calibration page');
    });

    it('carries no paragraph of running text for a start from a fixing', () => {
        expect(render(START)).not.toMatch(/<p class="text-sm/);
    });
});
