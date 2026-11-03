/* -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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

import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import { afterEach, describe, expect, it, vi } from 'vitest';
import { api } from '../../api/client.js';
import { TranslationProvider } from '../../i18n/Provider.js';
import {
    AwaitingReview,
    orderByWindow,
    readPair,
    ParameterHistory,
    VolatilityChart,
    conditionalVolatility,
    type StoredCalibration,
} from './varianceRecords.js';

const SHAPE = { alpha: 0.05, gamma: 0.1, beta: 0.8, persistence: 0.9, long_run_variance: 1e-4 };

function calibration(over: Partial<StoredCalibration> = {}): StoredCalibration {
    return {
        id: '9992a6e9-76f3-5421-8c1f-193f4b89b03e',
        version: 1,
        state: 'proposed',
        state_reason: '',
        method: 'gjr_garch_qmle',
        input_id: 'input-1',
        alpha: 0.0765,
        gamma: -0.0691,
        beta: 0.9483,
        persistence: 0.9903,
        long_run_variance: 2.5e-5,
        starting_variance_ratio: 0.4629,
        returns_used: 253,
        recorded_at: '2026-11-03 17:50:00.000000Z',
        ...over,
    };
}

function render(node: React.ReactNode): string {
    return renderToStaticMarkup(
        <MemoryRouter>
            <TranslationProvider>{node}</TranslationProvider>
        </MemoryRouter>,
    );
}

describe('the fitted volatility of a calibration', () => {
    it('follows the GJR-GARCH recursion from the long-run variance over the demeaned returns', () => {
        const returns = [0.01, -0.02, 0.005];
        const points = [
            { fixing_date: '2026-01-01', log_return: null, used: false },
            ...returns.map((r, i) => ({
                fixing_date: `2026-01-0${String(i + 2)}`,
                log_return: r,
                used: true,
            })),
            { fixing_date: '2026-01-05', log_return: 0.5, used: false },
        ];
        const mean = (0.01 - 0.02 + 0.005) / 3;
        const omega = 1e-4 * (1 - 0.9);
        const s = returns.map((r) => r - mean);
        const v0 = 1e-4;
        const v1 = omega + 0.05 * s[0]! * s[0]! + 0.8 * v0;
        const v2 = omega + (0.05 + 0.1) * s[1]! * s[1]! + 0.8 * v1;
        const v3 = omega + 0.05 * s[2]! * s[2]! + 0.8 * v2;

        const { days, endRatio } = conditionalVolatility(points, SHAPE);

        expect(days.map((d) => d.date)).toEqual(['2026-01-02', '2026-01-03', '2026-01-04']);
        expect(days[0]?.volatility).toBeCloseTo(Math.sqrt(v0), 15);
        expect(days[1]?.volatility).toBeCloseTo(Math.sqrt(v1), 15);
        expect(days[2]?.volatility).toBeCloseTo(Math.sqrt(v2), 15);
        expect(days[1]?.move).toBeCloseTo(Math.abs(s[1]!), 15);
        expect(endRatio).toBeCloseTo(v3 / v0, 12);
    });

    it('draws nothing for an input with no used return', () => {
        const { days, endRatio } = conditionalVolatility(
            [{ fixing_date: '2026-01-01', log_return: null, used: false }],
            SHAPE,
        );
        expect(days).toEqual([]);
        expect(Number.isNaN(endRatio)).toBe(true);
    });

    it('charts the volatility with a legend and the long-run level on one axis', () => {
        const html = render(
            <VolatilityChart
                days={[
                    { date: '2026-01-02', volatility: 0.01, move: 0.004 },
                    { date: '2026-01-03', volatility: 0.012, move: 0.02 },
                ]}
                longRun={1e-4}
            />,
        );
        expect(html).toContain('data-volatility-chart');
        expect(html).toContain('Fitted volatility');
        expect(html).toContain('Long-run level 1.00%');
        expect(html).toContain('2026-01-03');
    });
});

describe('a fit that waits for review', () => {
    it('says so, with the calibration and a link to the review', () => {
        const html = render(<AwaitingReview calibrations={[calibration()]} to="/review" />);
        expect(html).toContain('1 GJR-GARCH fit(s) of this pair wait for review');
        expect(html).toContain('9992a6e9');
        expect(html).toContain('0.9903');
        expect(html).toContain('href="/review"');
    });

    it('says nothing once every calibration is decided', () => {
        const html = render(
            <AwaitingReview
                calibrations={[
                    calibration({ state: 'approved' }),
                    calibration({ state: 'rejected' }),
                ]}
            />,
        );
        expect(html).toBe('');
    });
});

describe('the calibrations of a pair over time', () => {
    it('draws one small chart for each parameter, with its newest value', () => {
        const html = render(
            <ParameterHistory
                calibrations={[
                    calibration({ state: 'superseded', method: 'authored', persistence: 0.95 }),
                    calibration(),
                ]}
            />,
        );
        expect(html).toContain('data-parameter-history');
        expect(html.match(/<svg/g)?.length).toBe(4);
        expect(html).toContain('Alpha (α)');
        expect(html).toContain('0.9903');
    });
});

describe('the order of a pair\u2019s calibrations', () => {
    it('places each fit by the fixings it read to, not by its last decision, authored first', () => {
        const ordered = orderByWindow(
            [
                calibration({
                    id: 'older-fit',
                    input_id: 'a',
                    recorded_at: '2026-11-03 18:13:00Z',
                }),
                calibration({
                    id: 'newer-fit',
                    input_id: 'b',
                    recorded_at: '2026-11-03 18:10:00Z',
                }),
                calibration({
                    id: 'authored',
                    input_id: null,
                    recorded_at: '2026-11-03 18:10:00Z',
                }),
            ],
            new Map([
                ['a', '2026-11-02'],
                ['b', '2026-11-03'],
            ]),
        );
        expect(ordered.map((c) => c.id)).toEqual(['authored', 'older-fit', 'newer-fit']);
    });
});

describe('the read of a pair\u2019s records', () => {
    afterEach(() => {
        vi.restoreAllMocks();
    });

    it('reads page after page until the server\u2019s total, so no record is cut off', async () => {
        const all = Array.from({ length: 1203 }, (_, i) => ({
            id: `row-${String(i)}`,
            version: 1,
        }));
        const page = vi
            .spyOn(api, 'syntheticPage')
            .mockImplementation(async (_resource, query) => ({
                rows: all.slice(query.offset, query.offset + query.limit),
                total: all.length,
            }));

        const rows = await readPair('variance-calibrations', 'AUD', 'USD');

        expect(rows).toHaveLength(1203);
        expect(page).toHaveBeenCalledTimes(3);
        expect(page.mock.calls[0]?.[1].filters).toEqual({
            base_currency_code: 'AUD',
            quote_currency_code: 'USD',
        });
    });
});
