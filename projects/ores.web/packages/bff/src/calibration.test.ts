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

import { describe, expect, it } from 'vitest';
import {
    irCurveCoverage,
    type IrCoverageBootstrapConfig,
    type IrCoverageConfig,
    type IrCoverageFeed,
    type IrCoveragePublish,
    type IrCurveCoverageInput,
    type IrCurveCoverageRow,
} from './calibration.js';

const NOW = new Date('2026-11-13T12:00:00Z');

const CONFIG = '55555555-5555-4555-8555-555555555555';
const BOOTSTRAP = '66666666-6666-4666-8666-666666666666';
const SERIES = '77777777-7777-4777-8777-777777777777';

const USD_CONFIG: IrCoverageConfig = {
    id: CONFIG,
    currency_code: 'USD',
    source_name: 'synthetic.bloomberg.usdsofr',
    price_source: 'fixing',
};

const USD_BOOTSTRAP: IrCoverageBootstrapConfig = {
    id: BOOTSTRAP,
    currency_code: 'USD',
    name: 'USD-SOFR',
    output_series_id: SERIES,
};

function feedOf(
    start: Partial<IrCoverageFeed['start']> = {},
    over: Partial<IrCoverageFeed> = {},
): IrCoverageFeed {
    return {
        config_id: CONFIG,
        refused: false,
        start: { from_fixing: true, provider_code: 'bloomberg', note: '', ...start },
        ...over,
    };
}

function publish(asOf: string): IrCoveragePublish {
    return { as_of: asOf, state_code: 'published' };
}

function coverage(over: Partial<IrCurveCoverageInput> = {}): IrCurveCoverageRow[] {
    return irCurveCoverage({
        feeds: [feedOf()],
        generationConfigs: [USD_CONFIG],
        bootstrapConfigs: [USD_BOOTSTRAP],
        publishedBySeries: new Map([[SERIES, publish('2026-11-13 11:30:00Z')]]),
        windowDays: 90,
        now: NOW,
        ...over,
    });
}

describe('the IR curve coverage', () => {
    it('covers a currency whose feed, curve config and publish are all in place', () => {
        expect(coverage()).toEqual([
            {
                currency_code: 'USD',
                config_id: CONFIG,
                source_name: 'synthetic.bloomberg.usdsofr',
                price_source: 'fixing',
                provider_code: 'bloomberg',
                window_days: 90,
                bootstrap_config_id: BOOTSTRAP,
                bootstrap_config_name: 'USD-SOFR',
                published_as_of: '2026-11-13 11:30:00Z',
                published_state: 'published',
                gap: '',
                gap_reason: '',
            },
        ]);
    });

    it('states no_feed for a currency a bootstrap config holds but no feed does', () => {
        expect(coverage({ feeds: [], generationConfigs: [] })).toEqual([
            {
                currency_code: 'USD',
                config_id: '',
                source_name: '',
                price_source: '',
                provider_code: '',
                window_days: 90,
                bootstrap_config_id: BOOTSTRAP,
                bootstrap_config_name: 'USD-SOFR',
                published_as_of: '2026-11-13 11:30:00Z',
                published_state: 'published',
                gap: 'no_feed',
                gap_reason: 'No synthetic feed holds this currency.',
            },
        ]);
    });

    it("states no_source for a feed that keeps a fixed level, in the system's own words", () => {
        const note = 'No series of the class policy publishes an overnight rate in USD.';
        expect(
            coverage({ feeds: [feedOf({ from_fixing: false, provider_code: '', note })] }),
        ).toEqual([
            {
                currency_code: 'USD',
                config_id: CONFIG,
                source_name: 'synthetic.bloomberg.usdsofr',
                price_source: 'fixing',
                provider_code: '',
                window_days: 90,
                bootstrap_config_id: BOOTSTRAP,
                bootstrap_config_name: 'USD-SOFR',
                published_as_of: '2026-11-13 11:30:00Z',
                published_state: 'published',
                gap: 'no_source',
                gap_reason: note,
            },
        ]);
    });

    it('states no_source for a refused feed that recorded no note', () => {
        expect(
            coverage({
                feeds: [
                    feedOf({ from_fixing: false, provider_code: '', note: '' }, { refused: true }),
                ],
            }),
        ).toEqual([
            {
                currency_code: 'USD',
                config_id: CONFIG,
                source_name: 'synthetic.bloomberg.usdsofr',
                price_source: 'fixing',
                provider_code: '',
                window_days: 90,
                bootstrap_config_id: BOOTSTRAP,
                bootstrap_config_name: 'USD-SOFR',
                published_as_of: '2026-11-13 11:30:00Z',
                published_state: 'published',
                gap: 'no_source',
                gap_reason: 'The class policy refuses to start the feed without a fixing.',
            },
        ]);
    });

    it('states no_curve_config for a currency with a source but no bootstrap config', () => {
        expect(coverage({ bootstrapConfigs: [] })).toEqual([
            {
                currency_code: 'USD',
                config_id: CONFIG,
                source_name: 'synthetic.bloomberg.usdsofr',
                price_source: 'fixing',
                provider_code: 'bloomberg',
                window_days: 90,
                bootstrap_config_id: '',
                bootstrap_config_name: '',
                published_as_of: '',
                published_state: '',
                gap: 'no_curve_config',
                gap_reason: 'No bootstrap config holds this currency.',
            },
        ]);
    });

    it('states no_publish for a curve that has never been published', () => {
        expect(coverage({ publishedBySeries: new Map() })).toEqual([
            {
                currency_code: 'USD',
                config_id: CONFIG,
                source_name: 'synthetic.bloomberg.usdsofr',
                price_source: 'fixing',
                provider_code: 'bloomberg',
                window_days: 90,
                bootstrap_config_id: BOOTSTRAP,
                bootstrap_config_name: 'USD-SOFR',
                published_as_of: '',
                published_state: '',
                gap: 'no_publish',
                gap_reason: 'The curve has never been published.',
            },
        ]);
    });

    it('states stale_publish for a curve older than sixty minutes', () => {
        expect(
            coverage({
                publishedBySeries: new Map([[SERIES, publish('2026-11-13 10:59:00Z')]]),
            }),
        ).toEqual([
            {
                currency_code: 'USD',
                config_id: CONFIG,
                source_name: 'synthetic.bloomberg.usdsofr',
                price_source: 'fixing',
                provider_code: 'bloomberg',
                window_days: 90,
                bootstrap_config_id: BOOTSTRAP,
                bootstrap_config_name: 'USD-SOFR',
                published_as_of: '2026-11-13 10:59:00Z',
                published_state: 'published',
                gap: 'stale_publish',
                gap_reason: 'The latest curve was published more than 60 minutes ago.',
            },
        ]);
    });

    it('covers a curve published exactly sixty minutes ago', () => {
        const rows = coverage({
            publishedBySeries: new Map([[SERIES, publish('2026-11-13 11:00:00Z')]]),
        });
        expect(rows[0]).toMatchObject({ gap: '', gap_reason: '' });
    });

    it('reads a currency through the feed that starts from a public source', () => {
        const fixed: IrCoverageConfig = {
            id: '88888888-8888-4888-8888-888888888888',
            currency_code: 'USD',
            source_name: 'synthetic.demo.usdsofr',
            price_source: 'fixed',
        };
        const rows = coverage({
            feeds: [
                feedOf(
                    { from_fixing: false, note: 'The config starts from its fixed level.' },
                    { config_id: fixed.id },
                ),
                feedOf(),
            ],
            generationConfigs: [fixed, USD_CONFIG],
        });
        expect(rows[0]).toMatchObject({
            config_id: CONFIG,
            source_name: 'synthetic.bloomberg.usdsofr',
            gap: '',
        });
    });

    it('holds one row per currency, in currency order', () => {
        const eurConfig: IrCoverageConfig = {
            id: '99999999-9999-4999-8999-999999999999',
            currency_code: 'EUR',
            source_name: 'synthetic.bloomberg.eurestr',
            price_source: 'fixing',
        };
        const eurBootstrap: IrCoverageBootstrapConfig = {
            id: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa',
            currency_code: 'EUR',
            name: 'EUR-ESTR',
            output_series_id: 'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb',
        };
        const rows = coverage({
            feeds: [feedOf(), feedOf({}, { config_id: eurConfig.id })],
            generationConfigs: [USD_CONFIG, eurConfig],
            bootstrapConfigs: [USD_BOOTSTRAP, eurBootstrap],
            publishedBySeries: new Map([
                [SERIES, publish('2026-11-13 11:30:00Z')],
                [eurBootstrap.output_series_id, publish('2026-11-13 11:30:00Z')],
            ]),
        });
        expect(rows.map((row) => row.currency_code)).toEqual(['EUR', 'USD']);
        expect(rows.map((row) => row.gap)).toEqual(['', '']);
    });
});
