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
    type IrCoverageRun,
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

function runOf(
    start: Partial<NonNullable<IrCoverageRun['start']>> = {},
    over: Partial<IrCoverageRun> = {},
): IrCoverageRun {
    return {
        source_name: 'synthetic.bloomberg.usdsofr',
        start: { from_fixing: true, provider_code: 'bloomberg', note: '', ...start },
        ...over,
    };
}

function publish(asOf: string): IrCoveragePublish {
    return { as_of: asOf, state_code: 'published' };
}

function coverage(over: Partial<IrCurveCoverageInput> = {}): IrCurveCoverageRow[] {
    return irCurveCoverage({
        runs: [runOf()],
        generationConfigs: [USD_CONFIG],
        bootstrapConfigs: [USD_BOOTSTRAP],
        publishedBySeries: new Map([[SERIES, publish('2026-11-13 11:30:00Z')]]),
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
                provider_code: 'bloomberg',
                bootstrap_config_id: BOOTSTRAP,
                bootstrap_config_name: 'USD-SOFR',
                published_as_of: '2026-11-13 11:30:00Z',
                published_state: 'published',
                gap: '',
                gap_reason: '',
                curves: expect.any(Array),
            },
        ]);
    });

    it('states no_feed for a currency a bootstrap config holds but no feed does', () => {
        expect(coverage({ runs: [], generationConfigs: [] })).toEqual([
            {
                currency_code: 'USD',
                config_id: '',
                source_name: '',
                provider_code: '',
                bootstrap_config_id: BOOTSTRAP,
                bootstrap_config_name: 'USD-SOFR',
                published_as_of: '2026-11-13 11:30:00Z',
                published_state: 'published',
                gap: 'no_feed',
                gap_reason: 'No synthetic feed holds this currency.',
                curves: expect.any(Array),
            },
        ]);
    });

    it("states no_source for a feed that keeps a fixed level, in the system's own words", () => {
        const note = 'No series of the class policy publishes an overnight rate in USD.';
        expect(
            coverage({ runs: [runOf({ from_fixing: false, provider_code: '', note })] }),
        ).toEqual([
            {
                currency_code: 'USD',
                config_id: CONFIG,
                source_name: 'synthetic.bloomberg.usdsofr',
                provider_code: '',
                bootstrap_config_id: BOOTSTRAP,
                bootstrap_config_name: 'USD-SOFR',
                published_as_of: '2026-11-13 11:30:00Z',
                published_state: 'published',
                gap: 'no_source',
                gap_reason: note,
                curves: expect.any(Array),
            },
        ]);
    });

    it('states no_source for a run that kept its level and recorded no note', () => {
        const [row] = coverage({ runs: [runOf({ from_fixing: false, provider_code: '' })] });
        expect(row).toMatchObject({
            provider_code: '',
            gap: 'no_source',
            gap_reason:
                'The feed keeps a fixed level, because no public source publishes this currency.',
        });
    });

    it('states no_source for a config that starts from its fixed level, running or not', () => {
        const fixed = { ...USD_CONFIG, price_source: 'fixed' };
        for (const runs of [[], [runOf()]]) {
            expect(coverage({ runs, generationConfigs: [fixed] })[0]).toMatchObject({
                gap: 'no_source',
                gap_reason: 'The feed config starts from its fixed level, not from a fixing.',
            });
        }
    });

    it('covers a currency whose feed takes a fixing but does not run now, with no provider', () => {
        const [row] = coverage({ runs: [] });
        expect(row).toMatchObject({ provider_code: '', gap: '' });
    });

    it('states no_curve_config for a currency with a source but no bootstrap config', () => {
        expect(coverage({ bootstrapConfigs: [] })).toEqual([
            {
                currency_code: 'USD',
                config_id: CONFIG,
                source_name: 'synthetic.bloomberg.usdsofr',
                provider_code: 'bloomberg',
                bootstrap_config_id: '',
                bootstrap_config_name: '',
                published_as_of: '',
                published_state: '',
                gap: 'no_curve_config',
                gap_reason: 'No bootstrap config holds this currency.',
                curves: expect.any(Array),
            },
        ]);
    });

    it('states no_publish for a curve that has never been published', () => {
        expect(coverage({ publishedBySeries: new Map() })).toEqual([
            {
                currency_code: 'USD',
                config_id: CONFIG,
                source_name: 'synthetic.bloomberg.usdsofr',
                provider_code: 'bloomberg',
                bootstrap_config_id: BOOTSTRAP,
                bootstrap_config_name: 'USD-SOFR',
                published_as_of: '',
                published_state: '',
                gap: 'no_publish',
                gap_reason: 'The curve has never been published.',
                curves: expect.any(Array),
            },
        ]);
    });

    it('states not_live for a latest set that is no longer in use', () => {
        const rows = coverage({
            publishedBySeries: new Map([
                [SERIES, { as_of: '2026-11-13 11:55:00Z', state_code: 'withdrawn' }],
            ]),
        });
        expect(rows[0]).toMatchObject({
            gap: 'not_live',
            gap_reason: 'The latest set is not the set in use. It is withdrawn.',
        });
    });

    it('states stale_publish for a curve older than thirty minutes', () => {
        expect(
            coverage({
                publishedBySeries: new Map([[SERIES, publish('2026-11-13 11:29:00Z')]]),
            }),
        ).toEqual([
            {
                currency_code: 'USD',
                config_id: CONFIG,
                source_name: 'synthetic.bloomberg.usdsofr',
                provider_code: 'bloomberg',
                bootstrap_config_id: BOOTSTRAP,
                bootstrap_config_name: 'USD-SOFR',
                published_as_of: '2026-11-13 11:29:00Z',
                published_state: 'published',
                gap: 'stale_publish',
                gap_reason: 'The latest curve was published more than 30 minutes ago.',
                curves: expect.any(Array),
            },
        ]);
    });

    it('covers a curve published exactly thirty minutes ago', () => {
        const rows = coverage({
            publishedBySeries: new Map([[SERIES, publish('2026-11-13 11:30:00Z')]]),
        });
        expect(rows[0]).toMatchObject({ gap: '', gap_reason: '' });
    });

    it('reads a currency through the feed that starts from a public source', () => {
        const fixed: IrCoverageConfig = {
            id: '88888888-8888-4888-8888-888888888888',
            currency_code: 'USD',
            source_name: 'synthetic.demo.usdsofr',
            price_source: 'fixing',
        };
        const rows = coverage({
            runs: [
                runOf(
                    { from_fixing: false, note: 'The config starts from its fixed level.' },
                    { source_name: fixed.source_name },
                ),
                runOf(),
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
            runs: [runOf(), runOf({}, { source_name: eurConfig.source_name })],
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

    const LIBOR_3M: IrCoverageBootstrapConfig = {
        id: '88888888-8888-4888-8888-888888888888',
        currency_code: 'USD',
        name: 'USD-LIBOR-3M',
        output_series_id: '99999999-9999-4999-8999-999999999999',
        curve_family_role: 'PROJECTION',
        index_tenor: '3M',
        discount_curve_config_id: BOOTSTRAP,
    };
    const SOFR = { ...USD_BOOTSTRAP, curve_family_role: 'FUNDING', index_tenor: '' };

    it('lists every curve of a currency, the funding curve first, each projection naming its discount curve', () => {
        const [row] = coverage({
            bootstrapConfigs: [LIBOR_3M, SOFR],
            publishedBySeries: new Map([
                [SERIES, publish('2026-11-13 11:30:00Z')],
                [LIBOR_3M.output_series_id, publish('2026-11-13 11:45:00Z')],
            ]),
        });
        expect(row?.curves).toEqual([
            {
                bootstrap_config_id: BOOTSTRAP,
                name: 'USD-SOFR',
                role: 'FUNDING',
                index_tenor: '',
                discount_curve_name: '',
                published_as_of: '2026-11-13 11:30:00Z',
                published_state: 'published',
                gap: '',
                gap_reason: '',
            },
            {
                bootstrap_config_id: LIBOR_3M.id,
                name: 'USD-LIBOR-3M',
                role: 'PROJECTION',
                index_tenor: '3M',
                discount_curve_name: 'USD-SOFR',
                published_as_of: '2026-11-13 11:45:00Z',
                published_state: 'published',
                gap: '',
                gap_reason: '',
            },
        ]);
    });

    it('states each curve by its own publish, so a live funding curve shows beside a stale projection', () => {
        const [row] = coverage({
            bootstrapConfigs: [LIBOR_3M, SOFR],
            publishedBySeries: new Map([[SERIES, publish('2026-11-13 11:45:00Z')]]),
        });
        expect(row?.curves.map((curve) => [curve.name, curve.gap])).toEqual([
            ['USD-SOFR', ''],
            ['USD-LIBOR-3M', 'no_publish'],
        ]);
        expect(row?.gap).toBe('');
    });

    it('covers a currency by its funding curve, not by a projection that published later', () => {
        const [row] = coverage({
            bootstrapConfigs: [LIBOR_3M, SOFR],
            publishedBySeries: new Map([
                [SERIES, publish('2026-11-13 10:00:00Z')],
                [LIBOR_3M.output_series_id, publish('2026-11-13 11:45:00Z')],
            ]),
        });
        expect(row?.bootstrap_config_name).toBe('USD-SOFR');
        expect(row?.gap).toBe('stale_publish');
    });

    it('reads a config with no role as the funding curve, as the first configs were written', () => {
        const [row] = coverage();
        expect(row?.curves.map((curve) => curve.role)).toEqual(['FUNDING']);
    });
});
