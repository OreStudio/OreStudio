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
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter, Route, Routes } from 'react-router';
import type { AccountAccess } from '@ores/wire-protocol/browser';
import type { AdapterStatus, RecordRow, SyntheticResourceView } from '../../api/client.js';
import { TranslationProvider } from '../../i18n/Provider.js';
import { MarketDataPage } from '../MarketDataPage.js';
import { ADAPTERS_KEY } from './adapterStatus.js';
import { IrFeedPage } from './irFeeds.js';
import { SyntheticScreen } from './SyntheticScreen.js';

const FEED_ID = '66666666-6666-4666-8666-666666666666';

function resource(key: string, name: string, search = false): SyntheticResourceView {
    return {
        key,
        entityType: `ores.synthetic.${name}`,
        keyField: 'id',
        writable: true,
        readOnlyBecause: null,
        search,
        sortable: search ? ['source_name'] : [],
        readPermission: `synthetic::${name}s:read`,
        writePermission: `synthetic::${name}s:write`,
        deletePermission: `synthetic::${name}s:delete`,
    };
}

const REGISTRY = [
    resource('ir-curve-feeds', 'ir_curve_generation_config', true),
    resource('ir-parameter-values', 'ir_curve_generation_config_process_parameter_value'),
    resource('ir-template-entries', 'ir_curve_template_entry'),
    resource('process-types', 'yield_curve_process_type'),
    resource('parameter-definitions', 'yield_curve_process_parameter_definition'),
];

function access(codes: string[]): AccountAccess {
    return {
        roles: [
            {
                roleId: '33333333-3333-3333-3333-333333333333' as AccountAccess['roles'][number]['roleId'],
                name: 'Quant',
                description: '',
                permissionCodes: codes,
                givenBy: 'priya',
                givenAt: '2026-10-04',
                reasonCode: 'access.new_joiner',
                commentary: '',
            },
        ],
    };
}

/** The adapters of one provider: a source that serves the ids it was demanded, or one that does not. */
function adapters(provider: string, state: string, demanded: number): readonly AdapterStatus[] {
    return [
        {
            adapter_id: `${provider}-sim`,
            provider_code: provider,
            state,
            last_heartbeat_at: '2026-10-25 15:00:00Z',
            heartbeat_counter: 4,
            served_count: 2,
            unknown_count: 0,
            demanded_count: demanded,
            routes_seen: 1,
            ticks_last_minute: 0,
            last_tick_age_ms: -1,
            accepted: [],
            rejected: [],
        },
    ];
}

const feed: RecordRow = {
    version: 2,
    id: FEED_ID,
    config_id: '88888888-8888-4888-8888-888888888888',
    currency_code: 'EUR',
    index_family: 'euribor',
    tenor: '6M',
    role: 'projection',
    process_type: 'VASICEK',
    ticks_per_hour: 3600,
    enabled: false,
    auto_start: false,
    price_source: 'fixed',
    vintage_source: '',
    vintage_date: '',
    vintage_series_uri: '',
    description: '',
    fixed_leg_payment_frequency_code: 'Annual',
    provider_code: 'bloomberg',
    source_name: 'synthetic.eureuribor6m',
    folder_id: null,
};

const PROCESS_TYPES: RecordRow[] = [
    { code: 'VASICEK', name: 'Vasicek', version: 1 },
    { code: 'HULL_WHITE', name: 'Hull-White', version: 1 },
];

const DEFINITIONS: RecordRow[] = [
    {
        id: 'd1',
        process_type_code: 'VASICEK',
        parameter_name: 'kappa',
        display_name: 'Mean reversion speed',
        symbol: 'κ',
        description: '',
        default_value: 0.5,
        min_value: 0,
        max_value: null,
        display_order: 1,
    },
    {
        id: 'd2',
        process_type_code: 'VASICEK',
        parameter_name: 'initial_rate',
        display_name: 'Initial short rate',
        symbol: '',
        description: '',
        default_value: 0.03,
        min_value: null,
        max_value: null,
        display_order: 2,
    },
];

function render(path: string, seed: (client: QueryClient) => void): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['registry', 'synthetic'], REGISTRY);
    seed(client);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[path]}>
                    <Routes>
                        <Route path="/market-data" element={<MarketDataPage />} />
                        <Route path="/market-data/synthetic" element={<SyntheticScreen />} />
                        <Route path="/market-data/synthetic/ir/:id" element={<IrFeedPage />} />
                    </Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the market data area', () => {
    it('offers the Synthetic screen to a person who reads the feeds', () => {
        const html = render('/market-data', (client) => {
            client.setQueryData(['my-access'], access(['synthetic::*']));
        });
        expect(html).toContain('href="/market-data/synthetic"');
    });

    it('draws the Synthetic screen with its dashboard and its provider screen', () => {
        const html = render('/market-data/synthetic', (client) => {
            client.setQueryData(['my-access'], access(['synthetic::*']));
        });
        expect(html).toContain('>Dashboard<');
        expect(html).toContain('>Provider<');
        expect(html).not.toContain('IR curve feeds');
    });

    it('draws no card for a person who does not', () => {
        const html = render('/market-data', (client) => {
            client.setQueryData(['my-access'], access(['refdata::currencies:read']));
        });
        expect(html).not.toContain('href="/market-data/synthetic"');
    });
});

describe('the IR curve feed screens', () => {
    it('shows a curve with its fields and the series its pillars land in', () => {
        const html = render(`/market-data/synthetic/ir/${FEED_ID}`, (client) => {
            client.setQueryData(['records', 'ir-curve-feeds', 'key', FEED_ID], feed);
            client.setQueryData(['my-access'], access(['synthetic::*']));
        });
        expect(html).toContain('synthetic.eureuribor6m');
        expect(html).toContain(
            'oresmd://ir/EUR?type=series&amp;instrument=ir_swap&amp;quote=rate&amp;fwd_start=0D&amp;tenor=6M',
        );
        expect(html).toContain('>Inspect<');
        expect(html).toContain('>Process<');
        expect(html).toContain('>Curve template<');
        expect(html).toContain('>History<');
        expect(html).toContain('Edit');
        expect(html).toContain('Delete');
    });

    it('offers a reader no change', () => {
        const html = render(`/market-data/synthetic/ir/${FEED_ID}`, (client) => {
            client.setQueryData(['records', 'ir-curve-feeds', 'key', FEED_ID], feed);
            client.setQueryData(
                ['my-access'],
                access(['synthetic::ir_curve_generation_configs:read']),
            );
        });
        expect(html).not.toContain('>Edit<');
        expect(html).not.toContain('>Delete<');
    });

    it('draws the engine and its parameters, each with the saved value or the default', () => {
        const html = render(`/market-data/synthetic/ir/${FEED_ID}?tab=process`, (client) => {
            client.setQueryData(['records', 'ir-curve-feeds', 'key', FEED_ID], feed);
            client.setQueryData(['records', 'process-types'], PROCESS_TYPES);
            client.setQueryData(['records', 'parameter-definitions'], DEFINITIONS);
            client.setQueryData(
                ['records', 'ir-parameter-values', FEED_ID],
                [
                    {
                        id: 'v1',
                        version: 4,
                        config_id: FEED_ID,
                        parameter_definition_id: 'd1',
                        parameter_value: 0.7,
                    },
                    {
                        id: 'v2',
                        version: 1,
                        config_id: FEED_ID,
                        parameter_definition_id: 'd2',
                        parameter_value: 0.03,
                    },
                ],
            );
            client.setQueryData(['records', 'ir-template-entries', FEED_ID], []);
            client.setQueryData(['my-access'], access(['synthetic::*']));
        });
        expect(html).toContain('Mean reversion speed');
        expect(html).toContain('>0.7</td>');
        expect(html).toContain('Initial short rate');
        expect(html).toContain('>0.03</td>');
        expect(html).not.toContain('>Min<');
        expect(html).not.toContain('>Max<');
        expect(html).not.toContain('Allowed range');
        expect(html).toContain('>Discard<');
        expect(html).not.toContain('Discard changes');
        expect(html).toContain('No unsaved changes.');
        expect(html).toContain('Hull-White');
    });

    it('asks the four plain questions in Simple mode, once the server has read the values', () => {
        const html = render(`/market-data/synthetic/ir/${FEED_ID}?tab=process`, (client) => {
            client.setQueryData(['records', 'ir-curve-feeds', 'key', FEED_ID], feed);
            client.setQueryData(['records', 'process-types'], PROCESS_TYPES);
            client.setQueryData(['records', 'parameter-definitions'], DEFINITIONS);
            client.setQueryData(['records', 'ir-parameter-values', FEED_ID], []);
            client.setQueryData(['records', 'ir-template-entries', FEED_ID], []);
            client.setQueryData(['my-access'], access(['synthetic::*']));
            client.setQueryData(['ir-reading', 'VASICEK', { kappa: 0.5, initial_rate: 0.03 }], {
                has_simple_form: true,
                message: '',
                caution: 'touches_zero',
                values: {
                    start_rate: 0.03,
                    long_run_rate: 0.045,
                    half_life_years: 1.386,
                    yearly_move_bp: 100,
                },
            });
        });
        expect(html).toContain('Where are rates today?');
        expect(html).toContain('How much do rates move in a year?');
        expect(html).toContain('Rates may touch zero');
        expect(html).toContain('>Simple<');
        expect(html).toContain('>Advanced<');
        expect(html).not.toContain('Mean reversion speed');
    });

    it('offers the defaults of a curve whose parameters were never saved, as unsaved', () => {
        const html = render(`/market-data/synthetic/ir/${FEED_ID}?tab=process`, (client) => {
            client.setQueryData(['records', 'ir-curve-feeds', 'key', FEED_ID], feed);
            client.setQueryData(['records', 'process-types'], PROCESS_TYPES);
            client.setQueryData(['records', 'parameter-definitions'], DEFINITIONS);
            client.setQueryData(['records', 'ir-parameter-values', FEED_ID], []);
            client.setQueryData(['records', 'ir-template-entries', FEED_ID], []);
            client.setQueryData(['my-access'], access(['synthetic::*']));
        });
        expect(html).toContain('>0.5</td>');
        expect(html).toContain('2 unsaved changes');
        expect(html).toContain('>New<');
        expect(html).not.toContain('No unsaved changes.');
    });

    it('lists the tenor entries of the curve template, in sequence', () => {
        const html = render(`/market-data/synthetic/ir/${FEED_ID}?tab=template`, (client) => {
            client.setQueryData(['records', 'ir-curve-feeds', 'key', FEED_ID], feed);
            client.setQueryData(['records', 'ir-parameter-values', FEED_ID], []);
            client.setQueryData(
                ['records', 'ir-template-entries', FEED_ID],
                [
                    {
                        id: 'e2',
                        version: 1,
                        ir_curve_config_id: FEED_ID,
                        sequence_index: 1,
                        start_tenor_code: 'SPOT',
                        end_tenor_code: '5Y',
                        instrument_code: 'IRS',
                    },
                    {
                        id: 'e1',
                        version: 1,
                        ir_curve_config_id: FEED_ID,
                        sequence_index: 0,
                        start_tenor_code: 'SPOT',
                        end_tenor_code: '3M',
                        instrument_code: 'DEPO',
                    },
                ],
            );
            client.setQueryData(['my-access'], access(['synthetic::*']));
        });
        expect(html.indexOf('DEPO')).toBeGreaterThan(-1);
        expect(html.indexOf('DEPO')).toBeLessThan(html.indexOf('IRS'));
        expect(html).toContain('Start tenor');
        expect(html).toContain('End tenor');
        expect(html).toContain('Instrument');
    });

    it('plots the last curves of a running feed, one tick for each tenor entry', () => {
        const entry = (id: string, index: number, end: string) => ({
            id,
            version: 1,
            ir_curve_config_id: FEED_ID,
            sequence_index: index,
            start_tenor_code: 'SPOT',
            end_tenor_code: end,
            instrument_code: 'IRS',
        });
        const html = render(`/market-data/synthetic/ir/${FEED_ID}?tab=ticks`, (client) => {
            client.setQueryData(['records', 'ir-curve-feeds', 'key', FEED_ID], feed);
            client.setQueryData(['records', 'ir-parameter-values', FEED_ID], []);
            client.setQueryData(
                ['records', 'ir-template-entries', FEED_ID],
                [entry('e1', 0, '3M'), entry('e2', 1, '5Y')],
            );
            client.setQueryData(['records', 'parameter-definitions'], DEFINITIONS);
            client.setQueryData(ADAPTERS_KEY, adapters('bloomberg', 'serving', 1));
            client.setQueryData(['feed-ticks', FEED_ID, 12], {
                sourceName: 'synthetic.eureuribor6m',
                ticks: [
                    { at: '2026-10-11 09:00:00Z', value: 0.02, uri: 'a' },
                    { at: '2026-10-11 09:00:00Z', value: 0.03, uri: 'b' },
                    { at: '2026-10-11 09:00:30Z', value: 0.021, uri: 'a' },
                    { at: '2026-10-11 09:00:30Z', value: 0.032, uri: 'b' },
                ],
            });
            client.setQueryData(['my-access'], access(['synthetic::*']));
        });
        expect(html).toContain('The last curves of the feed');
        expect(html).toContain('Newest curve bold; older snapshots overlaid, fading with age.');
        expect(html).toContain('>3M<');
        expect(html).toContain('>5Y<');
        expect(html).toContain('Short end (3M)');
        expect(html).toContain('Long end (5Y)');
        expect(html).toContain('Slope (bp)');
        expect(html).toContain('2.100%');
        expect(html).toContain('Mean reversion speed');
    });

    it('says a curve with no tenor entries cannot show its ticks', () => {
        const html = render(`/market-data/synthetic/ir/${FEED_ID}?tab=ticks`, (client) => {
            client.setQueryData(['records', 'ir-curve-feeds', 'key', FEED_ID], feed);
            client.setQueryData(['records', 'ir-parameter-values', FEED_ID], []);
            client.setQueryData(['records', 'ir-template-entries', FEED_ID], []);
            client.setQueryData(['records', 'parameter-definitions'], DEFINITIONS);
            client.setQueryData(['my-access'], access(['synthetic::*']));
        });
        expect(html).toContain('The curve has no tenor entries');
    });

    it('says a stopped feed must be started to show curves', () => {
        const html = render(`/market-data/synthetic/ir/${FEED_ID}?tab=ticks`, (client) => {
            client.setQueryData(['records', 'ir-curve-feeds', 'key', FEED_ID], feed);
            client.setQueryData(['records', 'ir-parameter-values', FEED_ID], []);
            client.setQueryData(
                ['records', 'ir-template-entries', FEED_ID],
                [
                    {
                        id: 'e1',
                        version: 1,
                        ir_curve_config_id: FEED_ID,
                        sequence_index: 0,
                        start_tenor_code: 'SPOT',
                        end_tenor_code: '3M',
                        instrument_code: 'DEPO',
                    },
                ],
            );
            client.setQueryData(['records', 'parameter-definitions'], DEFINITIONS);
            client.setQueryData(['my-access'], access(['synthetic::*']));
        });
        expect(html).toContain('The feed is not running');
    });

    it('draws the curve and its parameters and entries as one history', () => {
        const version = (n: number, at: string) => ({
            version: n,
            modifiedBy: 'priya',
            recordedAt: at,
            fields: [
                { name: 'Ticks Per Hour', value: '3600' },
                {
                    name: 'Change Reason Code',
                    value: n === 1 ? 'system.new_record' : 'common.correction',
                },
                { name: 'Change Commentary', value: '' },
            ],
            changes: [],
        });
        const html = render(`/market-data/synthetic/ir/${FEED_ID}?tab=history`, (client) => {
            client.setQueryData(['records', 'ir-curve-feeds', 'key', FEED_ID], feed);
            client.setQueryData(
                ['records', 'ir-parameter-values', FEED_ID],
                [{ id: 'v1', version: 1, config_id: FEED_ID, parameter_definition_id: 'd1' }],
            );
            client.setQueryData(['records', 'ir-template-entries', FEED_ID], []);
            client.setQueryData(
                ['history', 'ores.synthetic.ir_curve_generation_config', FEED_ID],
                [version(2, '2026-10-11 09:00:00Z'), version(1, '2026-10-10 09:00:00Z')],
            );
            client.setQueryData(
                [
                    'history',
                    'ores.synthetic.ir_curve_generation_config_process_parameter_value',
                    'v1',
                ],
                [version(2, '2026-10-11 09:05:00Z'), version(1, '2026-10-10 09:05:00Z')],
            );
            client.setQueryData(['my-access'], access(['synthetic::*']));
        });
        expect(html).toContain('ir_curve_generation_config');
        expect(html).toContain('ir_curve_generation_config_process_parameter_value');
        expect(html.match(/>Revert</g)?.length).toBe(2);
    });
});
