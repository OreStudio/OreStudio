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
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { describe, expect, it } from 'vitest';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter, Route, Routes } from 'react-router';
import type { AccountAccess } from '@ores/wire-protocol/browser';
import type { RecordRow } from '../api/client.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { FIRST_PAGE, pageKey } from '../refdata/RecordList.js';
import { invalidFields, writeOf, type FieldSpec } from '../refdata/records.js';
import { resourceView } from '../refdata/testSupport.js';
import { CurveBlueprintPage, CurveBlueprintsPage, blueprintFields } from './curveBlueprints.js';

const CONFIGS = 'ir-curve-bootstrap-configs';
const NAME = 'USD-SOFR-FOMC';
const SERIES = 'a1a1a1a1-0000-4000-8000-000000000001';
const URI = 'oresmd://ir/USD?type=series&instrument=discount&quote=rate&curve_id=USD-SOFR-FOMC';

const everything: AccountAccess = {
    roles: [
        {
            roleId: '33333333-3333-3333-3333-333333333333' as AccountAccess['roles'][number]['roleId'],
            name: 'TenantAdmin',
            description: '',
            permissionCodes: ['*'],
            givenBy: 'system',
            givenAt: '2026-10-04',
            reasonCode: 'system.new_record',
            commentary: '',
        },
    ],
};

const blueprint: RecordRow = {
    version: 1,
    id: 'b1b1b1b1-0000-4000-8000-000000000001',
    name: NAME,
    output_series_id: SERIES,
    party_id: 'd82fc8f0-b37a-491e-9acf-4ac69b82303e',
    currency_code: 'USD',
    index_tenor: '1D',
    curve_family_role: 'FUNDING',
    discount_curve_config_id: null,
    interpolation_method: 'FLAT_FORWARD_THEN_LOG_LINEAR',
    day_count_convention: 'A360',
    tenor_convention_code: 'RATES_SPOT_FOMC',
    split_tenor_code: '1Y',
    modified_by: 'tenant_admin',
    performed_by: 'tenant_admin',
    change_reason_code: 'system.initial_load',
    change_commentary: '',
    recorded_at: '2026-10-28 15:26:42.000000Z',
};

function pillar(end: string, role: string, sequence: number): RecordRow {
    return {
        version: 1,
        id: `c1c1c1c1-0000-4000-8000-00000000000${String(sequence)}`,
        party_id: 'd82fc8f0-b37a-491e-9acf-4ac69b82303e',
        bootstrap_config_id: blueprint['id'],
        sequence_index: sequence,
        start_tenor_code: sequence === 0 ? 'SPOT' : `F${String(sequence)}`,
        end_tenor_code: end,
        curve_role_code: role,
        modified_by: 'tenant_admin',
        performed_by: 'tenant_admin',
        change_reason_code: 'system.initial_load',
        change_commentary: '',
        recorded_at: '2026-10-28 15:26:42.000000Z',
    };
}

const PILLARS = [pillar('1Y', 'SWAP', 2), pillar('1F', 'DEPOSIT', 0), pillar('6M', 'SWAP', 1)];

const TENORS = [
    { code: 'SPOT', display_name: 'Spot' },
    { code: '1F', display_name: 'First meeting' },
    { code: '6M', display_name: 'Six months' },
    { code: '1Y', display_name: 'One year' },
];

async function render(
    path: string,
    options: {
        /** Whether the read of the series the party holds fails. */
        readonly seriesUnread?: boolean;
        /** The series the list's own read answered for the row, by state. */
        readonly rowSeriesState?: 'read' | 'missing' | 'unread';
    } = {},
): Promise<string> {
    // The cache is seeded, so nothing reads again on mount: a state a test
    // seeded is the state the page draws.
    const client = new QueryClient({
        defaultOptions: {
            queries: {
                retry: false,
                retryOnMount: false,
                staleTime: Infinity,
                refetchOnMount: false,
            },
        },
    });
    client.setQueryData(
        ['registry', 'refdata'],
        [
            resourceView(CONFIGS, ['id'], { search: true, sortable: ['currency_code'] }),
            resourceView('ir-curve-bootstrap-pillars', ['id']),
            resourceView('currencies', ['iso_code']),
        ],
    );
    client.setQueryData(['my-access'], everything);
    client.setQueryData(['labels'], { labels: [], domains: {}, mapped: {} });
    if (options.seriesUnread === true) {
        // A read that failed, as the page meets it: the entry is in the cache and
        // its state is an error, so the screen says so rather than claiming the
        // series is gone.
        const query = client.getQueryCache().build(client, { queryKey: ['held-market-series'] });
        query.setState({
            ...query.state,
            status: 'error',
            fetchStatus: 'idle',
            error: new Error('the catalogue did not answer'),
        });
    } else {
        client.setQueryData(['held-market-series'], new Map([[SERIES, URI]]));
    }
    client.setQueryData(['tenor-catalogue'], { tenors: TENORS, conventions: [], schedules: [] });
    client.setQueryData(['records', CONFIGS, 'all'], [blueprint]);
    client.setQueryData(['records', CONFIGS, 'funding', ''], { rows: [blueprint], total: 1 });
    client.setQueryData(['records', CONFIGS, 'funding', 'USD'], { rows: [blueprint], total: 1 });
    client.setQueryData(['records', CONFIGS, 'used-by', blueprint['id']], {
        rows: [],
        total: 0,
    });
    client.setQueryData(['records', CONFIGS, 'currencies'], [blueprint]);
    client.setQueryData(pageKey({ key: CONFIGS }, FIRST_PAGE), {
        rows: [blueprint],
        total: 1,
    });
    client.setQueryData(
        ['records', 'currencies'],
        [{ version: 1, iso_code: 'USD', name: 'US Dollar' }],
    );
    client.setQueryData(['records', CONFIGS, 'key', blueprint['id']], blueprint);
    client.setQueryData(['records', 'ir-curve-bootstrap-pillars', 'config', blueprint['id']], {
        rows: PILLARS,
        total: PILLARS.length,
    });
    client.setQueryData(['curve-sets', 'blueprints'], { sets: [], total: 0 });
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[path]}>
                    <Routes>
                        <Route
                            path="/market-data/curve-blueprints"
                            element={<CurveBlueprintsPage />}
                        />
                        <Route
                            path="/market-data/curve-blueprints/:id"
                            element={<CurveBlueprintPage />}
                        />
                    </Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

/** The fields a form is drawn from, with the series the party holds and the ladder. */
function specs(): readonly FieldSpec[] {
    return blueprintFields({
        tenors: TENORS.map((tenor) => ({ value: tenor.code, label: tenor.display_name })),
        conventions: [{ value: 'RATES_SPOT_FOMC', label: 'Rates, spot FOMC' }],
        fundings: [],
        series: [{ value: SERIES, label: URI }],
    });
}

describe('the blueprint field table', () => {
    it('asks for the name the person types, first and required', async () => {
        const name = specs()[0];
        expect(name?.field).toBe('name');
        expect(name?.kind).toEqual({ kind: 'text', max: 120 });
        expect(name?.optional).not.toBe(true);
    });

    it('asks for the output series the party holds beside the name, and never derives it', async () => {
        const series = specs()[1];
        expect(series?.field).toBe('output_series_id');
        expect(series?.kind).toEqual({ kind: 'pick', choices: [{ value: SERIES, label: URI }] });
        expect(series?.optional).not.toBe(true);
        expect(series?.fixed).toBe(true);
    });

    it('refuses to save a new blueprint with no name and no series', async () => {
        const invalid = invalidFields(specs(), {});
        expect(invalid).toContain('name');
        expect(invalid).toContain('output_series_id');
    });

    it('writes the name and the series the person gave', async () => {
        const write = writeOf(specs(), {
            name: NAME,
            output_series_id: SERIES,
            currency_code: 'USD',
            index_tenor: '1D',
            curve_family_role: 'FUNDING',
            interpolation_method: 'LOG_LINEAR_DISCOUNT',
            day_count_convention: 'A360',
            tenor_convention_code: 'RATES_SPOT_FOMC',
            split_tenor_code: '1Y',
        });
        expect(write['name']).toBe(NAME);
        expect(write['output_series_id']).toBe(SERIES);
        expect(write['currency_code']).toBe('USD');
        expect(write['split_tenor_code']).toBeNull();
        expect(write['discount_curve_config_id']).toBeNull();
    });
});

describe('the blueprint list', () => {
    it('offers Add, and says nothing about not being able to add', async () => {
        const html = await render('/market-data/curve-blueprints');
        expect(html).toContain('Add blueprint');
        expect(html).not.toContain('cannot be added here yet');
    });

    it("names each row by the record's own name, with its state line", async () => {
        const html = await render('/market-data/curve-blueprints');
        expect(html).toContain(NAME);
        expect(html).toContain('No pillars yet');
    });

    it("never parses the output series for the row's name", async () => {
        const html = await render('/market-data/curve-blueprints');
        expect(html).not.toContain(URI);
        expect(html).not.toContain(SERIES);
    });
});

describe('the blueprint record page', () => {
    it('titles the record and its breadcrumb by the name the record holds', async () => {
        const html = await render(`/market-data/curve-blueprints/${blueprint['id']}`);
        expect(html).toContain(NAME);
        expect(html).not.toContain(URI);
    });

    it('draws the output series on Details as a URI, and as a link', async () => {
        const html = await render(`/market-data/curve-blueprints/${blueprint['id']}`);
        // The URI is drawn as text inside the link, so the markup escapes the
        // ampersands the URI carries between its query members.
        expect(html).toContain('oresmd://ir/USD?type=series&amp;instrument=discount');
        expect(html).toContain(`/market-data/series?series=${SERIES}`);
        expect(html).toContain('US Dollar');
    });

    it('draws a dash for the discount curve a funding blueprint does not have', async () => {
        const html = await render(`/market-data/curve-blueprints/${blueprint['id']}`);
        // The label stands and the value is the dash, not the nil uuid and not
        // the empty string a missing read would leave.
        expect(html).toContain('Discount curve');
        expect(html).toContain('—');
        expect(html).not.toContain('00000000-0000-0000-0000-000000000000');
    });

    it('opens the Check tab on a funding blueprint without reading a discount curve', async () => {
        const html = await render(`/market-data/curve-blueprints/${blueprint['id']}?tab=check`);
        expect(html).toContain('Run the test');
        expect(html).toContain('Not checked yet.');
        // The absent discount used to be the nil uuid, and reading that id was
        // what the tab failed on. Nothing is read now, and nothing is reported.
        expect(html).not.toContain('There is no');
        expect(html).not.toContain('00000000-0000-0000-0000-000000000000');
    });

    it('lists the pillars in maturity order, with the sequence derived', async () => {
        const html = await render(`/market-data/curve-blueprints/${blueprint['id']}?tab=pillars`);
        const deposit = html.indexOf('DEPOSIT');
        const last = html.indexOf('>1Y<');
        expect(deposit).toBeGreaterThan(-1);
        expect(last).toBeGreaterThan(deposit);
        // The stored order was 1Y first; the panel draws the ladder by maturity.
        expect(html.indexOf('>1F<')).toBeGreaterThan(-1);
    });

    it('draws the id and says the series no longer resolves when the catalogue has lost it', async () => {
        // The cache is seeded, so nothing reads again on mount: a state a test
        // seeded is the state the page draws.
        const client = new QueryClient({
            defaultOptions: {
                queries: {
                    retry: false,
                    retryOnMount: false,
                    staleTime: Infinity,
                    refetchOnMount: false,
                },
            },
        });
        client.setQueryData(['held-market-series'], new Map([[SERIES, URI]]));
        // The record names a series the party's catalogue does not hold.
        const orphan = { ...blueprint, output_series_id: 'a9a9a9a9-0000-4000-8000-000000000009' };
        client.setQueryData(['records', CONFIGS, 'key', blueprint['id']], orphan);
        const html = renderToStaticMarkup(
            <QueryClientProvider client={client}>
                <TranslationProvider>
                    <MemoryRouter
                        initialEntries={[`/market-data/curve-blueprints/${blueprint['id']}`]}
                    >
                        <Routes>
                            <Route
                                path="/market-data/curve-blueprints/:id"
                                element={<CurveBlueprintPage />}
                            />
                        </Routes>
                    </MemoryRouter>
                </TranslationProvider>
            </QueryClientProvider>,
        );
        expect(html).toContain('a9a9a9a9-0000-4000-8000-000000000009');
        expect(html).toContain('no longer resolves');
        expect(html).not.toContain(`/market-data/series?series=a9a9a9a9`);
    });

    it('draws the id and says the series could not be read when the read fails', async () => {
        const html = await render(`/market-data/curve-blueprints/${blueprint['id']}`, {
            seriesUnread: true,
        });
        expect(html).toContain(SERIES);
        expect(html).toContain('could not be read');
        expect(html).not.toContain(`/market-data/series?series=${SERIES}`);
    });

    it('says a recipe with no pillar holds none, and names the pillars tab', async () => {
        const html = await render(`/market-data/curve-blueprints/${blueprint['id']}`);
        expect(html).not.toContain('holds no pillars');
    });
});
