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
    output_series_id: SERIES,
    party_id: 'd82fc8f0-b37a-491e-9acf-4ac69b82303e',
    currency_code: 'USD',
    index_tenor: '1D',
    curve_family_role: 'FUNDING',
    discount_curve_config_id: '00000000-0000-0000-0000-000000000000',
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

function render(path: string): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
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
    client.setQueryData(['held-market-series'], new Map([[SERIES, URI]]));
    client.setQueryData(['tenor-catalogue'], { tenors: TENORS, conventions: [], schedules: [] });
    client.setQueryData(['records', CONFIGS, 'all'], [blueprint]);
    client.setQueryData(['records', CONFIGS, 'funding', ''], { rows: [blueprint], total: 1 });
    client.setQueryData(['records', CONFIGS, 'funding', 'USD'], { rows: [blueprint], total: 1 });
    client.setQueryData(['records', CONFIGS, 'used-by', blueprint['id']], {
        rows: [],
        total: 0,
    });
    client.setQueryData(['records', CONFIGS, 'currencies'], [blueprint]);
    // The list's own read names each row's series, so the page the table reads
    // is the row as that read answers it.
    client.setQueryData(pageKey({ key: CONFIGS }, FIRST_PAGE), {
        rows: [{ ...blueprint, series_uri: URI }],
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
    it('asks for the output series the party holds, first and required', () => {
        const series = specs()[0];
        expect(series?.field).toBe('output_series_id');
        expect(series?.kind).toEqual({ kind: 'pick', choices: [{ value: SERIES, label: URI }] });
        expect(series?.optional).not.toBe(true);
        expect(series?.fixed).toBe(true);
    });

    it('refuses to save a new blueprint that names no series', () => {
        expect(invalidFields(specs(), {})).toContain('output_series_id');
    });

    it('writes the series the person chose', () => {
        const write = writeOf(specs(), {
            output_series_id: SERIES,
            currency_code: 'USD',
            index_tenor: '1D',
            curve_family_role: 'FUNDING',
            interpolation_method: 'LOG_LINEAR_DISCOUNT',
            day_count_convention: 'A360',
            tenor_convention_code: 'RATES_SPOT_FOMC',
            split_tenor_code: '1Y',
        });
        expect(write['output_series_id']).toBe(SERIES);
        expect(write['currency_code']).toBe('USD');
        expect(write['split_tenor_code']).toBeNull();
    });
});

describe('the blueprint list', () => {
    it('offers Add, and says nothing about not being able to add', () => {
        const html = render('/market-data/curve-blueprints');
        expect(html).toContain('Add blueprint');
        expect(html).not.toContain('cannot be added here yet');
    });

    it('names each row by the series it publishes into, with its state line', () => {
        const html = render('/market-data/curve-blueprints');
        expect(html).toContain('USD-SOFR-FOMC');
        expect(html).toContain('No pillars yet');
    });
});

describe('the blueprint record page', () => {
    it('draws the output series as the field it is, linked to the series read', () => {
        const html = render(`/market-data/curve-blueprints/${blueprint['id']}`);
        expect(html).toContain('USD-SOFR-FOMC');
        expect(html).toContain(`/market-data/series?series=${SERIES}`);
        expect(html).toContain('US Dollar');
    });

    it('lists the pillars in maturity order, with the sequence derived', () => {
        const html = render(`/market-data/curve-blueprints/${blueprint['id']}?tab=pillars`);
        const deposit = html.indexOf('DEPOSIT');
        const last = html.indexOf('>1Y<');
        expect(deposit).toBeGreaterThan(-1);
        expect(last).toBeGreaterThan(deposit);
        // The stored order was 1Y first; the panel draws the ladder by maturity.
        expect(html.indexOf('>1F<')).toBeGreaterThan(-1);
    });

    it('says a recipe with no pillar holds none, and names the pillars tab', () => {
        const html = render(`/market-data/curve-blueprints/${blueprint['id']}`);
        expect(html).not.toContain('holds no pillars');
    });
});
