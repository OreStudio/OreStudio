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

import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter, Route, Routes } from 'react-router';
import { describe, expect, it } from 'vitest';
import type { BundleRosterBundle } from '@ores/wire-protocol/generated/dq/protocol/bundle_roster_protocol';
import { TranslationProvider } from '../i18n/Provider.js';
import { DEFAULT_PAGE_SIZE } from '../ui/Pager.js';
import { DatasetPage } from './DatasetPage.js';

/**
 * The dataset page, called the way a person reaches it: a saved address names
 * the dataset, the server answers the bundle that carries it, and the page draws
 * the record page the standard shapes: the details, then the publications.
 */

function member(code: string, extra: Record<string, unknown> = {}) {
    return {
        display_order: 1,
        dataset_name: 'ISO 4217 Currency Codes',
        artefact_type: 'currencies',
        artefact_table: 'dq_currencies_artefact_tbl',
        target_subject: 'refdata.v1.ops.publish_currencies_from_dq',
        has_dataset: true,
        has_artefact_type: true,
        staged_count: 160,
        is_publishable: true,
        optional: false,
        dataset_code: code,
        ...extra,
    };
}

const TABLE = 'dq_currencies_artefact_tbl';

const WORLD: readonly BundleRosterBundle[] = [
    {
        code: 'world',
        name: 'World reference data',
        description: '',
        level: 'world',
        dataset_count: 1,
        members: [member('iso.currencies')],
    },
];

function render(
    roster: readonly BundleRosterBundle[],
    path: string,
    code = 'iso.currencies',
    runs: readonly unknown[] = [],
    tables: readonly unknown[] = [],
    page: unknown = null,
): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['dq-roster', 'dataset', code], roster);
    client.setQueryData(['dq-publications', code], runs);
    if (tables.length > 0) {
        client.setQueryData(['dq-tables', code], tables);
    }
    if (page !== null) {
        client.setQueryData(['dq-rows', code, TABLE, 0, DEFAULT_PAGE_SIZE], page);
    }
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[path]}>
                    <Routes>
                        <Route path="/data-quality/datasets/:code" element={<DatasetPage />} />
                    </Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the dataset page', () => {
    it('names the dataset, its key and the bundle that carries it', () => {
        const html = render(WORLD, '/data-quality/datasets/iso.currencies');

        expect(html).toContain('ISO 4217 Currency Codes');
        expect(html).toContain('iso.currencies');
        expect(html).toContain('Home');
        expect(html).toContain('Data quality');
        expect(html).toContain('Reference data coverage');
    });

    it('states where the rows are staged, what a publish dispatches to and both counts', () => {
        const html = render(WORLD, '/data-quality/datasets/iso.currencies');

        expect(html).toContain('dq_currencies_artefact_tbl');
        expect(html).toContain('refdata.v1.ops.publish_currencies_from_dq');
        expect(html).toContain('160');
        expect(html).toContain('Ready');
    });

    it('states that the published count and the last publication are not served', () => {
        const html = render(WORLD, '/data-quality/datasets/iso.currencies');

        expect(html).toContain('Not served yet');
        expect(html).toContain('Last published');
        expect(html).toContain('Nothing has published this dataset yet.');
    });

    it('draws the publications tab, which says nothing has published yet', () => {
        const html = render(WORLD, '/data-quality/datasets/iso.currencies?tab=publications');

        expect(html).toContain('Nothing has published this dataset yet.');
    });

    it('draws a run the log holds, with its outcome', () => {
        const html = render(
            WORLD,
            '/data-quality/datasets/iso.currencies?tab=publications',
            'iso.currencies',
            [
                {
                    published_at: '2026-11-27 15:49:00.000Z',
                    published_by: 'tenant_admin',
                    mode: 'upsert',
                    target_table: 'dq_currencies_artefact_tbl',
                    records_inserted: 160,
                    records_updated: 0,
                    records_skipped: 2,
                    records_deleted: 0,
                    outcome: 'published_with_warnings',
                    messages: '[{"severity":"warning","text":"2 row(s) were skipped"}]',
                },
            ],
        );

        expect(html).toContain('tenant_admin');
        expect(html).toContain('Published with warnings');
        expect(html).toContain('2 row(s) were skipped');
    });

    it('lists the tables the dataset stages into, with their columns and counts', () => {
        const html = render(
            WORLD,
            '/data-quality/datasets/iso.currencies?tab=contents',
            'iso.currencies',
            [],
            [
                {
                    table_name: 'dq_currencies_artefact_tbl',
                    display_order: 1,
                    columns: 'iso_code, numeric_code, name',
                    staged_count: 160,
                },
                {
                    table_name: 'dq_currency_aliases_artefact_tbl',
                    display_order: 2,
                    columns: 'iso_code, alias, locale',
                    staged_count: 12,
                },
            ],
        );

        // The artefact type is the family's name, so the list states it once.
        expect(html).toContain('currencies');
        expect(html).toContain('dq_currencies_artefact_tbl');
        expect(html).toContain('iso_code, numeric_code, name');
        expect(html).toContain('dq_currency_aliases_artefact_tbl');
        expect(html).toContain('12');
    });

    it("draws one table's rows with the columns the rows carry", () => {
        const html = render(
            WORLD,
            `/data-quality/datasets/iso.currencies?tab=contents&table=${TABLE}`,
            'iso.currencies',
            [],
            [
                {
                    table_name: TABLE,
                    display_order: 1,
                    columns: 'iso_code, name',
                    staged_count: 160,
                },
            ],
            {
                columns: [
                    { name: 'iso_code', type: 'text' },
                    { name: 'name', type: 'text' },
                ],
                rows: [
                    { iso_code: 'GBP', name: 'Pound Sterling' },
                    { iso_code: 'USD', name: 'US Dollar' },
                ],
                total: 160,
            },
        );

        expect(html).toContain('GBP');
        expect(html).toContain('Pound Sterling');
        expect(html).toContain('Every row staged for iso.currencies is in this table: 160 in all.');
        // Paging is the standard one: the page states its bounds, and the way to
        // the other pages is the shared Pager rather than a button of its own.
        expect(html).toContain('Rows 1–2 of 160');
        expect(html).toContain('Next');
    });

    it('says so when no bundle carries the dataset', () => {
        const html = render([], '/data-quality/datasets/no.such.dataset', 'no.such.dataset');

        expect(html).toContain('No bundle carries no.such.dataset.');
    });
});
