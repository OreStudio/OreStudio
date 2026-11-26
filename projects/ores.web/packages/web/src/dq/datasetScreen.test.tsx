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
): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['dq-roster', 'dataset', code], roster);
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

    it('states that the published count is not served', () => {
        const html = render(WORLD, '/data-quality/datasets/iso.currencies');

        expect(html).toContain('Not served yet');
    });

    it('draws the publications tab, which says why it holds nothing yet', () => {
        const html = render(WORLD, '/data-quality/datasets/iso.currencies?tab=publications');

        expect(html).toContain('Publications');
        expect(html).toContain('The run log has no writer');
    });

    it('says so when no bundle carries the dataset', () => {
        const html = render([], '/data-quality/datasets/no.such.dataset', 'no.such.dataset');

        expect(html).toContain('No bundle carries no.such.dataset.');
    });
});
