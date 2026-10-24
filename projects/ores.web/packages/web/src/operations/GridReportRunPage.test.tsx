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
import type { GridReportRunView } from '@ores/wire-protocol/browser';
import { TranslationProvider } from '../i18n/Provider.js';
import { GridReportRunPage, REPORT_RUN_QUERY_KEY } from './GridReportRunPage.js';

/**
 * The report run page, as a reader sees it.
 *
 * What is checked is what the page promises: the run is stated with its health, a
 * link leads back to the report, a link leads to the batch when one is visible, and
 * a run that is not a production run says so. The read is seeded, so no request is
 * made.
 */

const RUN_ID = 'a1b2c3d4-0000-4000-8000-000000000009';
const DEFINITION_ID = 'd0000000-0000-4000-8000-000000000001';
const BATCH_ID = 'b0000000-0000-4000-8000-000000000001';

function runView(overrides: Partial<GridReportRunView> = {}): GridReportRunView {
    return {
        id: RUN_ID,
        name: 'nightly_instance',
        description: 'A nightly run',
        definition_id: DEFINITION_ID,
        definition_name: 'NPV',
        party_id: 'd1111111-0000-4000-8000-000000000001',
        party_name: 'ACME Corporation UK plc',
        kind: 'production',
        state_name: 'Running',
        health: 'running',
        output_message: 'All trades priced',
        started_at: '2026-10-23 07:00:00.000000Z' as GridReportRunView['started_at'],
        completed_at: null,
        batch_id: BATCH_ID,
        modified_by: 'scheduler',
        recorded_at: '2026-10-23 07:00:00.000000Z' as GridReportRunView['recorded_at'],
        ...overrides,
    };
}

function render(view: GridReportRunView): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData([REPORT_RUN_QUERY_KEY, RUN_ID], view);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[`/operations/grid/report-runs/${RUN_ID}`]}>
                    <Routes>
                        <Route
                            path="/operations/grid/report-runs/:id"
                            element={<GridReportRunPage />}
                        />
                    </Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the report run page', () => {
    it('states the run, its message and the instants it has', () => {
        const html = render(runView());

        expect(html).toContain('nightly_instance');
        expect(html).toContain('All trades priced');
        expect(html).toContain('Running');
        expect(html).toContain('not yet known');
    });

    it('links to the report and to the batch', () => {
        const html = render(runView());

        expect(html).toContain(`/reporting/reports/${DEFINITION_ID}`);
        expect(html).toContain(`open=${BATCH_ID}`);
    });

    it('says no batch is visible when the read found none', () => {
        const html = render(runView({ batch_id: null }));

        expect(html).toContain('No batch of this run is visible to you.');
        expect(html).not.toContain('open=');
    });

    it('names the report and the party', () => {
        const html = render(runView());

        expect(html).toContain('Open the report NPV');
        expect(html).toContain('ACME Corporation UK plc');
    });

    it('says a failed run made no batch, and a run with none visible is not called failed', () => {
        const failed = render(runView({ health: 'failed', batch_id: null }));
        const hidden = render(runView({ health: 'running', batch_id: null }));

        expect(failed).toContain('This run made no batch.');
        expect(hidden).toContain('No batch of this run is visible to you.');
    });

    it('warns when the run is not a production run', () => {
        const html = render(runView({ kind: 'package' }));

        expect(html).toContain('This run is of the kind package, not a production run.');
    });
});
