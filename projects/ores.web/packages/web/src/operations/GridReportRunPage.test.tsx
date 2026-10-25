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
import {
    GridReportRunPage,
    REPORT_RUN_ERRORS_QUERY_KEY,
    REPORT_RUN_QUERY_KEY,
} from './GridReportRunPage.js';

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

/** One error row, as the error read returns it. */
function errorRow(overrides: Record<string, unknown> = {}): Record<string, unknown> {
    return {
        id: 'e0000000-0000-4000-8000-000000000001',
        scope: 'Curve',
        severity: 'error',
        message:
            'yield curve building failed for curve(s) Yield/GBP/GBP1D on date 2026-05-05: Could not find quote for Mandatory ID IR_SWAP/RATE/GBP/0D/1D/1M with as of date 2026-05-05',
        subject: 'curve',
        subject_id: '[IndexCurve(GBP-SONIA,Yield/GBP/GBP1D)]',
        subject_version: null,
        subject_as_of: null,
        time: '2026-10-25 09:51:53.527119Z',
        ...overrides,
    };
}

function tradeRow(overrides: Record<string, unknown> = {}): Record<string, unknown> {
    return errorRow({
        id: 'e0000000-0000-4000-8000-000000000002',
        scope: 'Trade',
        subject: 'trade',
        subject_id: '28e6d1c3-b0aa-55fa-9c14-142e3e34092e',
        message:
            'Cannot build all required curves! Building failed for: ([IndexCurve(GBP-SONIA,Yield/GBP/GBP1D)])',
        ...overrides,
    });
}

function render(
    view: GridReportRunView,
    options: { readonly errors?: readonly unknown[]; readonly tab?: string } = {},
): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData([REPORT_RUN_QUERY_KEY, RUN_ID], view);
    client.setQueryData([REPORT_RUN_ERRORS_QUERY_KEY, RUN_ID], options.errors ?? []);
    const address =
        options.tab === undefined
            ? `/operations/grid/report-runs/${RUN_ID}`
            : `/operations/grid/report-runs/${RUN_ID}?tab=${options.tab}`;
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[address]}>
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
    it('states the run and the instants it has', () => {
        const html = render(runView());

        expect(html).toContain('nightly_instance');
        expect(html).toContain('Running');
        expect(html).toContain('not yet known');
    });

    it('offers the run and its errors as two screens', () => {
        const html = render(runView());

        expect(html).toContain('role="tab"');
        expect(html).toContain('>Run<');
        expect(html).toContain('>Errors<');
    });

    it('says what is missing by curve and links into the errors', () => {
        const html = render(runView({ health: 'failed' }), {
            errors: [errorRow(), tradeRow()],
        });

        expect(html).toContain('Market data is missing for 1 curve(s).');
        expect(html).toContain('Priced at 2026-05-05');
        expect(html).toContain('View all 2 errors');
    });

    it('groups a failure by the curve, with the trades that named it as links', () => {
        const html = render(runView({ health: 'failed' }), {
            errors: [errorRow(), tradeRow()],
            tab: 'errors',
        });

        expect(html).toContain('Yield/GBP/GBP1D');
        expect(html).toContain('/trades?trade=28e6d1c3-b0aa-55fa-9c14-142e3e34092e');
        expect(html).toContain('Grouped');
        expect(html).toContain('Split');
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
