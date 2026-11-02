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
import { renderToStaticMarkup } from 'react-dom/server';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router';
import { TranslationProvider } from '../i18n/Provider.js';
import type { ReportRunErrorView } from '@ores/wire-protocol/browser';
import { ErrorSubject, RunErrors, runErrorsKey } from './RunErrorsTable.js';

/**
 * The errors of a run, one row each. What is checked is what a reader follows:
 * an error about a trade leads to that trade's page, and an error about
 * anything else names its subject as text.
 */

const TRADE = '28e6d1c3-b0aa-55fa-9c14-142e3e34092e';

const error = (over: Partial<ReportRunErrorView>): ReportRunErrorView => ({
    id: 'e0000000-0000-4000-8000-000000000001',
    severity: 'error',
    scope: 'Trade',
    about: TRADE,
    message: 'Cannot build all required curves!',
    subject: 'trade',
    subject_id: TRADE,
    cause: null,
    time: '2026-10-25 09:51:53.527119Z',
    ...over,
});

function render(row: ReportRunErrorView): string {
    return renderToStaticMarkup(
        <MemoryRouter>
            <ErrorSubject error={row} />
        </MemoryRouter>,
    );
}

describe('the subject of a run error', () => {
    it('links an error about a trade to the page of that trade', () => {
        const html = render(error({}));

        expect(html).toContain(`href="/trading/trades/${TRADE}"`);
    });

    it('names a curve as text, with no link to a trade', () => {
        const html = render(
            error({
                scope: 'Curve',
                about: 'Yield/USD/USD1D',
                subject: 'curve',
                subject_id: '0f1703a1-b674-548e-a959-dfa224806330',
            }),
        );

        expect(html).toContain('Yield/USD/USD1D');
        expect(html).not.toContain('/trading/trades/');
    });
});

describe('the errors tab of a run', () => {
    it('labels its views and its overview in words, never by their keys', () => {
        const run = '22222222-2222-4222-8222-222222222222';
        const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
        client.setQueryData(runErrorsKey(run), [
            error({}),
            error({
                id: 'e0000000-0000-4000-8000-000000000002',
                severity: 'warning',
                scope: 'Report',
            }),
        ]);
        const html = renderToStaticMarkup(
            <QueryClientProvider client={client}>
                <TranslationProvider>
                    <MemoryRouter initialEntries={[`/reporting/runs/${run}?tab=errors`]}>
                        <RunErrors runId={run} />
                    </MemoryRouter>
                </TranslationProvider>
            </QueryClientProvider>,
        );

        for (const label of [
            'Overview',
            'List',
            'Errors',
            'Warnings',
            'Total',
            'problems by scope',
        ]) {
            expect(html).toContain(label);
        }
        expect(html).not.toContain('reporting.run.');
    });
});
