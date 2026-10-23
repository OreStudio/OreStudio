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
import type { BatchView, ResultView, WorkunitView } from '@ores/wire-protocol/browser';
import { TranslationProvider } from '../i18n/Provider.js';
import { GridJobPage, canonicalRun, focusRun, secondsBetween } from './GridJobPage.js';
import { APP_VERSION_QUERY_KEY } from './GridAppVersionPage.js';
import { GRID_JOBS_QUERY_KEY } from './GridJobs.js';

/**
 * The job page, as a reader sees it.
 *
 * What is checked is what the page promises: the state is read from the canonical
 * result with the outcome beside it, a failed run is not called a success, a run in
 * flight says its node and output are not yet known, and the references lead to the
 * pages that exist. The reads are seeded, so no request is made.
 */

const BATCH_ID = 'b0000000-0000-4000-8000-000000000001';
const WORKUNIT_ID = 'a0000000-0000-4000-8000-000000000001';
const SIBLING_ID = 'a0000000-0000-4000-8000-000000000002';
const RESULT_ID = 'c0000000-0000-4000-8000-000000000001';
const HOST_ID = 'e0000000-0000-4000-8000-000000000001';
const APP_VERSION_ID = 'f0000000-0000-4000-8000-000000000001';
const PARTY_ID = 'd1111111-0000-4000-8000-000000000001';

const batch: BatchView = {
    id: BATCH_ID,
    external_ref: 'order-e',
    party_id: PARTY_ID,
    status: 'closed',
    modified_by: 'tenant_admin',
    recorded_at: '2026-10-13 02:27:27.000000Z' as BatchView['recorded_at'],
};

function workunit(overrides: Partial<WorkunitView> = {}): WorkunitView {
    return {
        id: WORKUNIT_ID,
        batch_id: BATCH_ID,
        app_version_id: APP_VERSION_ID,
        input_uri: '/api/v1/storage/ores/compute/input/batch.tar.gz',
        config_uri: null,
        priority: 1,
        target_redundancy: 1,
        canonical_result_id: RESULT_ID,
        modified_by: 'tenant_admin',
        recorded_at: '2026-10-13 02:27:28.000000Z' as WorkunitView['recorded_at'],
        ...overrides,
    };
}

function run(overrides: Partial<ResultView> = {}): ResultView {
    return {
        id: RESULT_ID,
        workunit_id: WORKUNIT_ID,
        host_id: HOST_ID,
        host_hidden: false,
        server_state: 5,
        outcome: 1,
        output_uri: '/api/v1/storage/ores/compute/output/result.tar.gz',
        error_message: null,
        received_at: '2026-10-13 02:27:38.000000Z' as NonNullable<ResultView['received_at']>,
        modified_by: 'ores_compute_service',
        recorded_at: '2026-10-13 02:27:28.000000Z' as ResultView['recorded_at'],
        ...overrides,
    };
}

function page<Row>(rows: readonly Row[], limit = 15) {
    return {
        read_at: '2026-10-13 03:00:00.000000Z' as BatchView['recorded_at'],
        rows: [...rows],
        total: rows.length,
        offset: 0,
        limit,
    };
}

function render(
    wu: WorkunitView,
    runs: readonly ResultView[],
    permissions: readonly string[] = ['storage::objects:read'],
): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['my-access'], {
        roles: [
            {
                roleId: 'r',
                name: 'Viewer',
                description: '',
                permissionCodes: [...permissions],
                givenBy: 'system',
                givenAt: '2026-10-05 09:30:00Z',
                reasonCode: 'access.initial',
                commentary: '',
            },
        ],
    });
    client.setQueryData([GRID_JOBS_QUERY_KEY, 'job', WORKUNIT_ID], wu);
    client.setQueryData([GRID_JOBS_QUERY_KEY, 'job-batch', BATCH_ID], batch);
    client.setQueryData([GRID_JOBS_QUERY_KEY, 'job-runs', WORKUNIT_ID], page(runs, 50));
    client.setQueryData(
        [GRID_JOBS_QUERY_KEY, 'workunits', BATCH_ID, 0, 15],
        page([wu, workunit({ id: SIBLING_ID, canonical_result_id: null })]),
    );
    client.setQueryData([APP_VERSION_QUERY_KEY, APP_VERSION_ID], {
        version: { engine_version: '1.8.17.0', wrapper_version: '1.0.0' },
    });
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[`/operations/grid/jobs/${WORKUNIT_ID}`]}>
                    <Routes>
                        <Route path="/operations/grid/jobs/:workunitId" element={<GridJobPage />} />
                    </Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the job page', () => {
    it('reads a job as done with the outcome of its accepted run beside it', () => {
        const html = render(workunit(), [run()]);

        expect(html).toContain('>Done<');
        expect(html).toContain('>Success<');
        expect(html).not.toContain('A closed batch is not a successful batch.');
    });

    it('says a job that ran and failed is done, and warns that a closed batch is not a success', () => {
        const html = render(workunit(), [
            run({
                outcome: 3,
                output_uri: null,
                error_message: 'Failed to open file Input/ore.xml',
            }),
        ]);

        expect(html).toContain('>Done<');
        expect(html).toContain('>ClientError<');
        expect(html).toContain('A closed batch is not a successful batch.');
        expect(html).toContain('Failed to open file Input/ore.xml');
    });

    it('says the node and the output are not yet known while a run is in flight', () => {
        const html = render(workunit({ canonical_result_id: null }), [
            run({
                server_state: 4,
                outcome: 0,
                host_id: null,
                output_uri: null,
                received_at: null,
            }),
        ]);

        expect(html).toContain('>Not done<');
        expect(html).toContain('not yet known');
        expect(html).toContain('not yet received');
        expect(html).toContain('still running');
        expect(html).toContain('>InProgress<');
        expect(html).toContain('not set (0)');
    });

    it('links the node to a role that may read it and hides it from one that may not', () => {
        const operator = render(workunit(), [run()]);
        const other = render(workunit(), [run({ host_id: null, host_hidden: true })]);

        expect(operator).toContain(`href="/operations/grid/nodes/${HOST_ID}"`);
        expect(other).toContain('hidden for your role');
        expect(other).not.toContain(HOST_ID);
    });

    it('names the fields as the models do', () => {
        const html = render(workunit(), [run()]);

        for (const name of [
            'Workunit ID',
            'External Ref',
            'Batch Status',
            'Host ID',
            'Batch Recorded At',
            'Received At',
            'Target Redundancy',
            'Canonical Result ID',
            'App Version ID',
        ]) {
            expect(html).toContain(name);
        }
    });

    it('leads to the batch, the app version and the other jobs of the batch', () => {
        const html = render(workunit(), [run()]);

        expect(html).toContain(`sel=batch%3A${BATCH_ID}`);
        expect(html).toContain(`href="/operations/grid/app-versions/${APP_VERSION_ID}"`);
        expect(html).toContain('1.8.17.0');
        expect(html).toContain(`href="/operations/grid/jobs/${SIBLING_ID}"`);
        expect(html).toContain('(this job)');
    });

    it('links the files to the download routes', () => {
        const html = render(workunit(), [run()]);

        expect(html).toContain(`href="/api/operations/grid/jobs/workunits/${WORKUNIT_ID}/input"`);
        expect(html).toContain(`href="/api/operations/grid/jobs/results/${RESULT_ID}/output"`);
    });

    it('shows the files as text to a person without the storage read code', () => {
        const html = render(workunit(), [run()], []);

        expect(html).toContain('/api/v1/storage/ores/compute/input/batch.tar.gz');
        expect(html).not.toContain('/api/operations/grid/jobs/workunits/');
    });

    it('marks the accepted run among the runs', () => {
        const html = render(workunit({ target_redundancy: 2 }), [
            run(),
            run({ id: 'c0000000-0000-4000-8000-000000000002', outcome: 1 }),
        ]);

        expect(html).toContain('2 runs');
        expect(render(workunit(), [run()])).toContain('1 run<');
        expect(html).toContain('(canonical)');
    });

    it('warns only when the accepted run failed, not when another run did', () => {
        const html = render(workunit({ target_redundancy: 2 }), [
            run({ id: 'c0000000-0000-4000-8000-000000000002', outcome: 3, output_uri: null }),
            run(),
        ]);

        expect(html).not.toContain('A closed batch is not a successful batch.');
    });

    it('says the report behind a batch is not a checked relationship', () => {
        const html = render(workunit(), [run()]);

        expect(html).toContain('The model does not state this as a relationship');
    });
});

describe('the helpers of the job page', () => {
    it('finds the run the grid accepted', () => {
        const runs = [run({ id: 'other' }), run()];

        expect(canonicalRun(workunit(), runs)?.id).toBe(RESULT_ID);
        expect(canonicalRun(workunit({ canonical_result_id: null }), runs)).toBeUndefined();
    });

    it('speaks for the accepted run, and for the first when none is accepted', () => {
        const other = run({ id: 'other' });
        const runs = [other, run()];

        expect(focusRun(workunit(), runs)?.id).toBe(RESULT_ID);
        expect(focusRun(workunit({ canonical_result_id: null }), runs)?.id).toBe('other');
        expect(focusRun(workunit(), [])).toBeUndefined();
    });

    it('never counts a negative number of seconds', () => {
        expect(secondsBetween('2026-10-13 02:27:38.000000Z', '2026-10-13 02:27:28.000000Z')).toBe(
            0,
        );
    });

    it('counts the seconds from a write to the arrival of the output', () => {
        expect(secondsBetween('2026-10-13 02:27:28.000000Z', '2026-10-13 02:27:38.000000Z')).toBe(
            10,
        );
        expect(secondsBetween('2026-10-13 02:27:28.000000Z', null)).toBeNull();
    });
});
