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
import type {
    BatchView,
    GridReportRunView,
    ResultView,
    WorkunitView,
} from '@ores/wire-protocol/browser';
import { ApiFailure } from '../api/transport.js';
import { TranslationProvider } from '../i18n/Provider.js';
import {
    GridJobPage,
    canonicalRun,
    findTarget,
    focusRun,
    formatDuration,
    phasesOf,
    secondsBetween,
} from './GridJobPage.js';
import { APP_VERSION_QUERY_KEY } from './GridAppVersionPage.js';
import { GRID_JOBS_QUERY_KEY } from './GridJobs.js';
import { REPORT_RUN_QUERY_KEY } from './GridReportRunPage.js';

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
    report_run_name: null,
    party_name: null,
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
        started_at: null,
        duration_ms: null,
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
    shownBatch: BatchView = batch,
    reportRun?: GridReportRunView,
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
    client.setQueryData([GRID_JOBS_QUERY_KEY, 'job-batch', BATCH_ID], shownBatch);
    client.setQueryData([GRID_JOBS_QUERY_KEY, 'job-runs', WORKUNIT_ID], page(runs, 50));
    client.setQueryData(
        [GRID_JOBS_QUERY_KEY, 'workunits', BATCH_ID, 0, 15],
        page([wu, workunit({ id: SIBLING_ID, canonical_result_id: null })]),
    );
    client.setQueryData(
        [GRID_JOBS_QUERY_KEY, 'lineage-jobs', BATCH_ID],
        page([wu, workunit({ id: SIBLING_ID, canonical_result_id: null })]),
    );
    if (reportRun !== undefined) {
        client.setQueryData([REPORT_RUN_QUERY_KEY, reportRun.id], reportRun);
    }
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

    it('links a batch named by a report run id to the page of that run', () => {
        const runId = 'a1b2c3d4-0000-4000-8000-000000000009';
        const html = render(workunit(), [run()], ['storage::objects:read'], {
            ...batch,
            external_ref: runId,
            report_run_name: 'NPV 2026-10-23 22:07:58',
        });

        expect(html).toContain(`/operations/grid/report-runs/${runId}`);
        expect(html).not.toContain('No report is behind this batch.');
    });

    it('says no report is behind a batch that the shell dispatched', () => {
        const html = render(workunit(), [run()]);

        expect(html).toContain('No report is behind this batch.');
        expect(html).not.toContain('/operations/grid/report-runs/');
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
});

describe('the layout and the finder of the job page', () => {
    it('draws the job, the runs and the batch jobs on the left and the files, release and lineage on the right', () => {
        const html = render(workunit(), [run()]);
        const columns = html.split('min-w-0 space-y-6');

        expect(html).toContain('lg:grid-cols-[minmax(0,3fr)_minmax(280px,2fr)]');
        expect(columns).toHaveLength(3);
        for (const left of ['Runs of the job', 'Jobs of the batch']) {
            expect(columns[1]).toContain(left);
        }
        for (const right of ['Files', 'Release that ran it', 'Lineage']) {
            expect(columns[2]).toContain(right);
        }
    });

    it('offers a find bar for a batch reference, a job id or a result id', () => {
        const html = render(workunit(), [run()]);

        expect(html).toContain('batch reference, job id or result id');
        expect(html).toContain('>Find<');
    });

    it('links back to the batch of the job and names it', () => {
        const html = render(workunit(), [run()]);

        expect(html).toContain('Jobs tab, batch order-e');
        expect(html).toContain(`open=${BATCH_ID}`);
    });
});

describe('the lineage of the job page', () => {
    const RUN_ID = 'a1b2c3d4-0000-4000-8000-000000000009';
    const DEFINITION_ID = 'd0000000-0000-4000-8000-0000000000de';
    const reportBatch: BatchView = {
        ...batch,
        external_ref: RUN_ID,
        report_run_name: 'NPV 2026-10-23 22:07:58',
    };
    const reportRun: GridReportRunView = {
        id: RUN_ID,
        name: 'NPV 2026-10-23 22:07:58',
        description: '',
        definition_id: DEFINITION_ID,
        definition_name: 'NPV',
        party_id: PARTY_ID,
        party_name: null,
        kind: 'production',
        state_name: 'completed',
        health: 'completed clean',
        output_message: '',
        started_at: null,
        completed_at: null,
        batch_id: BATCH_ID,
        modified_by: 'scheduler',
        recorded_at: '2026-10-23 22:07:58.000000Z' as GridReportRunView['recorded_at'],
    };

    it('draws the chain from the report to the job for a batch a report made', () => {
        const html = render(workunit(), [run()], ['storage::objects:read'], reportBatch, reportRun);
        const chain = html.slice(html.indexOf('>Lineage<'));

        for (const node of ['Scheduled job', '>NPV<', 'NPV 2026-10-23 22:07:58', 'this job']) {
            expect(chain).toContain(node);
        }
        expect(chain.indexOf('Scheduled job')).toBeLessThan(chain.indexOf('>NPV<'));
        expect(chain.indexOf('>NPV<')).toBeLessThan(chain.indexOf('this job'));
        expect(chain).toContain(`/reporting/reports/${DEFINITION_ID}`);
        expect(chain).toContain(`/operations/grid/report-runs/${RUN_ID}`);
    });

    it('says the batch names the run only in its External Ref, and counts the siblings', () => {
        const html = render(workunit(), [run()], ['storage::objects:read'], reportBatch, reportRun);

        expect(html).toContain('does not state this as a relationship');
        expect(html).toContain('1 sibling workunit in the same batch');
    });

    it('draws no report for a batch the shell dispatched, and says so', () => {
        const html = render(workunit(), [run()]);
        const chain = html.slice(html.indexOf('>Lineage<'));

        expect(chain).toContain('No report is behind this batch.');
        expect(chain).not.toContain('Scheduled job');
        expect(chain).not.toContain('Report definition');
        expect(chain).toContain('this job');
    });

    it('draws the run as a link when the session cannot read it, and no definition', () => {
        const html = render(workunit(), [run()], ['storage::objects:read'], reportBatch);
        const chain = html.slice(html.indexOf('>Lineage<'));

        expect(chain).toContain(`/operations/grid/report-runs/${RUN_ID}`);
        expect(chain).not.toContain('/reporting/reports/');
    });
});

describe('where the find bar opens', () => {
    const refuse = () =>
        Promise.reject(new ApiFailure(404, { code: 'not-found', message: 'no such record' }));
    const reads = (found: Partial<Record<'workunit' | 'result' | 'batch', string>>) => ({
        workunit: found.workunit ? () => Promise.resolve({ id: found.workunit as string }) : refuse,
        result: found.result
            ? () => Promise.resolve({ workunit_id: found.result as string })
            : refuse,
        batch: found.batch ? () => Promise.resolve({ id: found.batch as string }) : refuse,
    });
    const ID = 'a0000000-0000-4000-8000-0000000000aa';

    it('opens a job when the id is one', async () => {
        expect(await findTarget(ID, reads({ workunit: ID }))).toBe(`/operations/grid/jobs/${ID}`);
    });

    it('opens the job of a result when the id is a result', async () => {
        expect(await findTarget(ID, reads({ result: WORKUNIT_ID }))).toBe(
            `/operations/grid/jobs/${WORKUNIT_ID}`,
        );
    });

    it('opens the Jobs tab at a batch when the id is a batch', async () => {
        const target = await findTarget(ID, reads({ batch: BATCH_ID }));

        expect(target).toContain('tab=jobs');
        expect(target).toContain(`open=${BATCH_ID}`);
    });

    it('opens nothing for an id that is none of them', async () => {
        expect(await findTarget(ID, reads({}))).toBeNull();
    });

    it('moves on after a refusal and does not move on after a failure of the service', async () => {
        const forbidden = () =>
            Promise.reject(new ApiFailure(403, { code: 'forbidden', message: 'your role cannot' }));
        const broken = () =>
            Promise.reject(new ApiFailure(502, { code: 'upstream-unavailable', message: 'down' }));
        const batch = () => Promise.resolve({ id: BATCH_ID });

        expect(await findTarget(ID, { workunit: forbidden, result: refuse, batch })).toContain(
            `open=${BATCH_ID}`,
        );
        await expect(findTarget(ID, { workunit: broken, result: refuse, batch })).rejects.toThrow(
            'down',
        );
    });

    it('lists the batches a part of a reference matches, without reading anything', async () => {
        const never = () => Promise.reject(new Error('read'));
        const target = await findTarget('order', {
            workunit: never,
            result: never,
            batch: never,
        });

        expect(target).toBe('/operations/grid?tab=jobs&find=order');
    });
});

describe('the phases of a run', () => {
    const at = (time: string) => time as NonNullable<ResultView['started_at']>;

    it('splits the time from the claim to the arrival by what the engine ran', () => {
        const phases = phasesOf(
            run({
                started_at: at('2026-10-13 02:27:30.000000Z'),
                received_at: at('2026-10-13 02:27:40.000000Z'),
                duration_ms: 7000,
            }),
        );

        expect(phases).toEqual({ totalMs: 10000, ranMs: 7000, otherMs: 3000 });
    });

    it('states no split when the node did not report the engine time', () => {
        const phases = phasesOf(
            run({
                started_at: at('2026-10-13 02:27:30.000000Z'),
                received_at: at('2026-10-13 02:27:40.000000Z'),
                duration_ms: null,
            }),
        );

        expect(phases).toEqual({ totalMs: 10000, ranMs: null, otherMs: null });
    });

    it('cuts an engine time that exceeds the whole to the whole', () => {
        const phases = phasesOf(
            run({
                started_at: at('2026-10-13 02:27:30.000000Z'),
                received_at: at('2026-10-13 02:27:31.000000Z'),
                duration_ms: 5000,
            }),
        );

        expect(phases).toEqual({ totalMs: 1000, ranMs: 1000, otherMs: 0 });
    });

    it('has no phases for a run with no recorded claim or one still in flight', () => {
        expect(phasesOf(run({ started_at: null }))).toBeNull();
        expect(
            phasesOf(run({ started_at: at('2026-10-13 02:27:30.000000Z'), received_at: null })),
        ).toBeNull();
    });

    it('writes a time span in whole seconds, then minutes', () => {
        expect(formatDuration(0)).toBe('< 1 s');
        expect(formatDuration(400)).toBe('< 1 s');
        expect(formatDuration(850)).toBe('1 s');
        expect(formatDuration(2400)).toBe('2 s');
        expect(formatDuration(95000)).toBe('1 min 35 s');
    });
});

describe('the time bar of the job page', () => {
    const at = (time: string) => time as NonNullable<ResultView['started_at']>;

    it('names the claim, the engine time and the rest when the run has a recorded claim', () => {
        const html = render(workunit(), [
            run({
                started_at: at('2026-10-13 02:27:30.000000Z'),
                received_at: at('2026-10-13 02:27:40.000000Z'),
                duration_ms: 7000,
            }),
        ]);

        expect(html).toContain('Claimed');
        expect(html).toContain('Engine ran 7 s');
        expect(html).toContain('Transfers and overhead 3 s');
        expect(html).toContain('claim to arrival');
    });

    it('says the engine time was not reported, and draws no split', () => {
        const html = render(workunit(), [
            run({
                started_at: at('2026-10-13 02:27:30.000000Z'),
                received_at: at('2026-10-13 02:27:40.000000Z'),
                duration_ms: null,
            }),
        ]);

        expect(html).toContain('The node did not report how long the engine ran.');
        expect(html).not.toContain('Engine ran');
    });

    it('keeps the two stored times for a run with no recorded claim', () => {
        const html = render(workunit(), [run({ started_at: null, duration_ms: null })]);

        expect(html).toContain('two stored times');
        expect(html).not.toContain('claim to arrival');
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
