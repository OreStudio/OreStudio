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
import type { ReactNode } from 'react';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import type {
    BatchPage,
    BatchView,
    ResultPage,
    ResultView,
    WorkunitPage,
    WorkunitView,
} from '@ores/wire-protocol/browser';
import { ApiFailure } from '../api/transport.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { GRID_JOBS_QUERY_KEY, GridJobsTab, ReadFailure, parseSelection } from './GridJobs.js';

/**
 * The Jobs tab, as a reader sees it.
 *
 * What is checked is what the tab promises: a page of batches in the order the
 * server gave, one page of children for each row that is open, the fields of
 * the selected record under the models' own headers, and no node name for a
 * role that may not read it. The reads are seeded, so no request is made.
 */

const BATCH_ID = 'b0000000-0000-4000-8000-000000000001';
const OTHER_BATCH_ID = 'b0000000-0000-4000-8000-000000000002';
const WORKUNIT_ID = 'a0000000-0000-4000-8000-000000000001';
const RESULT_ID = 'c0000000-0000-4000-8000-000000000001';
const FAILED_RESULT_ID = 'c0000000-0000-4000-8000-000000000002';
const PARTY_ID = 'd1111111-0000-4000-8000-000000000001';
const HOST_ID = 'e0000000-0000-4000-8000-000000000001';
const APP_VERSION_ID = 'f0000000-0000-4000-8000-000000000001';

function batch(id: string, ref: string, status = 'closed'): BatchView {
    return {
        id,
        external_ref: ref,
        party_id: PARTY_ID,
        status,
        modified_by: 'tenant_admin',
        recorded_at: '2026-10-13 02:27:27.000000Z' as BatchView['recorded_at'],
    };
}

function workunit(id: string, overrides: Partial<WorkunitView> = {}): WorkunitView {
    return {
        id,
        batch_id: BATCH_ID,
        app_version_id: APP_VERSION_ID,
        input_uri: '/api/v1/storage/ores/compute/input/batch.tar.gz',
        config_uri: null,
        priority: 1,
        target_redundancy: 1,
        canonical_result_id: null,
        modified_by: 'tenant_admin',
        recorded_at: '2026-10-13 02:27:28.000000Z' as WorkunitView['recorded_at'],
        ...overrides,
    };
}

function result(id: string, overrides: Partial<ResultView> = {}): ResultView {
    return {
        id,
        workunit_id: WORKUNIT_ID,
        host_id: HOST_ID,
        host_hidden: false,
        server_state: 5,
        outcome: 1,
        output_uri: '/api/v1/storage/ores/compute/output/result.tar.gz',
        error_message: null,
        received_at: '2026-10-13 02:28:00.000000Z' as NonNullable<ResultView['received_at']>,
        modified_by: 'ores_compute_service',
        recorded_at: '2026-10-13 02:28:00.000000Z' as ResultView['recorded_at'],
        ...overrides,
    };
}

function pageOf<Row>(rows: readonly Row[], total = rows.length, offset = 0, limit = 15) {
    return {
        read_at: '2026-10-13 03:00:00.000000Z' as BatchPage['read_at'],
        rows: [...rows],
        total,
        offset,
        limit,
    };
}

interface Seeds {
    readonly batches?: BatchPage;
    readonly batchOffset?: number;
    readonly byOutcome?: { readonly outcome: number; readonly page: ResultPage };
    readonly filter?: { find?: string; status?: string; party?: string };
    readonly parties?: readonly { id: string; name: string }[];
    readonly workunits?: WorkunitPage;
    readonly results?: ResultPage;
    readonly record?: readonly [kind: string, id: string, value: unknown];
}

function render(
    path: string,
    seeds: Seeds,
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
    if (seeds.batches !== undefined) {
        client.setQueryData(
            [
                GRID_JOBS_QUERY_KEY,
                'batches',
                seeds.batchOffset ?? 0,
                15,
                seeds.filter?.find ?? '',
                seeds.filter?.status ?? '',
                seeds.filter?.party ?? '',
            ],
            seeds.batches,
        );
    }
    if (seeds.byOutcome !== undefined) {
        client.setQueryData(
            [GRID_JOBS_QUERY_KEY, 'by-outcome', seeds.byOutcome.outcome, 0, 15],
            seeds.byOutcome.page,
        );
    }
    if (seeds.workunits !== undefined) {
        client.setQueryData([GRID_JOBS_QUERY_KEY, 'workunits', BATCH_ID, 0], seeds.workunits);
    }
    if (seeds.results !== undefined) {
        client.setQueryData([GRID_JOBS_QUERY_KEY, 'results', WORKUNIT_ID, 0], seeds.results);
    }
    if (seeds.record !== undefined) {
        client.setQueryData([GRID_JOBS_QUERY_KEY, 'record', seeds.record[0], seeds.record[1]], {
            kind: seeds.record[0],
            data: seeds.record[2],
        });
    }
    const element: ReactNode = <GridJobsTab parties={seeds.parties ?? []} />;
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[path]}>{element}</MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

const TWO_BATCHES = pageOf([batch(OTHER_BATCH_ID, 'order-f'), batch(BATCH_ID, 'order-e')]);

describe('the Jobs tab', () => {
    it('lists the batches in the order the server gave, with their status', () => {
        const html = render('/?tab=jobs', { batches: TWO_BATCHES });

        expect(html).toContain('Batches, workunits and results');
        expect(html.indexOf('order-f')).toBeLessThan(html.indexOf('order-e'));
        expect(html).toContain('>closed<');
        expect(html).toContain('Batches 1 to 2 of 2');
    });

    it('says so when no batch has been recorded', () => {
        const html = render('/?tab=jobs', { batches: pageOf([]) });

        expect(html).toContain('No batch has been recorded yet.');
        expect(html).not.toContain('Batches 1 to');
    });

    it('nests the children of an open row inside its list item', () => {
        const html = render(`/?tab=jobs&open=${BATCH_ID}`, {
            batches: TWO_BATCHES,
            workunits: pageOf([workunit(WORKUNIT_ID)], 1, 0, 5),
        });

        expect(html).not.toMatch(/<ul[^>]*><div/);
        expect(html).toMatch(/<\/button><ul[^>]*>.*<\/ul><\/li>/);
    });

    it('offers the first page again when the address names a page past the last', () => {
        const html = render('/?tab=jobs&page=99', {
            batches: pageOf([], 2, 1470),
            batchOffset: 1470,
        });

        expect(html).toContain('This page is past the last one.');
        expect(html).toContain('Back to the first page');
        expect(html).not.toContain('No batch has been recorded yet.');
    });

    it('offers a search, the statuses and no party choice for one party', () => {
        const html = render('/?tab=jobs', { batches: TWO_BATCHES });

        expect(html).toContain('part of a batch reference');
        for (const status of ['open', 'dispatched', 'processing', 'assimilating', 'closed']) {
            expect(html).toContain(`<option value="${status}">`);
        }
        expect(html).not.toContain('>Party<');
    });

    it('offers a choice of parties to a session that acts for more than one', () => {
        const html = render('/?tab=jobs', {
            batches: TWO_BATCHES,
            parties: [
                { id: PARTY_ID, name: 'Acme Corporation Plc' },
                { id: 'd2222222-0000-4000-8000-000000000002', name: 'Acme UK Ltd' },
            ],
        });

        expect(html).toContain('>Party<');
        expect(html).toContain(`<option value="${PARTY_ID}">Acme Corporation Plc</option>`);
    });

    it('reads the page the address filters and keeps the choice in the controls', () => {
        const html = render('/?tab=jobs&status=closed&find=order', {
            batches: pageOf([batch(BATCH_ID, 'order-e')]),
            filter: { status: 'closed', find: 'order' },
        });

        expect(html).toContain('order-e');
        expect(html).toContain('value="order"');
        expect(html).toMatch(/<option value="closed" selected="">/);
    });

    it('says no batch matches when a filter finds none, and not that none was recorded', () => {
        const html = render('/?tab=jobs&status=closed', {
            batches: pageOf([]),
            filter: { status: 'closed' },
        });

        expect(html).toContain('No batch matches. Clear the filters.');
        expect(html).not.toContain('No batch has been recorded yet.');
    });

    it('offers the outcomes a person may ask for', () => {
        const html = render('/?tab=jobs', { batches: TWO_BATCHES });

        expect(html).toContain('Outcome');
        for (const code of [1, 3, 4, 0]) {
            expect(html).toContain(`<option value="${code}">`);
        }
    });

    it('lists the runs that ended with the outcome, each leading to its workunit', () => {
        const html = render('/?tab=jobs&outcome=3', {
            byOutcome: {
                outcome: 3,
                page: pageOf(
                    [result(FAILED_RESULT_ID, { outcome: 3 }), result(RESULT_ID, { outcome: 3 })],
                    7,
                ),
            },
        });

        expect(html).toContain('Runs with the outcome');
        expect(html).toContain(FAILED_RESULT_ID.slice(0, 8));
        expect(html).toContain(`of workunit ${WORKUNIT_ID.slice(0, 8)}`);
        expect(html).toContain('>ClientError<');
        expect(html).toContain('Results 1 to 2 of 7');
        expect(html).not.toContain('Batches, workunits and results');
    });

    it('turns off the batch filters while the outcome narrows the runs', () => {
        const html = render('/?tab=jobs&outcome=3', {
            byOutcome: { outcome: 3, page: pageOf([result(RESULT_ID, { outcome: 3 })]) },
        });

        expect(html).toMatch(/<input[^>]*type="search"[^>]*disabled/);
    });

    it('says no run ended with the outcome when the answer is empty', () => {
        const html = render('/?tab=jobs&outcome=4', {
            byOutcome: { outcome: 4, page: pageOf([]) },
        });

        expect(html).toContain('No run ended with this outcome.');
    });

    it('ignores an outcome that is not a code the tab knows', () => {
        const html = render('/?tab=jobs&outcome=9', { batches: TWO_BATCHES });

        expect(html).toContain('Batches, workunits and results');
    });

    it('prompts for a selection until one is made', () => {
        const html = render('/?tab=jobs', { batches: TWO_BATCHES });

        expect(html).toContain('Select a batch, a workunit or a result to read its details.');
    });

    it('opens a batch to one page of its workunits and pages them', () => {
        const html = render(`/?tab=jobs&open=${BATCH_ID}`, {
            batches: TWO_BATCHES,
            workunits: pageOf([workunit(WORKUNIT_ID, { priority: 2 })], 12, 0, 5),
        });

        expect(html).toContain(WORKUNIT_ID.slice(0, 8));
        expect(html).toContain('priority 2 · target redundancy 1');
        expect(html).toContain('Workunits 1 to 1 of 12');
    });

    it('opens a workunit to its results with their state and outcome', () => {
        const html = render(`/?tab=jobs&open=${BATCH_ID},${WORKUNIT_ID}`, {
            batches: TWO_BATCHES,
            workunits: pageOf([workunit(WORKUNIT_ID)], 1, 0, 5),
            results: pageOf(
                [result(RESULT_ID), result(FAILED_RESULT_ID, { outcome: 3, output_uri: null })],
                2,
                0,
                5,
            ),
        });

        expect(html).toContain(RESULT_ID.slice(0, 8));
        expect(html).toContain('>Done<');
        expect(html).toContain('>Success<');
        expect(html).toContain('>ClientError<');
    });

    it('draws the fields of a selected batch under the model headers', () => {
        const html = render(`/?tab=jobs&sel=batch:${BATCH_ID}`, {
            batches: TWO_BATCHES,
            record: ['batch', BATCH_ID, batch(BATCH_ID, 'order-e')],
        });

        for (const header of [
            'ID',
            'External Ref',
            'Party ID',
            'Status',
            'Modified By',
            'Recorded At',
        ]) {
            expect(html).toContain(header);
        }
        expect(html).toContain('order-e');
        expect(html).toContain('Read only');
    });

    it('links a workunit to its batch and its canonical result', () => {
        const html = render(`/?tab=jobs&sel=workunit:${WORKUNIT_ID}`, {
            batches: TWO_BATCHES,
            record: [
                'workunit',
                WORKUNIT_ID,
                workunit(WORKUNIT_ID, { canonical_result_id: RESULT_ID }),
            ],
        });

        expect(html).toContain(`sel=batch%3A${BATCH_ID}`);
        expect(html).toContain(`sel=result%3A${RESULT_ID}`);
        expect(html).toContain('Target Redundancy');
        expect(html).toContain('Canonical Result ID');
    });

    it('leads from a workunit or a result to the job page and from a workunit to its app version', () => {
        const workunitHtml = render(`/?tab=jobs&sel=workunit:${WORKUNIT_ID}`, {
            batches: TWO_BATCHES,
            record: ['workunit', WORKUNIT_ID, workunit(WORKUNIT_ID)],
        });
        const resultHtml = render(`/?tab=jobs&sel=result:${RESULT_ID}`, {
            batches: TWO_BATCHES,
            record: ['result', RESULT_ID, result(RESULT_ID)],
        });
        const batchHtml = render(`/?tab=jobs&sel=batch:${BATCH_ID}`, {
            batches: TWO_BATCHES,
            record: ['batch', BATCH_ID, batch(BATCH_ID, 'order-e')],
        });

        expect(workunitHtml).toContain(`href="/operations/grid/jobs/${WORKUNIT_ID}"`);
        expect(workunitHtml).toContain(`href="/operations/grid/app-versions/${APP_VERSION_ID}"`);
        expect(resultHtml).toContain(`href="/operations/grid/jobs/${WORKUNIT_ID}"`);
        expect(workunitHtml).toContain('Follow this job');
        expect(batchHtml).not.toContain('Follow this job');
    });

    it('links the stored input of a workunit and the stored output of a run to the download routes', () => {
        const workunitHtml = render(`/?tab=jobs&sel=workunit:${WORKUNIT_ID}`, {
            batches: TWO_BATCHES,
            record: ['workunit', WORKUNIT_ID, workunit(WORKUNIT_ID)],
        });
        const resultHtml = render(`/?tab=jobs&sel=result:${RESULT_ID}`, {
            batches: TWO_BATCHES,
            record: ['result', RESULT_ID, result(RESULT_ID)],
        });
        const noOutput = render(`/?tab=jobs&sel=result:${RESULT_ID}`, {
            batches: TWO_BATCHES,
            record: ['result', RESULT_ID, result(RESULT_ID, { output_uri: null })],
        });

        expect(workunitHtml).toContain(
            `href="/api/operations/grid/jobs/workunits/${WORKUNIT_ID}/input" download=""`,
        );
        expect(resultHtml).toContain(
            `href="/api/operations/grid/jobs/results/${RESULT_ID}/output" download=""`,
        );
        expect(noOutput).not.toContain('/output"');
    });

    it('shows the stored address as text to a person without the storage read code', () => {
        const html = render(
            `/?tab=jobs&sel=workunit:${WORKUNIT_ID}`,
            {
                batches: TWO_BATCHES,
                record: ['workunit', WORKUNIT_ID, workunit(WORKUNIT_ID)],
            },
            [],
        );

        expect(html).toContain('/api/v1/storage/ores/compute/input/batch.tar.gz');
        expect(html).not.toContain('/api/operations/grid/jobs/workunits/');
    });

    it('states none for an unset field instead of an empty value', () => {
        const html = render(`/?tab=jobs&sel=workunit:${WORKUNIT_ID}`, {
            batches: TWO_BATCHES,
            record: ['workunit', WORKUNIT_ID, workunit(WORKUNIT_ID)],
        });

        expect(html).toContain('>none<');
    });

    it('links the node of a result to an operator and hides it from other roles', () => {
        const operator = render(`/?tab=jobs&sel=result:${RESULT_ID}`, {
            batches: TWO_BATCHES,
            record: ['result', RESULT_ID, result(RESULT_ID)],
        });
        const other = render(`/?tab=jobs&sel=result:${RESULT_ID}`, {
            batches: TWO_BATCHES,
            record: ['result', RESULT_ID, result(RESULT_ID, { host_id: null, host_hidden: true })],
        });

        expect(operator).toContain(`/operations/grid/nodes/${HOST_ID}`);
        expect(other).toContain('hidden for your role');
        expect(other).not.toContain(HOST_ID);
    });

    it('says an outcome is not set where a run has not finished', () => {
        const html = render(`/?tab=jobs&sel=result:${RESULT_ID}`, {
            batches: TWO_BATCHES,
            record: [
                'result',
                RESULT_ID,
                result(RESULT_ID, {
                    server_state: 4,
                    outcome: 0,
                    host_id: null,
                    output_uri: null,
                    received_at: null,
                }),
            ],
        });

        expect(html).toContain('>InProgress<');
        expect(html).toContain('not set (0)');
    });
});

describe('the selection in the address', () => {
    it('names a kind and an id', () => {
        expect(parseSelection(`result:${RESULT_ID}`)).toEqual({ kind: 'result', id: RESULT_ID });
        expect(parseSelection(`batch:${BATCH_ID}`)).toEqual({ kind: 'batch', id: BATCH_ID });
    });

    it('is nothing when the address names none or names it badly', () => {
        expect(parseSelection(null)).toBeNull();
        expect(parseSelection('')).toBeNull();
        expect(parseSelection('batch')).toBeNull();
        expect(parseSelection('batch:')).toBeNull();
        expect(parseSelection(`host:${HOST_ID}`)).toBeNull();
    });
});

describe('a failed read', () => {
    function renderFailure(error: unknown): string {
        return renderToStaticMarkup(
            <TranslationProvider>
                <ReadFailure error={error} onRetry={() => undefined} />
            </TranslationProvider>,
        );
    }

    it('says the role cannot read jobs when the server refuses the read', () => {
        const html = renderFailure(
            new ApiFailure(403, { code: 'forbidden', message: 'You do not have access to this.' }),
        );

        expect(html).toContain('Your role cannot read jobs.');
        expect(html).not.toContain('Try again');
    });

    it('states the failure and offers to try again for any other failure', () => {
        const html = renderFailure(new Error('The compute service did not answer.'));

        expect(html).toContain('The jobs could not be read. The compute service did not answer.');
        expect(html).toContain('Try again');
    });
});
