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

import { describe, expect, it } from 'vitest';
import type { AuthenticatedCaller } from './account-operations.js';
import {
    GridJobsRefusedError,
    readBatch,
    readBatches,
    readResult,
    readResultsByOutcome,
    readResultsOfWorkunit,
    readWorkunit,
    readWorkunitsOfBatch,
    toResultView,
    toWorkunitView,
} from './grid-jobs.js';

/**
 * The jobs reads: the subject and body each one sends, and how a stored unset
 * value reaches a screen.
 *
 * The compute service states an unset id as the nil uuid, an unset text as an
 * empty string and an unset instant as the epoch. A view states none of them.
 */

const NIL = '00000000-0000-0000-0000-000000000000';
const EPOCH = '1970-01-01 00:00:00.000000Z';
const TENANT = '11111111-1111-4111-8111-111111111111';
const PARTY = '99999999-9999-4999-8999-999999999999';
const BATCH = '22222222-2222-4222-8222-222222222222';
const WORKUNIT = '33333333-3333-4333-8333-333333333333';
const RESULT = '44444444-4444-4444-8444-444444444444';
const HOST = '55555555-5555-4555-8555-555555555555';
const OK = { outcome: 'ok', code: '', message: '', fields: [] };

const audit = {
    version: 1,
    tenant_id: TENANT,
    modified_by: 'compute_service',
    performed_by: 'compute_service',
    change_reason_code: 'new',
    change_commentary: '',
    recorded_at: '2026-10-13 02:27:27.000000Z',
};

function wireBatch(overrides: Record<string, unknown> = {}): Record<string, unknown> {
    return {
        ...audit,
        id: BATCH,
        external_ref: 'soak-1013031448-9-p3',
        party_id: null,
        status: 'closed',
        ...overrides,
    };
}

function wireWorkunit(overrides: Record<string, unknown> = {}): Record<string, unknown> {
    return {
        ...audit,
        id: WORKUNIT,
        party_id: null,
        batch_id: BATCH,
        app_version_id: '66666666-6666-4666-8666-666666666666',
        input_uri: '/storage/input/a.tar.gz',
        config_uri: '',
        priority: 1,
        target_redundancy: 1,
        canonical_result_id: NIL,
        ...overrides,
    };
}

function wireResult(overrides: Record<string, unknown> = {}): Record<string, unknown> {
    return {
        ...audit,
        id: RESULT,
        party_id: null,
        workunit_id: WORKUNIT,
        host_id: HOST,
        server_state: 5,
        outcome: 1,
        output_uri: '/storage/output/a.tar.gz',
        error_message: '',
        received_at: '2026-10-13 02:27:53.000000Z',
        ...overrides,
    };
}

/** A caller that records what it was asked and answers from the subject. */
function callerAnswering(answers: Record<string, unknown>) {
    const calls: { subject: string; body: unknown }[] = [];
    const caller: AuthenticatedCaller = {
        async callAuthenticated(subject, body, schema) {
            calls.push({ subject, body });
            return schema.parse(answers[subject]);
        },
    };
    return { caller, calls };
}

describe('readBatches', () => {
    it('asks for a page in the default order, which the model states as newest first', async () => {
        const { caller, calls } = callerAnswering({
            'compute.v1.batches.list': { result: OK, batches: [wireBatch()], total: 41 },
        });

        const page = await readBatches(caller, { offset: 20, limit: 10 });

        expect(calls).toHaveLength(1);
        expect(calls[0]?.body).toMatchObject({
            offset: 20,
            limit: 10,
            order: { field: '', descending: false },
            filter: { status: null, party_id: null, search: null },
        });
        expect(page.total).toBe(41);
        expect(page.batches.map((batch) => batch.id)).toEqual([BATCH]);
    });

    it('sends the status, the party and the reference the person asked for', async () => {
        const { caller, calls } = callerAnswering({
            'compute.v1.batches.list': { result: OK, batches: [], total: 0 },
        });

        await readBatches(
            caller,
            { offset: 0, limit: 25 },
            { status: 'closed', partyId: PARTY, find: 'soak' },
        );

        expect(calls[0]?.body).toMatchObject({
            filter: { status: 'closed', party_id: PARTY, search: 'soak' },
        });
    });

    it('raises the outcome the service stated when it refuses the read', async () => {
        const { caller } = callerAnswering({
            'compute.v1.batches.list': {
                result: { ...OK, outcome: 'denied', message: 'No read code.' },
            },
        });

        const failure = await readBatches(caller, { offset: 0, limit: 10 }).catch(
            (error: unknown) => error,
        );

        expect(failure).toBeInstanceOf(GridJobsRefusedError);
        expect((failure as GridJobsRefusedError).outcome).toBe('denied');
        expect((failure as GridJobsRefusedError).message).toBe('No read code.');
    });
});

describe('readBatch', () => {
    it('reads one batch through the id filter, because the keyed read names the reference', async () => {
        const { caller, calls } = callerAnswering({
            'compute.v1.batches.list': { result: OK, batches: [wireBatch()], total: 1 },
        });

        const batch = await readBatch(caller, BATCH);

        expect(calls[0]?.body).toMatchObject({ limit: 1, filter: { id_one_of: [BATCH] } });
        expect(batch?.id).toBe(BATCH);
    });

    it('states nothing when no row is visible', async () => {
        const { caller } = callerAnswering({
            'compute.v1.batches.list': { result: OK, batches: [], total: 0 },
        });

        expect(await readBatch(caller, BATCH)).toBeNull();
    });
});

describe('the workunits of a batch and the results of a workunit', () => {
    it('sends the parent id with the page', async () => {
        const { caller, calls } = callerAnswering({
            'compute.v1.workunits.list_by_batch_id': {
                result: OK,
                workunits: [wireWorkunit()],
                total: 12,
            },
            'compute.v1.results.list_by_workunit_id': {
                result: OK,
                results: [wireResult()],
                total: 1,
            },
        });

        const units = await readWorkunitsOfBatch(caller, BATCH, { offset: 5, limit: 5 });
        const runs = await readResultsOfWorkunit(caller, WORKUNIT, { offset: 0, limit: 25 });

        expect(calls[0]?.body).toMatchObject({ batch_id: BATCH, offset: 5, limit: 5 });
        expect(calls[1]?.body).toMatchObject({ workunit_id: WORKUNIT, offset: 0, limit: 25 });
        expect([units.total, runs.total]).toEqual([12, 1]);
    });
});

describe('readResultsByOutcome', () => {
    it('asks for the runs with the outcome and states the page', async () => {
        const { caller, calls } = callerAnswering({
            'compute.v1.results.list': {
                result: OK,
                results: [wireResult({ outcome: 3 })],
                total: 2,
            },
        });

        const failed = await readResultsByOutcome(caller, 3, { offset: 10, limit: 5 });

        expect(calls).toHaveLength(1);
        expect(calls[0]?.body).toMatchObject({
            offset: 10,
            limit: 5,
            order: { field: 'recorded_at', descending: true },
            filter: { outcome: 3, workunit_id: null },
        });
        expect(failed.total).toBe(2);
        expect(failed.results.map((run) => run.outcome)).toEqual([3]);
    });
});

describe('readWorkunit and readResult', () => {
    it('state nothing for a row the service reports missing', async () => {
        const { caller } = callerAnswering({
            'compute.v1.workunits.get': { result: { ...OK, outcome: 'missing' } },
            'compute.v1.results.get': { result: { ...OK, outcome: 'missing' } },
        });

        expect(await readWorkunit(caller, WORKUNIT)).toBeNull();
        expect(await readResult(caller, RESULT)).toBeNull();
    });

    it('return the row the service states', async () => {
        const { caller, calls } = callerAnswering({
            'compute.v1.workunits.get': { result: OK, workunit: wireWorkunit() },
            'compute.v1.results.get': { result: OK, result_value: wireResult() },
        });

        expect((await readWorkunit(caller, WORKUNIT))?.id).toBe(WORKUNIT);
        expect((await readResult(caller, RESULT))?.id).toBe(RESULT);
        expect(calls.map((call) => call.body)).toEqual([
            { key: { id: WORKUNIT } },
            { key: { id: RESULT } },
        ]);
    });
});

describe('the views', () => {
    it('state an unset workunit id and unset text as null', async () => {
        const { caller } = callerAnswering({
            'compute.v1.workunits.get': { result: OK, workunit: wireWorkunit() },
        });
        const workunit = await readWorkunit(caller, WORKUNIT);

        const view = toWorkunitView(workunit!);

        expect(view.canonical_result_id).toBeNull();
        expect(view.config_uri).toBeNull();
        expect(view.input_uri).toBe('/storage/input/a.tar.gz');
    });

    it('state a run in flight with no node, no output and no instant received', async () => {
        const { caller } = callerAnswering({
            'compute.v1.results.get': {
                result: OK,
                result_value: wireResult({
                    host_id: NIL,
                    server_state: 4,
                    outcome: 0,
                    output_uri: '',
                    received_at: EPOCH,
                }),
            },
        });
        const result = await readResult(caller, RESULT);

        const view = toResultView(result!, true);

        expect(view).toMatchObject({
            host_id: null,
            host_hidden: false,
            output_uri: null,
            received_at: null,
            outcome: 0,
        });
    });

    it('state when a node took the run and how long the engine ran', async () => {
        const { caller } = callerAnswering({
            'compute.v1.results.get': {
                result: OK,
                result_value: wireResult({
                    started_at: '2026-10-13 02:27:40.000000Z',
                    duration_ms: 9500,
                }),
            },
        });
        const result = await readResult(caller, RESULT);

        expect(toResultView(result!, true)).toMatchObject({
            started_at: '2026-10-13 02:27:40.000000Z',
            duration_ms: 9500,
        });
    });

    it('read an unset start and an unreported duration as unknown, and never as zero', async () => {
        const { caller } = callerAnswering({
            'compute.v1.results.get': {
                result: OK,
                result_value: wireResult({ started_at: EPOCH, duration_ms: null }),
            },
        });
        const result = await readResult(caller, RESULT);

        expect(toResultView(result!, true)).toMatchObject({ started_at: null, duration_ms: null });
    });

    it('read a duration that is not a whole number of milliseconds as unknown', async () => {
        const { caller } = callerAnswering({
            'compute.v1.results.get': {
                result: OK,
                result_value: wireResult({ duration_ms: -5 }),
            },
        });
        const result = await readResult(caller, RESULT);

        expect(toResultView(result!, true).duration_ms).toBeNull();
    });

    it('read a run from a service that predates the two columns as unknown', async () => {
        const { caller } = callerAnswering({
            'compute.v1.results.get': { result: OK, result_value: wireResult() },
        });
        const result = await readResult(caller, RESULT);

        expect(toResultView(result!, true)).toMatchObject({ started_at: null, duration_ms: null });
    });

    it('withhold the node from a reader who may not see it, and say so', async () => {
        const { caller } = callerAnswering({
            'compute.v1.results.get': { result: OK, result_value: wireResult() },
        });
        const result = await readResult(caller, RESULT);

        const view = toResultView(result!, false);

        expect(view.host_id).toBeNull();
        expect(view.host_hidden).toBe(true);
    });

    it('keep the node for a reader who may see it', async () => {
        const { caller } = callerAnswering({
            'compute.v1.results.get': { result: OK, result_value: wireResult() },
        });
        const result = await readResult(caller, RESULT);

        expect(toResultView(result!, true)).toMatchObject({ host_id: HOST, host_hidden: false });
    });
});
