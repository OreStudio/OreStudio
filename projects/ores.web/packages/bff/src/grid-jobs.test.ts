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

import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';
import type { OresClient, SessionMode } from '@ores/wire-protocol';
import type { Config } from './config.js';
import { buildServer, sessionCookieName } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

/**
 * The jobs routes: one generated read each, the page the person asked for, and
 * what a refusal and an unset value become.
 *
 * What is checked is what the BFF decides. The compute service decides which
 * rows a session may see, so these tests state only that each route asks the
 * session's own client once, with the page and the parent id, and answers with
 * the status and the shape the screen reads.
 */

const config: Config = {
    port: 0,
    host: '127.0.0.1',
    logLevel: 'silent',
    session: { ttlSeconds: 3600, cookieSecure: false },
    allowedOrigins: [],
    loginAttemptsPerMinute: 100,
};

const ENVIRONMENT_ID = 'eager_maxwell';
const SESSION_COOKIE = sessionCookieName(ENVIRONMENT_ID);

function siteConfiguration(): ReturnType<typeof loadSiteConfiguration> {
    const path = resolve(
        dirname(fileURLToPath(import.meta.url)),
        '../../../config/environments.json',
    );
    return loadSiteConfiguration({
        environment: { [SITE_CONFIG_VARIABLE]: path },
        environmentId: ENVIRONMENT_ID,
    });
}

const SYSTEM_TENANT = 'ffffffff-ffff-4fff-8fff-ffffffffffff';
const ACCOUNT = '11111111-1111-4111-8111-111111111111';
const BATCH = '22222222-2222-4222-8222-222222222222';
const WORKUNIT = '33333333-3333-4333-8333-333333333333';
const RESULT = '44444444-4444-4444-8444-444444444444';
const HOST = '55555555-5555-4555-8555-555555555555';
const NIL = '00000000-0000-0000-0000-000000000000';
const OK = { outcome: 'ok', code: '', message: '', fields: [] };

const audit = {
    version: 1,
    tenant_id: SYSTEM_TENANT,
    modified_by: 'compute_service',
    performed_by: 'compute_service',
    change_reason_code: 'new',
    change_commentary: '',
    recorded_at: '2026-10-13 02:27:27.000000Z',
};

const wireBatch = {
    ...audit,
    id: BATCH,
    external_ref: 'soak-1013031448-9-p3',
    party_id: null,
    status: 'closed',
};

const wireWorkunit = {
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
};

const wireResult = {
    ...audit,
    id: RESULT,
    party_id: null,
    workunit_id: WORKUNIT,
    host_id: HOST,
    server_state: 5,
    outcome: 3,
    output_uri: '',
    error_message: 'engine exited with code 3',
    received_at: '2026-10-13 02:27:53.000000Z',
};

type Answers = Readonly<Record<string, unknown>>;

const ANSWERS: Answers = {
    'compute.v1.batches.list': { result: OK, batches: [wireBatch], total: 41 },
    'compute.v1.workunits.list_by_batch_id': { result: OK, workunits: [wireWorkunit], total: 12 },
    'compute.v1.workunits.get': { result: OK, workunit: wireWorkunit },
    'compute.v1.results.list_by_workunit_id': { result: OK, results: [wireResult], total: 1 },
    'compute.v1.results.get': { result: OK, result_value: wireResult },
};

function buildTestServer(mode: SessionMode, answers: Answers = ANSWERS) {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const calls: { subject: string; body: unknown }[] = [];
    const client = {
        async callAuthenticated(
            subject: string,
            body: unknown,
            schema: { parse: (value: unknown) => unknown },
        ): Promise<unknown> {
            calls.push({ subject, body });
            if (!(subject in answers)) {
                throw new Error(`unexpected call ${subject}`);
            }
            return schema.parse(answers[subject]);
        },
        async close(): Promise<void> {
            return undefined;
        },
    } as unknown as OresClient;
    const session = sessions.create({
        client,
        session: null,
        username: 'sysadmin',
        email: 'sysadmin@acme.example',
        accountId: ACCOUNT,
        tenantId: SYSTEM_TENANT,
        tenantName: 'System',
        mode,
        version: 'v0.0.25 (test)',
        availableParties: [
            {
                id: SYSTEM_TENANT,
                name: 'System',
                partyCategory: 'Operational',
                businessCenterCode: 'GBLO',
            },
        ],
        accessLifetimeSeconds: 1800,
        passwordResetRequired: false,
        sessionId: '77777777-7777-4777-8777-777777777777',
    });
    const server = buildServer({
        config,
        site: siteConfiguration(),
        sessions,
        createClient: () => ({ client, connect: async () => undefined }),
    });
    return { server, cookies: { [SESSION_COOKIE]: session.id }, calls };
}

async function get(mode: SessionMode, url: string, answers: Answers = ANSWERS, anonymous = false) {
    const { server, cookies, calls } = buildTestServer(mode, answers);
    const response = await server.inject({
        method: 'GET',
        url,
        ...(anonymous ? {} : { cookies }),
    });
    await server.close();
    return { response, calls };
}

describe('GET /api/operations/grid/jobs/batches', () => {
    it('serves a page of batches with the total and the page it was asked for', async () => {
        const { response, calls } = await get(
            'tenant-administration',
            '/api/operations/grid/jobs/batches?offset=20&limit=10',
        );

        expect(response.statusCode).toBe(200);
        const body = response.json<{
            rows: { id: string; external_ref: string; party_id: string | null }[];
            total: number;
            offset: number;
            limit: number;
        }>();
        expect(body.rows).toEqual([
            expect.objectContaining({ id: BATCH, external_ref: 'soak-1013031448-9-p3' }),
        ]);
        expect([body.total, body.offset, body.limit]).toEqual([41, 20, 10]);
        expect(calls.map((call) => call.subject)).toEqual(['compute.v1.batches.list']);
        expect(calls[0]?.body).toMatchObject({
            offset: 20,
            limit: 10,
            order: { field: '', descending: false },
        });
    });

    it('reads the first 25 when the address names no page', async () => {
        const { response } = await get(
            'tenant-administration',
            '/api/operations/grid/jobs/batches',
        );

        expect(response.json<{ offset: number; limit: number }>()).toMatchObject({
            offset: 0,
            limit: 25,
        });
    });

    it.each([
        ['a limit of zero', '?limit=0'],
        ['a limit above the most a page holds', '?limit=201'],
        ['a negative offset', '?offset=-1'],
        ['a field the read does not take', '?status=closed'],
    ])('refuses %s before any call goes out', async (_name, query) => {
        const { response, calls } = await get(
            'tenant-administration',
            `/api/operations/grid/jobs/batches${query}`,
        );

        expect(response.statusCode).toBe(400);
        expect(calls).toEqual([]);
    });

    it('answers 403 when the compute service denies the read', async () => {
        const { response } = await get(
            'tenant-administration',
            '/api/operations/grid/jobs/batches',
            {
                'compute.v1.batches.list': {
                    result: { ...OK, outcome: 'denied', message: 'No read code.' },
                },
            },
        );

        expect(response.statusCode).toBe(403);
    });

    it('refuses an anonymous caller', async () => {
        const { response, calls } = await get(
            'tenant-administration',
            '/api/operations/grid/jobs/batches',
            ANSWERS,
            true,
        );

        expect(response.statusCode).toBe(401);
        expect(calls).toEqual([]);
    });
});

describe('GET /api/operations/grid/jobs/batches/:id', () => {
    it('serves one batch', async () => {
        const { response, calls } = await get(
            'tenant-administration',
            `/api/operations/grid/jobs/batches/${BATCH}`,
        );

        expect(response.statusCode).toBe(200);
        expect(response.json<{ id: string }>().id).toBe(BATCH);
        expect(calls[0]?.body).toMatchObject({ filter: { id_one_of: [BATCH] } });
    });

    it('answers 404 when the session sees no such batch', async () => {
        const { response } = await get(
            'tenant-administration',
            `/api/operations/grid/jobs/batches/${BATCH}`,
            { 'compute.v1.batches.list': { result: OK, batches: [], total: 0 } },
        );

        expect(response.statusCode).toBe(404);
    });

    it('refuses an id that is not a uuid before any call goes out', async () => {
        const { response, calls } = await get(
            'tenant-administration',
            '/api/operations/grid/jobs/batches/not-an-id',
        );

        expect(response.statusCode).toBe(400);
        expect(calls).toEqual([]);
    });
});

describe('the workunits of a batch', () => {
    it('serves a page of the workunits of the batch, with an unset canonical result as null', async () => {
        const { response, calls } = await get(
            'tenant-administration',
            `/api/operations/grid/jobs/batches/${BATCH}/workunits?limit=5`,
        );

        expect(response.statusCode).toBe(200);
        const body = response.json<{
            rows: { id: string; canonical_result_id: string | null; config_uri: string | null }[];
            total: number;
        }>();
        expect(body.rows[0]).toMatchObject({
            id: WORKUNIT,
            canonical_result_id: null,
            config_uri: null,
        });
        expect(body.total).toBe(12);
        expect(calls[0]?.body).toMatchObject({ batch_id: BATCH, offset: 0, limit: 5 });
    });

    it('serves one workunit, and answers 404 when the service reports it missing', async () => {
        const found = await get(
            'tenant-administration',
            `/api/operations/grid/jobs/workunits/${WORKUNIT}`,
        );
        const missing = await get(
            'tenant-administration',
            `/api/operations/grid/jobs/workunits/${WORKUNIT}`,
            { 'compute.v1.workunits.get': { result: { ...OK, outcome: 'missing' } } },
        );

        expect(found.response.statusCode).toBe(200);
        expect(missing.response.statusCode).toBe(404);
    });
});

describe('the results of a workunit', () => {
    it('serves the runs with the node for an operator', async () => {
        const { response, calls } = await get(
            'system-administration',
            `/api/operations/grid/jobs/workunits/${WORKUNIT}/results`,
        );

        expect(response.statusCode).toBe(200);
        expect(response.json<{ rows: unknown[] }>().rows[0]).toMatchObject({
            id: RESULT,
            host_id: HOST,
            host_hidden: false,
            outcome: 3,
            output_uri: null,
            error_message: 'engine exited with code 3',
        });
        expect(calls[0]?.body).toMatchObject({ workunit_id: WORKUNIT });
    });

    it('withholds the node from a tenant administrator and says so', async () => {
        const { response } = await get(
            'tenant-administration',
            `/api/operations/grid/jobs/workunits/${WORKUNIT}/results`,
        );

        expect(response.json<{ rows: unknown[] }>().rows[0]).toMatchObject({
            host_id: null,
            host_hidden: true,
        });
    });

    it('withholds the node from one result too', async () => {
        const { response } = await get('tenant', `/api/operations/grid/jobs/results/${RESULT}`);

        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({ id: RESULT, host_id: null, host_hidden: true });
    });

    it('states a run in flight with no node, no output and no instant received', async () => {
        const { response } = await get(
            'system-administration',
            `/api/operations/grid/jobs/results/${RESULT}`,
            {
                'compute.v1.results.get': {
                    result: OK,
                    result_value: {
                        ...wireResult,
                        host_id: NIL,
                        server_state: 4,
                        outcome: 0,
                        error_message: '',
                        received_at: '1970-01-01 00:00:00.000000Z',
                    },
                },
            },
        );

        expect(response.json()).toMatchObject({
            host_id: null,
            host_hidden: false,
            output_uri: null,
            error_message: null,
            received_at: null,
        });
    });

    it('answers 404 when the service reports the result missing', async () => {
        const { response } = await get(
            'system-administration',
            `/api/operations/grid/jobs/results/${RESULT}`,
            { 'compute.v1.results.get': { result: { ...OK, outcome: 'missing' } } },
        );

        expect(response.statusCode).toBe(404);
    });
});
