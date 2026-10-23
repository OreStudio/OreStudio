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

import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { afterEach, describe, expect, it, vi } from 'vitest';
import type { OresClient, SessionMode } from '@ores/wire-protocol';
import type { Config } from './config.js';
import { dispositionFor, downloadName, storagePath } from './grid-downloads.js';
import { buildServer, sessionCookieName } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

/**
 * The download routes: the stored input of a workunit and the stored output of a run.
 *
 * What is checked is what the BFF decides: it reads the owning record on the session's
 * own client first, so a caller who may not read it is refused before the storage
 * service is asked for any byte, and it streams the file with the session's token under
 * the file's own name.
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
const NIL = '00000000-0000-0000-0000-000000000000';
const OK = { outcome: 'ok', code: '', message: '', fields: [] };
const INPUT = '/api/v1/storage/ores/compute/input/batch.tar.gz';
const OUTPUT = '/api/v1/storage/ores/compute/output/result.tar.gz';

const audit = {
    version: 1,
    tenant_id: SYSTEM_TENANT,
    modified_by: 'compute_service',
    performed_by: 'compute_service',
    change_reason_code: 'new',
    change_commentary: '',
    recorded_at: '2026-10-13 02:27:27.000000Z',
};

const wireWorkunit = (inputUri = INPUT) => ({
    ...audit,
    id: WORKUNIT,
    party_id: null,
    batch_id: BATCH,
    app_version_id: '66666666-6666-4666-8666-666666666666',
    input_uri: inputUri,
    config_uri: '',
    priority: 1,
    target_redundancy: 1,
    canonical_result_id: NIL,
});

const wireResult = (outputUri = OUTPUT) => ({
    ...audit,
    id: RESULT,
    party_id: null,
    workunit_id: WORKUNIT,
    host_id: NIL,
    server_state: 5,
    outcome: 1,
    output_uri: outputUri,
    error_message: '',
    received_at: '2026-10-13 02:27:53.000000Z',
});

type Answers = Readonly<Record<string, unknown>>;

const ANSWERS: Answers = {
    'compute.v1.workunits.get': { result: OK, workunit: wireWorkunit() },
    'compute.v1.results.get': { result: OK, result_value: wireResult() },
};

function buildTestServer(
    answers: Answers = ANSWERS,
    base: string | null = 'http://files.test:20200',
) {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const calls: string[] = [];
    const client = {
        token: 'token-123',
        async discoverHttpBaseUrl(): Promise<string | null> {
            return base;
        },
        async callAuthenticated(
            subject: string,
            _body: unknown,
            schema: { parse: (value: unknown) => unknown },
        ): Promise<unknown> {
            calls.push(subject);
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
        mode: 'tenant-administration' as SessionMode,
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

function stubStorage(response: Response) {
    const fetchMock = vi.fn(async () => response);
    vi.stubGlobal('fetch', fetchMock);
    return fetchMock;
}

async function get(url: string, answers: Answers = ANSWERS, base?: string | null) {
    const { server, cookies, calls } = buildTestServer(answers, base);
    const response = await server.inject({ method: 'GET', url, cookies });
    await server.close();
    return { response, calls };
}

afterEach(() => {
    vi.unstubAllGlobals();
});

const INPUT_URL = `/api/operations/grid/jobs/workunits/${WORKUNIT}/input`;
const OUTPUT_URL = `/api/operations/grid/jobs/results/${RESULT}/output`;

describe('GET /api/operations/grid/jobs/workunits/:id/input', () => {
    it('streams the stored input with the session token, under the name of the file', async () => {
        const fetchMock = stubStorage(
            new Response('input-bytes', {
                status: 200,
                headers: { 'content-type': 'application/gzip', 'content-length': '11' },
            }),
        );

        const { response } = await get(INPUT_URL);

        expect(response.statusCode).toBe(200);
        expect(response.body).toBe('input-bytes');
        expect(response.headers['content-type']).toBe('application/gzip');
        expect(response.headers['content-disposition']).toBe(
            `attachment; filename="batch.tar.gz"; filename*=UTF-8''batch.tar.gz`,
        );
        expect(response.headers['cache-control']).toBe('private, no-store');
        const [url, init] = fetchMock.mock.calls[0] as unknown as [URL, RequestInit];
        expect(url.toString()).toBe(`http://files.test:20200${INPUT}`);
        expect(init.headers).toEqual({ Authorization: 'Bearer token-123' });
    });

    it('refuses a caller who may not read the workunit before it asks for any byte', async () => {
        const fetchMock = stubStorage(new Response('secret'));

        const { response } = await get(INPUT_URL, {
            'compute.v1.workunits.get': { result: { ...OK, outcome: 'denied', message: 'no' } },
        });

        expect(response.statusCode).toBe(403);
        expect(fetchMock).not.toHaveBeenCalled();
    });

    it('answers 404 for a workunit the session may not see, and asks for no byte', async () => {
        const fetchMock = stubStorage(new Response('secret'));

        const { response } = await get(INPUT_URL, {
            'compute.v1.workunits.get': { result: { ...OK, outcome: 'missing' }, workunit: null },
        });

        expect(response.statusCode).toBe(404);
        expect(fetchMock).not.toHaveBeenCalled();
    });

    it('answers 404 when the stored file is gone', async () => {
        stubStorage(new Response('', { status: 404 }));

        const { response } = await get(INPUT_URL);

        expect(response.statusCode).toBe(404);
    });

    it('answers 404 when the deployment has no file service', async () => {
        const fetchMock = stubStorage(new Response('x'));

        const { response } = await get(INPUT_URL, ANSWERS, null);

        expect(response.statusCode).toBe(404);
        expect(fetchMock).not.toHaveBeenCalled();
    });

    it('asks the storage service for nothing outside the storage prefix', async () => {
        const fetchMock = stubStorage(new Response('x'));

        const { response } = await get(INPUT_URL, {
            'compute.v1.workunits.get': {
                result: OK,
                workunit: wireWorkunit('/api/v1/storage/../admin/secret'),
            },
        });

        expect(response.statusCode).toBe(404);
        expect(fetchMock).not.toHaveBeenCalled();
    });

    it('answers 400 for an id that is not a uuid', async () => {
        const { response, calls } = await get('/api/operations/grid/jobs/workunits/x/input');

        expect(response.statusCode).toBe(400);
        expect(calls).toEqual([]);
    });
});

describe('GET /api/operations/grid/jobs/results/:id/output', () => {
    it('streams the stored output of a run', async () => {
        stubStorage(new Response(new TextEncoder().encode('output-bytes'), { status: 200 }));

        const { response } = await get(OUTPUT_URL);

        expect(response.statusCode).toBe(200);
        expect(response.body).toBe('output-bytes');
        expect(response.headers['content-disposition']).toContain('filename="result.tar.gz"');
        expect(response.headers['content-type']).toBe('application/octet-stream');
    });

    it('answers 404 for a run that has no output yet', async () => {
        const fetchMock = stubStorage(new Response('x'));

        const { response } = await get(OUTPUT_URL, {
            'compute.v1.results.get': { result: OK, result_value: wireResult('') },
        });

        expect(response.statusCode).toBe(404);
        expect(fetchMock).not.toHaveBeenCalled();
    });

    it('refuses a caller who may not read the run before it asks for any byte', async () => {
        const fetchMock = stubStorage(new Response('secret'));

        const { response } = await get(OUTPUT_URL, {
            'compute.v1.results.get': { result: { ...OK, outcome: 'denied', message: 'no' } },
        });

        expect(response.statusCode).toBe(403);
        expect(fetchMock).not.toHaveBeenCalled();
    });
});

describe('the object path of a stored file', () => {
    it('is the address when it is a plain path under the storage prefix', () => {
        expect(storagePath(INPUT)).toBe(INPUT);
        expect(downloadName(INPUT)).toBe('batch.tar.gz');
    });

    it.each([
        [''],
        ['/api/v1/storage/'],
        ['/etc/passwd'],
        ['/api/v1/storage/../admin'],
        ['/api/v1/storage/a/./b'],
        ['/api/v1/storage/a//b'],
        ['/api/v1/storage/a?x=1'],
        ['/api/v1/storage/a#b'],
        ['/api/v1/storage/a\\b'],
        ['/api/v1/storage/%2e%2e/%2e%2e/auth/x'],
        ['/api/v1/storage/%2E/a'],
        ['/api/v1/storage/.%2e/a'],
        ['/api/v1/storage/a%2fb'],
        ['/api/v1/storage/a%5cb'],
        ['/api/v1/storage/a%0d%0ab'],
        ['/api/v1/storage/%zz'],
        ['http://elsewhere.test/api/v1/storage/a'],
    ])('is nothing for %s', (uri) => {
        expect(storagePath(uri)).toBeNull();
    });
});

describe('the name a download is saved as', () => {
    it('keeps an ASCII fallback and the real name in the extended form', () => {
        expect(dispositionFor('report one.tar.gz')).toBe(
            `attachment; filename="report one.tar.gz"; filename*=UTF-8''report%20one.tar.gz`,
        );
        expect(dispositionFor('résumé.tar.gz')).toContain('filename="r_sum_.tar.gz"');
        expect(dispositionFor('résumé.tar.gz')).toContain(
            "filename*=UTF-8''r%C3%A9sum%C3%A9.tar.gz",
        );
    });

    it('lets no character break the header', () => {
        const header = dispositionFor('a"b;c%d.tar.gz');

        expect(header).toContain('filename="a_b_c_d.tar.gz"');
        expect(header).not.toMatch(/[\r\n]/);
    });
});
