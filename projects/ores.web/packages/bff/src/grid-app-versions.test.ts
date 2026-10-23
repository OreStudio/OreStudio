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
import { describe, expect, it } from 'vitest';
import type { OresClient, SessionMode } from '@ores/wire-protocol';
import type { Config } from './config.js';
import { buildServer, sessionCookieName } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

/**
 * The app version routes: the page of one build with its packages and siblings, and
 * the edit of its release note.
 *
 * What is checked is what the BFF decides: the reads go to the session's own client,
 * the page says who may write, and the note is written by the super administrator
 * alone, with the version the editor read.
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
const APP = '22222222-2222-4222-8222-222222222222';
const VERSION = '33333333-3333-4333-8333-333333333333';
const OLDER = '44444444-4444-4444-8444-444444444444';
const LINUX = '55555555-5555-4555-8555-555555555555';
const WINDOWS = '66666666-6666-4666-8666-666666666666';
const OK = { outcome: 'ok', code: '', message: '', fields: [] };

const audit = {
    version: 3,
    tenant_id: SYSTEM_TENANT,
    modified_by: 'super_admin',
    performed_by: 'compute_service',
    change_reason_code: 'new',
    change_commentary: '',
    recorded_at: '2026-10-23 08:43:09.000000Z',
};

const wireVersion = {
    ...audit,
    id: VERSION,
    app_id: APP,
    wrapper_version: '1.0.0',
    engine_version: '1.8.17.0',
    min_ram_mb: 512,
    release_note: 'ORE 1.8.17.0',
};

const wireOlder = {
    ...audit,
    version: 2,
    id: OLDER,
    app_id: APP,
    wrapper_version: '1.0.0',
    engine_version: '1.8.16.0',
    min_ram_mb: 512,
    release_note: '',
    recorded_at: '2026-10-22 08:00:00.000000Z',
};

const wireOtherApp = {
    ...wireOlder,
    id: '77777777-7777-4777-8777-777777777777',
    app_id: '88888888-8888-4888-8888-888888888888',
    recorded_at: '2026-10-24 08:00:00.000000Z',
};

const platform = (id: string, code: string, active: boolean) => ({
    ...audit,
    id,
    code,
    display_name: code,
    description: '',
    os_family: '',
    cpu_arch: '',
    abi: '',
    is_active: active,
});

const wirePackage = (platformId: string, uri: string) => ({
    ...audit,
    app_version_id: VERSION,
    platform_id: platformId,
    package_uri: uri,
    sha256: 'abc123',
});

type Answers = Readonly<Record<string, unknown>>;

const ANSWERS: Answers = {
    'compute.v1.app_versions.get': { result: OK, app_version: wireVersion },
    'compute.v1.app_versions.list': {
        result: OK,
        app_versions: [wireOlder, wireVersion, wireOtherApp],
        total: 3,
    },
    'compute.v1.apps.list': {
        result: OK,
        apps: [{ ...audit, id: APP, name: 'ORE', description: 'Open Source Risk Engine' }],
        total: 1,
    },
    'compute.v1.platforms.list': {
        result: OK,
        platforms: [platform(LINUX, 'x64-linux', true), platform(WINDOWS, 'x64-windows', false)],
        total: 2,
    },
    'compute.v1.app_version_platforms.list_by_app_version_id': {
        result: OK,
        app_version_platforms: [
            wirePackage(WINDOWS, '/storage/ORE-1.8.17.0-x64-windows.tar.gz'),
            wirePackage(LINUX, '/storage/ORE-1.8.17.0-x64-linux.tar.gz'),
        ],
        total: 2,
    },
    'compute.v1.app_versions.put': { result: OK, app_version: wireVersion },
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

async function send(
    mode: SessionMode,
    method: 'GET' | 'PUT',
    url: string,
    payload?: unknown,
    answers: Answers = ANSWERS,
) {
    const { server, cookies, calls } = buildTestServer(mode, answers);
    const response = await server.inject({
        method,
        url,
        cookies,
        ...(payload === undefined ? {} : { payload: payload as Record<string, unknown> }),
    });
    await server.close();
    return { response, calls };
}

const PAGE_URL = `/api/operations/grid/app-versions/${VERSION}`;
const NOTE_URL = `${PAGE_URL}/release-note`;

describe('GET /api/operations/grid/app-versions/:id', () => {
    it('serves the build, its packages by platform code and the versions of its app', async () => {
        const { response } = await send('tenant-administration', 'GET', PAGE_URL);

        expect(response.statusCode).toBe(200);
        const body = response.json<{
            version: { id: string; app_name: string; release_note: string; version: number };
            packages: { code: string; is_active: boolean; package_uri: string }[];
            versions: { id: string }[];
            may_write: boolean;
        }>();
        expect(body.version).toMatchObject({ id: VERSION, app_name: 'ORE', version: 3 });
        expect(body.version.release_note).toBe('ORE 1.8.17.0');
        expect(body.packages.map((p) => [p.code, p.is_active])).toEqual([
            ['x64-linux', true],
            ['x64-windows', false],
        ]);
        expect(body.versions.map((v) => v.id)).toEqual([VERSION, OLDER]);
    });

    it('says only an operator in system administration may write the note', async () => {
        const operator = await send('system-administration', 'GET', PAGE_URL);
        const tenant = await send('tenant-administration', 'GET', PAGE_URL);
        const member = await send('application', 'GET', PAGE_URL);

        expect(operator.response.json<{ may_write: boolean }>().may_write).toBe(true);
        expect(tenant.response.json<{ may_write: boolean }>().may_write).toBe(false);
        expect(member.response.json<{ may_write: boolean }>().may_write).toBe(false);
    });

    it('answers 404 for a version the session may not see', async () => {
        const { response } = await send('application', 'GET', PAGE_URL, undefined, {
            ...ANSWERS,
            'compute.v1.app_versions.get': {
                result: { ...OK, outcome: 'missing' },
                app_version: null,
            },
        });

        expect(response.statusCode).toBe(404);
    });

    it('answers 400 for an id that is not a uuid', async () => {
        const { response, calls } = await send(
            'application',
            'GET',
            '/api/operations/grid/app-versions/not-a-uuid',
        );

        expect(response.statusCode).toBe(400);
        expect(calls).toEqual([]);
    });

    it('answers 403 when the compute service denies the read', async () => {
        const { response } = await send('application', 'GET', PAGE_URL, undefined, {
            ...ANSWERS,
            'compute.v1.app_versions.get': {
                result: { ...OK, outcome: 'denied', message: 'no' },
                app_version: null,
            },
        });

        expect(response.statusCode).toBe(403);
    });
});

describe('PUT /api/operations/grid/app-versions/:id/release-note', () => {
    it('writes the note with the version the editor read and answers the refreshed page', async () => {
        const { response, calls } = await send('system-administration', 'PUT', NOTE_URL, {
            release_note: 'New note',
            version: 3,
        });

        expect(response.statusCode).toBe(200);
        const put = calls.find((call) => call.subject === 'compute.v1.app_versions.put');
        expect(put?.body).toMatchObject({
            change: {
                write: { id: VERSION, app_id: APP, release_note: 'New note', min_ram_mb: 512 },
                precondition: { kind: 'must_match_version', version: 3 },
            },
        });
        expect(response.json<{ may_write: boolean }>().may_write).toBe(true);
    });

    it.each([['tenant-administration'], ['application']] as const)(
        'refuses %s before any call goes out',
        async (mode) => {
            const { response, calls } = await send(mode, 'PUT', NOTE_URL, {
                release_note: 'x',
                version: 3,
            });

            expect(response.statusCode).toBe(403);
            expect(calls).toEqual([]);
        },
    );

    it.each([
        ['a body without the version', { release_note: 'x' }],
        ['a field the route does not take', { release_note: 'x', version: 3, app_id: 'y' }],
        ['a note past the limit', { release_note: 'x'.repeat(20001), version: 3 }],
    ])('refuses %s with 400', async (_name, payload) => {
        const { response, calls } = await send('system-administration', 'PUT', NOTE_URL, payload);

        expect(response.statusCode).toBe(400);
        expect(calls).toEqual([]);
    });

    it('answers 409 when the note was edited since the editor read it', async () => {
        const { response } = await send(
            'system-administration',
            'PUT',
            NOTE_URL,
            { release_note: 'x', version: 2 },
            {
                ...ANSWERS,
                'compute.v1.app_versions.put': {
                    result: {
                        ...OK,
                        outcome: 'conflict',
                        message: 'The app_version is at version 3.',
                    },
                    app_version: null,
                },
            },
        );

        expect(response.statusCode).toBe(409);
    });
});
