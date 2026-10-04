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
import type { OresClient } from '@ores/wire-protocol';
import type { Config } from './config.js';
import { buildServer } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

/**
 * The session reads the audit screen starts from.
 *
 * The active-sessions read is the one worth a case of its own: its subject
 * exists and the handler behind it answers `{success: true}` with no rows, so a
 * route that treated an empty list as a failure would break the screen the day
 * the handler starts answering, and a route that reported it as data has to say
 * so on the screen instead.
 */

const config: Config = {
    port: 0,
    host: '127.0.0.1',
    logLevel: 'silent',
    session: { ttlSeconds: 3600, cookieSecure: false },
    allowedOrigins: [],
    loginAttemptsPerMinute: 100,
};

function siteConfiguration(): ReturnType<typeof loadSiteConfiguration> {
    const path = resolve(
        dirname(fileURLToPath(import.meta.url)),
        '../../../config/environments.json',
    );
    return loadSiteConfiguration({
        environment: { [SITE_CONFIG_VARIABLE]: path },
        environmentId: 'eager_maxwell',
    });
}

const northwind = {
    id: '22222222-2222-2222-2222-222222222222',
    name: 'Northwind Capital',
    partyCategory: 'Operational',
    businessCenterCode: 'GBLO',
};

const ACCOUNT_ID = '11111111-1111-1111-1111-111111111111';
const TENANT_ID = 'ffffffff-ffff-ffff-ffff-ffffffffffff';
const SESSION_ID = '44444444-4444-4444-4444-444444444444';

const wireSession = {
    tenant_id: TENANT_ID,
    id: SESSION_ID,
    account_id: ACCOUNT_ID,
    start_time: '2026-09-30 08:12:00Z',
    end_time: '',
    client_ip: '203.0.113.44',
    client_identifier: 'ores.web',
    client_version_major: 0,
    client_version_minor: 25,
    bytes_sent: 1024,
    bytes_received: 4096,
    country_code: 'GB',
    protocol: 'https',
};

interface Call {
    readonly subject: string;
    readonly body: unknown;
}

function buildTestServer(replies: Readonly<Record<string, unknown>>): {
    readonly server: ReturnType<typeof buildServer>;
    readonly sessionId: string;
    readonly calls: Call[];
} {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const calls: Call[] = [];
    const client = {
        async callAuthenticated(
            subject: string,
            body: unknown,
            schema: { parse: (value: unknown) => unknown },
        ): Promise<unknown> {
            calls.push({ subject, body });
            if (!(subject in replies)) {
                throw new Error(`No canned reply for ${subject}`);
            }
            return schema.parse(replies[subject]);
        },
        async close(): Promise<void> {
            return undefined;
        },
    } as unknown as OresClient;
    const session = sessions.create({
        client,
        session: {
            kind: 'active',
            token: 'token',
            accountId: ACCOUNT_ID,
            tenantId: TENANT_ID,
            tenantName: 'Northwind Capital',
            version: 'v0.0.25 (test)',
            username: 'tenant_admin',
            email: 'admin@northwind.example.com',
            party: northwind,
            availableParties: [northwind],
            accessLifetimeSeconds: 1800,
            passwordResetRequired: false,
            sessionId: SESSION_ID,
        },
        username: 'tenant_admin',
        email: 'admin@northwind.example.com',
        accountId: ACCOUNT_ID,
        tenantId: TENANT_ID,
        tenantName: 'Northwind Capital',
        version: 'v0.0.25 (test)',
        availableParties: [northwind],
        accessLifetimeSeconds: 1800,
        passwordResetRequired: false,
        sessionId: SESSION_ID,
    });
    return {
        server: buildServer({
            config,
            site: siteConfiguration(),
            sessions,
            createClient: () => ({ client, connect: async () => undefined }),
        }),
        sessionId: session.id,
        calls,
    };
}

describe('GET /api/sessions', () => {
    it('lists the sessions, counting them by the wire\u2019s own name', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'iam.v1.sessions.list': { sessions: [wireSession], total: 1 },
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/sessions',
            cookies: { ores_web_session: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            sessions: [
                {
                    tenantId: TENANT_ID,
                    id: SESSION_ID,
                    accountId: ACCOUNT_ID,
                    startTime: '2026-09-30 08:12:00Z',
                    endTime: '',
                    clientIp: '203.0.113.44',
                    clientIdentifier: 'ores.web',
                    clientVersionMajor: 0,
                    clientVersionMinor: 25,
                    bytesSent: 1024,
                    bytesReceived: 4096,
                    countryCode: 'GB',
                    protocol: 'https',
                },
            ],
            totalCount: 1,
        });
        expect(calls).toEqual([
            {
                subject: 'iam.v1.sessions.list',
                body: {
                    offset: 0,
                    limit: 100,
                    order: { field: '', descending: false },
                    filter: null,
                },
            },
        ]);

        await server.close();
    });

    it('refuses a request with no session', async () => {
        const { server, calls } = buildTestServer({
            'iam.v1.sessions.list': { sessions: [], total: 0 },
        });

        const response = await server.inject({ method: 'GET', url: '/api/sessions' });

        expect(response.statusCode).toBe(401);
        expect(response.json()).toMatchObject({ code: 'not-authenticated' });
        expect(calls).toEqual([]);

        await server.close();
    });
});

describe('GET /api/sessions/active', () => {
    it('answers the open sessions', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'iam.v1.sessions.active': {
                sessions: [wireSession],
                success: true,
                message: '',
            },
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/sessions/active',
            cookies: { ores_web_session: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({
            sessions: [{ id: SESSION_ID, clientIdentifier: 'ores.web', endTime: '' }],
        });
        expect(calls).toEqual([{ subject: 'iam.v1.sessions.active', body: {} }]);

        await server.close();
    });

    it('answers an empty list while the handler is a stub, rather than failing', async () => {
        const { server, sessionId } = buildTestServer({
            'iam.v1.sessions.active': { success: true, message: '' },
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/sessions/active',
            cookies: { ores_web_session: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({ sessions: [] });

        await server.close();
    });
});

/*
 * A person's own screen lists their sessions only. The server answers the
 * tenant's open sessions, the platform's services' among them, so the read
 * keeps the rows of the signed-in account.
 */
describe('GET /api/me/sessions', () => {
    it('keeps the signed-in account sessions and drops every other account', async () => {
        const { server, sessionId } = buildTestServer({
            'iam.v1.sessions.active': {
                sessions: [
                    wireSession,
                    {
                        ...wireSession,
                        id: '99999999-9999-9999-9999-999999999999',
                        account_id: '88888888-8888-8888-8888-888888888888',
                        client_identifier: 'ores.service.binary',
                    },
                ],
                success: true,
                message: '',
            },
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/me/sessions',
            cookies: { ores_web_session: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json().sessions).toHaveLength(1);
        expect(response.json().sessions[0]).toMatchObject({
            id: SESSION_ID,
            accountId: ACCOUNT_ID,
        });

        await server.close();
    });
});
