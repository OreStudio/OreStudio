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
import { type OresClient } from '@ores/wire-protocol';
import type { Config } from './config.js';
import { buildServer, sessionCookieName } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

/**
 * The audit sign-ins routes.
 *
 * Two things are asserted here that the screen would otherwise find out in the
 * browser: the subject and the body each route sends, and the shape of the
 * answer. The window itself is the deployment's, so it is checked as a window
 * rather than as two instants this test invented.
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

const northwind = {
    id: '22222222-2222-2222-2222-222222222222',
    name: 'Northwind Capital',
    partyCategory: 'Operational',
    businessCenterCode: 'GBLO',
};

const ACCOUNT_ID = '11111111-1111-1111-1111-111111111111';
const TENANT_ID = 'ffffffff-ffff-ffff-ffff-ffffffffffff';
const SESSION_ID = '33333333-3333-4333-8333-333333333333';

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
            sessionId: '44444444-4444-4444-4444-444444444444',
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
        sessionId: '44444444-4444-4444-4444-444444444444',
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

/** One authentication event as the server writes it. */
const wireEvent = {
    id: '55555555-5555-4555-8555-555555555555',
    event_time: '2026-10-01 22:14:00Z',
    account_id: '',
    event_type: 'login_failure',
    username: 'jonas.lindqvist',
    session_id: '',
    party_id: '',
    error_detail: 'bad password',
};

/** One statistics row as the server writes it. */
const wireStatistics = {
    day: '2026-10-01',
    account_id: ACCOUNT_ID,
    session_count: 4,
    avg_duration_seconds: 1800.0,
    total_bytes_sent: 18840000,
    total_bytes_received: 2400000,
    avg_bytes_sent: 4710000.0,
    avg_bytes_received: 600000.0,
    unique_countries: 2,
};

describe('GET /api/auth-events', () => {
    it('reads the events and renames the wire\u2019s fields', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'iam.v1.auth_events.list': { events: [wireEvent], success: true, message: '' },
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/auth-events?period=all&eventType=login_failure&limit=50',
            cookies: { [SESSION_COOKIE]: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            events: [
                {
                    id: wireEvent.id,
                    eventTime: '2026-10-01 22:14:00Z',
                    accountId: '',
                    eventType: 'login_failure',
                    username: 'jonas.lindqvist',
                    sessionId: '',
                    partyId: '',
                    errorDetail: 'bad password',
                },
            ],
        });
        expect(calls).toEqual([
            {
                subject: 'iam.v1.auth_events.list',
                body: {
                    account_id: '',
                    event_type: 'login_failure',
                    from_time: '',
                    to_time: '',
                    offset: 0,
                    limit: 50,
                },
            },
        ]);

        await server.close();
    });

    it('names a window on the deployment\u2019s clock for a period', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'iam.v1.auth_events.list': { events: [], success: true, message: '' },
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/auth-events?period=day',
            cookies: { [SESSION_COOKIE]: sessionId },
        });

        expect(response.statusCode).toBe(200);
        const body = calls[0]?.body as { from_time: string; to_time: string };
        expect(body.from_time).toMatch(/^\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}Z$/);
        expect(body.to_time).toMatch(/^\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}Z$/);
        expect(Date.parse(body.to_time)).toBeGreaterThan(Date.parse(body.from_time));

        await server.close();
    });

    it('refuses a request with no session', async () => {
        const { server, calls } = buildTestServer({
            'iam.v1.auth_events.list': { events: [], success: true, message: '' },
        });

        const response = await server.inject({ method: 'GET', url: '/api/auth-events' });

        expect(response.statusCode).toBe(401);
        expect(calls).toEqual([]);

        await server.close();
    });
});

describe('GET /api/session-statistics', () => {
    it('reads the statistics and renames the wire\u2019s fields', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'iam.v1.ops.get_session_statistics': {
                rows: [wireStatistics],
                success: true,
                message: '',
            },
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/session-statistics?period=all',
            cookies: { [SESSION_COOKIE]: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            rows: [
                {
                    day: '2026-10-01',
                    accountId: ACCOUNT_ID,
                    sessionCount: 4,
                    avgDurationSeconds: 1800,
                    totalBytesSent: 18840000,
                    totalBytesReceived: 2400000,
                    avgBytesSent: 4710000,
                    avgBytesReceived: 600000,
                    uniqueCountries: 2,
                },
            ],
        });
        expect(calls).toEqual([
            {
                subject: 'iam.v1.ops.get_session_statistics',
                body: {
                    account_id: '',
                    from_time: '',
                    to_time: '',
                    offset: 0,
                    limit: 100,
                },
            },
        ]);

        await server.close();
    });
});

describe('POST /api/sessions/:sessionId/end', () => {
    it('ends one session and answers the write', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'iam.v1.ops.end_session': { success: true, message: 'Session ended' },
        });

        const response = await server.inject({
            method: 'POST',
            url: `/api/sessions/${SESSION_ID}/end`,
            cookies: { [SESSION_COOKIE]: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({ success: true });
        expect(calls).toEqual([
            { subject: 'iam.v1.ops.end_session', body: { session_id: SESSION_ID } },
        ]);

        await server.close();
    });

    it('turns a refusal in the body into a failure the screen can show', async () => {
        const { server, sessionId } = buildTestServer({
            'iam.v1.ops.end_session': { success: false, message: 'Session already ended' },
        });

        const response = await server.inject({
            method: 'POST',
            url: `/api/sessions/${SESSION_ID}/end`,
            cookies: { [SESSION_COOKIE]: sessionId },
        });

        expect(response.statusCode).toBe(409);
        expect(response.json()).toMatchObject({ message: 'Session already ended' });

        await server.close();
    });
});

describe('GET /api/geo/country', () => {
    it('resolves an address and answers with its country', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'iam.v1.ops.lookup_country': {
                country_code: 'GB',
                found: true,
                success: true,
                message: '',
            },
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/geo/country?address=8.8.8.8',
            cookies: { [SESSION_COOKIE]: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({ found: true, countryCode: 'GB' });
        expect(calls).toEqual([
            { subject: 'iam.v1.ops.lookup_country', body: { address: '8.8.8.8' } },
        ]);

        await server.close();
    });

    it('answers not found for an address the tenant\u2019s ranges do not cover', async () => {
        const { server, sessionId } = buildTestServer({
            'iam.v1.ops.lookup_country': {
                country_code: '',
                found: false,
                success: true,
                message: '',
            },
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/geo/country?address=10.0.0.1',
            cookies: { [SESSION_COOKIE]: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({ found: false, countryCode: '' });

        await server.close();
    });
});
