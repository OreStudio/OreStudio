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
import {
    ServerError,
    fromWireTimestamp,
    type OresClient,
    type WireTimestamp,
} from '@ores/wire-protocol';
import type { Config } from './config.js';
import { buildServer, sessionCookieName } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

const config: Config = {
    port: 0,
    host: '127.0.0.1',
    logLevel: 'silent',
    session: { ttlSeconds: 3600, cookieSecure: false },
    allowedOrigins: [],
    loginAttemptsPerMinute: 100,
};

/** The environment this file's site configuration serves. */
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

const ok = { outcome: 'ok', code: '', message: '' };

function buildTestServer(
    replies: Readonly<Record<string, unknown>>,
    tenantId = '44444444-4444-4444-4444-444444444444',
): {
    readonly server: ReturnType<typeof buildServer>;
    readonly sessionId: string;
    readonly calls: { subject: string; body: unknown }[];
} {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const calls: { subject: string; body: unknown }[] = [];
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
            const canned = replies[subject];
            if (canned instanceof Error) {
                throw canned;
            }
            return schema.parse(canned);
        },
        async close(): Promise<void> {
            return undefined;
        },
    } as unknown as OresClient;
    const session = sessions.create({
        client,
        session: null,
        username: 'tenant_admin',
        email: 'tenant_admin@acme.example',
        accountId: '11111111-1111-1111-1111-111111111111',
        tenantId,
        tenantName: 'Acme',
        mode: 'tenant-administration',
        version: 'v0.0.25 (test)',
        availableParties: [],
        accessLifetimeSeconds: 1800,
        passwordResetRequired: false,
        sessionId: '33333333-3333-3333-3333-333333333333',
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

function send(
    server: ReturnType<typeof buildServer>,
    sessionId: string,
    method: 'GET' | 'POST' | 'PUT' | 'DELETE',
    url: string,
    payload?: unknown,
): ReturnType<ReturnType<typeof buildServer>['inject']> {
    return server.inject({
        method,
        url,
        cookies: { [SESSION_COOKIE]: sessionId },
        ...(payload === undefined ? {} : { payload: payload as Record<string, unknown> }),
    });
}

const SUBJECT = 'marketdata.v1.market_observations.list_by_series_id';
const SERIES = 'aaaaaaaa-0000-4000-8000-000000000001';
const OK = { outcome: 'ok', code: '', message: '' };

const row = (at: string, value: string) => ({
    observation_datetime: at,
    value,
    source: 'synthetic',
    id: 'x',
    series_id: SERIES,
});

const REPLY = {
    result: OK,
    market_observations: [
        row('2026-10-12 10:02:00Z', '1.30'),
        row('2026-10-12 10:01:00Z', '1.20'),
        row('2026-10-12 10:00:00Z', '1.10'),
    ],
    total: 41,
};

describe('the series observations route', () => {
    it('needs a signed-in session', async () => {
        const { server } = buildTestServer({});
        const response = await server.inject({
            method: 'GET',
            url: `/api/series/${SERIES}/observations`,
        });
        expect(response.statusCode).toBe(401);
    });

    it('reads the observations of the series and returns them oldest first with the total', async () => {
        const { server, sessionId, calls } = buildTestServer({ [SUBJECT]: REPLY });
        const response = await send(server, sessionId, 'GET', `/api/series/${SERIES}/observations`);
        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            observations: [
                { at: '2026-10-12 10:00:00Z', value: '1.10', source: 'synthetic' },
                { at: '2026-10-12 10:01:00Z', value: '1.20', source: 'synthetic' },
                { at: '2026-10-12 10:02:00Z', value: '1.30', source: 'synthetic' },
            ],
            total: 41,
        });
        expect(calls).toEqual([
            {
                subject: SUBJECT,
                body: {
                    series_id: SERIES,
                    scope: 'direct',
                    offset: 0,
                    limit: 500,
                    order: { field: '', descending: false },
                    filter: {
                        series_id: null,
                        observation_datetime: null,
                        id_one_of: null,
                        series_id_one_of: null,
                        observation_datetime_one_of: null,
                        observation_datetime_from: null,
                        observation_datetime_to: null,
                    },
                },
            },
        ]);
    });

    it('narrows a named window to the instant it starts at, on the deployment clock', async () => {
        const { server, sessionId, calls } = buildTestServer({ [SUBJECT]: REPLY });
        const before = Date.now();
        await send(server, sessionId, 'GET', `/api/series/${SERIES}/observations?window=day`);
        const after = Date.now();
        const { filter } = calls[0]?.body as {
            filter: { observation_datetime_from: string | null };
        };
        expect(filter.observation_datetime_from).not.toBeNull();
        const since = fromWireTimestamp(
            filter.observation_datetime_from as WireTimestamp,
        ).getTime();
        const day = 24 * 60 * 60 * 1000;
        expect(since).toBeGreaterThanOrEqual(before - day);
        expect(since).toBeLessThanOrEqual(after - day);
    });

    it('reads every observation for the all window and for a window it does not know', async () => {
        const { server, sessionId, calls } = buildTestServer({ [SUBJECT]: REPLY });
        await send(server, sessionId, 'GET', `/api/series/${SERIES}/observations?window=all`);
        const answer = await send(
            server,
            sessionId,
            'GET',
            `/api/series/${SERIES}/observations?window=fortnight`,
        );
        expect(answer.statusCode).toBe(400);
        expect(calls).toHaveLength(1);
        expect(
            (calls[0]?.body as { filter: { observation_datetime_from: string | null } }).filter,
        ).toMatchObject({ observation_datetime_from: null });
    });

    it('asks for the number it is given and clamps a request for more than the cap', async () => {
        const { server, sessionId, calls } = buildTestServer({ [SUBJECT]: REPLY });
        await send(server, sessionId, 'GET', `/api/series/${SERIES}/observations?limit=50`);
        await send(server, sessionId, 'GET', `/api/series/${SERIES}/observations?limit=5000`);
        const limits = calls.map((call) => (call.body as { limit: number }).limit);
        expect(limits).toEqual([50, 1000]);
    });

    it('falls back to the number of observations read when the server sends no total', async () => {
        const { total: _total, ...withoutTotal } = REPLY;
        const { server, sessionId } = buildTestServer({ [SUBJECT]: withoutTotal });
        const response = await send(server, sessionId, 'GET', `/api/series/${SERIES}/observations`);
        expect(response.json().total).toBe(3);
    });

    it('answers an empty list for a series with no observation', async () => {
        const empty = { result: OK, market_observations: [], total: 0 };
        const { server, sessionId } = buildTestServer({ [SUBJECT]: empty });
        const response = await send(server, sessionId, 'GET', `/api/series/${SERIES}/observations`);
        expect(response.json()).toEqual({ observations: [], total: 0 });
    });

    it('refuses an id that is not an identifier and a limit that is not a number, without asking the server', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        for (const url of [
            '/api/series/not-an-id/observations',
            `/api/series/${SERIES}/observations?limit=0`,
            `/api/series/${SERIES}/observations?limit=lots`,
        ]) {
            const response = await send(server, sessionId, 'GET', url);
            expect(response.statusCode, url).toBe(400);
        }
        expect(calls).toEqual([]);
    });

    it('turns a refused read into the status that says why', async () => {
        for (const [outcome, status] of [
            ['denied', 403],
            ['missing', 404],
            ['failed', 502],
        ] as const) {
            const { server, sessionId } = buildTestServer({
                [SUBJECT]: { ...REPLY, result: { outcome, code: '', message: 'no' } },
            });
            const response = await send(
                server,
                sessionId,
                'GET',
                `/api/series/${SERIES}/observations`,
            );
            expect(response.statusCode, outcome).toBe(status);
        }
    });

    it('shows a missing permission on the call as a refusal', async () => {
        const { server, sessionId } = buildTestServer({
            [SUBJECT]: new ServerError('forbidden', SUBJECT),
        });
        const response = await send(server, sessionId, 'GET', `/api/series/${SERIES}/observations`);
        expect(response.statusCode).toBe(403);
    });
});
