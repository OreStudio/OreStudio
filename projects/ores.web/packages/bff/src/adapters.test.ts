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
import { ServiceUnavailableError, type OresClient } from '@ores/wire-protocol';
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

function buildTestServer(replies: Readonly<Record<string, unknown>>): {
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
        tenantId: '44444444-4444-4444-4444-444444444444',
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
    sessionId: string | undefined,
): ReturnType<ReturnType<typeof buildServer>['inject']> {
    return server.inject({
        method: 'GET',
        url: '/api/marketdata/adapters',
        ...(sessionId === undefined ? {} : { cookies: { [SESSION_COOKIE]: sessionId } }),
    });
}

/** One adapter as the service reports it, with every field the screen reads. */
function served(provider: string, demanded: number): Record<string, unknown> {
    return {
        adapter_id: `${provider}-sim`,
        provider_code: provider,
        state: 'serving',
        last_heartbeat_at: '2026-10-25 15:00:00Z',
        heartbeat_counter: 42,
        served_count: 26,
        unknown_count: 0,
        demanded_count: demanded,
        routes_seen: 3,
        ticks_last_minute: 60,
        last_tick_age_ms: 200,
        accepted: [],
        rejected: [],
    };
}

const SUBJECT = 'marketdata.v1.ops.list_adapter_status';

describe('the adapter status route', () => {
    it('proxies the state of every adapter the service tracks', async () => {
        const adapters = [served('bloomberg', 13), { ...served('ore', 0), state: 'ready' }];
        const { server, sessionId, calls } = buildTestServer({
            [SUBJECT]: { success: true, message: '', adapters },
        });
        const response = await send(server, sessionId);
        expect(response.statusCode).toBe(200);
        expect(response.json().success).toBe(true);
        expect(response.json().adapters).toEqual(adapters);
        expect(calls[0]?.subject).toBe(SUBJECT);
        expect(calls[0]?.body).toEqual({});
    });

    it('reports a service failure as an error, not as an empty set of adapters', async () => {
        const { server, sessionId } = buildTestServer({
            [SUBJECT]: { success: false, message: 'the adapter registry is not ready' },
        });
        const response = await send(server, sessionId);
        expect(response.statusCode).toBe(502);
        expect(response.json().code).toBe('upstream-unavailable');
        expect(response.json().message).toBe('the adapter registry is not ready');
    });

    it('reports a market data service that is not running as unavailable', async () => {
        const { server, sessionId } = buildTestServer({
            [SUBJECT]: new ServiceUnavailableError(SUBJECT),
        });
        const response = await send(server, sessionId);
        expect(response.statusCode).toBe(503);
        expect(response.json().code).toBe('upstream-unavailable');
    });

    it('needs a session', async () => {
        const { server, calls } = buildTestServer({});
        const response = await send(server, undefined);
        expect(response.statusCode).toBe(401);
        expect(calls).toHaveLength(0);
    });
});
