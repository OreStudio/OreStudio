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
import { buildServer, sessionCookieName } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

/**
 * The session cookie is named after the environment.
 *
 * Every environment is served from the same host on its own port, and a cookie
 * carries no port, so one shared name lets one environment overwrite another's
 * session. These cases pin the name to the environment and pin that a cookie
 * written for one environment is invisible to another.
 */

const config: Config = {
    port: 0,
    host: '127.0.0.1',
    logLevel: 'silent',
    session: { ttlSeconds: 3600, cookieSecure: false },
    allowedOrigins: [],
    loginAttemptsPerMinute: 100,
};

const CONFIG_PATH = resolve(
    dirname(fileURLToPath(import.meta.url)),
    '../../../config/environments.json',
);

const northwind = {
    id: '22222222-2222-2222-2222-222222222222',
    name: 'Northwind Capital',
    partyCategory: 'Operational',
    businessCenterCode: 'GBLO',
};

const DATABASE = {
    fingerprint: 'e4803181e989327c',
    environment: 'local',
    commit: 'abc1234',
    created: '2026-10-07 21:17:00+00',
};

function siteConfiguration(environmentId: string): ReturnType<typeof loadSiteConfiguration> {
    return loadSiteConfiguration({
        environment: { [SITE_CONFIG_VARIABLE]: CONFIG_PATH },
        environmentId,
    });
}

function buildTestServer(environmentId: string): ReturnType<typeof buildServer> {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const client = {
        async bootstrapStatus(): Promise<unknown> {
            return { isInBootstrapMode: false, message: '', version: 'v0.0.25 (test)' };
        },
        async login(): Promise<unknown> {
            return {
                kind: 'active',
                token: 'token',
                accountId: '11111111-1111-1111-1111-111111111111',
                tenantId: 'ffffffff-ffff-ffff-ffff-ffffffffffff',
                tenantName: 'Northwind Capital',
                tenantBootstrapping: false,
                version: 'v0.0.25 (test)',
                database: DATABASE,
                username: 'tenant_admin',
                email: 'admin@northwind.example.com',
                party: northwind,
                availableParties: [northwind],
                accessLifetimeSeconds: 1800,
                passwordResetRequired: false,
                sessionId: '33333333-3333-3333-3333-333333333333',
            };
        },
        async close(): Promise<void> {
            return undefined;
        },
    } as unknown as OresClient;
    return buildServer({
        config,
        site: siteConfiguration(environmentId),
        sessions,
        createClient: () => ({ client, connect: async () => undefined }),
    });
}

function signIn(server: ReturnType<typeof buildServer>) {
    return server.inject({
        method: 'POST',
        url: '/api/session',
        payload: { username: 'tenant_admin@northwind.example.com', password: 'Secret-1!' },
    });
}

describe('the session cookie name', () => {
    it('differs between two environments served from one host', () => {
        expect(sessionCookieName('brave_hopper')).not.toBe(sessionCookieName('bright_faraday'));
    });

    it('is stable for one environment', () => {
        expect(sessionCookieName('eager_maxwell')).toBe('ores_web_session_eager_maxwell');
        expect(sessionCookieName('brave_hopper')).toBe('ores_web_session_brave_hopper');
    });

    it('stays a valid cookie-name token when the id holds other characters', () => {
        const name = sessionCookieName('brave hopper/a.b');
        expect(name).toBe('ores_web_session_brave_hopper_a_b');
        expect(name).toMatch(/^[A-Za-z0-9_-]+$/);
    });
});

describe('a session on a shared host', () => {
    it('is written under the name of the environment that signed in', async () => {
        const server = buildTestServer('eager_maxwell');
        const login = await signIn(server);
        const name = login.cookies[0]?.name;
        await server.close();

        expect(login.statusCode).toBe(200);
        expect(name).toBe(sessionCookieName('eager_maxwell'));
    });

    it('is not read when it arrives under another environment name', async () => {
        const server = buildTestServer('eager_maxwell');
        const login = await signIn(server);
        const value = login.cookies[0]?.value ?? '';

        const own = await server.inject({
            method: 'GET',
            url: '/api/session',
            cookies: { [sessionCookieName('eager_maxwell')]: value },
        });
        const foreign = await server.inject({
            method: 'GET',
            url: '/api/session',
            cookies: { [sessionCookieName('brave_hopper')]: value },
        });
        await server.close();

        expect(own.statusCode).toBe(200);
        expect(foreign.statusCode).toBe(401);
    });

    it('clears the cookie of its own environment', async () => {
        const server = buildTestServer('eager_maxwell');
        const login = await signIn(server);
        const value = login.cookies[0]?.value ?? '';

        const cleared = await server.inject({
            method: 'DELETE',
            url: '/api/session',
            cookies: { [sessionCookieName('eager_maxwell')]: value },
        });
        await server.close();

        const names = cleared.cookies.map((cookie) => cookie.name);
        expect(names).toContain(sessionCookieName('eager_maxwell'));
        expect(names).not.toContain(sessionCookieName('brave_hopper'));
    });
});
