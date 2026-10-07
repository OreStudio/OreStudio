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
 * The build a session was opened against.
 *
 * A deployment that has already bootstrapped is signed in to, not set up, so
 * the version cannot come from the bootstrap read alone: the answer that opens
 * a session states it, and every later read of that session repeats it. What
 * is asserted here is both halves of that, because a client that signs in
 * should not have to ask a second question to say which build it is talking
 * to.
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

const VERSION = 'v0.0.25 [x64-linux] (local abc1234)';

const DATABASE = {
    fingerprint: 'e4803181e989327c',
    environment: 'local',
    commit: 'abc1234',
    created: '2026-10-07 21:17:00+00',
};

function buildTestServer(): ReturnType<typeof buildServer> {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const client = {
        async bootstrapStatus(): Promise<unknown> {
            return { isInBootstrapMode: false, message: '', version: VERSION };
        },
        async login(): Promise<unknown> {
            return {
                kind: 'active',
                token: 'token',
                accountId: '11111111-1111-1111-1111-111111111111',
                tenantId: 'ffffffff-ffff-ffff-ffff-ffffffffffff',
                tenantName: 'Northwind Capital',
                version: VERSION,
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
        site: siteConfiguration(),
        sessions,
        createClient: () => ({ client, connect: async () => undefined }),
    });
}

describe('POST /api/session', () => {
    it('states the build the session was opened against, and keeps stating it', async () => {
        const server = buildTestServer();

        const login = await server.inject({
            method: 'POST',
            url: '/api/session',
            payload: { username: 'tenant_admin@northwind.example.com', password: 'Secret-1!' },
        });

        expect(login.statusCode).toBe(200);
        expect(login.json()).toMatchObject({ outcome: 'active' });
        expect(login.json().session.version).toBe(VERSION);

        const cookie = login.cookies[0];
        const session = await server.inject({
            method: 'GET',
            url: '/api/session',
            cookies: { [cookie?.name ?? 'ores_web_session']: cookie?.value ?? '' },
        });

        expect(session.statusCode).toBe(200);
        expect(session.json()).toMatchObject({ version: VERSION });

        await server.close();
    });
});
