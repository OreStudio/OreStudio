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
 * The mode the browser is given, on the answer that opens the session and on
 * every read of it afterwards.
 *
 * The mode is what the shell draws its menu from, so it is asserted through the
 * routes rather than on the rule: a caller that signs in and reads its session
 * back sees the context it is acting in, and the browser is never given a fact
 * it would have to work out for itself.
 */

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

const systemTenant = 'ffffffff-ffff-ffff-ffff-ffffffffffff';
const northwindTenant = '44444444-4444-4444-4444-444444444444';

const party = {
    id: '22222222-2222-2222-2222-222222222222',
    name: 'Northwind Capital',
    partyCategory: 'Operational',
    businessCenterCode: 'GBLO',
};

function buildTestServer(tenantId: string, tenantName: string): ReturnType<typeof buildServer> {
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
                tenantId,
                tenantName,
                tenantBootstrapping: false,
                version: 'v0.0.25 (test)',
                database: {
                    fingerprint: 'e4803181e989327c',
                    environment: 'test',
                    commit: 'abc1234',
                    created: '2026-10-07 21:17:00+00',
                },
                username: 'tenant_admin',
                email: 'admin@northwind.example.com',
                party,
                availableParties: [party],
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

describe('the mode a session is given', () => {
    it('reads system administration for an account of the system tenant, and keeps saying so', async () => {
        const server = buildTestServer(systemTenant, 'System');

        const login = await server.inject({
            method: 'POST',
            url: '/api/session',
            payload: { username: 'super_admin@system.ores', password: 'Secret-1!' },
        });

        expect(login.statusCode).toBe(200);
        expect(login.json()).toMatchObject({
            outcome: 'active',
            session: { mode: 'system-administration' },
        });

        const cookie = login.cookies[0];
        const session = await server.inject({
            method: 'GET',
            url: '/api/session',
            cookies: { [cookie?.name ?? SESSION_COOKIE]: cookie?.value ?? '' },
        });

        expect(session.statusCode).toBe(200);
        expect(session.json()).toMatchObject({ mode: 'system-administration' });

        await server.close();
    });

    it('reads the application for an account of any other tenant', async () => {
        const server = buildTestServer(northwindTenant, 'Northwind Capital');

        const login = await server.inject({
            method: 'POST',
            url: '/api/session',
            payload: { username: 'tenant_admin@northwind.example.com', password: 'Secret-1!' },
        });

        expect(login.statusCode).toBe(200);
        expect(login.json()).toMatchObject({
            outcome: 'active',
            session: { mode: 'application' },
        });

        await server.close();
    });
});
