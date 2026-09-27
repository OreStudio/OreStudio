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
 * The route the tenant journey opens on.
 *
 * It is the browser's only way to the starting points: the client method
 * behind it joins three subjects and the route serves the result, so what is
 * asserted here is the body a browser receives, not the call the route makes.
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

interface TestServer {
    readonly server: ReturnType<typeof buildServer>;
    readonly sessionId: string;
}

function buildTestServer(profiles: unknown): TestServer {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const client = {
        async seedProfiles(): Promise<unknown> {
            return profiles;
        },
        async close(): Promise<void> {
            return undefined;
        },
    } as unknown as OresClient;
    const session = sessions.create({
        client,
        session: null,
        username: 'admin',
        email: 'admin@acme.test',
        accountId: '11111111-1111-1111-1111-111111111111',
        tenantId: 'ffffffff-ffff-ffff-ffff-ffffffffffff',
        tenantName: 'System',
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
    };
}

const emptyOperational = {
    code: 'empty_operational',
    name: 'Empty operational',
    description: 'A production tenant: the base reference data and the parties you name.',
    audience: 'For production',
    tenant: { name: '', code: '', hostname: '', adminUsername: '', adminEmail: '' },
    inheritsAdminPassword: false,
    forcePasswordChange: true,
    order: 10,
    steps: [
        { kind: 'publish_bundle', order: 10 },
        { kind: 'import_lei_hierarchy', order: 20 },
        { kind: 'provision_party', order: 30 },
    ],
    parameters: [
        {
            name: 'counterparty_count',
            dataType: 'integer',
            defaultValue: '50',
            required: true,
            description: 'How many counterparties the tenant starts with.',
            order: 10,
        },
        {
            name: 'gleif_root_lei',
            dataType: 'string',
            defaultValue: '',
            required: false,
            description: 'GLEIF root LEI to import the parties under.',
            order: 20,
        },
    ],
};

const acmeDemo = {
    code: 'acme_demo',
    name: 'ACME demo',
    description: 'The Acme Corporation holding group. Every record is test data.',
    audience: 'For demonstration',
    tenant: {
        name: 'Acme Corporation',
        code: 'acme_corporation',
        hostname: 'acme_corporation',
        adminUsername: 'tenant_admin',
        adminEmail: 'admin@acme_corporation.com',
    },
    inheritsAdminPassword: true,
    forcePasswordChange: false,
    order: 20,
    steps: [
        { kind: 'publish_bundle', order: 10 },
        { kind: 'import_lei_hierarchy', order: 20 },
        { kind: 'provision_party', order: 30 },
        { kind: 'load_staff', order: 40 },
        { kind: 'attach_photos', order: 50 },
        { kind: 'start_market_feeds', order: 60 },
    ],
    parameters: [],
};

describe('GET /api/seed-profiles', () => {
    it('serves the starting points the client read', async () => {
        const { server, sessionId } = buildTestServer([emptyOperational, acmeDemo]);

        const response = await server.inject({
            method: 'GET',
            url: '/api/seed-profiles',
            cookies: { ores_web_session: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({ profiles: [emptyOperational, acmeDemo] });

        await server.close();
    });

    it('refuses a read with no session', async () => {
        const { server } = buildTestServer([emptyOperational]);

        const response = await server.inject({ method: 'GET', url: '/api/seed-profiles' });

        expect(response.statusCode).toBe(401);
        expect(response.json()).toMatchObject({ code: 'not-authenticated' });

        await server.close();
    });
});
