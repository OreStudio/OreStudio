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
import type { OresClient, ProvisionTenantRequest } from '@ores/wire-protocol';
import type { Config } from './config.js';
import { buildServer } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

/**
 * The route the tenant journey provisions through.
 *
 * It is the browser's only way to the provision verb: the route parses the
 * body, calls the client method and serves the result, so what is asserted
 * here is the body a browser sends and receives, and the input the client
 * method was given.
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
    readonly provisioned: ProvisionTenantRequest[];
}

function buildTestServer(result: unknown): TestServer {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const provisioned: ProvisionTenantRequest[] = [];
    const client = {
        async provisionTenant(input: ProvisionTenantRequest): Promise<unknown> {
            provisioned.push(input);
            return result;
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
        provisioned,
    };
}

const body = {
    profileCode: 'empty_operational',
    tenantCode: 'northwind',
    tenantName: 'Northwind Capital',
    tenantHostname: 'northwind.example.com',
    tenantDescription: '',
    adminUsername: 'tenant_admin',
    adminEmail: 'admin@northwind.example.com',
    adminPassword: 'Secure-Password-123',
    parameters: { root_lei: '9695ACMEGROUP0000030', counterparty_size: 'large' },
};

const provisioned = {
    success: true,
    message: '',
    instanceId: '9c1f0f5a-6bd2-4f2a-9a4a-6f1a3a2b4c5d',
    tenantId: '0d0f9f24-6f3a-4c2b-9c2e-1a2b3c4d5e6f',
    accountId: '7a6b5c4d-3e2f-4a1b-8c9d-0e1f2a3b4c5d',
};

describe('POST /api/provision-tenant', () => {
    it('provisions the tenant the browser asked for and serves the run it started', async () => {
        const { server, sessionId, provisioned: calls } = buildTestServer(provisioned);

        const response = await server.inject({
            method: 'POST',
            url: '/api/provision-tenant',
            cookies: { ores_web_session: sessionId },
            payload: body,
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual(provisioned);
        expect(calls).toEqual([body]);

        await server.close();
    });

    it('serves a refusal the server stated as a result, not as a failed call', async () => {
        const refusal = {
            success: false,
            message: "The value 'huge' for 'counterparty_size' is not one of: small, large.",
            instanceId: '',
            tenantId: '',
            accountId: '',
        };
        const { server, sessionId } = buildTestServer(refusal);

        const response = await server.inject({
            method: 'POST',
            url: '/api/provision-tenant',
            cookies: { ores_web_session: sessionId },
            payload: body,
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual(refusal);

        await server.close();
    });

    it('refuses a request with no session', async () => {
        const { server } = buildTestServer(provisioned);

        const response = await server.inject({
            method: 'POST',
            url: '/api/provision-tenant',
            payload: body,
        });

        expect(response.statusCode).toBe(401);
        expect(response.json()).toMatchObject({ code: 'not-authenticated' });

        await server.close();
    });

    it('refuses a body with no profile code', async () => {
        const { server, sessionId, provisioned: calls } = buildTestServer(provisioned);

        const response = await server.inject({
            method: 'POST',
            url: '/api/provision-tenant',
            cookies: { ores_web_session: sessionId },
            payload: { ...body, profileCode: '' },
        });

        expect(response.statusCode).toBe(400);
        expect(response.json()).toMatchObject({ code: 'invalid-request' });
        expect(calls).toEqual([]);

        await server.close();
    });
});
