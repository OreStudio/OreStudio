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
    OperationFailedError,
    TenantSessionEndedError,
    uuid,
    type EnteredTenant,
    type OresClient,
    type SessionMode,
} from '@ores/wire-protocol';
import type { Config } from './config.js';
import { buildServer } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

/**
 * A system administrator entering and leaving a tenant.
 *
 * The client owns the entered tenant, so the stub here keeps it the way the
 * real client does: entering sets it, leaving and a lapsed tenant session
 * clear it. What is asserted is what the browser sees and what the routes
 * refuse.
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

const SYSTEM_TENANT = 'ffffffff-ffff-ffff-ffff-ffffffffffff';
const ACME: EnteredTenant = {
    tenantId: '44444444-4444-4444-4444-444444444444',
    tenantCode: 'acme_corporation',
    tenantName: 'Acme Corporation',
    partyId: uuid('66666666-6666-6666-6666-666666666666'),
    partyName: 'System Party',
    accessLifetimeSeconds: 900,
};

interface StubClient {
    entered: EnteredTenant | undefined;
    calls: string[];
}

function buildTestServer(
    options: {
        readonly mode?: SessionMode;
        readonly refuseEntry?: string;
        readonly lapseOnRead?: boolean;
    } = {},
) {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const stub: StubClient = { entered: undefined, calls: [] };
    const client = {
        get enteredTenant() {
            return stub.entered;
        },
        async enterTenant(tenantId: string): Promise<EnteredTenant> {
            stub.calls.push(`enter ${tenantId}`);
            if (options.refuseEntry !== undefined) {
                throw new OperationFailedError('iam.v1.ops.enter_tenant', options.refuseEntry);
            }
            stub.entered = ACME;
            return ACME;
        },
        async leaveTenant(): Promise<void> {
            stub.calls.push('leave');
            stub.entered = undefined;
        },
        async callAuthenticated(
            subject: string,
            _body: unknown,
            schema: { parse: (value: unknown) => unknown },
        ): Promise<unknown> {
            stub.calls.push(subject);
            if (options.lapseOnRead === true && stub.entered !== undefined) {
                stub.entered = undefined;
                throw new TenantSessionEndedError();
            }
            return schema.parse({ accounts: [], total_available_count: 0, success: true });
        },
        async close(): Promise<void> {
            return undefined;
        },
    } as unknown as OresClient;
    const systemParty = {
        id: uuid('22222222-2222-2222-2222-222222222222'),
        name: 'System Party',
        partyCategory: 'System',
        businessCenterCode: 'WRLD',
    };
    const own = {
        accountId: '11111111-1111-1111-1111-111111111111',
        tenantId: SYSTEM_TENANT,
        tenantName: 'System',
        version: 'v0.0.25 (test)',
        username: 'admin',
        email: 'admin@example.com',
        availableParties: [systemParty],
        accessLifetimeSeconds: 1800,
        passwordResetRequired: false,
        sessionId: '33333333-3333-3333-3333-333333333333',
    };
    const session = sessions.create({
        ...own,
        client,
        session: { ...own, kind: 'active', token: 't', party: systemParty },
        mode: options.mode ?? 'system-administration',
    });
    const server = buildServer({
        config,
        site: siteConfiguration(),
        sessions,
        createClient: () => ({ client, connect: async () => undefined }),
    });
    const cookies = { ores_web_session: session.id };
    return { server, cookies, stub };
}

describe('entering and leaving a tenant', () => {
    it('enters the tenant and reads as the tenant from then on', async () => {
        const { server, cookies, stub } = buildTestServer();

        const entered = await server.inject({
            method: 'POST',
            url: '/api/session/tenant',
            cookies,
            payload: { tenantId: ACME.tenantId },
        });
        const read = await server.inject({ method: 'GET', url: '/api/session', cookies });
        await server.close();

        expect(entered.statusCode).toBe(200);
        expect(stub.calls).toEqual([`enter ${ACME.tenantId}`]);
        expect(read.json()).toMatchObject({
            mode: 'tenant-administration',
            tenantId: ACME.tenantId,
            tenantName: 'Acme Corporation',
            party: { id: ACME.partyId, name: 'System Party' },
            actingIn: {
                tenantId: ACME.tenantId,
                tenantCode: 'acme_corporation',
                tenantName: 'Acme Corporation',
            },
        });
    });

    it('passes the server refusal on, and stays outside', async () => {
        const { server, cookies } = buildTestServer({ refuseEntry: 'No tenant has this id.' });

        const entered = await server.inject({
            method: 'POST',
            url: '/api/session/tenant',
            cookies,
            payload: { tenantId: ACME.tenantId },
        });
        const read = await server.inject({ method: 'GET', url: '/api/session', cookies });
        await server.close();

        expect(entered.statusCode).toBe(403);
        expect(entered.json().message).toBe('No tenant has this id.');
        expect(read.json()).toMatchObject({ mode: 'system-administration', actingIn: null });
    });

    it('refuses a session outside system administration without asking the server', async () => {
        const { server, cookies, stub } = buildTestServer({ mode: 'application' });

        const entered = await server.inject({
            method: 'POST',
            url: '/api/session/tenant',
            cookies,
            payload: { tenantId: ACME.tenantId },
        });
        await server.close();

        expect(entered.statusCode).toBe(403);
        expect(stub.calls).toEqual([]);
    });

    /*
     * Inside a tenant, data is read only: a change is refused before its
     * route runs, so even a route that checks no permission cannot write.
     */
    it('refuses every change while inside, and still reads', async () => {
        const { server, cookies, stub } = buildTestServer();
        await server.inject({
            method: 'POST',
            url: '/api/session/tenant',
            cookies,
            payload: { tenantId: ACME.tenantId },
        });
        stub.calls.length = 0;

        const change = await server.inject({
            method: 'POST',
            url: '/api/account/password',
            cookies,
            payload: { currentPassword: 'a', newPassword: 'b' },
        });
        const read = await server.inject({ method: 'GET', url: '/api/accounts', cookies });
        await server.close();

        expect(change.statusCode).toBe(403);
        expect(change.json().message).toContain('read only inside a tenant');
        expect(read.statusCode).toBe(200);
        expect(stub.calls).toEqual(['iam.v1.accounts.list']);
    });

    it('leaves the tenant and returns to system administration', async () => {
        const { server, cookies, stub } = buildTestServer();
        await server.inject({
            method: 'POST',
            url: '/api/session/tenant',
            cookies,
            payload: { tenantId: ACME.tenantId },
        });

        const left = await server.inject({ method: 'DELETE', url: '/api/session/tenant', cookies });
        const again = await server.inject({
            method: 'DELETE',
            url: '/api/session/tenant',
            cookies,
        });
        await server.close();

        expect(left.statusCode).toBe(200);
        expect(left.json()).toMatchObject({
            mode: 'system-administration',
            tenantId: SYSTEM_TENANT,
            actingIn: null,
        });
        expect(stub.calls).toContain('leave');
        expect(again.statusCode).toBe(400);
    });

    it('says the time inside ended, and the session is back outside', async () => {
        const { server, cookies } = buildTestServer({ lapseOnRead: true });
        await server.inject({
            method: 'POST',
            url: '/api/session/tenant',
            cookies,
            payload: { tenantId: ACME.tenantId },
        });

        const read = await server.inject({ method: 'GET', url: '/api/accounts', cookies });
        const session = await server.inject({ method: 'GET', url: '/api/session', cookies });
        await server.close();

        expect(read.statusCode).toBe(409);
        expect(read.json().code).toBe('tenant-session-ended');
        expect(session.json()).toMatchObject({ mode: 'system-administration', actingIn: null });
    });
});
