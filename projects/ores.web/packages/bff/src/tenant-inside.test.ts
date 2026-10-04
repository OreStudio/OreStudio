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
    uuid,
    type AuthenticatedCaller,
    type OresClient,
    type SessionMode,
} from '@ores/wire-protocol';
import type { Config } from './config.js';
import { buildServer } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

/**
 * A tenant's parties and people, read from system administration.
 *
 * The read runs inside the tenant for as long as it lasts. The stub records
 * what was read inside and what outside, so a route that read the tenant's data
 * with the session's own token, or changed the session, fails here.
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
const ACME = '44444444-4444-4444-4444-444444444444';

function wireTenant(id: string, code: string) {
    return {
        version: 1,
        tenant_id: SYSTEM_TENANT,
        id,
        code,
        name: code === 'system' ? 'System' : 'Acme Corporation',
        type: 'production',
        description: '',
        hostname: code,
        status: 'active',
        is_registration_default: false,
        modified_by: 'system',
        performed_by: 'system',
        change_reason_code: 'new',
        change_commentary: '',
        recorded_at: '2026-10-01 09:00:00Z',
    };
}

const acmeParty = {
    id: '77777777-7777-7777-7777-777777777777',
    short_code: 'ACMCOR',
    full_name: 'Acme Corporation Plc',
    party_category: 'Operational',
    party_type: 'Corporate',
    parent_party_id: null,
    business_center_code: 'GBLO',
    status: 'active',
};

const acmePerson = {
    version: 1,
    id: '88888888-8888-8888-8888-888888888888',
    tenant_id: ACME,
    username: 'priya',
    full_name: 'Priya Natarajan',
    email: 'priya@acme.example',
    account_type: 'user',
    job_title: '',
    modified_by: 'system',
    change_reason_code: 'new',
    change_commentary: '',
    performed_by: 'system',
    recorded_at: '2026-10-01 09:00:00Z',
};

function buildTestServer(options: { mode?: SessionMode; refuseEntry?: string } = {}) {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const calls: string[] = [];
    const reply = (subject: string, body: unknown): unknown => {
        if (subject === 'iam.v1.tenants.get') {
            const code = (body as { key: { code: string } }).key.code;
            if (code === 'acme_corporation') {
                return { result: { outcome: 'ok' }, tenant: wireTenant(ACME, code) };
            }
            if (code === 'system') {
                return { result: { outcome: 'ok' }, tenant: wireTenant(SYSTEM_TENANT, code) };
            }
            return { result: { outcome: 'missing' }, tenant: null };
        }
        if (subject === 'refdata.v1.parties.list') {
            return { result: { outcome: 'ok' }, parties: [acmeParty], total: 1 };
        }
        if (subject === 'iam.v1.accounts.list') {
            return { accounts: [acmePerson], total: 1 };
        }
        throw new Error(`unexpected subject ${subject}`);
    };
    const client = {
        async callAuthenticated(
            subject: string,
            body: unknown,
            schema: { parse: (value: unknown) => unknown },
        ): Promise<unknown> {
            calls.push(`outside ${subject}`);
            return schema.parse(reply(subject, body));
        },
        async readInsideTenant<T>(
            tenantId: string,
            read: (caller: AuthenticatedCaller) => Promise<T>,
        ): Promise<T> {
            calls.push(`enter ${tenantId}`);
            if (options.refuseEntry !== undefined) {
                throw new OperationFailedError('iam.v1.ops.enter_tenant', options.refuseEntry);
            }
            try {
                return await read({
                    callAuthenticated: async (subject, body, schema) => {
                        calls.push(`inside ${subject}`);
                        return schema.parse(reply(subject, body));
                    },
                });
            } finally {
                calls.push('leave');
            }
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
    return { server, cookies: { ores_web_session: session.id }, calls };
}

describe("a tenant's data, read from system administration", () => {
    it("reads the tenant's parties inside it, and leaves", async () => {
        const { server, cookies, calls } = buildTestServer();

        const response = await server.inject({
            method: 'GET',
            url: '/api/tenants/acme_corporation/parties?offset=0&limit=20',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({
            totalCount: 1,
            parties: [{ code: 'ACMCOR', name: 'Acme Corporation Plc', parentId: null }],
        });
        expect(calls).toEqual([
            'outside iam.v1.tenants.get',
            `enter ${ACME}`,
            'inside refdata.v1.parties.list',
            'leave',
        ]);
    });

    it("reads the tenant's people inside it, and leaves", async () => {
        const { server, cookies, calls } = buildTestServer();

        const response = await server.inject({
            method: 'GET',
            url: '/api/tenants/acme_corporation/people',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({
            totalCount: 1,
            accounts: [{ username: 'priya', fullName: 'Priya Natarajan' }],
        });
        expect(calls).toEqual([
            'outside iam.v1.tenants.get',
            `enter ${ACME}`,
            'inside iam.v1.accounts.list',
            'leave',
        ]);
    });

    /*
     * The read is the screen's, not the session's: before and after it, the
     * session reads as system administration in its own tenant.
     */
    it('leaves the session in system administration', async () => {
        const { server, cookies } = buildTestServer();

        await server.inject({
            method: 'GET',
            url: '/api/tenants/acme_corporation/parties',
            cookies,
        });
        const session = await server.inject({ method: 'GET', url: '/api/session', cookies });
        await server.close();

        expect(session.json()).toMatchObject({
            mode: 'system-administration',
            tenantName: 'System',
        });
        expect(session.json()).not.toHaveProperty('actingIn');
    });

    it('answers no tenant for an unknown code or the system tenant, and enters nothing', async () => {
        for (const code of ['nobody', 'system']) {
            const { server, cookies, calls } = buildTestServer();

            const response = await server.inject({
                method: 'GET',
                url: `/api/tenants/${code}/parties`,
                cookies,
            });
            await server.close();

            expect(response.statusCode).toBe(404);
            expect(calls.filter((call) => call.startsWith('enter'))).toHaveLength(0);
        }
    });

    it('passes the server refusal of the entry on', async () => {
        const { server, cookies } = buildTestServer({ refuseEntry: 'Not permitted.' });

        const response = await server.inject({
            method: 'GET',
            url: '/api/tenants/acme_corporation/people',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(403);
    });

    it('refuses a session outside system administration without asking the server', async () => {
        const { server, cookies, calls } = buildTestServer({ mode: 'tenant-administration' });

        const response = await server.inject({
            method: 'GET',
            url: '/api/tenants/acme_corporation/parties',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(403);
        expect(calls).toHaveLength(0);
    });
});
