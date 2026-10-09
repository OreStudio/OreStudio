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
import { buildServer, sessionCookieName } from './server.js';
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

const GB_FLAG = '99999999-9999-9999-9999-999999999991';
const PRIYA_PHOTO = '99999999-9999-9999-9999-999999999992';

const images: Record<string, { mime_type: string; data: string | number[] }> = {
    [GB_FLAG]: { mime_type: 'image/svg+xml', data: '<svg id="gb"/>' },
    [PRIYA_PHOTO]: { mime_type: 'image/jpeg', data: [0xff, 0xd8, 0xff, 0xe0] },
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
    image_id: PRIYA_PHOTO,
    modified_by: 'system',
    change_reason_code: 'new',
    change_commentary: '',
    performed_by: 'system',
    recorded_at: '2026-10-01 09:00:00Z',
};

function buildTestServer(
    options: { mode?: SessionMode; refuseEntry?: string; imagesFail?: boolean } = {},
) {
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
        if (subject === 'iam.v1.accounts.get') {
            const username = (body as { key: { username: string } }).key.username;
            return { account: username === 'priya' ? acmePerson : null };
        }
        if (subject === 'iam.v1.login_info.get') {
            return {
                login_info: {
                    tenant_id: ACME,
                    account_id: acmePerson.id,
                    last_ip: '203.0.113.44',
                    last_attempt_ip: '203.0.113.44',
                    failed_logins: 2,
                    locked: false,
                    last_login: '2026-10-04 09:00:00Z',
                    online: true,
                    password_reset_required: false,
                },
            };
        }
        if (subject === 'iam.v1.sessions.list') {
            const request = body as {
                order: { field: string; descending: boolean };
                filter: { account_id: string };
            };
            expect(request.order).toEqual({ field: 'start_time', descending: true });
            expect(request.filter.account_id).toBe(acmePerson.id);
            return {
                sessions: [
                    {
                        tenant_id: ACME,
                        id: '77777777-0000-0000-0000-000000000001',
                        account_id: acmePerson.id,
                        start_time: '2026-10-04 09:00:00Z',
                        end_time: '',
                        client_ip: '203.0.113.44',
                        client_identifier: 'ores.web',
                        client_version_major: 0,
                        client_version_minor: 25,
                        bytes_sent: 10,
                        bytes_received: 20,
                        country_code: 'GB',
                        protocol: 'https',
                    },
                ],
                total: 1,
            };
        }
        if (subject === 'refdata.v1.business_centres.list') {
            const codes = (body as { filter: { code_one_of: string[] } }).filter.code_one_of;
            expect(codes).toEqual(['GBLO']);
            return {
                result: { outcome: 'ok' },
                centres: [{ code: 'GBLO', country_alpha2_code: 'GB' }],
            };
        }
        if (subject === 'refdata.v1.countries.list') {
            const codes = (body as { filter: { alpha2_code_one_of: string[] } }).filter
                .alpha2_code_one_of;
            expect(codes).toEqual(['GB']);
            return {
                result: { outcome: 'ok' },
                countries: [{ alpha2_code: 'GB', image_id: GB_FLAG }],
            };
        }
        if (subject === 'assets.v1.images.list') {
            if (options.imagesFail === true) throw new Error('assets service unavailable');
            const ids = (body as { filter: { id_one_of: string[] } }).filter.id_one_of;
            return {
                result: { outcome: 'ok' },
                images: ids
                    .filter((id) => images[id] !== undefined)
                    .map((id) => ({ id, ...images[id] })),
            };
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
        tenantBootstrapping: false,
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
    return { server, cookies: { [SESSION_COOKIE]: session.id }, calls };
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
            parties: [
                {
                    code: 'ACMCOR',
                    name: 'Acme Corporation Plc',
                    parentId: null,
                    businessCentreCode: 'GBLO',
                    flagImageId: GB_FLAG,
                },
            ],
        });
        expect(calls).toEqual([
            'outside iam.v1.tenants.get',
            `enter ${ACME}`,
            'inside refdata.v1.parties.list',
            'inside refdata.v1.business_centres.list',
            'inside refdata.v1.countries.list',
            'inside assets.v1.images.list',
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
            accounts: [{ username: 'priya', fullName: 'Priya Natarajan', imageId: PRIYA_PHOTO }],
        });
        expect(calls).toEqual([
            'outside iam.v1.tenants.get',
            `enter ${ACME}`,
            'inside iam.v1.accounts.list',
            'inside assets.v1.images.list',
            'leave',
        ]);
    });

    /*
     * The page read the pictures in its own visit, so fetching one enters
     * nothing: a page of twenty faces costs one entry, not twenty-one.
     */
    it('serves a picture the page named without entering the tenant again', async () => {
        const { server, cookies, calls } = buildTestServer();

        await server.inject({
            method: 'GET',
            url: '/api/tenants/acme_corporation/people',
            cookies,
        });
        calls.length = 0;
        const response = await server.inject({
            method: 'GET',
            url: `/api/tenants/acme_corporation/images/${PRIYA_PHOTO}`,
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(response.headers['content-type']).toBe('image/jpeg');
        expect([...response.rawPayload]).toEqual([0xff, 0xd8, 0xff, 0xe0]);
        expect(calls).toEqual(['outside iam.v1.tenants.get']);
    });

    it('reads a picture inside the tenant when no page has named it', async () => {
        const { server, cookies, calls } = buildTestServer();

        const response = await server.inject({
            method: 'GET',
            url: `/api/tenants/acme_corporation/images/${GB_FLAG}`,
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(response.headers['content-type']).toBe('image/svg+xml');
        expect(response.body).toBe('<svg id="gb"/>');
        expect(calls).toEqual([
            'outside iam.v1.tenants.get',
            `enter ${ACME}`,
            'inside assets.v1.images.list',
            'leave',
        ]);
    });

    it('answers the people page when their pictures cannot be read', async () => {
        const { server, cookies } = buildTestServer({ imagesFail: true });

        const response = await server.inject({
            method: 'GET',
            url: '/api/tenants/acme_corporation/people',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({ accounts: [{ imageId: PRIYA_PHOTO }] });
    });

    /*
     * An account opened from a tenant's People tab shows its sign-in state and
     * its sessions, newest first, read inside the tenant in one visit.
     */
    it("reads one account's sign-ins inside the tenant, newest first", async () => {
        const { server, cookies, calls } = buildTestServer();

        const response = await server.inject({
            method: 'GET',
            url: '/api/tenants/acme_corporation/accounts/priya/sign-ins',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({
            account: { username: 'priya' },
            loginInfo: { failedLogins: 2, locked: false },
            sessions: [{ clientIdentifier: 'ores.web', endTime: '' }],
            totalCount: 1,
        });
        expect(calls.filter((call) => call.startsWith('enter'))).toHaveLength(1);
    });

    it('reads the system tenant accounts as the session, and answers no account it lacks', async () => {
        const { server, cookies, calls } = buildTestServer();

        const found = await server.inject({
            method: 'GET',
            url: '/api/tenants/system/accounts/priya/sign-ins',
            cookies,
        });
        const missing = await server.inject({
            method: 'GET',
            url: '/api/tenants/system/accounts/nobody/sign-ins',
            cookies,
        });
        await server.close();

        expect(found.statusCode).toBe(200);
        expect(missing.statusCode).toBe(404);
        expect(calls.filter((call) => call.startsWith('enter'))).toHaveLength(0);
    });

    it('answers no image for an identifier the tenant does not hold', async () => {
        const { server, cookies } = buildTestServer();

        const unknown = await server.inject({
            method: 'GET',
            url: '/api/tenants/acme_corporation/images/99999999-9999-9999-9999-999999999999',
            cookies,
        });
        const malformed = await server.inject({
            method: 'GET',
            url: '/api/tenants/acme_corporation/images/not-an-id',
            cookies,
        });
        await server.close();

        expect(unknown.statusCode).toBe(404);
        expect(malformed.statusCode).toBe(404);
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

    it('answers no tenant for an unknown code, and enters nothing', async () => {
        const { server, cookies, calls } = buildTestServer();

        const response = await server.inject({
            method: 'GET',
            url: '/api/tenants/nobody/parties',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(404);
        expect(calls.filter((call) => call.startsWith('enter'))).toHaveLength(0);
    });

    /*
     * The system tenant is the session's own, so its parties are read as the
     * session, with nothing to enter or leave.
     */
    it('reads the system tenant as the session, entering nothing', async () => {
        const { server, cookies, calls } = buildTestServer();

        const response = await server.inject({
            method: 'GET',
            url: '/api/tenants/system/parties',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({ parties: [{ code: 'ACMCOR' }] });
        expect(calls).toEqual([
            'outside iam.v1.tenants.get',
            'outside refdata.v1.parties.list',
            'outside refdata.v1.business_centres.list',
            'outside refdata.v1.countries.list',
            'outside assets.v1.images.list',
        ]);
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
