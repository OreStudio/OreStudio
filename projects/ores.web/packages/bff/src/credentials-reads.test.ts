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
import { SUBJECTS, type OresClient } from '@ores/wire-protocol';
import type { Config } from './config.js';
import { buildServer } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

/**
 * The reads the credentials screens start from.
 *
 * Two things are asserted here that a screen would otherwise find out in the
 * browser: the subject and the body each route sends, and the translation of
 * the wire's snake_case page into the shape the BFF answers with. The count is
 * the one worth a test of its own, because the wire calls it `total` while
 * other components call it `total_available_count`, and a schema that reads the
 * wrong name answers zero rather than failing.
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

const ACCOUNT_ID = '11111111-1111-1111-1111-111111111111';
const TENANT_ID = 'ffffffff-ffff-ffff-ffff-ffffffffffff';

/** One account as the server writes it, credentials absent. */
const wireAccount = {
    version: 3,
    id: ACCOUNT_ID,
    tenant_id: TENANT_ID,
    username: 'jdoe',
    full_name: 'Jane Doe',
    email: 'jane.doe@example.com',
    account_type: 'user',
    job_title: 'Analyst',
    reports_to_account_id: null,
    default_party_id: null,
    modified_by: 'admin',
    change_reason_code: 'new',
    change_commentary: '',
    performed_by: 'admin',
    recorded_at: '2026-09-30 12:00:00Z',
};

/** One login record as the server writes it. */
const wireLoginInfo = {
    tenant_id: TENANT_ID,
    account_id: ACCOUNT_ID,
    last_ip: '203.0.113.44',
    last_attempt_ip: '203.0.113.44',
    failed_logins: 7,
    locked: true,
    last_login: '2026-09-29 21:03:00Z',
    online: true,
    password_reset_required: false,
};

interface Call {
    readonly subject: string;
    readonly body: unknown;
}

function buildTestServer(replies: Readonly<Record<string, unknown>>): {
    readonly server: ReturnType<typeof buildServer>;
    readonly sessionId: string;
    readonly calls: Call[];
} {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const calls: Call[] = [];
    const client = {
        /*
         * The real client decodes the reply and parses it with the schema the
         * caller supplied, which is where the wire's names become the shape a
         * screen reads. The fake parses too, so a test fails when the schema
         * and the canned reply stop agreeing.
         */
        async callAuthenticated(
            subject: string,
            body: unknown,
            schema: { parse: (value: unknown) => unknown },
        ): Promise<unknown> {
            calls.push({ subject, body });
            if (!(subject in replies)) {
                throw new Error(`No canned reply for ${subject}`);
            }
            return schema.parse(replies[subject]);
        },
        async close(): Promise<void> {
            return undefined;
        },
    } as unknown as OresClient;
    const session = sessions.create({
        client,
        session: {
            kind: 'active',
            token: 'token',
            accountId: ACCOUNT_ID,
            tenantId: TENANT_ID,
            tenantName: 'Northwind Capital',
            version: 'v0.0.25 (test)',
            username: 'tenant_admin',
            email: 'admin@northwind.example.com',
            party: northwind,
            availableParties: [northwind],
            accessLifetimeSeconds: 1800,
            passwordResetRequired: false,
            sessionId: '33333333-3333-3333-3333-333333333333',
        },
        username: 'tenant_admin',
        email: 'admin@northwind.example.com',
        accountId: ACCOUNT_ID,
        tenantId: TENANT_ID,
        tenantName: 'Northwind Capital',
        version: 'v0.0.25 (test)',
        availableParties: [northwind],
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

describe('GET /api/accounts', () => {
    it('lists the tenant\u2019s accounts, counting them by the wire\u2019s own name', async () => {
        const { server, sessionId, calls } = buildTestServer({
            [SUBJECTS.listAccounts]: { accounts: [wireAccount], total: 1 },
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/accounts',
            cookies: { ores_web_session: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            accounts: [
                {
                    version: 3,
                    id: ACCOUNT_ID,
                    tenantId: TENANT_ID,
                    username: 'jdoe',
                    fullName: 'Jane Doe',
                    email: 'jane.doe@example.com',
                    accountType: 'user',
                    jobTitle: 'Analyst',
                    reportsToAccountId: null,
                    defaultPartyId: null,
                    imageId: null,
                    modifiedBy: 'admin',
                    changeReasonCode: 'new',
                    changeCommentary: '',
                    performedBy: 'admin',
                    recordedAt: '2026-09-30 12:00:00Z',
                },
            ],
            totalCount: 1,
        });
        expect(calls).toEqual([
            {
                subject: 'iam.v1.accounts.list',
                body: {
                    offset: 0,
                    limit: 100,
                    order: { field: '', descending: false },
                    as_of: null,
                    filter: null,
                },
            },
        ]);

        await server.close();
    });

    it('pages with what the caller asked for', async () => {
        const { server, sessionId, calls } = buildTestServer({
            [SUBJECTS.listAccounts]: { accounts: [], total: 0 },
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/accounts?offset=20&limit=5',
            cookies: { ores_web_session: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({ accounts: [], totalCount: 0 });
        expect(calls).toEqual([
            {
                subject: 'iam.v1.accounts.list',
                body: {
                    offset: 20,
                    limit: 5,
                    order: { field: '', descending: false },
                    as_of: null,
                    filter: null,
                },
            },
        ]);

        await server.close();
    });

    it('refuses a request with no session', async () => {
        const { server, calls } = buildTestServer({
            [SUBJECTS.listAccounts]: { accounts: [], total: 0 },
        });

        const response = await server.inject({ method: 'GET', url: '/api/accounts' });

        expect(response.statusCode).toBe(401);
        expect(response.json()).toMatchObject({ code: 'not-authenticated' });
        expect(calls).toEqual([]);

        await server.close();
    });

    it('refuses a page size the server would not accept', async () => {
        const { server, sessionId, calls } = buildTestServer({
            [SUBJECTS.listAccounts]: { accounts: [], total: 0 },
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/accounts?limit=0',
            cookies: { ores_web_session: sessionId },
        });

        expect(response.statusCode).toBe(400);
        expect(response.json()).toMatchObject({ code: 'invalid-request' });
        expect(calls).toEqual([]);

        await server.close();
    });
});

describe('GET /api/accounts/:username', () => {
    it('answers one account, keyed by the username the server takes', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'iam.v1.accounts.get': { account: wireAccount },
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/accounts/jdoe',
            cookies: { ores_web_session: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({
            account: { username: 'jdoe', fullName: 'Jane Doe' },
        });
        expect(calls).toEqual([
            { subject: 'iam.v1.accounts.get', body: { key: { username: 'jdoe' } } },
        ]);

        await server.close();
    });

    it('answers with nothing when the account has gone since the list was read', async () => {
        const { server, sessionId } = buildTestServer({
            'iam.v1.accounts.get': { account: null },
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/accounts/jdoe',
            cookies: { ores_web_session: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({ account: null });

        await server.close();
    });
});

describe('GET /api/login-info', () => {
    it('lists the login records and renames the wire\u2019s fields', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'iam.v1.login_info.list': { login_info: [wireLoginInfo], total: 1 },
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/login-info',
            cookies: { ores_web_session: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            loginInfo: [
                {
                    tenantId: TENANT_ID,
                    accountId: ACCOUNT_ID,
                    lastIp: '203.0.113.44',
                    lastAttemptIp: '203.0.113.44',
                    failedLogins: 7,
                    locked: true,
                    lastLogin: '2026-09-29 21:03:00Z',
                    online: true,
                    passwordResetRequired: false,
                },
            ],
            totalCount: 1,
        });
        expect(calls).toEqual([
            {
                subject: 'iam.v1.login_info.list',
                body: {
                    offset: 0,
                    limit: 100,
                    order: { field: '', descending: false },
                    filter: null,
                },
            },
        ]);

        await server.close();
    });
});

describe('GET /api/login-info/:accountId', () => {
    it('answers one account\u2019s login record', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'iam.v1.login_info.get': { login_info: wireLoginInfo },
        });

        const response = await server.inject({
            method: 'GET',
            url: `/api/login-info/${ACCOUNT_ID}`,
            cookies: { ores_web_session: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({ loginInfo: { failedLogins: 7, locked: true } });
        expect(calls).toEqual([
            { subject: 'iam.v1.login_info.get', body: { key: { account_id: ACCOUNT_ID } } },
        ]);

        await server.close();
    });

    it('answers with nothing for an account that has never signed in', async () => {
        const { server, sessionId } = buildTestServer({
            'iam.v1.login_info.get': { login_info: null },
        });

        const response = await server.inject({
            method: 'GET',
            url: `/api/login-info/${ACCOUNT_ID}`,
            cookies: { ores_web_session: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({ loginInfo: null });

        await server.close();
    });

    it('refuses an account id that is not one', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'iam.v1.login_info.get': { login_info: null },
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/login-info/not-a-uuid',
            cookies: { ores_web_session: sessionId },
        });

        expect(response.statusCode).toBe(400);
        expect(response.json()).toMatchObject({ code: 'invalid-request' });
        expect(calls).toEqual([]);

        await server.close();
    });
});
