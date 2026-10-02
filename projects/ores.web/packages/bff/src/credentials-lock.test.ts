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
 * The writes the administrator's screen makes.
 *
 * The subject answers with one result per account rather than with a single
 * flag, so the cases worth having are the two the screen has to tell apart: a
 * lock the server accepted, and a lock the server refused for one account. A
 * route that returned the list would push that decision onto every screen, and
 * a route that ignored it would report a refusal as a success.
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

describe('POST /api/accounts/:accountId/lock', () => {
    it('locks the one account the screen named', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'iam.v1.accounts.lock': { results: [{ success: true, message: '' }] },
        });

        const response = await server.inject({
            method: 'POST',
            url: `/api/accounts/${ACCOUNT_ID}/lock`,
            cookies: { ores_web_session: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({ success: true });
        expect(calls).toEqual([
            { subject: 'iam.v1.accounts.lock', body: { account_ids: [ACCOUNT_ID] } },
        ]);

        await server.close();
    });

    it('answers the refusal as a failure, not as a list', async () => {
        const { server, sessionId } = buildTestServer({
            'iam.v1.accounts.lock': {
                results: [{ success: false, message: 'The account is already locked.' }],
            },
        });

        const response = await server.inject({
            method: 'POST',
            url: `/api/accounts/${ACCOUNT_ID}/lock`,
            cookies: { ores_web_session: sessionId },
        });

        expect(response.statusCode).toBe(409);
        expect(response.json()).toMatchObject({
            code: 'invalid-request',
            message: 'The account is already locked.',
        });

        await server.close();
    });

    it('refuses a request with no session', async () => {
        const { server, calls } = buildTestServer({
            'iam.v1.accounts.lock': { results: [{ success: true, message: '' }] },
        });

        const response = await server.inject({
            method: 'POST',
            url: `/api/accounts/${ACCOUNT_ID}/lock`,
        });

        expect(response.statusCode).toBe(401);
        expect(calls).toEqual([]);

        await server.close();
    });
});

describe('POST /api/accounts/:accountId/unlock', () => {
    it('asks the server for the unlock subject, not the lock one', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'iam.v1.accounts.unlock': { results: [{ success: true, message: '' }] },
        });

        const response = await server.inject({
            method: 'POST',
            url: `/api/accounts/${ACCOUNT_ID}/unlock`,
            cookies: { ores_web_session: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({ success: true });
        expect(calls).toEqual([
            { subject: 'iam.v1.accounts.unlock', body: { account_ids: [ACCOUNT_ID] } },
        ]);

        await server.close();
    });

    it('reports an empty result list as a refusal rather than a success', async () => {
        const { server, sessionId } = buildTestServer({
            'iam.v1.accounts.unlock': { results: [] },
        });

        const response = await server.inject({
            method: 'POST',
            url: `/api/accounts/${ACCOUNT_ID}/unlock`,
            cookies: { ores_web_session: sessionId },
        });

        expect(response.statusCode).toBe(409);

        await server.close();
    });
});
