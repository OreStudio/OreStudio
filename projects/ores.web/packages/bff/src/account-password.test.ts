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
import { ACCOUNT_SUBJECTS, type OresClient } from '@ores/wire-protocol';
import type { Config } from './config.js';
import { buildServer } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

/**
 * The route a forced password change writes through.
 *
 * The account is the session's own, so the route takes no account id: what is
 * asserted here is that the session's account is the one changed, that the
 * current password travels with the request, and that the session stops
 * reporting the change as outstanding once it has happened.
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

interface Call {
    readonly subject: string;
    readonly body: unknown;
}

function buildTestServer(result: unknown): {
    readonly server: ReturnType<typeof buildServer>;
    readonly sessionId: string;
    readonly calls: Call[];
} {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const calls: Call[] = [];
    const client = {
        async callAuthenticated(subject: string, body: unknown): Promise<unknown> {
            calls.push({ subject, body });
            return result;
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
            accountId: '11111111-1111-1111-1111-111111111111',
            tenantId: 'ffffffff-ffff-ffff-ffff-ffffffffffff',
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
        accountId: '11111111-1111-1111-1111-111111111111',
        tenantId: 'ffffffff-ffff-ffff-ffff-ffffffffffff',
        tenantName: 'Northwind Capital',
        mode: 'application',
        version: 'v0.0.25 (test)',
        availableParties: [northwind],
        accessLifetimeSeconds: 1800,
        passwordResetRequired: true,
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

const body = { currentPassword: 'Issued-Password-1', newPassword: 'Chosen-Password-2' };

describe('POST /api/account/password', () => {
    it('changes the session\u2019s own password and clears the change it was asked for', async () => {
        const { server, sessionId, calls } = buildTestServer({ success: true, message: '' });

        const response = await server.inject({
            method: 'POST',
            url: '/api/account/password',
            cookies: { ores_web_session: sessionId },
            payload: body,
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({ success: true });
        expect(calls).toEqual([
            {
                subject: ACCOUNT_SUBJECTS.changePassword,
                body: {
                    current_password: 'Issued-Password-1',
                    new_password: 'Chosen-Password-2',
                },
            },
        ]);

        const session = await server.inject({
            method: 'GET',
            url: '/api/session',
            cookies: { ores_web_session: sessionId },
        });
        expect(session.json()).toMatchObject({ passwordResetRequired: false });

        await server.close();
    });

    it('keeps the change outstanding when the server refuses the new password', async () => {
        const { server, sessionId } = buildTestServer({
            success: false,
            message: 'The password must hold an uppercase letter.',
        });

        const response = await server.inject({
            method: 'POST',
            url: '/api/account/password',
            cookies: { ores_web_session: sessionId },
            payload: body,
        });

        expect(response.statusCode).toBe(409);
        expect(response.json()).toMatchObject({ code: 'invalid-request' });

        const session = await server.inject({
            method: 'GET',
            url: '/api/session',
            cookies: { ores_web_session: sessionId },
        });
        expect(session.json()).toMatchObject({ passwordResetRequired: true });

        await server.close();
    });

    it('refuses a request with no session', async () => {
        const { server, calls } = buildTestServer({ success: true, message: '' });

        const response = await server.inject({
            method: 'POST',
            url: '/api/account/password',
            payload: body,
        });

        expect(response.statusCode).toBe(401);
        expect(response.json()).toMatchObject({ code: 'not-authenticated' });
        expect(calls).toEqual([]);

        await server.close();
    });

    it('refuses a body with no new password', async () => {
        const { server, sessionId, calls } = buildTestServer({ success: true, message: '' });

        const response = await server.inject({
            method: 'POST',
            url: '/api/account/password',
            cookies: { ores_web_session: sessionId },
            payload: { ...body, newPassword: '' },
        });

        expect(response.statusCode).toBe(400);
        expect(response.json()).toMatchObject({ code: 'invalid-request' });
        expect(calls).toEqual([]);

        await server.close();
    });
});
