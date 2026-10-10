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
 * A person reads their own account without holding any account permission.
 *
 * A plain member holds no iam::accounts:read, so the ordinary account read
 * refuses them even for themselves. The screens ask for an account by
 * username, so the server answers a request that names the signed-in person's
 * own username with the self read, which takes no account id, and any other
 * username with the ordinary read. These cases pin which operation each
 * request reaches.
 */

const config: Config = {
    port: 0,
    host: '127.0.0.1',
    logLevel: 'silent',
    session: { ttlSeconds: 3600, cookieSecure: false },
    allowedOrigins: [],
    loginAttemptsPerMinute: 100,
};

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
const ASHLEY = '11111111-1111-1111-1111-111111111111';
const COLLEAGUE = '22222222-2222-2222-2222-222222222222';
const PHOTO = '33333333-3333-3333-3333-333333333333';

function wireAccount(id: string, username: string, image: string | null = null) {
    return {
        version: 2,
        id,
        tenant_id: SYSTEM_TENANT,
        username,
        full_name: `Full ${username}`,
        email: `${username}@acme.example`,
        account_type: 'user',
        job_title: 'Analyst',
        reports_to_account_id: null,
        default_party_id: null,
        image_id: image,
        modified_by: 'system',
        change_reason_code: 'system.new_record',
        change_commentary: '',
        performed_by: 'system',
        recorded_at: '2026-10-09 12:00:00Z',
    };
}

type Answer = unknown | ((body: unknown) => unknown);

function buildTestServer(answers: Record<string, Answer>) {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const calls: { subject: string; body: unknown }[] = [];
    const client = {
        async callAuthenticated(
            subject: string,
            body: unknown,
            schema: { parse: (value: unknown) => unknown },
        ): Promise<unknown> {
            calls.push({ subject, body });
            const answer = answers[subject];
            if (answer === undefined) throw new Error(`unexpected subject ${subject}`);
            return schema.parse(typeof answer === 'function' ? answer(body) : answer);
        },
        async close(): Promise<void> {
            return undefined;
        },
    } as unknown as OresClient;
    const session = sessions.create({
        client,
        session: null,
        username: 'ashley.moore',
        email: 'ashley.moore@acme.example',
        accountId: ASHLEY,
        tenantId: SYSTEM_TENANT,
        tenantName: 'Acme',
        mode: 'application',
        version: 'v0.0.25 (test)',
        availableParties: [],
        accessLifetimeSeconds: 1800,
        passwordResetRequired: false,
        sessionId: '66666666-6666-6666-6666-666666666666',
    });
    const server = buildServer({
        config,
        site: siteConfiguration(),
        sessions,
        createClient: () => ({ client, connect: async () => undefined }),
    });
    return { server, cookies: { [SESSION_COOKIE]: session.id }, calls };
}

describe('reading your own account', () => {
    it('answers the signed-in person through the self read, which takes no account id', async () => {
        const { server, cookies, calls } = buildTestServer({
            'iam.v1.ops.get_my_account': {
                result: { outcome: 'ok' },
                account: wireAccount(ASHLEY, 'ashley.moore'),
            },
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/accounts/ashley.moore',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(response.json().account.username).toBe('ashley.moore');
        expect(calls).toEqual([{ subject: 'iam.v1.ops.get_my_account', body: {} }]);
    });

    it('answers a colleague through the ordinary read, which needs the account permission', async () => {
        const { server, cookies, calls } = buildTestServer({
            'iam.v1.accounts.get': {
                result: { outcome: 'ok' },
                account: wireAccount(COLLEAGUE, 'daniel'),
            },
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/accounts/daniel',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(calls.map((call) => call.subject)).toEqual(['iam.v1.accounts.get']);
    });

    it('draws the signed-in person’s own picture through the self read', async () => {
        const { server, cookies, calls } = buildTestServer({
            'iam.v1.ops.get_my_account': {
                result: { outcome: 'ok' },
                account: wireAccount(ASHLEY, 'ashley.moore', PHOTO),
            },
            'assets.v1.images.list': {
                result: { outcome: 'ok' },
                images: [{ id: PHOTO, mime_type: 'image/jpeg', data: [0xff, 0xd8] }],
            },
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/accounts/ashley.moore/picture',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(response.headers['content-type']).toBe('image/jpeg');
        expect(calls.map((call) => call.subject)).toEqual([
            'iam.v1.ops.get_my_account',
            'assets.v1.images.list',
        ]);
    });
});
