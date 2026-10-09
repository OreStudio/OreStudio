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
 * The tenant administrator's own setup run.
 *
 * The route is the tenant's read of its own setup, so what is pinned here is
 * the run it names and the fact that it answers with no run rather than an
 * error when the tenant has none. The session is what scopes the read: the
 * request carries no tenant, and a browser with no session is refused.
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

const ACME_TENANT = '44444444-4444-4444-4444-444444444444';
const RUN = '55555555-5555-5555-5555-555555555555';

const party = {
    id: '22222222-2222-2222-2222-222222222222',
    name: 'Acme Operations',
    partyCategory: 'Operational',
    businessCenterCode: 'GBLO',
};

const RUNS = {
    success: true,
    instances: [
        {
            id: RUN,
            type: 'tenant_setup_workflow',
            status: 'in_progress',
            step_count: 4,
            created_at: '2026-10-01T09:00:00Z',
            error: '',
            target_kind: 'tenant',
            target_id: ACME_TENANT,
        },
    ],
};

type Answers = Record<string, unknown>;

function buildTestServer(
    answers: Answers,
    options: { readonly tenantBootstrapping?: boolean } = {},
): {
    readonly server: ReturnType<typeof buildServer>;
    readonly sessionId: string;
    readonly calls: { subject: string; body: unknown }[];
} {
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
            if (answer instanceof Error) {
                throw answer;
            }
            return schema.parse(answer);
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
            tenantId: ACME_TENANT,
            tenantName: 'Acme Corporation',
            tenantBootstrapping: options.tenantBootstrapping ?? false,
            version: 'v0.0.25 (test)',
            database: {
                fingerprint: '',
                environment: '',
                commit: '',
                created: '',
            },
            username: 'acme_admin',
            email: 'acme_admin@acme',
            party,
            availableParties: [party],
            accessLifetimeSeconds: 1800,
            passwordResetRequired: false,
            sessionId: '33333333-3333-3333-3333-333333333333',
        },
        username: 'acme_admin',
        email: 'acme_admin@acme',
        accountId: '11111111-1111-1111-1111-111111111111',
        tenantId: ACME_TENANT,
        tenantName: 'Acme Corporation',
        tenantBootstrapping: options.tenantBootstrapping ?? false,
        mode: 'application',
        version: 'v0.0.25 (test)',
        availableParties: [party],
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

async function get(answers: Answers, withSession = true) {
    const built = buildTestServer(answers);
    const response = await built.server.inject({
        method: 'GET',
        url: '/api/tenant-setup',
        ...(withSession ? { cookies: { [SESSION_COOKIE]: built.sessionId } } : {}),
    });
    await built.server.close();
    return { response, calls: built.calls };
}

describe('GET /api/tenant-setup', () => {
    it('answers the tenant setup run the session owns, and names its type', async () => {
        const { response, calls } = await get({ 'workflow.v1.instances.list': RUNS });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            instanceId: RUN,
            status: 'in_progress',
            error: '',
        });
        expect(calls).toHaveLength(1);
        expect(calls[0]?.subject).toBe('workflow.v1.instances.list');
        expect(calls[0]?.body).toMatchObject({ type_filter: 'tenant_setup_workflow' });
    });

    it('answers an empty instance id when the tenant has no run to follow', async () => {
        const { response } = await get({
            'workflow.v1.instances.list': { success: true, instances: [] },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({ instanceId: '', status: '', error: '' });
    });

    it('refuses a request with no session', async () => {
        const { response, calls } = await get({ 'workflow.v1.instances.list': RUNS }, false);

        expect(response.statusCode).toBe(401);
        expect(response.json()).toMatchObject({ code: 'not-authenticated' });
        expect(calls).toHaveLength(0);
    });

    it('fails rather than answering no run when the read itself fails', async () => {
        const { response } = await get({
            'workflow.v1.instances.list': new Error('down'),
        });

        expect(response.statusCode).toBe(500);
    });
});

describe('GET /api/session and the tenant setup rail', () => {
    async function readSession(answers: Answers, tenantBootstrapping: boolean) {
        const built = buildTestServer(answers, { tenantBootstrapping });
        const response = await built.server.inject({
            method: 'GET',
            url: '/api/session',
            cookies: { [SESSION_COOKIE]: built.sessionId },
        });
        await built.server.close();
        return { response, calls: built.calls };
    }

    it('clears the flag once the tenant setup run has finished', async () => {
        const finished = {
            ...RUNS,
            instances: [{ ...RUNS.instances[0], status: 'completed' }],
        };
        const { response } = await readSession({ 'workflow.v1.instances.list': finished }, true);

        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({ tenantBootstrapping: false });
    });

    it('keeps the flag while the run is still going', async () => {
        const { response } = await readSession({ 'workflow.v1.instances.list': RUNS }, true);

        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({ tenantBootstrapping: true });
    });

    it('does not read a run for a session whose tenant is already active', async () => {
        const { response, calls } = await readSession({}, false);

        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({ tenantBootstrapping: false });
        expect(calls).toHaveLength(0);
    });
});
