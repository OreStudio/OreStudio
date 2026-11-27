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
import type { OresClient, SessionMode } from '@ores/wire-protocol';
import type { Config } from './config.js';
import { buildServer, sessionCookieName } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

/**
 * The housekeeping routes: the two reads and the one write.
 *
 * What is checked is what the BFF decides. The scheduler decides whether a store
 * is bounded and the database decides what the write must hold, so these tests
 * state only that each route asks the session's own client, that the write takes
 * the store's contract from the catalogue and never from the request, and what a
 * refusal becomes.
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

const SYSTEM_TENANT = 'ffffffff-ffff-4fff-8fff-ffffffffffff';
const ACCOUNT = '11111111-1111-4111-8111-111111111111';

const wireStore = {
    code: 'marketdata.delivery_counts',
    display_name: 'Market data delivery counts',
    description: 'How many items each feed delivered.',
    owner_component: 'marketdata',
    target_kind: 'table',
    target: 'public.ores_marketdata_delivery_counts_tbl',
    time_column: 'period_start',
    measure_kind: 'relation_rows',
    bound_kind: 'policy',
    bound: {
        action: 'delete_rows',
        retention_days: 30,
        cadence_hours: 24,
        policy_code: 'delivery_counts_30d',
        policy_enabled: true,
        policy_version: 7,
        policy_description: 'A delivery counter is read for a month',
        mechanism: 'sweep',
        bounded_now: true,
    },
    last_run: {
        outcome: 'applied',
        ran_at: '2026-11-27 03:00:00.000000Z',
        removed: 2,
        detail: '',
    },
    size: { size_bytes: 32768, measure_count: 27, sampled_at: '2026-11-27 03:15:00.000000Z' },
};

type Answers = Readonly<Record<string, unknown>>;

const ANSWERS: Answers = {
    'scheduler.v1.store_bounds.list': { success: true, message: '', stores: [wireStore] },
    'scheduler.v1.store_samples.list': {
        success: true,
        message: '',
        samples: [
            {
                sampled_at: '2026-11-27 03:15:00.000000Z',
                size_bytes: 32768,
                measure_count: 27,
            },
        ],
    },
    'scheduler.v1.retention_policies.put': {
        result: { outcome: 'ok', code: '', message: '' },
        retention_policy: null,
    },
};

function buildTestServer(mode: SessionMode, answers: Answers = ANSWERS) {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const calls: { subject: string; body: unknown }[] = [];
    const client = {
        async callAuthenticated(
            subject: string,
            body: unknown,
            schema: { parse: (value: unknown) => unknown },
        ): Promise<unknown> {
            calls.push({ subject, body });
            if (!(subject in answers)) {
                throw new Error(`unexpected call ${subject}`);
            }
            return schema.parse(answers[subject]);
        },
        async close(): Promise<void> {
            return undefined;
        },
    } as unknown as OresClient;
    const session = sessions.create({
        client,
        session: null,
        username: 'sysadmin',
        email: 'sysadmin@acme.example',
        accountId: ACCOUNT,
        tenantId: SYSTEM_TENANT,
        tenantName: 'System',
        mode,
        version: 'v0.0.31 (test)',
        availableParties: [
            {
                id: SYSTEM_TENANT,
                name: 'System',
                partyCategory: 'Operational',
                businessCenterCode: 'GBLO',
            },
        ],
        accessLifetimeSeconds: 1800,
        passwordResetRequired: false,
        sessionId: '77777777-7777-4777-8777-777777777777',
    });
    const server = buildServer({
        config,
        site: siteConfiguration(),
        sessions,
        createClient: () => ({ client, connect: async () => undefined }),
    });
    return { server, cookies: { [SESSION_COOKIE]: session.id }, calls };
}

async function get(mode: SessionMode, url: string, answers: Answers = ANSWERS) {
    const { server, cookies, calls } = buildTestServer(mode, answers);
    const response = await server.inject({ method: 'GET', url, cookies });
    await server.close();
    return { response, calls };
}

async function put(mode: SessionMode, url: string, payload: unknown, answers: Answers = ANSWERS) {
    const { server, cookies, calls } = buildTestServer(mode, answers);
    const response = await server.inject({ method: 'PUT', url, cookies, payload });
    await server.close();
    return { response, calls };
}

describe('GET /api/operations/housekeeping/stores', () => {
    it('serves every store with its bound and the moment it was read', async () => {
        const { response, calls } = await get(
            'system-administration',
            '/api/operations/housekeeping/stores',
        );

        expect(response.statusCode).toBe(200);
        const body = response.json<{ read_at: string; rows: { code: string }[] }>();
        expect(body.rows.map((row) => row.code)).toEqual(['marketdata.delivery_counts']);
        expect(body.read_at).toBeTruthy();
        expect(calls.map((call) => call.subject)).toEqual(['scheduler.v1.store_bounds.list']);
    });

    it('answers the refusal the read states rather than an empty catalogue', async () => {
        const { response } = await get(
            'system-administration',
            '/api/operations/housekeeping/stores',
            {
                'scheduler.v1.store_bounds.list': {
                    success: false,
                    message: 'Your role cannot read the scheduler.',
                    stores: [],
                },
            },
        );

        expect(response.statusCode).toBe(409);
    });
});

describe('GET /api/operations/housekeeping/stores/:code/samples', () => {
    it('sends the store and the limit the screen asked for', async () => {
        const { response, calls } = await get(
            'system-administration',
            '/api/operations/housekeeping/stores/marketdata.delivery_counts/samples?limit=168',
        );

        expect(response.statusCode).toBe(200);
        expect(calls[0]?.body).toEqual({
            store_code: 'marketdata.delivery_counts',
            limit: 168,
        });
        expect(response.json<{ samples: unknown[] }>().samples).toHaveLength(1);
    });

    it('refuses a limit above the one the read allows', async () => {
        const { response } = await get(
            'system-administration',
            '/api/operations/housekeeping/stores/marketdata.delivery_counts/samples?limit=100000',
        );

        expect(response.statusCode).toBe(400);
    });
});

describe('PUT /api/operations/housekeeping/stores/:code/policy', () => {
    const body = {
        enabled: false,
        retention_days: 90,
        cadence_hours: 12,
        reason_code: 'common.correction',
        commentary: 'the audit asked for a quarter',
    };

    it('composes the policy from the catalogue and sends only the three numbers as the change', async () => {
        const { response, calls } = await put(
            'system-administration',
            '/api/operations/housekeeping/stores/marketdata.delivery_counts/policy',
            body,
        );

        expect(response.statusCode).toBe(200);
        expect(calls.map((call) => call.subject)).toEqual([
            'scheduler.v1.store_bounds.list',
            'scheduler.v1.retention_policies.put',
        ]);
        expect(calls[1]?.body).toEqual({
            change: {
                write: {
                    code: 'delivery_counts_30d',
                    target_kind: 'table',
                    target: 'public.ores_marketdata_delivery_counts_tbl',
                    store_code: 'marketdata.delivery_counts',
                    action: 'delete_rows',
                    retention_days: 90,
                    cadence_hours: 12,
                    enabled: false,
                    description: 'A delivery counter is read for a month',
                },
                precondition: { kind: 'must_match_version', version: 7 },
            },
            intent: {
                reason_code: 'common.correction',
                commentary: 'the audit asked for a quarter',
            },
        });
    });

    it('names no store it does not keep', async () => {
        const { response, calls } = await put(
            'system-administration',
            '/api/operations/housekeeping/stores/nothing.here/policy',
            body,
        );

        expect(response.statusCode).toBe(404);
        expect(calls.map((call) => call.subject)).toEqual(['scheduler.v1.store_bounds.list']);
    });

    it('turns a policy that moved since the read into a conflict', async () => {
        const { response } = await put(
            'system-administration',
            '/api/operations/housekeeping/stores/marketdata.delivery_counts/policy',
            body,
            {
                ...ANSWERS,
                'scheduler.v1.retention_policies.put': {
                    result: { outcome: 'conflict', code: '', message: 'The policy moved on.' },
                    retention_policy: null,
                },
            },
        );

        expect(response.statusCode).toBe(409);
    });

    it('refuses a change with no reason, and asks the service nothing', async () => {
        const { response, calls } = await put(
            'system-administration',
            '/api/operations/housekeeping/stores/marketdata.delivery_counts/policy',
            { enabled: true, retention_days: 0, cadence_hours: 24, reason_code: '' },
        );

        expect(response.statusCode).toBe(400);
        expect(calls).toEqual([]);
    });
});
