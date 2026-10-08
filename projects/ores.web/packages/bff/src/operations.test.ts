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
import { toWireTimestamp, type OresClient, type SessionMode } from '@ores/wire-protocol';
import type { Config } from './config.js';
import { secondsSinceReport } from './operations.js';
import { buildServer } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

/**
 * The operations routes: the services roster, and who may read it.
 *
 * What is checked is what the BFF decides: that it asks the session's client
 * once for the roster, that it marks each row's age from the deployment's
 * clock, and that a session which does not act on the deployment is refused
 * before any call goes out.
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

const SYSTEM_TENANT = 'ffffffff-ffff-4fff-8fff-ffffffffffff';
const ACCOUNT = '11111111-1111-4111-8111-111111111111';
const INSTANCE = '91b0f33d-4b32-4f65-8809-2d3e4f506172';

/** One roster slot as the read answers it. */
function wireSlot(overrides: Record<string, unknown> = {}): Record<string, unknown> {
    return {
        service_name: 'ores.iam.service',
        display_name: 'IAM Service',
        description: '',
        service_account: null,
        slot: 1,
        state: 'running',
        instance_id: INSTANCE,
        host_id: null,
        version: 'v0.0.25',
        sampled_at: toWireTimestamp(new Date(Date.now() - 5_000)),
        ...overrides,
    };
}

function buildTestServer(mode: SessionMode, slots: readonly Record<string, unknown>[]) {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const calls: { subject: string; body: unknown }[] = [];
    const client = {
        async serviceRoster(): Promise<readonly Record<string, unknown>[]> {
            calls.push({ subject: 'telemetry.v1.ops.get_service_roster', body: {} });
            return slots;
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
        version: 'v0.0.25 (test)',
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
    return { server, cookies: { ores_web_session: session.id }, calls };
}

describe('GET /api/operations/services', () => {
    it('answers the roster with each row dated from the deployment clock', async () => {
        const { server, cookies, calls } = buildTestServer('system-administration', [
            wireSlot(),
            wireSlot({
                service_name: 'ores.analytics.service',
                state: 'missing',
                instance_id: null,
                version: null,
                sampled_at: null,
            }),
        ]);

        const response = await server.inject({
            method: 'GET',
            url: '/api/operations/services',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        const rows = (response.json() as { rows: Record<string, unknown>[] }).rows;
        expect(rows.map((row) => row['service_name'])).toEqual([
            'ores.iam.service',
            'ores.analytics.service',
        ]);
        // The running row is about five seconds old; the missing slot never
        // reported, so it carries no age at all rather than a made-up one.
        expect(typeof rows[0]?.['age_seconds']).toBe('number');
        expect(rows[1]?.['age_seconds']).toBeNull();
        expect(rows[1]?.['sampled_at']).toBeNull();
        // One read, sent with the empty request the roster operation declares.
        expect(calls).toEqual([{ subject: 'telemetry.v1.ops.get_service_roster', body: {} }]);
    });

    it('refuses a query field the read does not take', async () => {
        const { server, cookies, calls } = buildTestServer('system-administration', [wireSlot()]);

        const response = await server.inject({
            method: 'GET',
            url: '/api/operations/services?scope=all',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(400);
        expect(response.json()).toMatchObject({ code: 'invalid-request' });
        // Refused at the boundary: nothing was read from the deployment.
        expect(calls).toEqual([]);
    });

    it('refuses a session that does not act on the deployment', async () => {
        const { server, cookies, calls } = buildTestServer('tenant-administration', [wireSlot()]);

        const response = await server.inject({
            method: 'GET',
            url: '/api/operations/services',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(403);
        expect(response.json()).toMatchObject({ code: 'forbidden' });
        // Refused at the boundary: nothing was read from the deployment.
        expect(calls).toEqual([]);
    });

    it('refuses a caller with no session at all', async () => {
        const { server } = buildTestServer('system-administration', [wireSlot()]);

        const response = await server.inject({ method: 'GET', url: '/api/operations/services' });
        await server.close();

        expect(response.statusCode).toBe(401);
    });
});

describe('the age a roster row is marked with', () => {
    const NOW = Date.UTC(2026, 9, 4, 14, 32, 5);

    it('is the seconds since the instance reported', () => {
        expect(secondsSinceReport('2026-10-04 14:32:00Z', NOW)).toBe(5);
        expect(secondsSinceReport('2026-10-04 14:27:05Z', NOW)).toBe(300);
    });

    it('is nothing when the slot never reported, or its time is unreadable', () => {
        expect(secondsSinceReport(null, NOW)).toBeNull();
        expect(secondsSinceReport('', NOW)).toBeNull();
        expect(secondsSinceReport('not a time', NOW)).toBeNull();
    });

    it('is never negative when the report time is ahead of the clock', () => {
        expect(secondsSinceReport('2026-10-04 14:32:10Z', NOW)).toBe(0);
    });
});
