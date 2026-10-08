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
 * The system administrator's home: the counts, the tenants that need
 * attention, the first tenants and the newest setups.
 *
 * The stub answers each tenant list by the filter it was sent, so a count read
 * with the wrong filter answers the wrong number and the case fails.
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
const GLOBEX = '55555555-5555-5555-5555-555555555555';
const INITECH = '66666666-6666-6666-6666-666666666666';

const party = {
    id: '22222222-2222-2222-2222-222222222222',
    name: 'System Party',
    partyCategory: 'System',
    businessCenterCode: 'WRLD',
};

const TYPES = {
    types: [
        { type: 'production', name: 'Production', display_order: 1 },
        { type: 'evaluation', name: 'Evaluation', display_order: 2 },
        { type: 'automation', name: 'Automation', display_order: 3 },
        { type: 'system', name: 'System', display_order: 4 },
    ],
};

function wireTenant(id: string, code: string, name: string, status: string, type = 'production') {
    return {
        version: 1,
        tenant_id: SYSTEM_TENANT,
        id,
        code,
        name,
        type,
        description: '',
        hostname: code,
        status,
        is_registration_default: false,
        modified_by: 'system',
        performed_by: 'system',
        change_reason_code: 'new',
        change_commentary: '',
        recorded_at: '2026-10-01 09:00:00Z',
    };
}

const acme = wireTenant(ACME, 'acme', 'Acme Corporation', 'active');
const globex = wireTenant(GLOBEX, 'globex', 'Globex Markets', 'bootstrapping');
const initech = wireTenant(INITECH, 'initech', 'Initech Capital', 'suspended', 'evaluation');

function wireRun(id: string, target: string, status: string, step: number, error = '') {
    return {
        id,
        type: 'provision_tenant_workflow',
        status,
        current_step_index: step,
        step_count: 7,
        correlation_id: '',
        created_by: 'admin',
        created_at: '2026-10-04T09:00:00Z',
        completed_at: status === 'in_progress' ? null : '2026-10-04T09:05:00Z',
        error,
        target_kind: 'tenant',
        target_id: target,
    };
}

interface TenantFilter {
    readonly type: string | null;
    readonly status: string | null;
    readonly id_one_of: string[] | null;
    readonly type_one_of: string[] | null;
}

/** The tenant list's answer, chosen by the filter the read sent. */
function tenantList(filter: TenantFilter) {
    const page = (tenants: unknown[], total: number) => ({
        result: { outcome: 'ok' },
        tenants,
        total,
    });
    if (filter.id_one_of !== null) {
        return page(
            [acme, globex, initech].filter((t) => filter.id_one_of?.includes(t.id)),
            filter.id_one_of.length,
        );
    }
    if (filter.status === 'active') return page([], 3);
    if (filter.type === 'evaluation') return page([], 1);
    if (filter.status === 'bootstrapping') return page([], 2);
    if (filter.status === 'suspended') return page([initech], 1);
    return page([acme, globex], 9);
}

function buildTestServer(
    mode: 'system-administration' | 'tenant-administration' = 'system-administration',
    runsFail = false,
) {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const calls: { subject: string; body: unknown }[] = [];
    const client = {
        async callAuthenticated(
            subject: string,
            body: unknown,
            schema: { parse: (value: unknown) => unknown },
        ): Promise<unknown> {
            calls.push({ subject, body });
            if (subject === 'iam.v1.tenant_types.list') return schema.parse(TYPES);
            if (subject === 'dq.v1.badge_definitions.list') return schema.parse({});
            if (subject === 'iam.v1.tenants.list') {
                return schema.parse(tenantList((body as { filter: TenantFilter }).filter));
            }
            if (subject === 'workflow.v1.instances.list') {
                if (runsFail) throw new Error('workflow service unavailable');
                const request = body as { status_filter: string; target_ids_filter: string[] };
                const instances =
                    request.status_filter === 'failed'
                        ? [
                              wireRun('run-globex', GLOBEX, 'failed', 4, 'Publishing failed.'),
                              wireRun('run-acme-first', ACME, 'failed', 2, 'Timed out.'),
                          ]
                        : request.target_ids_filter.length > 0
                          ? [wireRun('run-acme', ACME, 'completed', 6)]
                          : [
                                wireRun('run-globex', GLOBEX, 'failed', 4, 'Publishing failed.'),
                                wireRun('run-acme', ACME, 'completed', 6),
                            ];
                return schema.parse({ success: true, message: '', instances });
            }
            throw new Error(`unexpected subject ${subject}`);
        },
        async close(): Promise<void> {
            return undefined;
        },
    } as unknown as OresClient;
    const session = sessions.create({
        client,
        session: null,
        username: 'super_admin',
        email: 'super_admin@system.ores',
        accountId: '11111111-1111-1111-1111-111111111111',
        tenantId: SYSTEM_TENANT,
        tenantName: 'Root Tenant',
        mode,
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
        cookies: { [SESSION_COOKIE]: session.id },
        calls,
    };
}

describe('GET /api/overview', () => {
    /*
     * Acme's first setup failed and a second one set it up, so its old failed
     * run is still in the engine. It needs no attention: it is active.
     */
    it('answers the counts, the attention list, the first tenants and the newest setups', async () => {
        const { server, cookies } = buildTestServer();

        const response = await server.inject({ method: 'GET', url: '/api/overview', cookies });
        await server.close();

        expect(response.statusCode).toBe(200);
        const body = response.json();
        // Globex is bootstrapping with a failed run, so it needs attention rather than counting as setting up.
        expect(body).toMatchObject({
            inService: 3,
            onEvaluation: 1,
            settingUp: 1,
            totalCount: 9,
            activityUnavailable: false,
        });
        expect(
            body.attention.map((a: { tenant: { code: string }; reason: string }) => [
                a.tenant.code,
                a.reason,
            ]),
        ).toEqual([
            ['globex', 'setup-failed'],
            ['initech', 'suspended'],
        ]);
        expect(body.attention[0].tenant.setup).toMatchObject({
            status: 'failed',
            currentStepIndex: 4,
            error: 'Publishing failed.',
        });
        expect(body.tenants.map((t: { code: string }) => t.code)).toEqual(['acme', 'globex']);
        expect(body.tenants[0].setup).toMatchObject({ status: 'completed' });
        expect(body.activity).toEqual([
            {
                instanceId: 'run-globex',
                tenantName: 'Globex Markets',
                status: 'failed',
                currentStepIndex: 4,
                stepCount: 7,
                error: 'Publishing failed.',
                at: '2026-10-04T09:05:00Z',
            },
            {
                instanceId: 'run-acme',
                tenantName: 'Acme Corporation',
                status: 'completed',
                currentStepIndex: 6,
                stepCount: 7,
                error: '',
                at: '2026-10-04T09:05:00Z',
            },
        ]);
    });

    it('never counts the system tenant or test tenants', async () => {
        const { server, cookies, calls } = buildTestServer();

        await server.inject({ method: 'GET', url: '/api/overview', cookies });
        await server.close();

        const lists = calls.filter((call) => call.subject === 'iam.v1.tenants.list');
        expect(lists.length).toBeGreaterThan(0);
        for (const call of lists) {
            expect((call.body as { filter: TenantFilter }).filter.type_one_of).toEqual([
                'production',
                'evaluation',
            ]);
        }
    });

    it('answers the tenants and says so when the runs cannot be read', async () => {
        const { server, cookies } = buildTestServer('system-administration', true);

        const response = await server.inject({ method: 'GET', url: '/api/overview', cookies });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({
            inService: 3,
            settingUp: 2,
            activity: [],
            activityUnavailable: true,
            attention: [{ tenant: { code: 'initech' }, reason: 'suspended' }],
        });
    });

    it('refuses a session outside system administration', async () => {
        const { server, cookies, calls } = buildTestServer('tenant-administration');

        const response = await server.inject({ method: 'GET', url: '/api/overview', cookies });
        await server.close();

        expect(response.statusCode).toBe(403);
        expect(calls).toHaveLength(0);
    });
});
