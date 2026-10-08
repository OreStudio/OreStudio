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
 * The roster the system administration area reads.
 *
 * What is asserted here is the translation from the wire's names, and the one
 * decision the route makes: test tenants are hidden unless asked for. The
 * system tenant is a row like any other, and the roster lists it.
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
const ACME_TENANT = '44444444-4444-4444-4444-444444444444';
const RUN_FAILED = '55555555-5555-5555-5555-555555555555';
const RUN_OLDER = '66666666-6666-6666-6666-666666666666';

const party = {
    id: '22222222-2222-2222-2222-222222222222',
    name: 'System Party',
    partyCategory: 'System',
    businessCenterCode: 'WRLD',
};

/** One tenant as the registry writes it. */
function wireTenant(id: string, code: string, name: string, status: string): unknown {
    return {
        version: 1,
        tenant_id: SYSTEM_TENANT,
        id,
        code,
        name,
        type: 'operational',
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

/** The run list's answer when no tenant has a run on record. */
const NO_RUNS = { success: true, message: '', instances: [] };

/** One provisioning run as the instances list answers it. */
function wireRun(id: string, target: string, status: string, error = ''): unknown {
    return {
        id,
        type: 'provision_tenant_workflow',
        status,
        step_count: 7,
        correlation_id: '',
        created_by: 'super_admin',
        created_at: '2026-10-01T09:00:00Z',
        error,
        target_kind: 'tenant',
        target_id: target,
    };
}

/** One step as the progress read answers it. */
function wireStep(status: string): unknown {
    return { status };
}

/** A run's steps: `done` finished, and a trailing failure when asked for. */
function wireProgress(done: number, failed = false): unknown {
    const finished = Array.from({ length: done }, () => wireStep('completed'));
    return {
        success: true,
        message: '',
        status: failed ? 'failed' : 'completed',
        error: failed ? 'Seeding failed.' : '',
        step_count: 7,
        steps: failed ? [...finished, wireStep('failed')] : finished,
    };
}

/** The deployment's tenant types, as the type list answers them. */
const TYPES = {
    types: [
        { type: 'production', name: 'Production', display_order: 1 },
        { type: 'evaluation', name: 'Evaluation', display_order: 2 },
        { type: 'automation', name: 'Automation', display_order: 3 },
        { type: 'system', name: 'System', display_order: 4 },
    ],
};

/** The calls the roster made to the tenant list, in order. */
function tenantLists(calls: readonly { subject: string; body: unknown }[]): unknown[] {
    return calls.filter((call) => call.subject === 'iam.v1.tenants.list').map((call) => call.body);
}

/** A filter with nothing set, which each case overrides. */
const NO_FILTER = {
    type: null,
    status: null,
    id_one_of: null,
    type_one_of: null,
    status_one_of: null,
    search: null,
};

function buildTestServer(
    reply: unknown,
    mode: 'system-administration' | 'application' = 'system-administration',
    runs: unknown = NO_RUNS,
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
            if (subject === 'iam.v1.tenant_types.list') {
                return schema.parse(TYPES);
            }
            if (subject === 'dq.v1.badge_definitions.list') {
                return schema.parse({});
            }
            if (subject === 'workflow.v1.instances.list') {
                if (runs instanceof Error) {
                    throw runs;
                }
                return schema.parse(runs);
            }
            return schema.parse(reply);
        },
        async workflowProgress(instanceId: string): Promise<unknown> {
            return instanceId === RUN_FAILED ? wireProgress(3, true) : wireProgress(7);
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
        sessionId: session.id,
        calls,
    };
}

describe('GET /api/tenants', () => {
    it('lists one page of the tenants the search answers', async () => {
        const { server, sessionId, calls } = buildTestServer({
            result: { outcome: 'ok' },
            tenants: [wireTenant(ACME_TENANT, 'acme_corporation', 'Acme Corporation', 'active')],
            total: 1,
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/tenants',
            cookies: { [SESSION_COOKIE]: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            tenants: [
                {
                    id: ACME_TENANT,
                    code: 'acme_corporation',
                    name: 'Acme Corporation',
                    type: 'operational',
                    description: '',
                    hostname: 'acme_corporation',
                    status: 'active',
                    registrationDefault: false,
                    setup: null,
                },
            ],
            totalCount: 1,
            setupUnavailable: false,
            hiddenTestCount: 1,
        });
        /*
         * The page names the types it shows, which leaves test infrastructure
         * out; a second read counts what it hid.
         */
        expect(tenantLists(calls)).toEqual([
            {
                offset: 0,
                limit: 100,
                order: { field: 'code', descending: false },
                as_of: null,
                filter: { ...NO_FILTER, type_one_of: ['production', 'evaluation', 'system'] },
            },
            {
                offset: 0,
                limit: 1,
                order: { field: 'code', descending: false },
                as_of: null,
                filter: { ...NO_FILTER, type_one_of: ['automation'] },
            },
        ]);

        await server.close();
    });

    /*
     * The search, the page and the total are the server's; the route passes the
     * person's search and page through and corrects nothing.
     */
    it('passes the search and the page to the server and its total back', async () => {
        const { server, sessionId, calls } = buildTestServer({
            result: { outcome: 'ok' },
            tenants: [wireTenant(ACME_TENANT, 'acme_corporation', 'Acme Corporation', 'active')],
            total: 37,
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/tenants?search=acme&offset=20&limit=10',
            cookies: { [SESSION_COOKIE]: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({ totalCount: 37 });
        expect(tenantLists(calls)[0]).toMatchObject({
            offset: 20,
            limit: 10,
            filter: { search: 'acme' },
        });

        await server.close();
    });

    /*
     * Test infrastructure is hidden by default, so a person asking to see it,
     * or asking for that type by name, gets it in the page and no hidden count
     * to report. The system tenant stays out either way.
     */
    it('leaves nothing out when test tenants are asked for', async () => {
        for (const url of ['/api/tenants?includeTest=true', '/api/tenants?type=automation']) {
            const { server, sessionId, calls } = buildTestServer({
                result: { outcome: 'ok' },
                tenants: [],
                total: 0,
            });

            const response = await server.inject({
                method: 'GET',
                url,
                cookies: { [SESSION_COOKIE]: sessionId },
            });

            expect(response.statusCode).toBe(200);
            expect(response.json()).toMatchObject({ hiddenTestCount: 0 });
            expect(tenantLists(calls)).toEqual([
                expect.objectContaining({
                    filter: expect.objectContaining({
                        type_one_of: ['production', 'evaluation', 'automation', 'system'],
                    }),
                }),
            ]);

            await server.close();
        }
    });

    it('passes the type and status filters to the list', async () => {
        const { server, sessionId, calls } = buildTestServer({
            result: { outcome: 'ok' },
            tenants: [],
            total: 0,
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/tenants?type=evaluation&status=suspended',
            cookies: { [SESSION_COOKIE]: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(tenantLists(calls)[0]).toMatchObject({
            filter: {
                type: 'evaluation',
                status: 'suspended',
                type_one_of: ['production', 'evaluation', 'system'],
            },
        });

        await server.close();
    });

    it('refuses a page that is not a whole number', async () => {
        const { server, sessionId, calls } = buildTestServer({
            result: { outcome: 'ok' },
            tenants: [],
            total: 0,
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/tenants?offset=ten',
            cookies: { [SESSION_COOKIE]: sessionId },
        });

        expect(response.statusCode).toBe(400);
        expect(calls).toHaveLength(0);

        await server.close();
    });

    it('asks for the system tenant by its type', async () => {
        const { server, sessionId, calls } = buildTestServer({
            result: { outcome: 'ok' },
            tenants: [],
            total: 0,
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/tenants?type=system',
            cookies: { [SESSION_COOKIE]: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(tenantLists(calls)[0]).toMatchObject({
            filter: { type: 'system', type_one_of: ['production', 'evaluation', 'system'] },
        });

        await server.close();
    });

    it('answers an empty roster for a deployment that holds no tenant of its own', async () => {
        const { server, sessionId } = buildTestServer({
            result: { outcome: 'ok' },
            tenants: [],
            total: 0,
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/tenants',
            cookies: { [SESSION_COOKIE]: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            tenants: [],
            totalCount: 0,
            setupUnavailable: false,
            hiddenTestCount: 0,
        });

        await server.close();
    });

    /*
     * The way back into a journey in progress is the tenant it acts on, so each
     * row carries the run that provisioned it. The engine answers the run that
     * moved last first, and that is the one a row reports.
     */
    it('joins each tenant with the latest run that provisions it', async () => {
        const { server, sessionId, calls } = buildTestServer(
            {
                result: { outcome: 'ok' },
                tenants: [
                    wireTenant(ACME_TENANT, 'acme_corporation', 'Acme Corporation', 'active'),
                ],
                total: 1,
            },
            'system-administration',
            {
                success: true,
                message: '',
                instances: [
                    wireRun(RUN_FAILED, ACME_TENANT, 'failed', 'Seeding failed.'),
                    wireRun(RUN_OLDER, ACME_TENANT, 'compensated'),
                ],
            },
        );

        const response = await server.inject({
            method: 'GET',
            url: '/api/tenants',
            cookies: { [SESSION_COOKIE]: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json().tenants[0].setup).toEqual({
            instanceId: RUN_FAILED,
            status: 'failed',
            stepsDone: 3,
            stepCount: 7,
            error: 'Seeding failed.',
        });
        /*
         * Every filter is on the wire, because the server refuses a request
         * that leaves one out; the ones not in use are empty.
         */
        expect(calls.find((call) => call.subject === 'workflow.v1.instances.list')?.body).toEqual({
            limit: 1000,
            status_filter: '',
            type_filter: 'provision_tenant_workflow',
            target_kind_filter: 'tenant',
            target_id_filter: '',
            target_ids_filter: [ACME_TENANT],
        });

        await server.close();
    });

    it('answers the roster and says so when the runs cannot be read', async () => {
        const { server, sessionId } = buildTestServer(
            {
                result: { outcome: 'ok' },
                tenants: [
                    wireTenant(ACME_TENANT, 'acme_corporation', 'Acme Corporation', 'active'),
                ],
                total: 1,
            },
            'system-administration',
            new Error('workflow service unavailable'),
        );

        const response = await server.inject({
            method: 'GET',
            url: '/api/tenants',
            cookies: { [SESSION_COOKIE]: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({
            tenants: [{ id: ACME_TENANT, setup: null }],
            setupUnavailable: true,
        });

        await server.close();
    });

    it('refuses a request with no session', async () => {
        const { server } = buildTestServer({ result: { outcome: 'ok' }, tenants: [], total: 0 });

        const response = await server.inject({ method: 'GET', url: '/api/tenants' });

        expect(response.statusCode).toBe(401);

        await server.close();
    });

    /*
     * The registry belongs to the deployment, so the context that acts on the
     * deployment is the one that reads it. A tenant administrator is a real
     * session with real authority, inside their own tenant, and this is not it.
     */
    it('refuses a session that is acting inside a tenant', async () => {
        const { server, sessionId, calls } = buildTestServer(
            {
                result: { outcome: 'ok' },
                tenants: [
                    wireTenant(ACME_TENANT, 'acme_corporation', 'Acme Corporation', 'active'),
                ],
                total: 1,
            },
            'application',
        );

        const response = await server.inject({
            method: 'GET',
            url: '/api/tenants',
            cookies: { [SESSION_COOKIE]: sessionId },
        });

        expect(response.statusCode).toBe(403);
        expect(response.json()).toMatchObject({ code: 'forbidden' });
        expect(calls).toHaveLength(0);

        await server.close();
    });
});
