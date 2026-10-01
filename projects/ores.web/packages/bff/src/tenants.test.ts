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
 * The roster the system administration area reads.
 *
 * What is asserted here is the translation from the wire's names, and the one
 * decision the route makes: the registry is system-scoped, so the system tenant
 * is a row like any other and it is the deployment's own bookkeeping rather
 * than a tenant somebody set up. A roster that lists it answers with one tenant
 * too many, and a count that includes it is off by one.
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
function wireRun(
    id: string,
    target: string,
    status: string,
    currentStep: number,
    error = '',
): unknown {
    return {
        id,
        type: 'provision_tenant_workflow',
        status,
        current_step_index: currentStep,
        step_count: 7,
        correlation_id: '',
        created_by: 'super_admin',
        created_at: '2026-10-01T09:00:00Z',
        error,
        target_kind: 'tenant',
        target_id: target,
    };
}

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
            if (subject === 'workflow.v1.instances.list') {
                if (runs instanceof Error) {
                    throw runs;
                }
                return schema.parse(runs);
            }
            return schema.parse(reply);
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
    it('lists the tenants the deployment holds, and not the deployment itself', async () => {
        const { server, sessionId, calls } = buildTestServer({
            tenants: [
                wireTenant(SYSTEM_TENANT, 'system', 'Root Tenant', 'active'),
                wireTenant(ACME_TENANT, 'acme_corporation', 'Acme Corporation', 'active'),
            ],
            total: 2,
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/tenants',
            cookies: { ores_web_session: sessionId },
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
        });
        expect(calls).toHaveLength(2);
        expect(calls[0]?.subject).toBe('iam.v1.tenants.list');
        expect(calls[0]?.body).toMatchObject({ offset: 0, limit: 100 });

        await server.close();
    });

    it('answers an empty roster for a deployment that holds no tenant of its own', async () => {
        const { server, sessionId } = buildTestServer({
            tenants: [wireTenant(SYSTEM_TENANT, 'system', 'Root Tenant', 'active')],
            total: 1,
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/tenants',
            cookies: { ores_web_session: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({ tenants: [], totalCount: 0, setupUnavailable: false });

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
                    wireRun(RUN_FAILED, ACME_TENANT, 'failed', 3, 'Seeding failed.'),
                    wireRun(RUN_OLDER, ACME_TENANT, 'compensated', 1),
                ],
            },
        );

        const response = await server.inject({
            method: 'GET',
            url: '/api/tenants',
            cookies: { ores_web_session: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json().tenants[0].setup).toEqual({
            instanceId: RUN_FAILED,
            status: 'failed',
            currentStepIndex: 3,
            stepCount: 7,
            error: 'Seeding failed.',
        });
        expect(calls[1]?.subject).toBe('workflow.v1.instances.list');
        /*
         * Every filter is on the wire, because the server refuses a request
         * that leaves one out; the ones not in use are empty.
         */
        expect(calls[1]?.body).toEqual({
            limit: 1000,
            status_filter: '',
            type_filter: 'provision_tenant_workflow',
            target_kind_filter: 'tenant',
            target_id_filter: '',
        });

        await server.close();
    });

    it('answers the roster and says so when the runs cannot be read', async () => {
        const { server, sessionId } = buildTestServer(
            {
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
            cookies: { ores_web_session: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({
            tenants: [{ id: ACME_TENANT, setup: null }],
            setupUnavailable: true,
        });

        await server.close();
    });

    it('refuses a request with no session', async () => {
        const { server } = buildTestServer({ tenants: [], total: 0 });

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
            cookies: { ores_web_session: sessionId },
        });

        expect(response.statusCode).toBe(403);
        expect(response.json()).toMatchObject({ code: 'forbidden' });
        expect(calls).toHaveLength(0);

        await server.close();
    });
});
