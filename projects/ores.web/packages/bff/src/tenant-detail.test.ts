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
 * One tenant's screen: the tenant read by its code, its run and its parties.
 *
 * The route makes three decisions, and these cases pin them: the system
 * tenant is answered as no tenant, a session outside system administration is
 * refused, and a failed run or party read empties that panel and says so
 * rather than failing the screen.
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
const RUN = '55555555-5555-5555-5555-555555555555';
const SYSTEM_PARTY = '66666666-6666-6666-6666-666666666666';
const GROUP = '77777777-7777-7777-7777-777777777777';

function wireTenant(id: string, code: string): unknown {
    return {
        version: 2,
        tenant_id: SYSTEM_TENANT,
        id,
        code,
        name: 'Acme Corporation',
        type: 'evaluation',
        description: 'Acme',
        hostname: code,
        status: 'active',
        is_registration_default: false,
        modified_by: 'admin',
        performed_by: 'ores.iam.service',
        change_reason_code: 'system.initial_load',
        change_commentary: '',
        recorded_at: '2026-10-01 09:00:00Z',
    };
}

const FOUND = { result: { outcome: 'ok' }, tenant: wireTenant(ACME_TENANT, 'acme') };

const RUNS = {
    success: true,
    instances: [
        {
            id: RUN,
            type: 'provision_tenant_workflow',
            status: 'failed',
            current_step_index: 3,
            step_count: 7,
            created_at: '2026-10-01T09:00:00Z',
            error: 'Seeding failed.',
            target_kind: 'tenant',
            target_id: ACME_TENANT,
        },
    ],
};

const PARTIES = {
    success: true,
    parties: [
        {
            id: GROUP,
            short_code: 'acme_group',
            full_name: 'Acme Group',
            party_category: 'Operational',
            party_type: 'Corporate',
            status: 'Active',
            parent_party_id: SYSTEM_PARTY,
        },
        {
            id: SYSTEM_PARTY,
            short_code: 'system',
            full_name: 'Acme System',
            party_category: 'System',
            party_type: 'Internal',
            status: 'Active',
            parent_party_id: null,
        },
    ],
    total: 2,
};

type Answers = Record<string, unknown>;

function buildTestServer(
    answers: Answers,
    mode: 'system-administration' | 'application' = 'system-administration',
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
        session: null,
        username: 'super_admin',
        email: 'super_admin@system.ores',
        accountId: '11111111-1111-1111-1111-111111111111',
        tenantId: SYSTEM_TENANT,
        tenantName: 'Root Tenant',
        mode,
        version: 'v0.0.25 (test)',
        availableParties: [
            {
                id: '22222222-2222-2222-2222-222222222222',
                name: 'System Party',
                partyCategory: 'System',
                businessCenterCode: 'WRLD',
            },
        ],
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

const ALL = {
    'iam.v1.tenants.get': FOUND,
    'workflow.v1.instances.list': RUNS,
    'refdata.v1.parties.list-of-tenant': PARTIES,
};

async function get(answers: Answers, code = 'acme', mode?: 'application') {
    const built = buildTestServer(answers, mode);
    const response = await built.server.inject({
        method: 'GET',
        url: `/api/tenants/${code}`,
        cookies: { ores_web_session: built.sessionId },
    });
    await built.server.close();
    return { response, calls: built.calls };
}

describe('GET /api/tenants/:code', () => {
    it('answers the tenant with its provenance, its run and its parties', async () => {
        const { response, calls } = await get(ALL);

        expect(response.statusCode).toBe(200);
        const body = response.json();
        expect(body.tenant).toMatchObject({
            id: ACME_TENANT,
            code: 'acme',
            version: 2,
            changeReasonCode: 'system.initial_load',
            setup: { instanceId: RUN, status: 'failed', error: 'Seeding failed.' },
        });
        expect(body.parties.map((p: { code: string }) => p.code)).toEqual(['system', 'acme_group']);
        expect(body.parties[1].parentName).toBe('Acme System');
        expect(body.partyCount).toBe(2);
        expect(body.setupUnavailable).toBe(false);
        expect(body.partiesUnavailable).toBe(false);
        expect(calls.find((c) => c.subject === 'iam.v1.tenants.get')?.body).toEqual({
            key: { code: 'acme' },
        });
        expect(calls.find((c) => c.subject === 'workflow.v1.instances.list')?.body).toMatchObject({
            target_id_filter: ACME_TENANT,
        });
        expect(
            calls.find((c) => c.subject === 'refdata.v1.parties.list-of-tenant')?.body,
        ).toMatchObject({ tenant_id: ACME_TENANT });
    });

    it('answers not found for a code no tenant holds', async () => {
        const { response } = await get({
            ...ALL,
            'iam.v1.tenants.get': { result: { outcome: 'missing' }, tenant: null },
        });

        expect(response.statusCode).toBe(404);
        expect(response.json().code).toBe('not-found');
    });

    it('answers the system tenant as no tenant', async () => {
        const { response, calls } = await get(
            {
                ...ALL,
                'iam.v1.tenants.get': {
                    result: { outcome: 'ok' },
                    tenant: wireTenant(SYSTEM_TENANT, 'system'),
                },
            },
            'system',
        );

        expect(response.statusCode).toBe(404);
        expect(calls.map((c) => c.subject)).toEqual(['iam.v1.tenants.get']);
    });

    it('refuses a session outside system administration', async () => {
        const { response, calls } = await get(ALL, 'acme', 'application');

        expect(response.statusCode).toBe(403);
        expect(calls).toHaveLength(0);
    });

    it('still answers the tenant when the runs and the parties cannot be read', async () => {
        const { response } = await get({
            'iam.v1.tenants.get': FOUND,
            'workflow.v1.instances.list': new Error('down'),
            'refdata.v1.parties.list-of-tenant': new Error('down'),
        });

        expect(response.statusCode).toBe(200);
        const body = response.json();
        expect(body.tenant.setup).toBeNull();
        expect(body.setupUnavailable).toBe(true);
        expect(body.parties).toEqual([]);
        expect(body.partiesUnavailable).toBe(true);
    });
});
