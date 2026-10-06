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
import { uuid, type OresClient, type SessionMode } from '@ores/wire-protocol';
import type { Config } from './config.js';
import { buildServer } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

/**
 * The parties of the session's own tenant, one page at a time.
 *
 * The route names no tenant: row-level security scopes the read from the
 * session's token. It refuses system administration, whose own tenant the
 * party policy lets read every tenant's parties.
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

const SYSTEM_PARTY = '66666666-6666-6666-6666-666666666666';

function wireParty(id: string, code: string, name: string, parent: string | null): unknown {
    return {
        id,
        short_code: code,
        full_name: name,
        party_category: parent === null ? 'System' : 'Operational',
        party_type: 'Corporate',
        parent_party_id: parent,
        business_center_code: '',
        status: 'Active',
    };
}

function buildTestServer(mode: SessionMode) {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const calls: { subject: string; body: unknown }[] = [];
    const client = {
        enteredTenant: undefined,
        async callAuthenticated(
            subject: string,
            body: unknown,
            schema: { parse: (value: unknown) => unknown },
        ): Promise<unknown> {
            calls.push({ subject, body });
            return schema.parse({
                result: { outcome: 'ok' },
                parties: [
                    wireParty(SYSTEM_PARTY, 'system', 'Acme System', null),
                    wireParty(
                        '77777777-7777-7777-7777-777777777777',
                        'acme_group',
                        'Acme Group',
                        SYSTEM_PARTY,
                    ),
                ],
                total: 42,
            });
        },
        async close(): Promise<void> {
            return undefined;
        },
    } as unknown as OresClient;
    const party = {
        id: uuid('22222222-2222-2222-2222-222222222222'),
        name: 'Acme System',
        partyCategory: 'System',
        businessCenterCode: '',
    };
    const own = {
        accountId: '11111111-1111-1111-1111-111111111111',
        tenantId: '44444444-4444-4444-4444-444444444444',
        tenantName: 'Acme Corporation',
        version: 'v0.0.25 (test)',
        username: 'admin',
        email: 'admin@example.com',
        availableParties: [party],
        accessLifetimeSeconds: 1800,
        passwordResetRequired: false,
        sessionId: '33333333-3333-3333-3333-333333333333',
    };
    const session = sessions.create({
        ...own,
        client,
        session: { ...own, kind: 'active', token: 't', party },
        mode,
    });
    const server = buildServer({
        config,
        site: siteConfiguration(),
        sessions,
        createClient: () => ({ client, connect: async () => undefined }),
    });
    return { server, cookies: { ores_web_session: session.id }, calls };
}

describe('GET /api/parties', () => {
    it('reads one page of the tenant parties with the server total', async () => {
        const { server, cookies, calls } = buildTestServer('tenant-administration');

        const response = await server.inject({
            method: 'GET',
            url: '/api/parties?offset=20&limit=2',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({
            totalCount: 42,
            parties: [
                { code: 'system', parentName: null },
                { code: 'acme_group', parentName: 'Acme System' },
            ],
        });
        expect(calls).toEqual([
            {
                subject: 'refdata.v1.parties.list',
                body: {
                    offset: 20,
                    limit: 2,
                    order: { field: '', descending: false },
                    as_of: null,
                    filter: null,
                },
            },
        ]);
    });

    it('refuses system administration, whose tenant sees every tenant parties', async () => {
        const { server, cookies, calls } = buildTestServer('system-administration');

        const response = await server.inject({ method: 'GET', url: '/api/parties', cookies });
        await server.close();

        expect(response.statusCode).toBe(403);
        expect(calls).toHaveLength(0);
    });

    it('refuses a page it cannot read', async () => {
        const { server, cookies } = buildTestServer('application');

        const response = await server.inject({
            method: 'GET',
            url: '/api/parties?offset=-1',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(400);
    });
});
