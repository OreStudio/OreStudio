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
 * The change reasons a screen offers.
 *
 * The server's decoder requires every field of the request, so the route must
 * send the order even when it asks for none; it once sent no order and every
 * read was refused as a bad request.
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

function buildTestServer() {
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
                reasons: [
                    {
                        code: 'system.new_record',
                        description: 'A new record',
                        category_code: 'system',
                        applies_to_new: true,
                        applies_to_amend: false,
                        applies_to_delete: false,
                        requires_commentary: false,
                        display_order: 1,
                    },
                ],
                total: 1,
            });
        },
        async close(): Promise<void> {
            return undefined;
        },
    } as unknown as OresClient;
    const party = {
        id: uuid('22222222-2222-2222-2222-222222222222'),
        name: 'System Party',
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
        mode: 'application' as SessionMode,
    });
    const server = buildServer({
        config,
        site: siteConfiguration(),
        sessions,
        createClient: () => ({ client, connect: async () => undefined }),
    });
    return { server, cookies: { ores_web_session: session.id }, calls };
}

describe('GET /api/change-reasons', () => {
    it('asks with every field the server requires, the order included', async () => {
        const { server, cookies, calls } = buildTestServer();

        const response = await server.inject({
            method: 'GET',
            url: '/api/change-reasons',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(calls).toEqual([
            {
                subject: 'dq.v1.change_reasons.list',
                body: { offset: 0, limit: 200, order: { field: '', descending: false } },
            },
        ]);
    });
});
