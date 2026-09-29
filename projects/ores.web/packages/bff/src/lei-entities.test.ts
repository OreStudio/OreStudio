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
 * The read behind the legal-entity search.
 *
 * What matters here is the boundary: the text a person typed reaches the read
 * rather than a page being fetched and filtered, and each match carries the
 * number of parties its hierarchy would create, in the shape the browser parses.
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

interface Call {
    readonly subject: string;
    readonly request: unknown;
}

function buildTestServer(): {
    readonly server: ReturnType<typeof buildServer>;
    readonly sessionId: string;
    readonly calls: Call[];
} {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const calls: Call[] = [];
    const client = {
        async callAuthenticated(subject: string, request: unknown): Promise<unknown> {
            calls.push({ subject, request });
            return {
                success: true,
                error_message: '',
                entities: [
                    {
                        lei: '213800LBQA1Y9L22JB70',
                        entity_legal_name: 'BARCLAYS PLC',
                        entity_category: 'Corporate',
                        country: 'GB',
                        party_count: 81,
                    },
                ],
            };
        },
        async close(): Promise<void> {
            return undefined;
        },
    } as unknown as OresClient;
    const session = sessions.create({
        client,
        session: null,
        username: 'admin',
        email: 'admin@acme.test',
        accountId: '11111111-1111-1111-1111-111111111111',
        tenantId: 'ffffffff-ffff-ffff-ffff-ffffffffffff',
        tenantName: 'System',
        version: 'v0.0.25 (test)',
        availableParties: [],
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

describe('GET /api/lei-entities', () => {
    it('matches what the person typed and states the work each match brings', async () => {
        const { server, sessionId, calls } = buildTestServer();

        const response = await server.inject({
            method: 'GET',
            url: '/api/lei-entities?search=barclays',
            cookies: { ores_web_session: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            entities: [
                {
                    lei: '213800LBQA1Y9L22JB70',
                    legalName: 'BARCLAYS PLC',
                    country: 'GB',
                    partyCount: 81,
                },
            ],
        });
        expect(calls).toEqual([
            {
                subject: 'dq.v1.lei-entities.search',
                request: { search: 'barclays', country_filter: '', offset: 0, limit: 20 },
            },
        ]);

        await server.close();
    });

    it('refuses a read with no session', async () => {
        const { server } = buildTestServer();

        const response = await server.inject({
            method: 'GET',
            url: '/api/lei-entities?search=barclays',
        });

        expect(response.statusCode).toBe(401);
        expect(response.json()).toMatchObject({ code: 'not-authenticated' });

        await server.close();
    });
});
