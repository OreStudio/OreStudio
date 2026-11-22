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
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

/**
 * The CORS answer of the BFF, which the C++ server states alike.
 *
 * A screen on another origin reads an answer only if the answer admits it, so the
 * three things below are one contract: who may call, whether the call carries a
 * session, and what of the answer the browser may read. A route that does not exist
 * carries them too, or a person reads a missing route as a broken network.
 */

const ORIGIN = 'http://localhost:20401';

const config: Config = {
    port: 0,
    host: '127.0.0.1',
    logLevel: 'silent',
    session: { ttlSeconds: 3600, cookieSecure: false },
    allowedOrigins: [ORIGIN],
    loginAttemptsPerMinute: 100,
};

function buildTestServer(): ReturnType<typeof buildServer> {
    const site = loadSiteConfiguration({
        environment: {
            [SITE_CONFIG_VARIABLE]: resolve(
                dirname(fileURLToPath(import.meta.url)),
                '../../../config/environments.json',
            ),
        },
        environmentId: 'eager_maxwell',
    });
    const client = {
        async callAuthenticated(): Promise<unknown> {
            throw new Error('A route that needs a session is not part of this test.');
        },
        async close(): Promise<void> {
            return undefined;
        },
    } as unknown as OresClient;
    return buildServer({
        config,
        site,
        createClient: () => ({ client, connect: async () => undefined }),
    });
}

describe('the CORS answer', () => {
    it('admits a listed origin, and says so to a cache', async () => {
        const response = await buildTestServer().inject({
            method: 'GET',
            url: '/api/no-such-route',
            headers: { origin: ORIGIN },
        });

        expect(response.statusCode).toBe(404);
        expect(response.headers['access-control-allow-origin']).toBe(ORIGIN);
        expect(response.headers['access-control-allow-credentials']).toBe('true');
        expect(response.headers.vary).toBe('Origin');
        expect(response.headers['access-control-expose-headers']).toBe(
            'X-Request-Id, Content-Disposition',
        );
    });

    it('states what a call from another origin may send', async () => {
        const response = await buildTestServer().inject({
            method: 'OPTIONS',
            url: '/api/site',
            headers: { origin: ORIGIN, 'access-control-request-headers': 'x-ores-request' },
        });

        expect(response.statusCode).toBe(204);
        expect(response.headers['access-control-allow-headers']).toContain('X-Ores-Request');
        expect(response.headers['access-control-allow-methods']).toContain('PATCH');
    });

    it('admits no origin the list does not name', async () => {
        const response = await buildTestServer().inject({
            method: 'GET',
            url: '/api/no-such-route',
            headers: { origin: 'http://evil.example' },
        });

        expect(response.headers['access-control-allow-origin']).toBeUndefined();
        expect(response.headers['access-control-allow-credentials']).toBeUndefined();
    });
});
