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
import type { OresClient, PasswordPolicy } from '@ores/wire-protocol';
import type { Config } from './config.js';
import { buildServer } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

/**
 * The route a sign-in screen reads the password rules through.
 *
 * It is served without a session, because the screen that shows the rules is
 * the one a person signs in on, so what is asserted here is that the answer
 * reaches a caller who has not signed in and that it is the server's answer.
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

const policy: PasswordPolicy = {
    success: true,
    message: '',
    minLength: 12,
    requireUppercase: true,
    requireLowercase: true,
    requireDigit: true,
    requireSpecial: true,
    specialChars: '!@#$%^&*()_+-=[]{}|;:,.<>?',
};

function buildTestServer(read: unknown) {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const client = {
        async passwordPolicy(): Promise<unknown> {
            return read;
        },
        async close(): Promise<void> {
            return undefined;
        },
    } as unknown as OresClient;
    return buildServer({
        config,
        site: siteConfiguration(),
        sessions,
        createClient: () => ({ client, connect: async () => undefined }),
    });
}

describe('GET /api/password-policy', () => {
    it('serves the server\u2019s rules to a caller with no session', async () => {
        const server = buildTestServer(policy);

        const response = await server.inject({ method: 'GET', url: '/api/password-policy' });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual(policy);

        await server.close();
    });
});
