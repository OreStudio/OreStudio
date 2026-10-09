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
 * The first-run status read, and the route that records its finish.
 *
 * The status is composed from two sources, so what is asserted here is that
 * both facts reach the browser: the IAM answer, and the variability setting
 * that says the system provisioner wizard finished. A read that fails must
 * answer false, because that is the state the gate treats as unfinished, and
 * an installation that never ran the wizard must not be released by accident.
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

const northwind = {
    id: '22222222-2222-2222-2222-222222222222',
    name: 'Northwind Capital',
    partyCategory: 'Operational',
    businessCenterCode: 'GBLO',
};

/** What IAM answers: an administrator exists, and no tenant of the deployment's own. */
const status = {
    isInBootstrapMode: false,
    hasTenant: false,
    message: 'This deployment has no tenant of its own.',
    version: 'v0.0.25 (test)',
};

interface Harness {
    readonly server: ReturnType<typeof buildServer>;
    readonly sessionId: string;
    readonly calls: string[];
}

function buildTestServer(
    overrides: {
        readonly setting?: () => Promise<boolean>;
        readonly tenantSetting?: () => Promise<boolean>;
        readonly complete?: () => Promise<void>;
    } = {},
): Harness {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const calls: string[] = [];
    const client = {
        async bootstrapStatus(): Promise<unknown> {
            return status;
        },
        async onboardingSystemComplete(): Promise<boolean> {
            calls.push('onboardingSystemComplete');
            return overrides.setting === undefined ? false : overrides.setting();
        },
        async onboardingTenantComplete(): Promise<boolean> {
            calls.push('onboardingTenantComplete');
            return overrides.tenantSetting === undefined ? false : overrides.tenantSetting();
        },
        async completeSystemOnboarding(): Promise<void> {
            calls.push('completeSystemOnboarding');
            if (overrides.complete !== undefined) {
                await overrides.complete();
            }
        },
        async close(): Promise<void> {
            return undefined;
        },
    } as unknown as OresClient;
    const session = sessions.create({
        client,
        session: {
            kind: 'active',
            token: 'token',
            accountId: '11111111-1111-1111-1111-111111111111',
            tenantId: 'ffffffff-ffff-ffff-ffff-ffffffffffff',
            tenantName: 'System',
            version: 'v0.0.25 (test)',
            username: 'super_admin',
            email: 'super_admin@system.ores',
            party: northwind,
            availableParties: [northwind],
            accessLifetimeSeconds: 1800,
            passwordResetRequired: false,
            sessionId: '33333333-3333-3333-3333-333333333333',
        },
        username: 'super_admin',
        email: 'super_admin@system.ores',
        accountId: '11111111-1111-1111-1111-111111111111',
        tenantId: 'ffffffff-ffff-ffff-ffff-ffffffffffff',
        tenantName: 'System',
        mode: 'system-administration',
        version: 'v0.0.25 (test)',
        availableParties: [northwind],
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

describe('GET /api/bootstrap', () => {
    it('carries the wizard flag beside the IAM facts, so a system-only run can leave the rail', async () => {
        const { server, sessionId } = buildTestServer({
            setting: async () => true,
            tenantSetting: async () => true,
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/bootstrap',
            cookies: { [SESSION_COOKIE]: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            isInBootstrapMode: false,
            hasTenant: false,
            onboardingComplete: true,
            onboardingTenantComplete: true,
            message: 'This deployment has no tenant of its own.',
            version: 'v0.0.25 (test)',
        });

        await server.close();
    });

    it('answers the tenant flag false when its setting read fails', async () => {
        const { server, sessionId } = buildTestServer({
            tenantSetting: async () => {
                throw new Error('the settings read was refused');
            },
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/bootstrap',
            cookies: { [SESSION_COOKIE]: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({ onboardingTenantComplete: false });

        await server.close();
    });

    it('answers onboardingComplete false when the setting read fails', async () => {
        const { server, sessionId } = buildTestServer({
            setting: async () => {
                throw new Error('the settings read was refused');
            },
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/bootstrap',
            cookies: { [SESSION_COOKIE]: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({ onboardingComplete: false, hasTenant: false });

        await server.close();
    });

    it('answers onboardingComplete false when there is no session to read through', async () => {
        const { server, calls } = buildTestServer();

        const response = await server.inject({ method: 'GET', url: '/api/bootstrap' });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({ onboardingComplete: false });
        expect(calls).toEqual([]);

        await server.close();
    });
});

describe('POST /api/bootstrap/complete', () => {
    it('records the finish through the caller\u2019s session', async () => {
        const { server, sessionId, calls } = buildTestServer();

        const response = await server.inject({
            method: 'POST',
            url: '/api/bootstrap/complete',
            cookies: { [SESSION_COOKIE]: sessionId },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({ success: true });
        expect(calls).toEqual(['completeSystemOnboarding']);

        await server.close();
    });

    it('refuses a request with no session', async () => {
        const { server, calls } = buildTestServer();

        const response = await server.inject({ method: 'POST', url: '/api/bootstrap/complete' });

        expect(response.statusCode).toBe(401);
        expect(response.json()).toMatchObject({ code: 'not-authenticated' });
        expect(calls).toEqual([]);

        await server.close();
    });
});
