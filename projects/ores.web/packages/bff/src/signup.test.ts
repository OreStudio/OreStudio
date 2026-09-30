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
import { createRateLimiter } from './rate-limit.js';
import { buildServer } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

/**
 * The door, at the browser's boundary.
 *
 * Two things are asserted here and nothing else: that the address the request
 * arrived at is what reaches the service, because the tenant is resolved from
 * it, and that a refusal keeps its identity. A refusal flattened into one
 * sentence would leave the screen unable to say whether the deployment is
 * closed, the tenant nominates no role, or the username is taken.
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

/** What the service answered, as the fake client reports it. */
interface FakeAnswers {
    readonly policy?: unknown;
    readonly signup?: unknown;
    readonly hostnames?: string[];
}

function buildTestServer(
    answers: FakeAnswers,
    overrides: {
        readonly loginLimiter?: ReturnType<typeof createRateLimiter>;
        readonly policyLimiter?: ReturnType<typeof createRateLimiter>;
    } = {},
): ReturnType<typeof buildServer> {
    const hostnames = answers.hostnames ?? [];
    const client = {
        async registrationPolicy(hostname: string): Promise<unknown> {
            hostnames.push(hostname);
            return (
                answers.policy ?? {
                    success: true,
                    message: '',
                    errorCode: '',
                    signupsEnabled: true,
                    authorizationRequired: false,
                    tenantId: 'ffffffff-ffff-ffff-ffff-ffffffffffff',
                    tenantName: 'Northwind Capital',
                    partyId: '22222222-2222-2222-2222-222222222222',
                    partyName: 'Northwind Operations',
                    roleId: '44444444-4444-4444-4444-444444444444',
                    roleName: 'Viewer',
                    usableNow: true,
                }
            );
        },
        async signup(input: { readonly hostname: string }): Promise<unknown> {
            hostnames.push(input.hostname);
            return (
                answers.signup ?? {
                    success: true,
                    message: '',
                    errorCode: '',
                    accountId: '55555555-5555-5555-5555-555555555555',
                    accountStatus: 'active',
                    partyId: '22222222-2222-2222-2222-222222222222',
                    roleId: '44444444-4444-4444-4444-444444444444',
                }
            );
        },
        async close(): Promise<void> {
            return undefined;
        },
    } as unknown as OresClient;

    return buildServer({
        config,
        site: siteConfiguration(),
        sessions: createSessionStore({ ttlSeconds: 60 }),
        ...(overrides.loginLimiter !== undefined && { loginLimiter: overrides.loginLimiter }),
        ...(overrides.policyLimiter !== undefined && { policyLimiter: overrides.policyLimiter }),
        createClient: () => ({ client, connect: async () => undefined }),
    });
}

describe('GET /api/registration-policy', () => {
    it('asks the service about the address the request arrived at', async () => {
        const hostnames: string[] = [];
        const server = buildTestServer({ hostnames });

        const answer = await server.inject({
            method: 'GET',
            url: '/api/registration-policy',
            headers: { host: 'northwind.example.com:3080' },
        });

        expect(answer.statusCode).toBe(200);
        // The port is not part of a tenant's address, so it never reaches the
        // lookup that resolves one.
        expect(hostnames).toEqual(['northwind.example.com']);
        expect(answer.json()).toMatchObject({
            signupsEnabled: true,
            tenantName: 'Northwind Capital',
            roleName: 'Viewer',
            usableNow: true,
        });

        await server.close();
    });

    it('answers a closed deployment as a state rather than an error', async () => {
        const server = buildTestServer({
            policy: {
                success: false,
                message: 'This deployment does not accept registrations.',
                errorCode: 'signup_disabled',
                signupsEnabled: false,
                authorizationRequired: false,
                tenantId: '',
                tenantName: '',
                partyId: '',
                partyName: '',
                roleId: '',
                roleName: '',
                usableNow: false,
            },
        });

        const answer = await server.inject({ method: 'GET', url: '/api/registration-policy' });

        expect(answer.statusCode).toBe(200);
        expect(answer.json()).toMatchObject({
            success: false,
            errorCode: 'signup_disabled',
            signupsEnabled: false,
        });

        await server.close();
    });
});

describe('POST /api/signup', () => {
    it('registers against the address it was served at, and answers with the state', async () => {
        const hostnames: string[] = [];
        const server = buildTestServer({ hostnames });

        const answer = await server.inject({
            method: 'POST',
            url: '/api/signup',
            headers: { host: 'northwind.example.com' },
            payload: {
                principal: 'jdoe',
                email: 'jdoe@northwind.example.com',
                password: 'Secret-1!',
            },
        });

        expect(answer.statusCode).toBe(200);
        expect(hostnames).toEqual(['northwind.example.com']);
        expect(answer.json()).toMatchObject({ success: true, accountStatus: 'active' });

        await server.close();
    });

    it('carries a refusal as a code the screen branches on', async () => {
        const server = buildTestServer({
            signup: {
                success: false,
                message: 'That username is already taken.',
                errorCode: 'username_taken',
                accountId: '',
                accountStatus: '',
                partyId: '',
                roleId: '',
            },
        });

        const answer = await server.inject({
            method: 'POST',
            url: '/api/signup',
            payload: {
                principal: 'jdoe',
                email: 'jdoe@northwind.example.com',
                password: 'Secret-1!',
            },
        });

        expect(answer.statusCode).toBe(409);
        expect(answer.json()).toEqual({
            code: 'username-taken',
            message: 'That username is already taken.',
        });

        await server.close();
    });

    it('reports a closed deployment as a refusal of the request, not a conflict', async () => {
        const server = buildTestServer({
            signup: {
                success: false,
                message: 'This deployment does not accept registrations.',
                errorCode: 'signup_disabled',
                accountId: '',
                accountStatus: '',
                partyId: '',
                roleId: '',
            },
        });

        const answer = await server.inject({
            method: 'POST',
            url: '/api/signup',
            payload: {
                principal: 'jdoe',
                email: 'jdoe@northwind.example.com',
                password: 'Secret-1!',
            },
        });

        expect(answer.statusCode).toBe(403);
        expect(answer.json()).toMatchObject({ code: 'signups-disabled' });

        await server.close();
    });

    it('keeps a code this build does not know apart from a client defect', async () => {
        const server = buildTestServer({
            signup: {
                success: false,
                message: 'The server refused for a reason this build has not learned.',
                errorCode: 'something_new',
                accountId: '',
                accountStatus: '',
                partyId: '',
                roleId: '',
            },
        });

        const answer = await server.inject({
            method: 'POST',
            url: '/api/signup',
            payload: {
                principal: 'jdoe',
                email: 'jdoe@northwind.example.com',
                password: 'Secret-1!',
            },
        });

        expect(answer.statusCode).toBe(403);
        expect(answer.json()).toMatchObject({ code: 'signup-refused' });

        await server.close();
    });

    it('refuses a body that is missing a credential', async () => {
        const server = buildTestServer({});

        const answer = await server.inject({
            method: 'POST',
            url: '/api/signup',
            payload: { principal: 'jdoe' },
        });

        expect(answer.statusCode).toBe(400);
        expect(answer.json()).toMatchObject({ code: 'invalid-request' });

        await server.close();
    });

    it('shares the sign-in limiter, because both are writes a stranger can drive', async () => {
        const server = buildTestServer(
            {},
            { loginLimiter: createRateLimiter({ maxAttempts: 1, windowSeconds: 60 }) },
        );
        const body = {
            principal: 'jdoe',
            email: 'jdoe@northwind.example.com',
            password: 'Secret-1!',
        };

        const first = await server.inject({ method: 'POST', url: '/api/signup', payload: body });
        const second = await server.inject({ method: 'POST', url: '/api/signup', payload: body });

        expect(first.statusCode).toBe(200);
        expect(second.statusCode).toBe(429);
        expect(second.json()).toEqual({
            code: 'too-many-requests',
            message: 'Too many registration attempts. Wait a minute and try again.',
        });

        await server.close();
    });
});

describe('the policy read', () => {
    it('is limited on its own budget, so a reload never spends a sign-in attempt', async () => {
        const server = buildTestServer(
            {},
            {
                loginLimiter: createRateLimiter({ maxAttempts: 100, windowSeconds: 60 }),
                policyLimiter: createRateLimiter({ maxAttempts: 1, windowSeconds: 60 }),
            },
        );

        const first = await server.inject({ method: 'GET', url: '/api/registration-policy' });
        const second = await server.inject({ method: 'GET', url: '/api/registration-policy' });

        expect(first.statusCode).toBe(200);
        expect(second.statusCode).toBe(429);
        expect(second.json()).toMatchObject({ code: 'too-many-requests' });

        await server.close();
    });
});
