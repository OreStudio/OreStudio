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
import type { OresClient, PartyRow } from '@ores/wire-protocol';
import type { Config } from './config.js';
import { buildServer } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

/**
 * The route the party journey adds a party through.
 *
 * It is three writes in one act -- the party, its LEI and the run that brings
 * it to life -- so what is asserted here is that each one is reached with what
 * the browser sent, that where the party sits is the deployment's answer, and
 * that a refusal stops the act where it happened rather than starting a run
 * that cannot do what it was asked.
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

const SYSTEM_PARTY = '11111111-1111-4111-8111-111111111111';
const ROOT_PARTY = '22222222-2222-4222-8222-222222222222';
const NEW_PARTY = '33333333-3333-4333-8333-333333333333';
const INSTANCE = '9c1f0f5a-6bd2-4f2a-9a4a-6f1a3a2b4c5d';

function row(overrides: Partial<PartyRow> = {}): PartyRow {
    return {
        id: ROOT_PARTY,
        short_code: 'BRCLYS',
        full_name: 'BARCLAYS PLC',
        party_category: 'Operational',
        party_type: 'Corporate',
        parent_party_id: null,
        business_center_code: 'WRLD',
        status: 'Active',
        ...overrides,
    };
}

interface Call {
    readonly kind: string;
    readonly input: unknown;
}

interface TestServer {
    readonly server: ReturnType<typeof buildServer>;
    readonly sessionId: string;
    readonly calls: Call[];
}

function buildTestServer(options: {
    readonly parties?: readonly PartyRow[];
    readonly create?: unknown;
    readonly start?: unknown;
}): TestServer {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const calls: Call[] = [];
    const client = {
        async listParties(): Promise<readonly PartyRow[]> {
            calls.push({ kind: 'list', input: undefined });
            return options.parties ?? [row()];
        },
        async createParty(input: unknown): Promise<unknown> {
            calls.push({ kind: 'create', input });
            return options.create ?? { success: true, message: '', partyId: NEW_PARTY };
        },
        async provisionParty(input: unknown): Promise<unknown> {
            calls.push({ kind: 'start', input });
            return (
                options.start ?? {
                    success: true,
                    message: '',
                    instanceId: INSTANCE,
                    partyId: NEW_PARTY,
                }
            );
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
        tenantName: 'Barclays',
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

const body = {
    fullName: 'NORTHWIND CAPITAL PLC',
    shortCode: 'NORCAP',
    lei: '213800LBQA1Y9L22JB70',
};

describe('POST /api/provision-party', () => {
    it('adds the party and starts the run that records its LEI', async () => {
        const { server, sessionId, calls } = buildTestServer({});

        const response = await server.inject({
            method: 'POST',
            url: '/api/provision-party',
            cookies: { ores_web_session: sessionId },
            payload: body,
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            success: true,
            message: '',
            instanceId: INSTANCE,
            partyId: NEW_PARTY,
        });
        expect(calls.map((call) => call.kind)).toEqual(['list', 'create', 'start']);
        // No starting point: the caller cannot read the profiles, so the
        // service that holds them chooses the deployment's party stage. The LEI
        // travels with the run, because a party identifier carries the party
        // its writing session acts in and this session works in another one.
        expect(calls[2]?.input).toEqual({
            party: NEW_PARTY,
            profileCode: '',
            lei: body.lei,
        });

        await server.close();
    });

    it('places the party under the tenant\u2019s own party', async () => {
        // The system party has no parent either, and it is the tenant's
        // bookkeeping rather than the organisation a party is added to.
        const { server, sessionId, calls } = buildTestServer({
            parties: [
                row({
                    id: SYSTEM_PARTY,
                    short_code: 'barclays_system',
                    full_name: 'System Party',
                    party_category: 'System',
                }),
                row({ id: '44444444-4444-4444-8444-444444444444', parent_party_id: ROOT_PARTY }),
                row(),
            ],
        });

        await server.inject({
            method: 'POST',
            url: '/api/provision-party',
            cookies: { ores_web_session: sessionId },
            payload: body,
        });

        expect(calls[1]?.input).toEqual({
            shortCode: 'NORCAP',
            fullName: 'NORTHWIND CAPITAL PLC',
            parentPartyId: ROOT_PARTY,
        });

        await server.close();
    });

    it('makes the party the tenant\u2019s root when the tenant has none', async () => {
        const { server, sessionId, calls } = buildTestServer({ parties: [] });

        await server.inject({
            method: 'POST',
            url: '/api/provision-party',
            cookies: { ores_web_session: sessionId },
            payload: body,
        });

        expect(calls[1]?.input).toMatchObject({ parentPartyId: null });

        await server.close();
    });

    it('asks for no LEI for a party that is not an entity the deployment holds', async () => {
        const { server, sessionId, calls } = buildTestServer({});

        const response = await server.inject({
            method: 'POST',
            url: '/api/provision-party',
            cookies: { ores_web_session: sessionId },
            payload: { ...body, lei: '' },
        });

        expect(response.statusCode).toBe(200);
        expect(calls[2]?.input).toEqual({ party: NEW_PARTY, profileCode: '', lei: '' });
        expect(response.json().success).toBe(true);

        await server.close();
    });

    it('answers with the party write\u2019s refusal and starts no run', async () => {
        const { server, sessionId, calls } = buildTestServer({
            create: {
                success: false,
                message: 'A party with this code already exists',
                partyId: '',
            },
        });

        const response = await server.inject({
            method: 'POST',
            url: '/api/provision-party',
            cookies: { ores_web_session: sessionId },
            payload: body,
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            success: false,
            message: 'A party with this code already exists',
            instanceId: '',
            partyId: '',
        });
        expect(calls.map((call) => call.kind)).toEqual(['list', 'create']);

        await server.close();
    });

    it('serves the run\u2019s refusal as a result, not as a failed call', async () => {
        const { server, sessionId } = buildTestServer({
            start: {
                success: false,
                message: "The seed profile 'nope' orders no party step.",
                instanceId: '',
                partyId: '',
            },
        });

        const response = await server.inject({
            method: 'POST',
            url: '/api/provision-party',
            cookies: { ores_web_session: sessionId },
            payload: body,
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            success: false,
            message: "The seed profile 'nope' orders no party step.",
            instanceId: '',
            partyId: '',
        });

        await server.close();
    });

    it('refuses a request with no session', async () => {
        const { server } = buildTestServer({});

        const response = await server.inject({
            method: 'POST',
            url: '/api/provision-party',
            payload: body,
        });

        expect(response.statusCode).toBe(401);
        expect(response.json()).toMatchObject({ code: 'not-authenticated' });

        await server.close();
    });

    it('refuses a party that has no name or no short code', async () => {
        const { server, sessionId, calls } = buildTestServer({});

        for (const payload of [
            { ...body, fullName: '' },
            { ...body, shortCode: '' },
        ]) {
            const response = await server.inject({
                method: 'POST',
                url: '/api/provision-party',
                cookies: { ores_web_session: sessionId },
                payload,
            });
            expect(response.statusCode).toBe(400);
            expect(response.json()).toMatchObject({ code: 'invalid-request' });
        }
        expect(calls).toEqual([]);

        await server.close();
    });
});
