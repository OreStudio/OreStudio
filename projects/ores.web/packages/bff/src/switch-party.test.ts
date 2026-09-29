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
import type { OresClient, PartyRow, PartySummary } from '@ores/wire-protocol';
import type { Config } from './config.js';
import { buildServer } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

/**
 * The route a journey switches the session through.
 *
 * A party added during a session is not in the list the login answered with, so
 * this route reads the tenant's parties rather than taking the caller's word
 * for the party, and states the chosen one in the session. What is asserted
 * here is the session the browser reads back and the party the client was asked
 * to re-scope to.
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

const OLD_PARTY = '11111111-1111-4111-8111-111111111111';
const NEW_PARTY = '22222222-2222-4222-8222-222222222222';

const oldSummary: PartySummary = {
    id: OLD_PARTY,
    name: 'BARCLAYS PLC',
    partyCategory: 'Operational',
    businessCenterCode: 'WRLD',
};

function newRow(): PartyRow {
    return {
        id: NEW_PARTY,
        short_code: 'NORCAP',
        full_name: 'NORTHWIND CAPITAL PLC',
        party_category: 'Operational',
        party_type: 'Corporate',
        parent_party_id: OLD_PARTY,
        business_center_code: 'WRLD',
        status: 'Active',
    };
}

interface TestServer {
    readonly server: ReturnType<typeof buildServer>;
    readonly sessionId: string;
    readonly switched: { readonly partyId: string }[];
    readonly sessions: ReturnType<typeof createSessionStore>;
}

function buildTestServer(): TestServer {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const switched: { readonly partyId: string }[] = [];
    const client = {
        async listParties(): Promise<readonly PartyRow[]> {
            return [newRow()];
        },
        async switchParty(input: { readonly partyId: string }): Promise<unknown> {
            switched.push({ partyId: input.partyId });
            return { token: 'token-two', party: oldSummary, accessLifetimeSeconds: 1800 };
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
        availableParties: [oldSummary],
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
        switched,
        sessions,
    };
}

describe('POST /api/session/switch-party', () => {
    it('works as the party it was asked for and answers the session that names it', async () => {
        const { server, sessionId, switched, sessions } = buildTestServer();

        const response = await server.inject({
            method: 'POST',
            url: '/api/session/switch-party',
            cookies: { ores_web_session: sessionId },
            payload: { partyId: NEW_PARTY },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({
            tenantName: 'Barclays',
            party: {
                id: NEW_PARTY,
                name: 'NORTHWIND CAPITAL PLC',
                partyCategory: 'Operational',
                businessCenterCode: 'WRLD',
            },
        });
        expect(switched).toEqual([{ partyId: NEW_PARTY }]);

        await server.close();
        expect(sessions.size).toBe(0);
    });

    it('answers the party the session works in', async () => {
        const { server, sessionId } = buildTestServer();

        await server.inject({
            method: 'POST',
            url: '/api/session/switch-party',
            cookies: { ores_web_session: sessionId },
            payload: { partyId: NEW_PARTY },
        });
        const session = await server.inject({
            method: 'GET',
            url: '/api/session',
            cookies: { ores_web_session: sessionId },
        });

        // The party added since the login is one of the ones the account may
        // work in, so the session names it in the list as well as in the party.
        expect(session.json().party.id).toBe(NEW_PARTY);
        expect(session.json().availableParties.map((party: PartySummary) => party.id)).toEqual([
            OLD_PARTY,
            NEW_PARTY,
        ]);

        await server.close();
    });

    it('refuses a party the tenant does not hold, and switches nothing', async () => {
        const { server, sessionId, switched } = buildTestServer();

        const response = await server.inject({
            method: 'POST',
            url: '/api/session/switch-party',
            cookies: { ores_web_session: sessionId },
            payload: { partyId: '99999999-9999-4999-8999-999999999999' },
        });

        expect(response.statusCode).toBe(400);
        expect(response.json()).toMatchObject({ code: 'invalid-request' });
        expect(switched).toEqual([]);

        await server.close();
    });

    it('refuses a switch with no session', async () => {
        const { server } = buildTestServer();

        const response = await server.inject({
            method: 'POST',
            url: '/api/session/switch-party',
            payload: { partyId: NEW_PARTY },
        });

        expect(response.statusCode).toBe(401);
        expect(response.json()).toMatchObject({ code: 'not-authenticated' });

        await server.close();
    });
});
