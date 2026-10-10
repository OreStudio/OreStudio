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

const ok = { outcome: 'ok', code: '', message: '' };

function buildTestServer(replies: Readonly<Record<string, unknown>>): {
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
            if (!(subject in replies)) {
                throw new Error(`No canned reply for ${subject}`);
            }
            return schema.parse(replies[subject]);
        },
        async close(): Promise<void> {
            return undefined;
        },
    } as unknown as OresClient;
    const session = sessions.create({
        client,
        session: null,
        username: 'tenant_admin',
        email: 'tenant_admin@acme.example',
        accountId: '11111111-1111-1111-1111-111111111111',
        tenantId: '44444444-4444-4444-4444-444444444444',
        tenantName: 'Acme',
        mode: 'tenant-administration',
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

function send(
    server: ReturnType<typeof buildServer>,
    sessionId: string,
    method: 'GET' | 'POST' | 'PUT' | 'DELETE',
    url: string,
    payload?: unknown,
): ReturnType<ReturnType<typeof buildServer>['inject']> {
    return server.inject({
        method,
        url,
        cookies: { [SESSION_COOKIE]: sessionId },
        ...(payload === undefined ? {} : { payload: payload as Record<string, unknown> }),
    });
}

const ID = '55555555-5555-4555-8555-555555555555';
const UNIT = '88888888-8888-4888-8888-888888888888';
const INTENT = { reason_code: 'common.correction', commentary: 'fix' };

describe('party details routes', () => {
    it('lists the tenant parties without the System party, searched on the server', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.parties.list': {
                result: ok,
                parties: [
                    { id: ID, short_code: 'ZED', party_category: 'Operational', version: 1 },
                    { id: UNIT, short_code: 'SYS', party_category: 'System', version: 1 },
                    { id: 'x', short_code: 'ACME', party_category: 'Operational', version: 1 },
                ],
            },
        });
        const response = await send(server, sessionId, 'GET', '/api/party-details?search=a');
        expect(response.json().rows.map((r: { short_code: string }) => r.short_code)).toEqual([
            'ACME',
            'ZED',
        ]);
        expect(response.json().total).toBe(2);
        expect((calls[0]?.body as { filter: { search: string } }).filter.search).toBe('a');
    });

    it('reads one set a party carries by its party id', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.party_countries.list_by_party_id': {
                result: ok,
                party_countries: [{ party_id: ID, country_alpha2_code: 'GB', version: 1 }],
            },
        });
        const response = await send(server, sessionId, 'GET', `/api/party-details/${ID}/countries`);
        expect(response.json().rows).toHaveLength(1);
        expect(calls[0]?.body).toMatchObject({ party_id: ID, scope: 'direct' });
        const unknown = await send(server, sessionId, 'GET', `/api/party-details/${ID}/bogus`);
        expect(unknown.statusCode).toBe(400);
    });

    it('refuses a party id that is not an id', async () => {
        const { server, sessionId } = buildTestServer({});
        const response = await send(server, sessionId, 'GET', '/api/party-details/nope/contacts');
        expect(response.statusCode).toBe(400);
    });

    it('writes the composite through the refdata operation and returns the refusal', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.ops.put_party_composite': {
                result: { outcome: 'conflict', code: 'stale_version', message: 'Stale.' },
                party: null,
            },
        });
        const response = await send(server, sessionId, 'PUT', '/api/party-details/composite', {
            intent: INTENT,
            party: { id: ID, version: 3 },
            identifiers: [],
            contacts: [],
        });
        expect(response.json().result.code).toBe('stale_version');
        expect(calls[0]?.subject).toBe('refdata.v1.ops.put_party_composite');
    });

    it('reads the composite as it stood at a version', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.ops.get_party_composite_as_of': {
                success: true,
                message: '',
                party: { id: ID },
                identifiers: [],
                contacts: [],
            },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            `/api/party-details/${ID}/composite?version=2`,
        );
        expect(response.json().party.id).toBe(ID);
        expect(calls[0]?.body).toEqual({ id: ID, version: 2 });
    });

    it('links and closes a membership', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.party_currencies.put': { result: ok },
            'refdata.v1.party_currencies.delete': { result: ok },
        });
        const linked = await send(
            server,
            sessionId,
            'PUT',
            `/api/party-details/${ID}/currencies/GBP`,
            {
                intent: INTENT,
            },
        );
        expect(linked.json().result.outcome).toBe('ok');
        expect(calls[0]?.body).toMatchObject({
            change: { write: { party_id: ID, currency_iso_code: 'GBP' } },
        });
        const closed = await send(
            server,
            sessionId,
            'DELETE',
            `/api/party-details/${ID}/currencies/GBP`,
            { intent: INTENT },
        );
        expect(closed.json().result.outcome).toBe('ok');
        expect(calls[1]?.body).toMatchObject({
            removal: { key: { party_id: ID, currency_iso_code: 'GBP' } },
        });
    });

    it('reclassifies a business unit against the version it read', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.business_units.put': {
                result: { outcome: 'invalid', code: 'level_violation', message: 'Level.' },
            },
        });
        const response = await send(
            server,
            sessionId,
            'PUT',
            `/api/party-details/${ID}/business-units`,
            {
                intent: INTENT,
                version: 4,
                write: { id: UNIT, party_id: ID, unit_type_id: null, unit_code: 'U1' },
            },
        );
        expect(response.json().result.code).toBe('level_violation');
        expect(calls[0]?.body).toMatchObject({
            change: { precondition: { kind: 'must_match_version', version: 4 } },
        });
    });

    it('refuses a business unit that names another party', async () => {
        const { server, sessionId } = buildTestServer({});
        const response = await send(
            server,
            sessionId,
            'PUT',
            `/api/party-details/${ID}/business-units`,
            {
                intent: INTENT,
                version: 1,
                write: { id: UNIT, party_id: UNIT, unit_type_id: null },
            },
        );
        expect(response.statusCode).toBe(400);
    });

    it('serves the history of a party', async () => {
        const { server, sessionId } = buildTestServer({
            'refdata.v1.history.get': { success: true, message: '', versions: [] },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            `/api/history?entityType=ores.refdata.party&entityId=${ID}`,
        );
        expect(response.statusCode).toBe(200);
    });

    it('retires an identifier by its value against the version it read', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.party_identifiers.list_by_party_id': {
                result: ok,
                party_identifiers: [{ id_value: 'LEI-OLD', version: 2 }],
            },
            'refdata.v1.party_identifiers.delete': { result: ok },
        });
        const response = await send(
            server,
            sessionId,
            'DELETE',
            `/api/party-details/${ID}/identifiers`,
            { intent: INTENT, idValue: 'LEI-OLD', version: 2 },
        );
        expect(response.json().result.outcome).toBe('ok');
        expect(calls[1]?.body).toMatchObject({
            removal: {
                key: { id_value: 'LEI-OLD' },
                precondition: { kind: 'must_match_version', version: 2 },
            },
        });
    });

    it('refuses to retire a value the party does not hold', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.party_identifiers.list_by_party_id': {
                result: ok,
                party_identifiers: [{ id_value: 'OTHER', version: 1 }],
            },
        });
        const response = await send(
            server,
            sessionId,
            'DELETE',
            `/api/party-details/${ID}/identifiers`,
            { intent: INTENT, idValue: 'LEI-OLD', version: 2 },
        );
        expect(response.statusCode).toBe(404);
        expect(calls).toHaveLength(1);
    });

    it('refuses an unlink with no body and a counterparty link that is not an id', async () => {
        const { server, sessionId } = buildTestServer({});
        const bare = await send(
            server,
            sessionId,
            'DELETE',
            `/api/party-details/${ID}/currencies/GBP`,
        );
        expect(bare.statusCode).toBe(400);
        const bad = await send(
            server,
            sessionId,
            'PUT',
            `/api/party-details/${ID}/counterparties/not-an-id`,
            { intent: INTENT },
        );
        expect(bad.statusCode).toBe(400);
    });
});
