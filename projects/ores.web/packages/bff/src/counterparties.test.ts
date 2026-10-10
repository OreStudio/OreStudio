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
const OTHER = '66666666-6666-4666-8666-666666666666';
const PARTY = '77777777-7777-4777-8777-777777777777';
const INTENT = { reason_code: 'common.new_record', commentary: '' };

function counterparty(id: string, shortCode: string, status: string): Record<string, unknown> {
    return { id, short_code: shortCode, full_name: `${shortCode} Ltd`, status, version: 1 };
}

const list = 'refdata.v1.counterparties.list';

describe('counterparty routes', () => {
    it('pages, searches and filters the landing list on the server', async () => {
        const { server, sessionId, calls } = buildTestServer({
            [list]: {
                result: ok,
                total: 3,
                counterparties: [
                    counterparty(ID, 'BETA', 'Active'),
                    counterparty(OTHER, 'ALPHA', 'Closed'),
                    counterparty(PARTY, 'GAMMA', 'Active'),
                ],
            },
        });
        const all = await send(server, sessionId, 'GET', '/api/counterparties?status=all');
        expect(all.json().total).toBe(3);
        expect(all.json().rows.map((r: { short_code: string }) => r.short_code)).toEqual([
            'ALPHA',
            'BETA',
            'GAMMA',
        ]);
        const active = await send(server, sessionId, 'GET', '/api/counterparties?status=active');
        expect(active.json().total).toBe(2);
        const closed = await send(server, sessionId, 'GET', '/api/counterparties?status=closed');
        expect(closed.json().rows).toHaveLength(1);
        const found = await send(server, sessionId, 'GET', '/api/counterparties?search=gam');
        expect(found.json().rows.map((r: { id: string }) => r.id)).toEqual([PARTY]);
        const paged = await send(server, sessionId, 'GET', '/api/counterparties?offset=1&limit=1');
        expect(paged.json().rows.map((r: { short_code: string }) => r.short_code)).toEqual([
            'BETA',
        ]);
        expect(paged.json().total).toBe(3);
        expect(calls[0]?.subject).toBe(list);
    });

    it('reads the children of the named counterparties, one call per child type', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.counterparty_identifiers.list': {
                result: ok,
                counterparty_identifiers: [{ counterparty_id: ID, id_value: 'LEI1', version: 1 }],
            },
            'refdata.v1.counterparty_contact_informations.list': {
                result: ok,
                counterparty_contact_informations: [{ counterparty_id: OTHER, version: 1 }],
            },
            'refdata.v1.counterparty_business_centres.list': {
                result: ok,
                counterparty_business_centres: [
                    { counterparty_id: ID, business_centre_code: 'GBLO', version: 1 },
                ],
            },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            `/api/counterparties/children?ids=${ID},${OTHER}`,
        );
        expect(response.statusCode).toBe(200);
        const [first, second] = response.json().children;
        expect(first.counterpartyId).toBe(ID);
        expect(first.identifiers).toHaveLength(1);
        expect(first.contacts).toHaveLength(0);
        expect(first.centres).toHaveLength(1);
        expect(second.contacts).toHaveLength(1);
        expect(calls).toHaveLength(3);
        expect(
            (calls[0]?.body as { filter: { counterparty_id_one_of: string[] } }).filter
                .counterparty_id_one_of,
        ).toEqual([ID, OTHER]);
    });

    it('refuses a children request that names no valid id', async () => {
        const { server, sessionId } = buildTestServer({});
        const response = await send(server, sessionId, 'GET', '/api/counterparties/children?ids=x');
        expect(response.statusCode).toBe(400);
    });

    it('reads the six pick lists', async () => {
        const rows = (field: string): unknown => ({
            result: ok,
            [field]: [{ code: 'A', version: 1 }],
        });
        const { server, sessionId } = buildTestServer({
            'refdata.v1.party_types.list': rows('types'),
            'refdata.v1.party_statuses.list': rows('statuses'),
            'refdata.v1.party_id_schemes.list': rows('schemes'),
            'refdata.v1.contact_types.list': rows('types'),
            'refdata.v1.business_centres.list': rows('centres'),
            'refdata.v1.currencies.list': rows('currencies'),
        });
        const response = await send(server, sessionId, 'GET', '/api/counterparties/pick-lists');
        expect(response.statusCode).toBe(200);
        expect(Object.keys(response.json()).sort()).toEqual([
            'businessCentres',
            'contactTypes',
            'currencies',
            'identifierSchemes',
            'partyStatuses',
            'partyTypes',
        ]);
    });

    it('writes the composite through the refdata operation and returns the typed refusal', async () => {
        const refusal = {
            outcome: 'invalid',
            code: 'validation_failed',
            message: 'Refused.',
            fields: [{ field: 'short_code', code: 'required', message: 'Needed.' }],
        };
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.ops.put_counterparty_composite': { result: refusal, counterparty: null },
        });
        const response = await send(server, sessionId, 'PUT', '/api/counterparties/composite', {
            intent: INTENT,
            counterparty: { id: ID, short_code: '' },
            identifiers: [],
            contacts: [],
        });
        expect(response.statusCode).toBe(200);
        expect(response.json().result.fields[0].field).toBe('short_code');
        expect(calls[0]?.subject).toBe('refdata.v1.ops.put_counterparty_composite');
        expect((calls[0]?.body as { csas: unknown[] }).csas).toEqual([]);
    });

    it('lists and writes the parties that see a counterparty', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.party_counterparties.list_by_counterparty_id': {
                result: ok,
                party_counterparties: [{ party_id: PARTY, counterparty_id: ID, version: 1 }],
            },
            'refdata.v1.party_counterparties.put': { result: ok },
        });
        const read = await send(server, sessionId, 'GET', `/api/counterparties/${ID}/visibility`);
        expect(read.json().partyCounterparties).toHaveLength(1);
        const written = await send(
            server,
            sessionId,
            'PUT',
            `/api/counterparties/${ID}/visibility`,
            {
                partyId: PARTY,
                intent: INTENT,
            },
        );
        expect(written.json().result.outcome).toBe('ok');
        expect(calls[1]?.body).toMatchObject({
            change: { write: { party_id: PARTY, counterparty_id: ID } },
        });
    });

    it('replaces the business centres by closing the dropped and linking the new', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.counterparty_business_centres.list': {
                result: ok,
                counterparty_business_centres: [
                    { counterparty_id: ID, business_centre_code: 'GBLO', version: 1 },
                    { counterparty_id: ID, business_centre_code: 'USNY', version: 1 },
                ],
            },
            'refdata.v1.counterparty_business_centres.delete': { result: ok },
            'refdata.v1.counterparty_business_centres.put': { result: ok },
        });
        const response = await send(
            server,
            sessionId,
            'PUT',
            `/api/counterparties/${ID}/business-centres`,
            { codes: ['GBLO', 'JPTO'], intent: INTENT },
        );
        expect(response.json().result.outcome).toBe('ok');
        expect(calls.map((call) => call.subject)).toEqual([
            'refdata.v1.counterparty_business_centres.list',
            'refdata.v1.counterparty_business_centres.delete',
            'refdata.v1.counterparty_business_centres.put',
        ]);
        expect(calls[1]?.body).toMatchObject({
            removal: { key: { business_centre_code: 'USNY' } },
        });
        expect(calls[2]?.body).toMatchObject({
            change: { write: { business_centre_code: 'JPTO' } },
        });
    });

    it('stops at the first refused centre write and reports it', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.counterparty_business_centres.list': {
                result: ok,
                counterparty_business_centres: [],
            },
            'refdata.v1.counterparty_business_centres.put': {
                result: { outcome: 'invalid', code: 'unknown_centre', message: 'No such centre.' },
            },
        });
        const response = await send(
            server,
            sessionId,
            'PUT',
            `/api/counterparties/${ID}/business-centres`,
            { codes: ['XXXX', 'GBLO'], intent: INTENT },
        );
        expect(response.json().result.code).toBe('unknown_centre');
        expect(calls).toHaveLength(2);
    });
});
