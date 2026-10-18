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
import { ServerError, type OresClient } from '@ores/wire-protocol';
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

function buildTestServer(
    replies: Readonly<Record<string, unknown>>,
    tenantId = '44444444-4444-4444-4444-444444444444',
): {
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
            const canned = replies[subject];
            if (canned instanceof Error) {
                throw canned;
            }
            return schema.parse(canned);
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
        tenantId,
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

const SCHEMA_SUBJECT = 'marketdata.v1.ops.describe_oresmd_schema';
const PARSE_SUBJECT = 'marketdata.v1.ops.parse_oresmd_uri';
const COMPOSE_SUBJECT = 'marketdata.v1.ops.compose_oresmd_uri';

const URI =
    'oresmd://ir/EUR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=6M&term=10Y';

const SCHEMA_REPLY = {
    assets: ['ir', 'fx'],
    instruments: [
        {
            code: 'ir_swap',
            ore_name: 'IR_SWAP',
            title: '{subject} swap',
            asset: 'ir',
            subject: 'ccy',
            fields: [
                { name: 'ccy', role: 'identity', may_be_none: false, kind: 'text', codes: [] },
                { name: 'term', role: 'coordinate', may_be_none: false, kind: 'term', codes: [] },
            ],
        },
    ],
    quotes: ['rate'],
    quote_titles: ['rate'],
    indexes: [
        { code: 'ibor', asset: 'ir', subject: 'ccy', fields: [{ name: 'name', optional: false }] },
    ],
    success: true,
    message: '',
};

const PARSE_REPLY = {
    success: true,
    message: '',
    kind: 'quote',
    instrument: 'ir_swap',
    quote: 'rate',
    asset: 'ir',
    subject: 'EUR',
    fields: [{ name: 'term', text: '10Y' }],
};

const COMPOSE_REPLY = { success: true, message: '', uri: URI };

describe('the oresmd routes', () => {
    it('need a signed-in session', async () => {
        const { server } = buildTestServer({});
        for (const [method, url] of [
            ['GET', '/api/oresmd/schema'],
            ['POST', '/api/oresmd/parse'],
            ['POST', '/api/oresmd/parse-many'],
            ['POST', '/api/oresmd/compose'],
        ] as const) {
            const response = await server.inject({ method, url, payload: {} });
            expect(response.statusCode).toBe(401);
        }
    });

    it('serve the schema, without the success and message fields, and let a browser keep it', async () => {
        const { server, sessionId, calls } = buildTestServer({ [SCHEMA_SUBJECT]: SCHEMA_REPLY });
        const response = await send(server, sessionId, 'GET', '/api/oresmd/schema');
        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            assets: SCHEMA_REPLY.assets,
            instruments: SCHEMA_REPLY.instruments,
            quotes: SCHEMA_REPLY.quotes,
            quote_titles: SCHEMA_REPLY.quote_titles,
            indexes: SCHEMA_REPLY.indexes,
        });
        expect(response.headers['cache-control']).toBe('private, max-age=300');
        expect(calls).toEqual([{ subject: SCHEMA_SUBJECT, body: {} }]);
    });

    it('forward a URI to the parser and return its parts', async () => {
        const { server, sessionId, calls } = buildTestServer({ [PARSE_SUBJECT]: PARSE_REPLY });
        const response = await send(server, sessionId, 'POST', '/api/oresmd/parse', { uri: URI });
        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual(PARSE_REPLY);
        expect(calls).toEqual([{ subject: PARSE_SUBJECT, body: { uri: URI } }]);
    });

    it("return a URI the codec refuses as an answer with the codec's words", async () => {
        const refused = {
            ...PARSE_REPLY,
            success: false,
            message: 'a query out of order',
            fields: [],
        };
        const { server, sessionId } = buildTestServer({ [PARSE_SUBJECT]: refused });
        const response = await send(server, sessionId, 'POST', '/api/oresmd/parse', { uri: 'x' });
        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({ success: false, message: 'a query out of order' });
    });

    it('read many URIs as one answer, asking the server about each', async () => {
        const { server, sessionId, calls } = buildTestServer({ [PARSE_SUBJECT]: PARSE_REPLY });
        const response = await send(server, sessionId, 'POST', '/api/oresmd/parse-many', {
            uris: [URI, 'oresmd://fx/EUR?ccy=USD'],
        });
        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({ parts: [PARSE_REPLY, PARSE_REPLY] });
        expect(calls).toEqual([
            { subject: PARSE_SUBJECT, body: { uri: URI } },
            { subject: PARSE_SUBJECT, body: { uri: 'oresmd://fx/EUR?ccy=USD' } },
        ]);
    });

    it('read no URI as an empty answer, without asking the server', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(server, sessionId, 'POST', '/api/oresmd/parse-many', {
            uris: [],
        });
        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({ parts: [] });
        expect(calls).toEqual([]);
    });

    it('forward the parts to the composer, filling what a body leaves out', async () => {
        const { server, sessionId, calls } = buildTestServer({ [COMPOSE_SUBJECT]: COMPOSE_REPLY });
        const response = await send(server, sessionId, 'POST', '/api/oresmd/compose', {
            kind: 'quote',
            instrument: 'ir_swap',
            quote: 'rate',
            subject: 'EUR',
            fields: [{ name: 'term', text: '10Y' }],
        });
        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual(COMPOSE_REPLY);
        expect(calls[0]).toEqual({
            subject: COMPOSE_SUBJECT,
            body: {
                kind: 'quote',
                instrument: 'ir_swap',
                quote: 'rate',
                subject: 'EUR',
                fields: [{ name: 'term', text: '10Y' }],
            },
        });
        const fixing = await send(server, sessionId, 'POST', '/api/oresmd/compose', {
            kind: 'fixing',
            instrument: 'ibor',
        });
        expect(fixing.statusCode).toBe(200);
        expect(calls[1]?.body).toEqual({
            kind: 'fixing',
            instrument: 'ibor',
            quote: '',
            subject: '',
            fields: [],
        });
    });

    it('return a refused composition as an answer with the codec words', async () => {
        const refused = { success: false, message: "'fwd_start' is required", uri: '' };
        const { server, sessionId } = buildTestServer({ [COMPOSE_SUBJECT]: refused });
        const response = await send(server, sessionId, 'POST', '/api/oresmd/compose', {
            kind: 'quote',
            instrument: 'ir_swap',
        });
        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual(refused);
    });

    it('refuse a body that is not the structure, without asking the server', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const long = 'a'.repeat(2049);
        const bodies: [string, unknown][] = [
            ['/api/oresmd/parse', {}],
            ['/api/oresmd/parse', { uri: 5 }],
            ['/api/oresmd/parse', { uri: long }],
            ['/api/oresmd/parse', { uri: URI, extra: 1 }],
            ['/api/oresmd/parse-many', {}],
            ['/api/oresmd/parse-many', { uris: 'x' }],
            ['/api/oresmd/parse-many', { uris: [long] }],
            ['/api/oresmd/parse-many', { uris: Array.from({ length: 201 }, () => URI) }],
            ['/api/oresmd/compose', { instrument: 'ir_swap' }],
            ['/api/oresmd/compose', { kind: 'quote', instrument: long }],
            ['/api/oresmd/compose', { kind: 'quote', instrument: 'x', fields: [{ name: 'a' }] }],
            [
                '/api/oresmd/compose',
                {
                    kind: 'quote',
                    instrument: 'x',
                    fields: Array.from({ length: 65 }, () => ({ name: 'a', text: 'b' })),
                },
            ],
        ];
        for (const [url, payload] of bodies) {
            const response = await send(server, sessionId, 'POST', url, payload);
            expect(response.statusCode, `${url} ${JSON.stringify(payload).slice(0, 40)}`).toBe(400);
        }
        expect(calls).toEqual([]);
    });

    it('show a missing permission as a refusal', async () => {
        for (const [method, url, subject, payload] of [
            ['GET', '/api/oresmd/schema', SCHEMA_SUBJECT, undefined],
            ['POST', '/api/oresmd/parse', PARSE_SUBJECT, { uri: URI }],
            ['POST', '/api/oresmd/parse-many', PARSE_SUBJECT, { uris: [URI] }],
            ['POST', '/api/oresmd/compose', COMPOSE_SUBJECT, { kind: 'quote', instrument: 'x' }],
        ] as const) {
            const { server, sessionId } = buildTestServer({
                [subject]: new ServerError('forbidden', subject),
            });
            const response = await send(server, sessionId, method, url, payload);
            expect(response.statusCode, url).toBe(403);
        }
    });

    it('turn an unsuccessful schema read into an invalid request', async () => {
        const { server, sessionId } = buildTestServer({
            [SCHEMA_SUBJECT]: { ...SCHEMA_REPLY, success: false, message: 'no schema' },
        });
        const response = await send(server, sessionId, 'GET', '/api/oresmd/schema');
        expect(response.statusCode).toBe(422);
    });
});
