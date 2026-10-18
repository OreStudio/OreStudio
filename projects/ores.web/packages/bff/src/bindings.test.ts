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
const PARTY = '77777777-7777-4777-8777-777777777777';
const ID = '55555555-5555-4555-8555-555555555555';
const OTHER_PARTY = '88888888-8888-4888-8888-888888888888';
const SERIES = '99999999-9999-4999-8999-999999999999';
const INTENT = { reason_code: 'common.rectification', commentary: 'Fixed' };

/** A session that works in PARTY, or in no party when `withParty` is false. */
function buildTestServer(
    replies: Readonly<Record<string, unknown>>,
    withParty = true,
): {
    readonly server: ReturnType<typeof buildServer>;
    readonly sessionId: string;
    readonly calls: { subject: string; body: unknown }[];
} {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const calls: { subject: string; body: unknown }[] = [];
    const client = {
        subscribeToMessages(): () => void {
            return () => undefined;
        },
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
    if (withParty) {
        sessions.switchParty(
            session.id,
            { id: PARTY, name: 'System Party', party_category: 'system', business_center_code: '' },
            [],
            1800,
        );
    }
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

const LIST = 'marketdata.v1.feed_bindings.list';
const PUT = 'marketdata.v1.feed_bindings.put_many';
const DELETE_ONE = 'marketdata.v1.feed_bindings.delete';
const ACTIVITY = 'marketdata.v1.ops.list_feed_binding_activity';
const FEEDS = 'synthetic.v1.ops.list_folder_feeds';
const OWNERS = 'synthetic.v1.ops.resolve_series_owners';

const binding = (over: Record<string, unknown> = {}): Record<string, unknown> => ({
    version: 3,
    tenant_id: '44444444-4444-4444-4444-444444444444',
    id: ID,
    party_id: PARTY,
    source_name: 'synthetic.bloomberg.audusd',
    producer_kind: 'SYNTHETIC',
    enabled: true,
    modified_by: 'op',
    performed_by: 'op',
    change_reason_code: 'common.rectification',
    change_commentary: '',
    recorded_at: '2026-10-13 08:15:00.000000Z',
    ...over,
});

describe('the feed bindings of the party', () => {
    it('reads one page of the bindings of the session party, searched and ordered by the server', async () => {
        const { server, sessionId, calls } = buildTestServer({
            [LIST]: { result: ok, feed_bindings: [binding()], total: 1 },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/synthetic/feed-bindings?offset=0&limit=15&sort=source_name&search=aud',
        );
        expect(response.statusCode).toBe(200);
        expect(response.json().total).toBe(1);
        expect(calls[0]?.body).toMatchObject({
            offset: 0,
            limit: 15,
            order: { field: 'source_name', descending: false },
            filter: { party_id: PARTY, search: 'aud' },
        });
    });

    it('adds the party to a read with no page, so no binding of another party is answered', async () => {
        const { server, sessionId, calls } = buildTestServer({
            [LIST]: { result: ok, feed_bindings: [], total: 0 },
        });
        await send(server, sessionId, 'GET', '/api/synthetic/feed-bindings');
        expect(calls[0]?.body).toMatchObject({ filter: { party_id: PARTY } });
    });

    it('refuses a read from a session that has not chosen a party', async () => {
        const { server, sessionId, calls } = buildTestServer({}, false);
        const response = await send(server, sessionId, 'GET', '/api/synthetic/feed-bindings');
        expect(response.statusCode).toBe(400);
        expect(calls).toEqual([]);
    });

    it('lists the source name and the enabled flag as the fields a person may order by', async () => {
        const { server, sessionId } = buildTestServer({});
        const response = await send(server, sessionId, 'GET', '/api/synthetic');
        const found = response
            .json()
            .resources.find((r: { key: string }) => r.key === 'feed-bindings');
        expect(found).toMatchObject({
            key: 'feed-bindings',
            writable: true,
            search: true,
            sortable: ['source_name', 'enabled'],
            readPermission: 'marketdata::feed_bindings:read',
            writePermission: 'marketdata::feed_bindings:write',
            deletePermission: 'marketdata::feed_bindings:delete',
        });
    });

    it('narrows a page by the kind of its binding, named in the address', async () => {
        const { server, sessionId, calls } = buildTestServer({
            [LIST]: { result: ok, feed_bindings: [], total: 0 },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/synthetic/feed-bindings?offset=0&limit=15&f.producer_kind=VENDOR',
        );
        expect(response.statusCode).toBe(200);
        expect(calls[0]?.body).toMatchObject({
            filter: { party_id: PARTY, producer_kind: 'VENDOR', search: null },
        });
    });

    it('refuses a narrowing by a member the resource does not offer', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/synthetic/feed-bindings?offset=0&limit=15&f.party_id=x',
        );
        expect(response.statusCode).toBe(400);
        expect(calls).toEqual([]);
    });

    it('refuses a ordering by a field the model did not make sortable', async () => {
        const { server, sessionId } = buildTestServer({});
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/synthetic/feed-bindings?offset=0&limit=15&sort=producer_kind',
        );
        expect(response.statusCode).toBe(400);
    });
});

describe('writing a binding', () => {
    const add = {
        intent: INTENT,
        changes: [
            {
                write: { id: ID, source_name: 'synthetic.bloomberg.gbpusd', enabled: true },
                version: null,
            },
        ],
    };

    it('completes a new binding with the party of the session and the synthetic kind', async () => {
        const { server, sessionId, calls } = buildTestServer({
            [LIST]: { result: ok, feed_bindings: [], total: 0 },
            [PUT]: {
                result: ok,
                feed_bindings: [binding({ source_name: 'synthetic.bloomberg.gbpusd' })],
            },
        });
        const response = await send(server, sessionId, 'PUT', '/api/synthetic/feed-bindings', add);
        expect(response.statusCode).toBe(200);
        const put = calls.find((c) => c.subject === PUT);
        expect(put?.body).toMatchObject({
            changes: [
                {
                    write: {
                        id: ID,
                        party_id: PARTY,
                        source_name: 'synthetic.bloomberg.gbpusd',
                        producer_kind: 'SYNTHETIC',
                        enabled: true,
                    },
                    precondition: { kind: 'must_not_exist' },
                },
            ],
        });
    });

    it('keeps the party and kind of a binding that exists when it is edited', async () => {
        const { server, sessionId, calls } = buildTestServer({
            [LIST]: { result: ok, feed_bindings: [binding({ party_id: OTHER_PARTY })], total: 1 },
            [PUT]: { result: ok, feed_bindings: [binding()] },
        });
        const response = await send(server, sessionId, 'PUT', '/api/synthetic/feed-bindings', {
            intent: INTENT,
            changes: [
                {
                    write: { id: ID, source_name: 'synthetic.bloomberg.eurusd', enabled: false },
                    version: 3,
                },
            ],
        });
        expect(response.statusCode).toBe(200);
        expect(calls.find((c) => c.subject === PUT)?.body).toMatchObject({
            changes: [
                {
                    write: { party_id: OTHER_PARTY, producer_kind: 'SYNTHETIC' },
                    precondition: { kind: 'must_match_version', version: 3 },
                },
            ],
        });
    });

    it("refuses a write that states a party or a kind, because they are not the browser's", async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(server, sessionId, 'PUT', '/api/synthetic/feed-bindings', {
            intent: INTENT,
            changes: [
                {
                    write: {
                        id: ID,
                        source_name: 'vendor.example.eurusd',
                        enabled: true,
                        producer_kind: 'VENDOR',
                    },
                    version: null,
                },
            ],
        });
        expect(response.statusCode).toBe(400);
        expect(calls).toEqual([]);
    });

    it('refuses a write to a binding that a real feed made, which is read only', async () => {
        const { server, sessionId, calls } = buildTestServer({
            [LIST]: { result: ok, feed_bindings: [binding({ producer_kind: 'VENDOR' })], total: 1 },
        });
        const response = await send(server, sessionId, 'PUT', '/api/synthetic/feed-bindings', {
            intent: INTENT,
            changes: [
                {
                    write: { id: ID, source_name: 'vendor.example.eurusd', enabled: false },
                    version: 1,
                },
            ],
        });
        expect(response.statusCode).toBe(403);
        expect(response.json().message).toContain('Real feeds are not supported');
        expect(calls.some((c) => c.subject === PUT)).toBe(false);
    });

    it('refuses a write with no change reason', async () => {
        const { server, sessionId } = buildTestServer({
            [LIST]: { result: ok, feed_bindings: [], total: 0 },
        });
        const response = await send(server, sessionId, 'PUT', '/api/synthetic/feed-bindings', {
            changes: add.changes,
        });
        expect(response.statusCode).toBe(400);
    });
});

describe('deleting a binding', () => {
    it('names the binding by its source name, which is how the server removes it, even a vendor binding', async () => {
        const { server, sessionId, calls } = buildTestServer({
            [LIST]: {
                result: ok,
                feed_bindings: [
                    binding({ producer_kind: 'VENDOR', source_name: 'vendor.example.eurusd' }),
                ],
                total: 1,
            },
            [DELETE_ONE]: { result: ok },
        });
        const response = await send(server, sessionId, 'DELETE', '/api/synthetic/feed-bindings', {
            intent: INTENT,
            removals: [{ key: ID, version: 3 }],
        });
        expect(response.statusCode).toBe(204);
        expect(calls.find((c) => c.subject === DELETE_ONE)?.body).toMatchObject({
            removal: {
                key: { source_name: 'vendor.example.eurusd' },
                precondition: { kind: 'must_match_version', version: 3 },
            },
        });
    });
});

describe('the history of a binding', () => {
    const HISTORY = 'marketdata.v1.history.get';
    const NAME = 'synthetic.bloomberg.audusd';
    const url = `/api/history?entityType=ores.marketdata.feed_binding&entityId=${NAME}`;

    it('is read for a binding of the session party, which is looked up by its exact name first', async () => {
        const { server, sessionId, calls } = buildTestServer({
            [LIST]: { result: ok, feed_bindings: [binding()], total: 1 },
            [HISTORY]: {
                success: true,
                versions: [
                    { version: 1, modified_by: 'op', recorded_at: '2026-10-13 08:15:00.000000Z' },
                ],
            },
        });
        const response = await send(server, sessionId, 'GET', url);
        expect(response.statusCode).toBe(200);
        expect(calls.map((c) => c.subject)).toEqual([LIST, HISTORY]);
        expect(calls[0]?.body).toMatchObject({ filter: { party_id: PARTY, search: NAME } });
    });

    it('is refused as not found for a name the party does not hold, even when another name contains it, and reads no history', async () => {
        const { server, sessionId, calls } = buildTestServer({
            [LIST]: { result: ok, feed_bindings: [binding({ source_name: `${NAME}2` })], total: 1 },
        });
        const response = await send(server, sessionId, 'GET', url);
        expect(response.statusCode).toBe(404);
        expect(calls.map((c) => c.subject)).toEqual([LIST]);
    });
});

describe('a write that names a binding of another party', () => {
    it('goes out as a new binding of the session party, which the server refuses as a key that exists', async () => {
        const { server, sessionId, calls } = buildTestServer({
            [LIST]: { result: ok, feed_bindings: [], total: 0 },
            [PUT]: {
                result: { outcome: 'conflict', code: 'exists', message: 'The key exists.' },
                feed_bindings: [],
            },
        });
        const response = await send(server, sessionId, 'PUT', '/api/synthetic/feed-bindings', {
            intent: INTENT,
            changes: [
                {
                    write: { id: ID, source_name: 'synthetic.bloomberg.audusd', enabled: true },
                    version: null,
                },
            ],
        });
        expect(response.statusCode).toBe(409);
        expect(calls.find((c) => c.subject === PUT)?.body).toMatchObject({
            changes: [{ write: { party_id: PARTY }, precondition: { kind: 'must_not_exist' } }],
        });
    });
});

describe('what each binding has written', () => {
    it('answers the series and last tick of each binding, and null for a silent source', async () => {
        const { server, sessionId } = buildTestServer({
            [ACTIVITY]: {
                success: true,
                message: '',
                activities: [
                    {
                        source_name: 'synthetic.bloomberg.audusd',
                        series_id: SERIES,
                        last_tick_at: '2026-10-12 23:50:26Z',
                    },
                    {
                        source_name: 'synthetic.bloomberg.nzdusd',
                        series_id: null,
                        last_tick_at: null,
                    },
                ],
            },
        });
        const response = await send(server, sessionId, 'GET', '/api/bindings/activity');
        expect(response.statusCode).toBe(200);
        expect(response.json().activities).toEqual([
            {
                source_name: 'synthetic.bloomberg.audusd',
                series_id: SERIES,
                last_tick_at: '2026-10-12 23:50:26Z',
            },
            { source_name: 'synthetic.bloomberg.nzdusd', series_id: null, last_tick_at: null },
        ]);
    });

    it('says so when the server could not read them', async () => {
        const { server, sessionId } = buildTestServer({
            [ACTIVITY]: { success: false, message: 'The request names no party.', activities: [] },
        });
        const response = await send(server, sessionId, 'GET', '/api/bindings/activity');
        expect(response.statusCode).toBe(502);
        expect(response.json().message).toBe('The request names no party.');
    });
});

describe('the sources a party may bind', () => {
    it('leaves out a bound source and names the feed that holds a series', async () => {
        const { server, sessionId, calls } = buildTestServer({
            [FEEDS]: {
                success: true,
                message: '',
                feeds: [
                    { source_name: 'a.bound', oresmd_uri: 'oresmd://fx/A', label: 'A' },
                    { source_name: 'a.free', oresmd_uri: 'oresmd://fx/B', label: 'B' },
                    { source_name: 'a.conflict', oresmd_uri: 'oresmd://fx/C', label: 'C' },
                ],
            },
            [LIST]: { result: ok, feed_bindings: [{ source_name: 'a.bound' }] },
            [OWNERS]: {
                success: true,
                message: '',
                owners: [
                    { series: 'oresmd://fx/B', source_name: '' },
                    { series: 'oresmd://fx/C', source_name: 'other.running' },
                ],
            },
        });

        const response = await send(server, sessionId, 'GET', '/api/bindings/bindable-sources');

        expect(response.statusCode).toBe(200);
        expect(response.json().sources).toEqual([
            expect.objectContaining({
                source_name: 'a.free',
                oresmd_uri: 'oresmd://fx/B',
                held_by: null,
            }),
            expect.objectContaining({
                source_name: 'a.conflict',
                oresmd_uri: 'oresmd://fx/C',
                held_by: 'other.running',
            }),
        ]);
        expect(calls.find((call) => call.subject === LIST)?.body).toMatchObject({
            filter: { party_id: PARTY },
        });
        expect(calls.find((call) => call.subject === OWNERS)?.body).toEqual({
            series: ['oresmd://fx/B', 'oresmd://fx/C'],
        });
    });

    it('asks nobody which feed holds a series when every source is bound', async () => {
        const { server, sessionId, calls } = buildTestServer({
            [FEEDS]: {
                success: true,
                message: '',
                feeds: [{ source_name: 'a.bound', oresmd_uri: 'oresmd://fx/A' }],
            },
            [LIST]: { result: ok, feed_bindings: [{ source_name: 'a.bound' }] },
        });

        const response = await send(server, sessionId, 'GET', '/api/bindings/bindable-sources');

        expect(response.statusCode).toBe(200);
        expect(response.json().sources).toEqual([]);
        expect(calls.some((call) => call.subject === OWNERS)).toBe(false);
    });

    it('says so when the feeds could not be read', async () => {
        const { server, sessionId } = buildTestServer({
            [FEEDS]: { success: false, message: 'The feeds could not be read.', feeds: [] },
        });

        const response = await send(server, sessionId, 'GET', '/api/bindings/bindable-sources');

        expect(response.statusCode).toBe(502);
        expect(response.json().message).toBe('The feeds could not be read.');
    });
});
