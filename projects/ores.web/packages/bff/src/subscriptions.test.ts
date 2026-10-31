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
import { ServiceUnavailableError, type OresClient } from '@ores/wire-protocol';
import type { Config } from './config.js';
import { buildServer, sessionCookieName } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

/**
 * The two reads the subscriptions screen makes, and the writes it offers.
 *
 * The service owns the party and the permission, so the routes here are checked
 * for the two things they add: the request they send, and what a refusal from
 * the service becomes.
 */

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
const ACCOUNT_ID = '11111111-1111-1111-1111-111111111111';
const TENANT_ID = '44444444-4444-4444-4444-444444444444';
const SESSION_ID = '33333333-3333-3333-3333-333333333333';
const PARTY_ID = '3f1e2d4c-0000-4000-8000-000000000010';

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

const party = {
    id: PARTY_ID,
    name: 'Acme Operations',
    partyCategory: 'Operational',
    businessCenterCode: 'GBLO',
};

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
        session: {
            kind: 'active',
            token: 'token',
            accountId: ACCOUNT_ID,
            tenantId: TENANT_ID,
            tenantName: 'Acme',
            version: 'v0.0.25 (test)',
            username: 'tenant_admin',
            email: 'tenant_admin@acme.example',
            party,
            availableParties: [party],
            accessLifetimeSeconds: 1800,
            passwordResetRequired: false,
            sessionId: SESSION_ID,
        },
        username: 'tenant_admin',
        email: 'tenant_admin@acme.example',
        accountId: ACCOUNT_ID,
        tenantId: TENANT_ID,
        tenantName: 'Acme',
        version: 'v0.0.25 (test)',
        availableParties: [party],
        accessLifetimeSeconds: 1800,
        passwordResetRequired: false,
        sessionId: SESSION_ID,
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

type TestServer = ReturnType<typeof buildTestServer>['server'];

function send(
    server: TestServer,
    sessionId: string | undefined,
    method: 'GET' | 'POST' | 'DELETE',
    url: string,
    body?: unknown,
): ReturnType<TestServer['inject']> {
    return server.inject({
        method,
        url,
        ...(body === undefined ? {} : { payload: body as Record<string, unknown> }),
        ...(sessionId === undefined ? {} : { cookies: { [SESSION_COOKIE]: sessionId } }),
    });
}

/** One subscription as the service states it. */
const subscription = {
    version: 2,
    tenant_id: TENANT_ID,
    id: '55555555-5555-4555-8555-555555555555',
    party_id: PARTY_ID,
    provider_code: 'bloomberg',
    display: true,
    non_display: true,
    derived: false,
    modified_by: 'tenant_admin',
    performed_by: 'tenant_admin',
    change_reason_code: 'system.new_record',
    change_commentary: '',
    recorded_at: '2026-10-26 09:00:00Z',
};

/** One route as the service states it. */
const route = {
    version: 1,
    tenant_id: TENANT_ID,
    id: '66666666-6666-4666-8666-666666666666',
    party_id: PARTY_ID,
    provider_code: 'bloomberg',
    source_id: '0a42775e-3da7-4c61-a732-6b62cb9f948f',
    modified_by: 'tenant_admin',
    performed_by: 'tenant_admin',
    change_reason_code: 'system.new_record',
    change_commentary: '',
    recorded_at: '2026-10-26 09:00:00Z',
};

const LIST_SUBSCRIPTIONS = 'marketdata.v1.subscriptions.list';
const LIST_ROUTES = 'marketdata.v1.routes.list';
const PUT_SUBSCRIPTION = 'marketdata.v1.subscriptions.put';
const DELETE_SUBSCRIPTION = 'marketdata.v1.subscriptions.delete';
const PUT_ROUTE = 'marketdata.v1.routes.put';
const LIST_PROVIDERS = 'marketdata.v1.providers.list';
const LIST_FEEDS = 'marketdata.v1.feeds.list';

const ok = { result: { outcome: 'ok', code: '', message: '', fields: [] } };

describe('the subscriptions read', () => {
    it('proxies the party subscriptions the service answers', async () => {
        const { server, sessionId, calls } = buildTestServer({
            [LIST_SUBSCRIPTIONS]: { ...ok, subscriptions: [subscription], total: 1 },
        });
        const response = await send(server, sessionId, 'GET', '/api/marketdata/subscriptions');
        expect(response.statusCode).toBe(200);
        expect(response.json().subscriptions).toEqual([subscription]);
        expect(calls[0]?.subject).toBe(LIST_SUBSCRIPTIONS);
    });

    it('reports a refused read as the refusal, not as an empty list', async () => {
        const { server, sessionId } = buildTestServer({
            [LIST_SUBSCRIPTIONS]: {
                result: {
                    outcome: 'denied',
                    code: 'forbidden',
                    message: 'The caller may not read market data subscriptions.',
                    fields: [],
                },
                subscriptions: [],
                total: 0,
            },
        });
        const response = await send(server, sessionId, 'GET', '/api/marketdata/subscriptions');
        expect(response.statusCode).toBe(403);
        expect(response.json().message).toBe('The caller may not read market data subscriptions.');
    });

    it('reports a market data service that is not running as unavailable', async () => {
        const { server, sessionId } = buildTestServer({
            [LIST_SUBSCRIPTIONS]: new ServiceUnavailableError(LIST_SUBSCRIPTIONS),
        });
        const response = await send(server, sessionId, 'GET', '/api/marketdata/subscriptions');
        expect(response.statusCode).toBe(503);
    });

    it('needs a session', async () => {
        const { server, calls } = buildTestServer({});
        const response = await send(server, undefined, 'GET', '/api/marketdata/subscriptions');
        expect(response.statusCode).toBe(401);
        expect(calls).toHaveLength(0);
    });
});

describe('the routes read', () => {
    it('proxies the routes the service answers', async () => {
        const { server, sessionId } = buildTestServer({
            [LIST_ROUTES]: { ...ok, routes: [route], total: 1 },
        });
        const response = await send(server, sessionId, 'GET', '/api/marketdata/routes');
        expect(response.statusCode).toBe(200);
        expect(response.json().routes).toEqual([route]);
    });

    it('reports a refused read as the refusal', async () => {
        const { server, sessionId } = buildTestServer({
            [LIST_ROUTES]: {
                result: {
                    outcome: 'denied',
                    code: 'forbidden',
                    message: 'The caller may not read market data routes.',
                    fields: [],
                },
                routes: [],
                total: 0,
            },
        });
        const response = await send(server, sessionId, 'GET', '/api/marketdata/routes');
        expect(response.statusCode).toBe(403);
        expect(response.json().message).toBe('The caller may not read market data routes.');
    });
});

describe('the catalogue read', () => {
    it('names the providers and the sources a route chooses from', async () => {
        const provider = {
            code: 'bloomberg',
            name: 'Bloomberg',
            kind: 'data_vendor',
            description: '',
            display_order: 1,
        };
        const feed = {
            id: '0a42775e-3da7-4c61-a732-6b62cb9f948f',
            name: 'bloomberg-a',
            publisher_code: 'bloomberg',
            service: 'simulated',
            is_default: true,
            description: '',
        };
        const { server, sessionId } = buildTestServer({
            [LIST_PROVIDERS]: { ...ok, providers: [provider], total: 1 },
            [LIST_FEEDS]: { ...ok, running_feeds: [feed], total: 1 },
        });
        const response = await send(server, sessionId, 'GET', '/api/marketdata/catalogue');
        expect(response.statusCode).toBe(200);
        expect(response.json().providers[0].code).toBe('bloomberg');
        expect(response.json().sources[0].name).toBe('bloomberg-a');
    });
});

describe('adding a subscription', () => {
    it('writes it for the party the session acts in, and states no party of its own', async () => {
        const { server, sessionId, calls } = buildTestServer({
            [PUT_SUBSCRIPTION]: { ...ok, subscription },
        });
        const response = await send(server, sessionId, 'POST', '/api/marketdata/subscriptions', {
            provider_code: 'bloomberg',
            display: true,
            non_display: true,
            derived: false,
        });
        expect(response.statusCode).toBe(200);
        const body = calls[0]?.body as {
            change: { write: Record<string, unknown>; precondition: { kind: string } };
            intent: { reason_code: string };
        };
        expect(body.change.write.party_id).toBe(PARTY_ID);
        expect(body.change.write.provider_code).toBe('bloomberg');
        expect(body.change.precondition.kind).toBe('must_not_exist');
        expect(body.intent.reason_code).toBe('system.new_record');
    });

    it('refuses a subscription with no right without calling the service', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(server, sessionId, 'POST', '/api/marketdata/subscriptions', {
            provider_code: 'bloomberg',
        });
        expect(response.statusCode).toBe(400);
        expect(response.json().message).toBe('A subscription needs at least one right.');
        expect(calls).toHaveLength(0);
    });

    it('shows the service refusal in the service words', async () => {
        const { server, sessionId } = buildTestServer({
            [PUT_SUBSCRIPTION]: {
                result: {
                    outcome: 'conflict',
                    code: 'already_exists',
                    message: 'The party already holds a subscription to bloomberg.',
                    fields: [],
                },
                subscription: null,
            },
        });
        const response = await send(server, sessionId, 'POST', '/api/marketdata/subscriptions', {
            provider_code: 'bloomberg',
            display: true,
        });
        expect(response.statusCode).toBe(400);
        expect(response.json().message).toBe(
            'The party already holds a subscription to bloomberg.',
        );
    });
});

describe('removing a subscription', () => {
    it('claims the version the screen read', async () => {
        const { server, sessionId, calls } = buildTestServer({ [DELETE_SUBSCRIPTION]: ok });
        const response = await send(
            server,
            sessionId,
            'DELETE',
            '/api/marketdata/subscriptions/bloomberg',
            { version: 2, reason_code: 'common.superseded', commentary: 'Moved the desk.' },
        );
        expect(response.statusCode).toBe(200);
        const body = calls[0]?.body as {
            removal: { key: { provider_code: string }; precondition: { kind: string } };
        };
        expect(body.removal.key.provider_code).toBe('bloomberg');
        expect(body.removal.precondition).toEqual({ kind: 'must_match_version', version: 2 });
    });

    it('reports a stale version as the refusal it is', async () => {
        const { server, sessionId } = buildTestServer({
            [DELETE_SUBSCRIPTION]: {
                result: {
                    outcome: 'conflict',
                    code: 'version_conflict',
                    message: 'The subscription changed since it was read.',
                    fields: [],
                },
            },
        });
        const response = await send(
            server,
            sessionId,
            'DELETE',
            '/api/marketdata/subscriptions/bloomberg',
            { version: 1, reason_code: 'common.superseded' },
        );
        expect(response.statusCode).toBe(400);
        expect(response.json().message).toBe('The subscription changed since it was read.');
    });
});

describe('adding a route', () => {
    it('writes the source the person chose for the session party', async () => {
        const { server, sessionId, calls } = buildTestServer({ [PUT_ROUTE]: { ...ok, route } });
        const response = await send(server, sessionId, 'POST', '/api/marketdata/routes', {
            provider_code: 'bloomberg',
            source_id: '0a42775e-3da7-4c61-a732-6b62cb9f948f',
        });
        expect(response.statusCode).toBe(200);
        const body = calls[0]?.body as { change: { write: Record<string, unknown> } };
        expect(body.change.write.party_id).toBe(PARTY_ID);
        expect(body.change.write.source_id).toBe('0a42775e-3da7-4c61-a732-6b62cb9f948f');
    });

    it('refuses a source that is not an id', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(server, sessionId, 'POST', '/api/marketdata/routes', {
            provider_code: 'bloomberg',
            source_id: 'bloomberg-a',
        });
        expect(response.statusCode).toBe(400);
        expect(calls).toHaveLength(0);
    });
});
