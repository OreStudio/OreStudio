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

const IDENTITY = {
    asset: 'fx',
    scope: 'option',
    instrument_type: 'fx_option',
    quote_type: 'quote',
    party_id: '',
    fields: [
        { name: 'ccy1', text: 'NZD' },
        { name: 'ccy2', text: 'PLN' },
    ],
};

const SNAPSHOT = {
    success: true,
    message: '',
    as_of: '2026-10-10 12:00:00Z',
    coordinate_fields: ['expiry', 'strike_label'],
    nodes: [{ coordinates: ['1Y', 'ATM'], status: '' }],
    values: ['0.080'],
    recorded_at: ['2026-10-10 09:00:00Z'],
    provenance: [],
    oldest_age_seconds: 10,
    spread_seconds: 0,
    warning: false,
};

const EVOLUTION = {
    success: true,
    message: '',
    coordinate_fields: ['expiry', 'strike_label'],
    nodes: [{ coordinates: ['1Y', 'ATM'], status: 'persistent' }],
    instants: [{ as_of: '2026-10-09 00:00:00Z', values: ['0.080'] }],
};

describe('the curve read routes', () => {
    it('resolves an identity to the series it names', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'marketdata.v1.ops.resolve_series_identity': {
                success: true,
                message: '',
                series: [{ id: 'a' }],
            },
        });
        const response = await send(server, sessionId, 'POST', '/api/curves/series', {
            identity: IDENTITY,
        });
        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({ series: [{ id: 'a' }] });
        expect(calls[0]?.body).toEqual(IDENTITY);
    });

    it('lists the series the session can see, as identities the other reads take', async () => {
        const listed = {
            success: true,
            message: '',
            truncated: false,
            series: [
                {
                    series_id: '77777777-7777-4777-8777-777777777777',
                    identity: { ...IDENTITY, party_id: '88888888-8888-4888-8888-888888888888' },
                    axes: ['expiry', 'strike_label'],
                },
            ],
        };
        const { server, sessionId, calls } = buildTestServer({
            'marketdata.v1.ops.list_series_identities': listed,
        });
        const response = await send(server, sessionId, 'POST', '/api/curves/series-list', {});
        expect(response.statusCode).toBe(200);
        expect(response.json().series[0].axes).toEqual(['expiry', 'strike_label']);
        expect(response.json().truncated).toBe(false);
        expect(response.json()).not.toHaveProperty('success');
        expect(calls[0]?.body).toEqual({ party_id: '' });
    });

    it('lists with no body at all, and names a party when asked', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'marketdata.v1.ops.list_series_identities': {
                success: true,
                message: '',
                truncated: true,
                series: [],
            },
        });
        const bare = await send(server, sessionId, 'POST', '/api/curves/series-list');
        expect(bare.statusCode).toBe(200);
        expect(bare.json().truncated).toBe(true);
        const party = '88888888-8888-4888-8888-888888888888';
        await send(server, sessionId, 'POST', '/api/curves/series-list', { party_id: party });
        expect(calls[0]?.body).toEqual({ party_id: '' });
        expect(calls[1]?.body).toEqual({ party_id: party });
    });

    it('accepts an empty party named outright, as the same as no party', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'marketdata.v1.ops.list_series_identities': {
                success: true,
                message: '',
                truncated: false,
                series: [],
            },
        });
        const response = await send(server, sessionId, 'POST', '/api/curves/series-list', {
            party_id: '',
        });
        expect(response.statusCode).toBe(200);
        expect(calls[0]?.body).toEqual({ party_id: '' });
    });

    it('refuses a party that is no UUID, without asking the server', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(server, sessionId, 'POST', '/api/curves/series-list', {
            party_id: 'acme',
        });
        expect(response.statusCode).toBe(400);
        expect(calls).toHaveLength(0);
    });

    it('reports a missing permission on the list as forbidden', async () => {
        const { server, sessionId } = buildTestServer({
            'marketdata.v1.ops.list_series_identities': new ServerError(
                'forbidden',
                'marketdata.v1.ops.list_series_identities',
            ),
        });
        const response = await send(server, sessionId, 'POST', '/api/curves/series-list', {});
        expect(response.statusCode).toBe(403);
    });

    it('reads a snapshot without provenance unless asked', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'marketdata.v1.curve_snapshot.get': SNAPSHOT,
        });
        const response = await send(server, sessionId, 'POST', '/api/curves/snapshot', {
            identity: IDENTITY,
        });
        expect(response.statusCode).toBe(200);
        expect(response.json().values).toEqual(['0.080']);
        expect(response.json()).not.toHaveProperty('success');
        expect(calls[0]?.body).toEqual({ identity: IDENTITY, include_provenance: false });
    });

    it('forwards a request for provenance', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'marketdata.v1.curve_snapshot.get': SNAPSHOT,
        });
        await send(server, sessionId, 'POST', '/api/curves/snapshot', {
            identity: IDENTITY,
            include_provenance: true,
        });
        expect(calls[0]?.body).toEqual({ identity: IDENTITY, include_provenance: true });
    });

    it('reads one slice over a range', async () => {
        const slice = {
            success: true,
            message: '',
            coordinate_field: 'expiry',
            coordinates: ['1Y'],
            instants: [{ as_of: '2026-10-09 00:00:00Z', values: ['0.080'] }],
        };
        const { server, sessionId, calls } = buildTestServer({
            'marketdata.v1.ops.get_series_slice': slice,
        });
        const request = {
            identity: IDENTITY,
            component_field: 'strike_label',
            component_value: 'ATM',
            from_instant: '2026-10-01 00:00:00Z',
            to_instant: '2026-10-09 00:00:00Z',
        };
        const response = await send(server, sessionId, 'POST', '/api/curves/slice', request);
        expect(response.json().coordinates).toEqual(['1Y']);
        expect(calls[0]?.body).toEqual(request);
    });

    it('reads the evolution over a range and states no instants', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'marketdata.v1.ops.get_series_evolution': EVOLUTION,
        });
        const response = await send(server, sessionId, 'POST', '/api/curves/evolution', {
            identity: IDENTITY,
            from_instant: '2026-10-01 00:00:00Z',
            to_instant: '2026-10-09 00:00:00Z',
        });
        expect(response.json().nodes[0].status).toBe('persistent');
        expect(calls[0]?.body).toEqual({
            identity: IDENTITY,
            from_instant: '2026-10-01 00:00:00Z',
            to_instant: '2026-10-09 00:00:00Z',
            instants: [],
        });
    });

    it('reads the evolution at stated instants and fills the range the server ignores', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'marketdata.v1.ops.get_series_evolution': EVOLUTION,
        });
        await send(server, sessionId, 'POST', '/api/curves/evolution', {
            identity: IDENTITY,
            instants: ['2026-09-30 00:00:00Z'],
        });
        expect(calls[0]?.body).toEqual({
            identity: IDENTITY,
            from_instant: '1970-01-01 00:00:00Z',
            to_instant: '1970-01-01 00:00:00Z',
            instants: ['2026-09-30 00:00:00Z'],
        });
    });

    it('refuses an evolution that states neither instants nor a range, without asking the server', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(server, sessionId, 'POST', '/api/curves/evolution', {
            identity: IDENTITY,
        });
        expect(response.statusCode).toBe(400);
        expect(calls).toHaveLength(0);
    });

    it('refuses an instant that is not a wire timestamp', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(server, sessionId, 'POST', '/api/curves/evolution', {
            identity: IDENTITY,
            instants: ['yesterday'],
        });
        expect(response.statusCode).toBe(400);
        expect(calls).toHaveLength(0);
    });

    it('refuses a request that names a field the route does not know', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(server, sessionId, 'POST', '/api/curves/snapshot', {
            identity: IDENTITY,
            as_of: '2026-10-01 00:00:00Z',
        });
        expect(response.statusCode).toBe(400);
        expect(calls).toHaveLength(0);
    });

    it('passes the server refusal on as a reason', async () => {
        const { server, sessionId } = buildTestServer({
            'marketdata.v1.ops.get_series_evolution': {
                success: false,
                message: 'the object declares no axis, so it is not a composite object',
            },
        });
        const response = await send(server, sessionId, 'POST', '/api/curves/evolution', {
            identity: IDENTITY,
            instants: ['2026-09-30 00:00:00Z'],
        });
        expect(response.statusCode).toBe(422);
        expect(response.json().message).toBe(
            'the object declares no axis, so it is not a composite object',
        );
    });

    it('drops the range when instants are stated', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'marketdata.v1.ops.get_series_evolution': EVOLUTION,
        });
        await send(server, sessionId, 'POST', '/api/curves/evolution', {
            identity: IDENTITY,
            from_instant: '2026-10-01 00:00:00Z',
            to_instant: '2026-10-09 00:00:00Z',
            instants: ['2026-09-30 00:00:00Z'],
        });
        expect(calls[0]?.body).toEqual({
            identity: IDENTITY,
            from_instant: '1970-01-01 00:00:00Z',
            to_instant: '1970-01-01 00:00:00Z',
            instants: ['2026-09-30 00:00:00Z'],
        });
    });

    it('refuses a range that ends before it starts, without asking the server', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const evolution = await send(server, sessionId, 'POST', '/api/curves/evolution', {
            identity: IDENTITY,
            from_instant: '2026-10-09 00:00:00Z',
            to_instant: '2026-10-01 00:00:00Z',
        });
        const slice = await send(server, sessionId, 'POST', '/api/curves/slice', {
            identity: IDENTITY,
            component_field: 'strike_label',
            component_value: 'ATM',
            from_instant: '2026-10-09 00:00:00Z',
            to_instant: '2026-10-01 00:00:00Z',
        });
        expect(evolution.statusCode).toBe(400);
        expect(slice.statusCode).toBe(400);
        expect(calls).toHaveLength(0);
    });

    it('reports a missing permission as forbidden and not as a refused read', async () => {
        const { server, sessionId } = buildTestServer({
            'marketdata.v1.curve_snapshot.get': new ServerError(
                'forbidden',
                'marketdata.v1.curve_snapshot.get',
            ),
        });
        const response = await send(server, sessionId, 'POST', '/api/curves/snapshot', {
            identity: IDENTITY,
        });
        expect(response.statusCode).toBe(403);
        expect(response.json().code).toBe('forbidden');
    });

    it('needs a session', async () => {
        const { server, calls } = buildTestServer({});
        const response = await server.inject({
            method: 'POST',
            url: '/api/curves/snapshot',
            payload: { identity: IDENTITY },
        });
        expect(response.statusCode).toBeGreaterThanOrEqual(401);
        expect(response.statusCode).toBeLessThan(500);
        expect(calls).toHaveLength(0);
    });
});
