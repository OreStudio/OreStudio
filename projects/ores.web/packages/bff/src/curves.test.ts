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
    provider_code: '',
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

    it('forwards the provider of an identity, so a party that holds two providers names one', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'marketdata.v1.ops.resolve_series_identity': {
                success: true,
                message: '',
                series: [{ id: 'a' }],
            },
        });
        const identity = { ...IDENTITY, provider_code: 'bloomberg' };
        const response = await send(server, sessionId, 'POST', '/api/curves/series', { identity });
        expect(response.statusCode).toBe(200);
        expect(calls[0]?.body).toMatchObject({ provider_code: 'bloomberg' });
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

    const KEY = {
        identity: IDENTITY,
        coordinates: ['2Y', 'ATM'],
        value: '0.090',
        change_reason_code: 'system.test',
        change_commentary: 'operator correction',
    };

    it('keys a manual point and forwards the whole request', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'marketdata.v1.ops.key_manual_point': { success: true, message: '' },
        });
        const response = await send(server, sessionId, 'POST', '/api/curves/manual-point', KEY);
        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({});
        expect(calls[0]?.body).toEqual(KEY);
    });

    it('answers a refused key with the server words as 422', async () => {
        const { server, sessionId } = buildTestServer({
            'marketdata.v1.ops.key_manual_point': {
                success: false,
                message: "'9Y' is not a declared expiry of the series.",
            },
        });
        const response = await send(server, sessionId, 'POST', '/api/curves/manual-point', KEY);
        expect(response.statusCode).toBe(422);
        expect(response.json().message).toContain('9Y');
    });

    it('reports a missing write permission on a key as forbidden', async () => {
        const { server, sessionId } = buildTestServer({
            'marketdata.v1.ops.key_manual_point': new ServerError(
                'forbidden',
                'marketdata.v1.ops.key_manual_point',
            ),
        });
        const response = await send(server, sessionId, 'POST', '/api/curves/manual-point', KEY);
        expect(response.statusCode).toBe(403);
    });

    it('refuses a key with no commentary, no coordinates or an unknown field, without asking the server', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        for (const bad of [
            { ...KEY, change_commentary: '  ' },
            { ...KEY, coordinates: [] },
            { ...KEY, party: 'x' },
        ]) {
            const response = await send(server, sessionId, 'POST', '/api/curves/manual-point', bad);
            expect(response.statusCode).toBe(400);
        }
        expect(calls).toHaveLength(0);
    });

    it('clears a manual point', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'marketdata.v1.ops.clear_manual_point': { success: true, message: '' },
        });
        const body = { identity: IDENTITY, coordinates: ['2Y', 'ATM'] };
        const response = await send(
            server,
            sessionId,
            'POST',
            '/api/curves/manual-point/clear',
            body,
        );
        expect(response.statusCode).toBe(200);
        expect(calls[0]?.body).toEqual(body);
    });

    it('answers a clear with nothing to clear as 422', async () => {
        const { server, sessionId } = buildTestServer({
            'marketdata.v1.ops.clear_manual_point': {
                success: false,
                message: 'The node holds no manual point.',
            },
        });
        const response = await send(server, sessionId, 'POST', '/api/curves/manual-point/clear', {
            identity: IDENTITY,
            coordinates: ['2Y', 'ATM'],
        });
        expect(response.statusCode).toBe(422);
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

/**
 * The published curve reads and the withdrawal.
 *
 * The routes add no rule of their own: the server decides who may read a set and
 * who may end one, so what is checked here is that the shape is forwarded as the
 * protocol names it and that a refusal from the server travels as its words.
 */
const PUBLISHED_SET = {
    publish_id: '3f1e2d4c-0000-4000-8000-000000000001',
    request_id: '5b42775e-3da7-4c61-a732-6b62cb9f948f',
    curve: 'USD-SOFR-ACME',
    series_id: '0a42775e-3da7-4c61-a732-6b62cb9f948f',
    series_uri: 'oresmd://ir/USD?type=series&instrument=discount&quote=rate&curve_id=USD-SOFR-ACME',
    bootstrap_config_id: 'd0e1f2a3-b4c5-4d6e-7f80-91a2b3c4d5e6',
    party: 'ACCOUS',
    party_id: '44444444-4444-4444-8444-444444444444',
    as_of: '2026-10-27 12:00:00Z',
    published_by: 'henry.jones',
    published_at: '2026-10-27 16:07:08Z',
    state_code: 'published',
    point_count: 2,
    withdrawn_by: '',
    withdrawn_at: null,
    withdrawn_reason_code: '',
    withdrawal_request_id: '',
    superseded_at: null,
    superseded_by_request_id: '',
    superseded_as_of: null,
    points: [],
};

describe('the published curve routes', () => {
    it('reads the tenant’s published sets with the filters the screen names', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'marketdata.v1.ops.list_published_curves': {
                success: true,
                message: '',
                sets: [PUBLISHED_SET],
                total: 1,
            },
        });
        const response = await send(server, sessionId, 'POST', '/api/curves/published', {
            party_id: '',
            search: 'USD-SOFR',
            include_candidates: true,
            include_ended: false,
            limit: 25,
        });
        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({ sets: [PUBLISHED_SET], total: 1 });
        expect(calls[0]?.body).toEqual({
            party_id: '',
            search: 'USD-SOFR',
            include_candidates: true,
            include_ended: false,
            request_id_one_of: [],
            offset: 0,
            limit: 25,
        });
    });

    it('reads one curve’s last sets with the points each holds', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'marketdata.v1.ops.get_published_curve_sets': {
                success: true,
                message: '',
                sets: [PUBLISHED_SET],
                published_count: 3,
            },
        });
        const response = await send(server, sessionId, 'POST', '/api/curves/published-sets', {
            series_id: PUBLISHED_SET.series_id,
            limit: 5,
        });
        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({ sets: [PUBLISHED_SET], published_count: 3 });
        expect(calls[0]?.body).toEqual({
            series_id: PUBLISHED_SET.series_id,
            limit: 5,
            include_candidates: true,
        });
    });

    it('ends a set with the reason the person chose', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'marketdata.v1.ops.withdraw_curve': {
                success: true,
                message: '',
                withdrawn_at: '2026-10-27 17:05:00Z',
                withdrawn_by: 'm.oduya',
            },
        });
        const response = await send(server, sessionId, 'POST', '/api/curves/published/withdraw', {
            publish_id: PUBLISHED_SET.publish_id,
            reason_code: 'common.stale_data',
            comment: 'The 2Y pillar was stale.',
        });
        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            withdrawn_at: '2026-10-27 17:05:00Z',
            withdrawn_by: 'm.oduya',
        });
        expect(calls[0]?.body).toEqual({
            publish_id: PUBLISHED_SET.publish_id,
            reason_code: 'common.stale_data',
            comment: 'The 2Y pillar was stale.',
            approval_request_id: '',
        });
    });

    it('travels the server’s refusal of a withdrawal as its words', async () => {
        const { server, sessionId } = buildTestServer({
            'marketdata.v1.ops.withdraw_curve': {
                success: false,
                message: 'That set was withdrawn at 2026-10-27T17:05:00Z by m.oduya.',
                withdrawn_at: '2026-10-27 17:05:00Z',
                withdrawn_by: 'm.oduya',
            },
        });
        const response = await send(server, sessionId, 'POST', '/api/curves/published/withdraw', {
            publish_id: PUBLISHED_SET.publish_id,
            reason_code: 'common.stale_data',
        });
        expect(response.statusCode).toBe(422);
        expect(response.json()).toMatchObject({
            message: 'That set was withdrawn at 2026-10-27T17:05:00Z by m.oduya.',
        });
    });

    it('needs a reason to end a set', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(server, sessionId, 'POST', '/api/curves/published/withdraw', {
            publish_id: PUBLISHED_SET.publish_id,
            reason_code: '',
        });
        // The body the route will not accept is a bad request; the server's own
        // refusal of a withdrawal that no reason supports comes back as its words.
        expect(response.statusCode).toBe(400);
        expect(calls).toHaveLength(0);
    });
});

describe('the curve blueprint read routes', () => {
    const CONFIG = '99999999-9999-4999-8999-999999999998';
    const SERIES = '77777777-7777-4777-8777-777777777777';
    const INSTANT = '2026-10-07 17:00:00Z';

    it('names the series a set of ids holds, so a blueprint can show what it builds into', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'marketdata.v1.market_series.list': {
                result: ok,
                total: 1,
                market_series: [
                    { version: 1, id: SERIES, oresmd_uri: 'DISCOUNT/RATE/USD/USD-SOFR-1D' },
                ],
            },
        });
        const response = await send(server, sessionId, 'POST', '/api/curves/series-by-id', {
            ids: [SERIES],
        });
        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            series: [{ id: SERIES, uri: 'DISCOUNT/RATE/USD/USD-SOFR-1D' }],
        });
        expect(calls[0]?.body).toMatchObject({ filter: { id_one_of: [SERIES] } });
    });

    it('builds a recipe at an instant and answers the points, without publishing', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'marketdata.v1.ops.compute_curve': {
                success: true,
                message: '',
                points: [{ point_id: '1Y', date: '2027-10-07', discount_factor: 0.97 }],
            },
        });
        const response = await send(server, sessionId, 'POST', '/api/curves/compute', {
            bootstrap_config_id: CONFIG,
            as_of: INSTANT,
        });
        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            points: [{ point_id: '1Y', date: '2027-10-07', discount_factor: 0.97 }],
        });
        expect(calls[0]?.body).toEqual({ bootstrap_config_id: CONFIG, as_of: INSTANT });
    });

    it("answers a refused build with the server's own words", async () => {
        const { server, sessionId } = buildTestServer({
            'marketdata.v1.ops.compute_curve': {
                success: false,
                message: 'output market_series not found for bootstrap config',
                points: [],
            },
        });
        const response = await send(server, sessionId, 'POST', '/api/curves/compute', {
            bootstrap_config_id: CONFIG,
            as_of: INSTANT,
        });
        expect(response.statusCode).toBe(422);
        expect(response.json()).toMatchObject({
            message: 'output market_series not found for bootstrap config',
        });
    });

    it('refuses an instant that is not a wire timestamp', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(server, sessionId, 'POST', '/api/curves/compute', {
            bootstrap_config_id: CONFIG,
            as_of: 'now',
        });
        expect(response.statusCode).toBe(400);
        expect(calls).toHaveLength(0);
    });
});
