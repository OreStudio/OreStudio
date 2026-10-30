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
        session: null,
        username: 'super_admin',
        email: 'tenant_admin@acme.example',
        accountId: '11111111-1111-1111-1111-111111111111',
        tenantId: '44444444-4444-4444-4444-444444444444',
        tenantName: 'Acme',
        mode: 'tenant-administration',
        version: 'v0.0.29 (test)',
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

const ok = { outcome: 'ok', code: '', message: '' };
const ECB_USD = '813ae520-d7f0-41b2-adc3-2c7cfc522e35';
const ECB_GBP = '813ae520-d7f0-41b2-adc3-2c7cfc522e36';
const FEED = 'f0000000-0000-0000-0000-000000000001';

function provider(code: string, terms: string): Record<string, unknown> {
    return {
        code,
        name: code.toUpperCase(),
        kind: 'central_bank',
        description: '',
        terms_url: terms,
        terms_credit: terms === '' ? '' : `Source: ${code}`,
        allows_storage: terms !== '',
        terms_accepted_digest: '',
        terms_accepted_by: '',
        terms_accepted_at: null,
    };
}

function series(id: string, quote: string): Record<string, unknown> {
    return {
        id,
        oresmd_uri: `oresmd://fx/EUR?type=fixing&index=fx&source=ECB&ccy=${quote}`,
        provider_code: 'ecb',
    };
}

/** The catalogue behind every public rates route, with the publisher it holds. */
function catalogue(extra: Readonly<Record<string, unknown>> = {}): Record<string, unknown> {
    return {
        'marketdata.v1.providers.list': {
            result: ok,
            providers: [provider('ecb', 'https://ecb.example/terms'), provider('bloomberg', '')],
        },
        'marketdata.v1.feeds.list': {
            result: ok,
            feeds: [
                {
                    id: FEED,
                    name: 'ecb-frankfurter',
                    publisher_code: 'ecb',
                    session_code: 'frankfurter-eu',
                    service: 'end_of_day',
                    provider_key: 'ECB',
                    is_default: true,
                },
            ],
        },
        'marketdata.v1.feed_scopes.list': {
            result: ok,
            feed_scopes: [{ feed_id: FEED, asset_class: 'fx' }],
        },
        'marketdata.v1.sessions.list': {
            result: ok,
            sessions: [{ code: 'frankfurter-eu', supplier_code: 'frankfurter' }],
        },
        'marketdata.v1.suppliers.list': {
            result: ok,
            suppliers: [{ code: 'frankfurter', name: 'Frankfurter' }],
        },
        'marketdata.v1.market_series.list': {
            result: ok,
            market_series: [
                series(ECB_USD, 'USD'),
                series(ECB_GBP, 'GBP'),
                { id: 'x', oresmd_uri: 'oresmd://fx/EUR?type=spot', provider_code: 'ecb' },
            ],
        },
        'marketdata.v1.market_fixings.list': {
            result: ok,
            market_fixings: [
                { series_id: ECB_USD, fixing_date: '2026-10-26', value: '1.16' },
                { series_id: ECB_USD, fixing_date: '2026-10-27', value: '1.15' },
                { series_id: ECB_GBP, fixing_date: '2026-10-27', value: '0.86' },
            ],
        },
        ...extra,
    };
}

function get(
    server: ReturnType<typeof buildServer>,
    sessionId: string,
    url: string,
): ReturnType<ReturnType<typeof buildServer>['inject']> {
    return server.inject({ method: 'GET', url, cookies: { [SESSION_COOKIE]: sessionId } });
}

describe('the public rates routes', () => {
    it('lists only the publishers that state terms, with their feeds and fixing series', async () => {
        const { server, sessionId } = buildTestServer(catalogue());
        const response = await get(server, sessionId, '/api/marketdata/public-rates');
        expect(response.statusCode).toBe(200);
        const providers = response.json().providers;
        expect(providers.map((p: { code: string }) => p.code)).toEqual(['ecb']);
        expect(providers[0].feeds[0]).toMatchObject({
            name: 'ecb-frankfurter',
            supplier_code: 'frankfurter',
            supplier_name: 'Frankfurter',
            asset_classes: ['fx'],
        });
        expect(providers[0].series.map((s: { quote: string }) => s.quote)).toEqual(['GBP', 'USD']);
    });

    it('summarises each series for the availability screen', async () => {
        const { server, sessionId } = buildTestServer(catalogue());
        const response = await get(server, sessionId, '/api/marketdata/public-rates/coverage');
        expect(response.statusCode).toBe(200);
        expect(response.json().series[ECB_USD]).toEqual({
            first: '2026-10-26',
            last: '2026-10-27',
            count: 2,
        });
    });

    it('downloads a publisher’s fixings as a file for the range asked for', async () => {
        const { server, sessionId } = buildTestServer(catalogue());
        const response = await get(
            server,
            sessionId,
            '/api/marketdata/public-rates/fixings.csv?publisher=ecb&from=2026-10-27&to=2026-10-27',
        );
        expect(response.statusCode).toBe(200);
        expect(response.headers['content-type']).toContain('text/csv');
        expect(response.headers['content-disposition']).toContain(
            'attachment; filename="ecb-fixings-2026-10-27_2026-10-27.csv"',
        );
        expect(response.headers['x-fixing-rows']).toBe('2');
        expect(response.body.split('\n')[0]).toBe('publisher,date,pair,item,value');
        expect(response.body).toContain('ecb,2026-10-27,EUR/USD');
        expect(response.body).toContain('ecb,2026-10-27,EUR/GBP');
    });

    it('refuses a read of more than twelve series before it asks the service', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const ids = Array.from(
            { length: 13 },
            (_, i) => `813ae520-d7f0-41b2-adc3-${String(i).padStart(12, '0')}`,
        );
        const response = await get(
            server,
            sessionId,
            `/api/marketdata/public-rates/fixings?series=${ids.join(',')}`,
        );
        expect(response.statusCode).toBe(400);
        expect(response.json().code).toBe('invalid-request');
        expect(calls).toHaveLength(0);
    });

    it('refuses a series that is not an id before it asks the service', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await get(
            server,
            sessionId,
            '/api/marketdata/public-rates/fixings?series=ecb-usd',
        );
        expect(response.statusCode).toBe(400);
        expect(calls).toHaveLength(0);
    });

    it('returns the fixings by series, oldest first, and an empty list for a series with none', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'marketdata.v1.market_fixings.list': {
                result: ok,
                market_fixings: [
                    { series_id: ECB_USD, fixing_date: '2026-10-27', value: '1.15' },
                    { series_id: ECB_USD, fixing_date: '2026-10-26', value: '1.16' },
                ],
            },
        });
        const response = await get(
            server,
            sessionId,
            `/api/marketdata/public-rates/fixings?series=${ECB_USD},${ECB_GBP}`,
        );
        expect(response.statusCode).toBe(200);
        expect(response.json().fixings).toEqual({
            [ECB_USD]: [
                { date: '2026-10-26', value: '1.16' },
                { date: '2026-10-27', value: '1.15' },
            ],
            [ECB_GBP]: [],
        });
        const body = calls[0]?.body as { filter: { series_id_one_of: string[] } };
        expect(body.filter.series_id_one_of).toEqual([ECB_USD, ECB_GBP]);
    });

    it('accepts the terms at the version the person read', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'marketdata.v1.ops.accept_provider_terms': {
                success: true,
                message: 'Accepted',
                accepted_by: 'super_admin',
                accepted_at: '2026-10-28 15:00:00Z',
            },
        });
        const response = await server.inject({
            method: 'POST',
            url: '/api/marketdata/public-rates/ecb/accept-terms',
            payload: { version: 3 },
            cookies: { [SESSION_COOKIE]: sessionId },
        });
        expect(response.statusCode).toBe(200);
        expect(response.json().accepted_by).toBe('super_admin');
        expect(calls[0]?.body).toEqual({ provider_code: 'ecb', provider_version: 3 });
    });

    it('refuses an acceptance that names no version before it asks the service', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await server.inject({
            method: 'POST',
            url: '/api/marketdata/public-rates/ecb/accept-terms',
            payload: {},
            cookies: { [SESSION_COOKIE]: sessionId },
        });
        expect(response.statusCode).toBe(400);
        expect(calls).toHaveLength(0);
    });

    it('reports terms that changed after they were read as a refusal', async () => {
        const { server, sessionId } = buildTestServer({
            'marketdata.v1.ops.accept_provider_terms': {
                success: false,
                message: "The provider 'ecb' changed after its terms were read. Read them again.",
            },
        });
        const response = await server.inject({
            method: 'POST',
            url: '/api/marketdata/public-rates/ecb/accept-terms',
            payload: { version: 2 },
            cookies: { [SESSION_COOKIE]: sessionId },
        });
        expect(response.statusCode).toBe(400);
        expect(response.json().message).toContain('Read them again');
    });
});
