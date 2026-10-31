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
} {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const client = {
        async callAuthenticated(
            _subject: string,
            _body: unknown,
            schema: { parse: (value: unknown) => unknown },
        ): Promise<unknown> {
            const canned = replies[_subject];
            if (canned === undefined) throw new Error(`No canned reply for ${_subject}`);
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
    };
}

const URI = 'oresmd://fx/EUR?type=fixing&index=fx&source=ECB&ccy=USD';
const ok = { result: { outcome: 'ok', code: '', message: '', fields: [] } };

function replies(): Record<string, unknown> {
    return {
        'marketdata.v1.market_series.list': {
            ...ok,
            market_series: [
                { id: 's1', oresmd_uri: URI, provider_code: 'ecb', version: 1 },
                {
                    id: 's2',
                    oresmd_uri: 'oresmd://fx/EUR?type=fixing&index=fx&source=ECB&ccy=GBP',
                    provider_code: 'ecb',
                    version: 1,
                },
            ],
            total: 2,
        },
        'marketdata.v1.market_fixings.list': {
            ...ok,
            market_fixings: [
                { series_id: 's1', fixing_date: '2026-10-29', value: '1.16' },
                { series_id: 's1', fixing_date: '2026-10-30', value: '1.15' },
            ],
            total: 2,
        },
        'marketdata.v1.providers.list': {
            ...ok,
            providers: [{ code: 'ecb', name: 'European Central Bank' }],
            total: 1,
        },
        'marketdata.v1.feeds.list': {
            ...ok,
            feeds: [
                {
                    id: 'f1',
                    name: 'ecb-frankfurter',
                    publisher_code: 'ecb',
                    session_code: 'frankfurter-eu',
                    service: 'end_of_day',
                },
            ],
            total: 1,
        },
        'marketdata.v1.feed_scopes.list': {
            ...ok,
            scopes: [{ feed_id: 'f1', item: URI }],
            total: 1,
        },
        'marketdata.v1.sessions.list': {
            ...ok,
            sessions: [
                { code: 'frankfurter-eu', supplier_code: 'frankfurter', environment: 'production' },
            ],
            total: 1,
        },
        'marketdata.v1.suppliers.list': {
            ...ok,
            suppliers: [{ code: 'frankfurter', name: 'Frankfurter' }],
            total: 1,
        },
    };
}

describe('the fixing series route', () => {
    it('answers the stored series, its fixings and the feeds that carry it', async () => {
        const { server, sessionId } = buildTestServer(replies());
        const response = await server.inject({
            method: 'GET',
            url: `/api/marketdata/fixing-series?uri=${encodeURIComponent(URI)}`,
            cookies: { [SESSION_COOKIE]: sessionId },
        });
        expect(response.statusCode).toBe(200);
        const body = response.json();
        expect(body.series).toMatchObject({ id: 's1', provider_code: 'ecb' });
        expect(body.publisher).toEqual({ code: 'ecb', name: 'European Central Bank' });
        expect(body.values).toMatchObject({ first: '2026-10-29', last: '2026-10-30', count: 2 });
        expect(body.feeds).toEqual([
            {
                id: 'f1',
                name: 'ecb-frankfurter',
                service: 'end_of_day',
                session_code: 'frankfurter-eu',
                environment: 'production',
                supplier_name: 'Frankfurter',
            },
        ]);
    });

    it('answers that the store holds nothing for a URI it does not know', async () => {
        const { server, sessionId } = buildTestServer(replies());
        const response = await server.inject({
            method: 'GET',
            url: '/api/marketdata/fixing-series?uri=oresmd%3A%2F%2Ffx%2FEUR%3Ftype%3Dfixing%26ccy%3DJPY',
            cookies: { [SESSION_COOKIE]: sessionId },
        });
        expect(response.statusCode).toBe(200);
        expect(response.json().series).toBeNull();
    });
});
