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

interface Harness {
    readonly server: ReturnType<typeof buildServer>;
    readonly sessionId: string;
    readonly calls: string[];
}

function buildTestServer(replies: Readonly<Record<string, unknown>>): Harness {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const calls: string[] = [];
    const client = {
        async callAuthenticated(subject: string): Promise<unknown> {
            calls.push(subject);
            if (!(subject in replies)) {
                throw new Error(`No canned reply for ${subject}`);
            }
            return replies[subject];
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
        calls,
    };
}

function send(
    server: ReturnType<typeof buildServer>,
    sessionId: string | undefined,
): ReturnType<ReturnType<typeof buildServer>['inject']> {
    return server.inject({
        method: 'GET',
        url: '/api/marketdata/connectivity',
        ...(sessionId === undefined ? {} : { cookies: { [SESSION_COOKIE]: sessionId } }),
    });
}

const FEED_ID = 'f0000000-0000-0000-0000-000000000001';

function page(rows: string, values: readonly unknown[]): Record<string, unknown> {
    return {
        result: { outcome: 'ok', code: '', message: '', fields: [] },
        [rows]: values,
        total: values.length,
    };
}

const REPLIES = {
    'marketdata.v1.providers.list': page('providers', [
        {
            code: 'ecb',
            name: 'European Central Bank',
            kind: 'central_bank',
            terms_url: 'https://ecb.example/terms',
            terms_credit: 'Source: ECB',
            terms_accepted_digest: 'abc',
            terms_accepted_by: 'admin',
            terms_accepted_at: '2026-10-30 10:00:00Z',
        },
    ]),
    'marketdata.v1.suppliers.list': page('suppliers', [
        {
            code: 'frankfurter',
            name: 'Frankfurter',
            kind: 'aggregator',
            contract_reference: 'C-1',
            support: 'support@frankfurter.example',
        },
    ]),
    'marketdata.v1.adapters.list': page('adapters', [
        {
            code: 'frankfurter',
            name: 'Frankfurter',
            module_version: '0.0.29',
            process: 'ores.marketdata.official',
            asset_classes: 'fx',
            services: 'end_of_day, reference',
            configuration_schema:
                '{"url":{"type":"url","required":false,"default":"https://api.frankfurter.dev"}}',
        },
    ]),
    'marketdata.v1.sessions.list': page('sessions', [
        {
            code: 'frankfurter-eu',
            name: 'Frankfurter EU',
            supplier_code: 'frankfurter',
            adapter_code: 'frankfurter',
            environment: 'production',
            endpoint: 'https://api.frankfurter.dev',
            credential_ref: 'none',
            settings: '{"poll_minutes":60}',
            enabled: true,
        },
    ]),
    'marketdata.v1.feeds.list': page('feeds', [
        {
            id: FEED_ID,
            name: 'ecb-frankfurter',
            publisher_code: 'ecb',
            session_code: 'frankfurter-eu',
            service: 'end_of_day',
            provider_key: 'ECB',
            is_default: true,
        },
    ]),
    'marketdata.v1.feed_scopes.list': page('feed_scopes', [
        {
            feed_id: FEED_ID,
            asset_class: 'fx',
            item: 'oresmd://fx/EUR?type=fixing&index=fx&source=ECB&ccy=USD',
            vendor_instrument_id: null,
        },
    ]),
    'marketdata.v1.routes.list': page('routes', [
        {
            party_id: '55555555-5555-5555-5555-555555555555',
            publisher_code: 'ecb',
            asset_class: 'fx',
            feed_id: FEED_ID,
        },
    ]),
} as const;

describe('the connectivity route', () => {
    it('answers the whole delivery tree in one read', async () => {
        const { server, sessionId } = buildTestServer(REPLIES);
        const response = await send(server, sessionId);
        expect(response.statusCode).toBe(200);
        const body = response.json();
        expect(body.publishers[0]).toMatchObject({
            code: 'ecb',
            terms_accepted: true,
            terms_accepted_by: 'admin',
        });
        expect(body.suppliers[0]).toMatchObject({ code: 'frankfurter', contract_reference: 'C-1' });
        expect(body.adapters[0]).toMatchObject({
            code: 'frankfurter',
            asset_classes: ['fx'],
            services: ['end_of_day', 'reference'],
            schema: [
                {
                    name: 'url',
                    type: 'url',
                    required: false,
                    default: 'https://api.frankfurter.dev',
                },
            ],
        });
        expect(body.sessions[0]).toMatchObject({
            code: 'frankfurter-eu',
            settings: { poll_minutes: 60 },
            enabled: true,
        });
        expect(body.feeds[0]).toMatchObject({
            name: 'ecb-frankfurter',
            is_default: true,
            scopes: [{ asset_class: 'fx' }],
            routes: [{ publisher_code: 'ecb', asset_class: 'fx' }],
        });
    });

    it('needs a session', async () => {
        const { server, calls } = buildTestServer({});
        const response = await send(server, undefined);
        expect(response.statusCode).toBe(401);
        expect(calls).toHaveLength(0);
    });
});
