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
    method: 'GET',
    url: string,
): ReturnType<ReturnType<typeof buildServer>['inject']> {
    return server.inject({ method, url, cookies: { [SESSION_COOKIE]: sessionId } });
}

const IBOR = {
    code: 'ir.index_fixing.ibor',
    asset_class: 'ir',
    series_subclass: 'index_fixing',
    index_family: 'ibor',
    description: 'A fixing of an interbank offered rate.',
};

describe('series identity shape field route', () => {
    it('serves each entry with the family parts of its shape', async () => {
        const { server, sessionId } = buildTestServer({
            'marketdata.v1.series_identity_shapes.list': { result: ok, shapes: [IBOR], total: 1 },
            'marketdata.v1.series_identity_shape_entries.list': {
                result: ok,
                entries: [
                    {
                        shape_code: IBOR.code,
                        field: 'ccy',
                        requirement: 'required',
                        description: 'Named by its currency.',
                        version: 1,
                    },
                ],
                total: 1,
            },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/marketdata/series-identity-shape-fields',
        );
        expect(response.statusCode).toBe(200);
        expect(response.json().fields).toEqual([
            {
                asset_class: 'ir',
                series_subclass: 'index_fixing',
                index_family: 'ibor',
                field: 'ccy',
                requirement: 'required',
                description: 'Named by its currency.',
            },
        ]);
    });

    it('leaves out an entry whose shape the session does not read', async () => {
        const { server, sessionId } = buildTestServer({
            'marketdata.v1.series_identity_shapes.list': { result: ok, shapes: [], total: 0 },
            'marketdata.v1.series_identity_shape_entries.list': {
                result: ok,
                entries: [{ shape_code: IBOR.code, field: 'ccy', requirement: 'required' }],
                total: 1,
            },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/marketdata/series-identity-shape-fields',
        );
        expect(response.json().fields).toEqual([]);
    });
});

describe('series classification rule route', () => {
    it('serves the rules as the four fields the editor reads', async () => {
        const { server, sessionId } = buildTestServer({
            'marketdata.v1.series_classification_rules.list': {
                result: ok,
                rules: [
                    {
                        instrument: 'zero',
                        quote: '',
                        key_kind: 'datum',
                        series_subclass_code: 'yield',
                        description: 'A zero rate.',
                        version: 1,
                    },
                ],
            },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/marketdata/series-classification-rules',
        );
        expect(response.json().rules).toEqual([
            { instrument: 'zero', quote: '', key_kind: 'datum', series_subclass_code: 'yield' },
        ]);
    });
});
