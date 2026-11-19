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

const INTENT = { reason_code: 'common.correction', commentary: '' };

const SYSTEM = 'ffffffff-ffff-ffff-ffff-ffffffffffff';
const OWN = '44444444-4444-4444-4444-444444444444';

const IBOR_CCY = {
    asset_class: 'ir',
    series_subclass: 'index_fixing',
    index_family: 'ibor',
    field: 'ccy',
};

describe('series identity shape routes', () => {
    it("lists every shape and says which are the shared catalogue's", async () => {
        const { server, sessionId } = buildTestServer({
            'marketdata.v1.series_identity_shapes.list': {
                result: ok,
                shapes: [
                    { ...IBOR_CCY, tenant_id: SYSTEM, requirement: 'required', version: 1 },
                    { ...IBOR_CCY, field: 'tenor', tenant_id: OWN, requirement: 'forbidden' },
                ],
            },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/marketdata/series-identity-shapes',
        );
        expect(response.statusCode).toBe(200);
        expect(
            response
                .json()
                .shapes.map((shape: { field: string; system: boolean }) => [
                    shape.field,
                    shape.system,
                ]),
        ).toEqual([
            ['ccy', true],
            ['tenor', false],
        ]);
    });

    it('reads the versions of one row by its four key parts, newest first', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'marketdata.v1.series_identity_shapes_versions.list': {
                result: ok,
                versions: [
                    { ...IBOR_CCY, tenant_id: SYSTEM, version: 1 },
                    { ...IBOR_CCY, tenant_id: SYSTEM, version: 2 },
                ],
            },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/marketdata/series-identity-shapes/versions?asset_class=ir&series_subclass=index_fixing&index_family=ibor&field=ccy',
        );
        expect(response.json().versions.map((row: { version: number }) => row.version)).toEqual([
            2, 1,
        ]);
        expect(calls[0]?.body).toMatchObject({ key: IBOR_CCY });
    });

    it('reads the versions of a family with no index family', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'marketdata.v1.series_identity_shapes_versions.list': { result: ok, versions: [] },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/marketdata/series-identity-shapes/versions?asset_class=ir&series_subclass=yield&index_family=&field=ccy',
        );
        expect(response.statusCode).toBe(200);
        expect(calls[0]?.body).toMatchObject({ key: { index_family: '' } });
    });

    it('writes a new row and corrects one against the version read', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'marketdata.v1.series_identity_shapes.put': {
                result: ok,
                series_identity_shape: { ...IBOR_CCY, tenant_id: OWN, version: 1 },
            },
        });
        const write = {
            ...IBOR_CCY,
            requirement: 'required',
            description: 'Named by its currency.',
        };
        const created = await send(
            server,
            sessionId,
            'PUT',
            '/api/marketdata/series-identity-shapes',
            {
                write,
                version: null,
                intent: INTENT,
            },
        );
        expect(created.json().shape.system).toBe(false);
        await send(server, sessionId, 'PUT', '/api/marketdata/series-identity-shapes', {
            write,
            version: 3,
            intent: INTENT,
        });
        expect(calls[0]?.body).toMatchObject({
            change: { write, precondition: { kind: 'must_not_exist' } },
        });
        expect(calls[1]?.body).toMatchObject({
            change: { precondition: { kind: 'must_match_version', version: 3 } },
        });
    });

    it('refuses a requirement the table does not hold, before asking the service', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(
            server,
            sessionId,
            'PUT',
            '/api/marketdata/series-identity-shapes',
            {
                write: { ...IBOR_CCY, requirement: 'optional', description: 'x' },
                version: null,
                intent: INTENT,
            },
        );
        expect(response.statusCode).toBe(400);
        expect(calls).toHaveLength(0);
    });

    it("removes a row by its key and the version read, and passes the service's refusal on", async () => {
        const denied = { outcome: 'denied', code: '', message: 'The row is the system tenant.' };
        const { server, sessionId, calls } = buildTestServer({
            'marketdata.v1.series_identity_shapes.delete': { result: denied },
        });
        const response = await send(
            server,
            sessionId,
            'DELETE',
            '/api/marketdata/series-identity-shapes',
            {
                key: IBOR_CCY,
                version: 2,
                intent: INTENT,
            },
        );
        expect(response.json().result.outcome).toBe('denied');
        expect(calls[0]?.body).toMatchObject({
            removal: { key: IBOR_CCY, precondition: { kind: 'must_match_version', version: 2 } },
        });
    });
});
