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
import { buildServer } from './server.js';
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

function siteConfiguration(): ReturnType<typeof loadSiteConfiguration> {
    const path = resolve(
        dirname(fileURLToPath(import.meta.url)),
        '../../../config/environments.json',
    );
    return loadSiteConfiguration({
        environment: { [SITE_CONFIG_VARIABLE]: path },
        environmentId: 'eager_maxwell',
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
        cookies: { ores_web_session: sessionId },
        ...(payload === undefined ? {} : { payload: payload as Record<string, unknown> }),
    });
}

const reason = { reasonCode: 'common.rectification', commentary: 'Tidy up' };

const euro = {
    iso_code: 'EUR',
    name: 'Euro',
    numeric_code: '978',
    symbol: 'E',
    fraction_symbol: 'c',
    fractions_per_unit: 100,
    rounding_type: 'Closest',
    rounding_precision: 2,
    format: '#,##0.00',
    monetary_nature: 'fiat',
    market_tier: 'g10',
    ore_currency_type: null,
    image_id: null,
    spot_days: 2,
    day_basis: 'ACT/360',
    base_precedence: 10,
};

describe('GET /api/refdata', () => {
    it('answers each resource with its key fields and permissions', async () => {
        const { server, sessionId } = buildTestServer({});
        const response = await send(server, sessionId, 'GET', '/api/refdata');
        const resources = (response.json() as { resources: Record<string, unknown>[] }).resources;
        expect(resources.find((resource) => resource.key === 'currency-countries')).toEqual({
            key: 'currency-countries',
            entityType: 'ores.refdata.currency_country',
            keyFields: ['currency_iso_code', 'country_alpha2_code'],
            versioned: false,
            writable: true,
            writePermission: 'refdata::currency_countries:write',
            deletePermission: 'refdata::currency_countries:delete',
        });
    });
});

describe('GET /api/refdata/:resource', () => {
    it("answers the rows of a resource in the server's field names", async () => {
        const { server, sessionId } = buildTestServer({
            'refdata.v1.currencies.list': { result: ok, currencies: [{ ...euro, version: 3 }] },
        });
        const response = await send(server, sessionId, 'GET', '/api/refdata/currencies');
        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({ rows: [{ iso_code: 'EUR', version: 3 }] });
    });

    it('answers 404 for a resource the registry does not name', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(server, sessionId, 'GET', '/api/refdata/trades');
        expect(response.statusCode).toBe(404);
        expect(calls).toHaveLength(0);
    });

    it('reads a junction by its parent', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.currency_calendars.list_by_currency_iso_code': {
                result: ok,
                currency_calendars: [
                    { currency_iso_code: 'EUR', calendar_code: 'TARGET', version: 1 },
                ],
            },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/refdata/currency-calendars/by/EUR',
        );
        expect(response.json()).toMatchObject({ rows: [{ calendar_code: 'TARGET' }] });
        expect(calls[0]?.body).toMatchObject({ currency_iso_code: 'EUR' });
    });

    it('refuses to read a resource by a parent it does not have', async () => {
        const { server, sessionId } = buildTestServer({});
        const response = await send(server, sessionId, 'GET', '/api/refdata/currencies/by/EUR');
        expect(response.statusCode).toBe(404);
    });
});

describe('PUT /api/refdata/:resource', () => {
    it('corrects a currency against the version read', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.currencies.put': { result: ok },
        });
        const response = await send(server, sessionId, 'PUT', '/api/refdata/currencies', {
            write: euro,
            version: 3,
            ...reason,
        });
        expect(response.statusCode).toBe(204);
        expect(calls[0]?.body).toMatchObject({
            change: {
                write: { iso_code: 'EUR' },
                precondition: { kind: 'must_match_version', version: 3 },
            },
            intent: { reason_code: 'common.rectification' },
        });
    });

    it('refuses a row that is not a valid row of the resource', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(server, sessionId, 'PUT', '/api/refdata/currencies', {
            write: { ...euro, iso_code: 'EURO', spot_days: -1 },
            version: null,
            ...reason,
        });
        expect(response.statusCode).toBe(400);
        expect(calls).toHaveLength(0);
    });

    it('refuses to write a resource that is read only here', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(server, sessionId, 'PUT', '/api/refdata/countries', {
            write: { alpha2_code: 'DE' },
            version: null,
            ...reason,
        });
        expect(response.statusCode).toBe(403);
        expect(calls).toHaveLength(0);
    });

    it('answers 409 when the row moved on since it was read', async () => {
        const { server, sessionId } = buildTestServer({
            'refdata.v1.currency_groups.put': {
                result: { outcome: 'conflict', code: '', message: '' },
            },
        });
        const response = await send(server, sessionId, 'PUT', '/api/refdata/currency-groups', {
            write: { code: 'G11', name: 'Group of eleven', description: '', display_order: 10 },
            version: 1,
            ...reason,
        });
        expect(response.statusCode).toBe(409);
        expect((response.json() as { message: string }).message).toContain('Reload and try again');
    });
});

describe('DELETE /api/refdata/:resource', () => {
    it('removes a junction row named by both its keys', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.currency_countries.delete': { result: ok },
        });
        const response = await send(
            server,
            sessionId,
            'DELETE',
            '/api/refdata/currency-countries',
            {
                key: { currency_iso_code: 'EUR', country_alpha2_code: 'DE' },
                ...reason,
            },
        );
        expect(response.statusCode).toBe(204);
        expect(calls[0]?.body).toMatchObject({
            removal: { key: { currency_iso_code: 'EUR', country_alpha2_code: 'DE' } },
        });
    });

    it('refuses a removal that does not name exactly the key fields', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(
            server,
            sessionId,
            'DELETE',
            '/api/refdata/currency-countries',
            {
                key: { currency_iso_code: 'EUR' },
                ...reason,
            },
        );
        expect(response.statusCode).toBe(400);
        expect(calls).toHaveLength(0);
    });
});

describe('GET /api/history for a record', () => {
    it('serves the history of a versioned record and refuses a junction', async () => {
        const { server, sessionId } = buildTestServer({
            'refdata.v1.history.get': {
                success: true,
                versions: [{ version: 1, modified_by: 'system' }],
            },
        });
        const served = await send(
            server,
            sessionId,
            'GET',
            '/api/history?entityType=ores.refdata.currency&entityId=EUR',
        );
        expect(served.statusCode).toBe(200);
        const refused = await send(
            server,
            sessionId,
            'GET',
            '/api/history?entityType=ores.refdata.currency_country&entityId=EUR',
        );
        expect(refused.statusCode).toBe(404);
    });
});
