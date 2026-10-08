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
            search: false,
            sortable: [],
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

    it('refuses a blank parent', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/refdata/currency-countries/by/%20%20',
        );
        expect(response.statusCode).toBe(400);
        expect(calls).toHaveLength(0);
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

describe('a currency pair write', () => {
    const pair = {
        pair_code: 'EUR/USD',
        base_currency: 'EUR',
        quote_currency: 'USD',
        classification: 'major',
    };

    it('refuses a pair code that is not BASE/QUOTE, and identical legs, with the reason', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const mismatched = await send(server, sessionId, 'PUT', '/api/refdata/currency-pairs', {
            write: { ...pair, pair_code: 'GBP/JPY' },
            version: null,
            ...reason,
        });
        expect(mismatched.statusCode).toBe(400);
        expect(mismatched.json()).toMatchObject({ message: expect.stringContaining('BASE/QUOTE') });
        const same = await send(server, sessionId, 'PUT', '/api/refdata/currency-pairs', {
            write: { ...pair, pair_code: 'EUR/EUR', quote_currency: 'EUR' },
            version: null,
            ...reason,
        });
        expect(same.statusCode).toBe(400);
        expect(calls).toHaveLength(0);
    });

    it('writes a pair whose code is its legs', async () => {
        const { server, sessionId } = buildTestServer({
            'refdata.v1.currency_pairs.put': { result: ok },
        });
        const response = await send(server, sessionId, 'PUT', '/api/refdata/currency-pairs', {
            write: pair,
            version: null,
            ...reason,
        });
        expect(response.statusCode).toBe(204);
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
            removal: {
                key: { currency_iso_code: 'EUR', country_alpha2_code: 'DE' },
                precondition: { kind: 'any' },
            },
        });
    });

    it('removes a versioned row against the version read, and answers 409 when it moved on', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.currency_groups.delete': {
                result: { outcome: 'conflict', code: 'version_conflict', message: '' },
            },
        });
        const response = await send(server, sessionId, 'DELETE', '/api/refdata/currency-groups', {
            key: { code: 'G11' },
            version: 3,
            ...reason,
        });
        expect(response.statusCode).toBe(409);
        expect(calls[0]?.body).toMatchObject({
            removal: { precondition: { kind: 'must_match_version', version: 3 } },
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

describe('a paged list read', () => {
    it('answers one page and the total, ordered and searched on the server', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.currencies.list': {
                result: ok,
                currencies: [{ iso_code: 'EUR', version: 2 }],
                total: 168,
            },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/refdata/currencies?offset=100&limit=50&search=eu&sort=name&descending=true',
        );
        expect(response.json()).toEqual({ rows: [{ iso_code: 'EUR', version: 2 }], total: 168 });
        expect(calls[0]?.body).toMatchObject({
            offset: 100,
            limit: 50,
            order: { field: 'name', descending: true },
            filter: { search: 'eu' },
        });
    });

    it('refuses an order the resource does not declare, a search it lacks, and a limit over 1000', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        for (const url of [
            '/api/refdata/currencies?limit=50&sort=symbol',
            '/api/refdata/countries?limit=50&search=fr',
            '/api/refdata/currencies?limit=5000',
        ]) {
            expect((await send(server, sessionId, 'GET', url)).statusCode).toBe(400);
        }
        expect(calls).toHaveLength(0);
    });
});

describe('GET /api/refdata/:resource/key/:key', () => {
    it('answers the one record, and 404 when there is none', async () => {
        const { server, sessionId } = buildTestServer({
            'refdata.v1.currency_pairs.list': {
                result: ok,
                pairs: [{ pair_code: 'EUR/USD', version: 3 }],
                total: 1,
            },
            'refdata.v1.currency_groups.list': { result: ok, groups: [], total: 0 },
        });
        const found = await send(
            server,
            sessionId,
            'GET',
            '/api/refdata/currency-pairs/key/EUR%2FUSD',
        );
        expect(found.json()).toEqual({ row: { pair_code: 'EUR/USD', version: 3 } });
        const missing = await send(
            server,
            sessionId,
            'GET',
            '/api/refdata/currency-groups/key/NONE',
        );
        expect(missing.statusCode).toBe(404);
    });
});

describe('GET /api/image-map', () => {
    it('answers which image each flagged code uses, cacheable for a few minutes', async () => {
        const { server, sessionId } = buildTestServer({
            'refdata.v1.currencies.list': {
                result: ok,
                currencies: [{ iso_code: 'EUR', image_id: 'eur-flag', version: 1 }],
            },
            'refdata.v1.countries.list': {
                result: ok,
                countries: [{ alpha2_code: 'GB', image_id: 'gb-flag', version: 1 }],
            },
            'refdata.v1.calendars.list': { result: ok, calendars: [] },
            'refdata.v1.business_centres.list': { result: ok, centres: [] },
            'assets.v1.images.list': {
                result: ok,
                images: [{ id: 'placeholder', code: 'xx', description: 'Flag of xx' }],
                total: 1,
            },
        });
        const response = await send(server, sessionId, 'GET', '/api/image-map');
        expect(response.headers['cache-control']).toBe('private, max-age=300');
        expect(response.json()).toEqual({
            currencies: { EUR: 'eur-flag' },
            countries: { GB: 'gb-flag' },
            calendars: {},
            businessCentres: {},
            noFlag: 'placeholder',
        });
    });
});

describe('GET /api/accounts', () => {
    it('asks for one page of people, searched and ordered on the server', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'iam.v1.accounts.list': { accounts: [], total: 0 },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/accounts?offset=15&limit=15&search=pri&sort=full_name&descending=true',
        );
        expect(response.statusCode).toBe(200);
        expect(calls[0]?.body).toMatchObject({
            offset: 15,
            limit: 15,
            order: { field: 'full_name', descending: true },
            filter: { id_one_of: null, search: 'pri' },
        });
    });

    it('refuses an order the account model does not declare', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(server, sessionId, 'GET', '/api/accounts?limit=15&sort=email');
        expect(response.statusCode).toBe(400);
        expect(calls).toHaveLength(0);
    });
});

describe('the calendar routes', () => {
    const bespoke = {
        code: 'ACME',
        name: 'Acme office',
        calendar_type: 'public_holiday',
        country_code: 'GB',
        image_id: null,
        source: 'user',
        is_editable: true,
        base_calendar_code: null,
    };

    it('writes a user calendar and refuses one that claims another source', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.calendars.put': { result: ok },
        });
        const saved = await send(server, sessionId, 'PUT', '/api/refdata/calendars', {
            write: bespoke,
            version: null,
            ...reason,
        });
        expect(saved.statusCode).toBe(204);
        const refused = await send(server, sessionId, 'PUT', '/api/refdata/calendars', {
            write: { ...bespoke, source: 'quantlib', is_editable: false },
            version: null,
            ...reason,
        });
        expect(refused.statusCode).toBe(400);
        expect(calls).toHaveLength(1);
    });

    it('refuses a rule of a kind the rule engine does not know', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(server, sessionId, 'PUT', '/api/refdata/calendar-rules', {
            write: {
                id: '55555555-5555-4555-8555-555555555555',
                calendar_code: 'ACME',
                kind: 'every_full_moon',
                month: null,
                day: null,
                weekday: null,
                occurrence: null,
                day_offset: null,
                shift: 'none',
                effective_from: null,
                effective_to: null,
            },
            version: null,
            ...reason,
        });
        expect(response.statusCode).toBe(400);
        expect(calls).toHaveLength(0);
    });

    it('answers the days of one year, and refuses a year out of range', async () => {
        const { server, sessionId } = buildTestServer({
            'refdata.v1.calendar_dates.list_by_calendar_code': {
                result: ok,
                calendar_dates: [
                    { date: '2025-12-31', is_business_day: true, source: 'user_defined' },
                    { date: '2026-01-01', is_business_day: false, source: 'user_defined' },
                    { date: '2027-01-01', is_business_day: false, source: 'user_defined' },
                ],
            },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/refdata/calendars/ACME/days?year=2026',
        );
        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            days: [{ date: '2026-01-01', businessDay: false, source: 'user_defined' }],
        });
        const refused = await send(
            server,
            sessionId,
            'GET',
            '/api/refdata/calendars/ACME/days?year=99',
        );
        expect(refused.statusCode).toBe(400);
    });

    it('rebuilds one calendar up to a year', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.ops.regenerate_calendar_dates': {
                success: true,
                message: '',
                rows_written: 730,
            },
        });
        const response = await send(
            server,
            sessionId,
            'POST',
            '/api/refdata/calendars/ACME/rebuild',
            {
                endYear: 2027,
            },
        );
        expect(response.json()).toEqual({ written: 730 });
        expect(calls[0]?.body).toEqual({ calendar_code: 'ACME', end_year: 2027 });
    });
});

describe('a calendar rule write', () => {
    /** The server answers a read by key with that one calendar. */
    const only = (calendar: { code: string; is_editable: boolean }) => ({
        'refdata.v1.calendars.list': { result: ok, calendars: [{ ...calendar, version: 1 }] },
        'refdata.v1.calendar_rules.put': { result: ok },
    });
    const calendars = only({ code: 'ACME', is_editable: true });
    const christmas = {
        id: '55555555-5555-4555-8555-555555555555',
        calendar_code: 'ACME',
        kind: 'fixed_date',
        month: 12,
        day: 25,
        weekday: null,
        occurrence: null,
        day_offset: null,
        shift: 'nearest_weekday',
        effective_from: null,
        effective_to: null,
    };
    const put = (server: ReturnType<typeof buildServer>, sessionId: string, write: unknown) =>
        send(server, sessionId, 'PUT', '/api/refdata/calendar-rules', {
            write,
            version: null,
            ...reason,
        });

    it('writes a rule with the fields its kind is made of', async () => {
        const { server, sessionId } = buildTestServer(calendars);
        expect((await put(server, sessionId, christmas)).statusCode).toBe(204);
    });

    it('refuses a rule missing a field its kind needs, or carrying one it does not', async () => {
        const { server, sessionId, calls } = buildTestServer(calendars);
        const missing = await put(server, sessionId, { ...christmas, day: null });
        expect(missing.statusCode).toBe(400);
        expect(missing.json()).toMatchObject({
            message: expect.stringContaining('fixed_date rule needs month, day'),
        });
        const extra = await put(server, sessionId, { ...christmas, weekday: 1 });
        expect(extra.statusCode).toBe(400);
        expect(calls).toHaveLength(0);
    });

    it('refuses a first year after the last year', async () => {
        const { server, sessionId } = buildTestServer(calendars);
        const response = await put(server, sessionId, {
            ...christmas,
            effective_from: 2030,
            effective_to: 2020,
        });
        expect(response.statusCode).toBe(400);
    });

    it('refuses a rule on a calendar that is not editable', async () => {
        const { server, sessionId, calls } = buildTestServer(
            only({ code: 'TARGET', is_editable: false }),
        );
        const response = await put(server, sessionId, { ...christmas, calendar_code: 'TARGET' });
        expect(response.statusCode).toBe(403);
        expect(calls.map((call) => call.subject)).not.toContain('refdata.v1.calendar_rules.put');
        expect(calls[0]?.body).toMatchObject({ limit: 1, filter: { code_one_of: ['TARGET'] } });
    });
});
