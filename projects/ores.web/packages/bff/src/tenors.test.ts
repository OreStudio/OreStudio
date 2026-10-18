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

describe('tenor routes', () => {
    it('reads the catalogue: tenors, conventions and schedules', async () => {
        const { server, sessionId } = buildTestServer({
            'refdata.v1.tenors.list': { result: ok, tenors: [{ code: '3M', version: 1 }] },
            'refdata.v1.tenor_conventions.list': {
                result: ok,
                conventions: [{ code: 'SWAP', version: 2 }],
            },
            'refdata.v1.tenor_schedules.list': { result: ok, schedules: [{ code: 'IMM' }] },
        });
        const response = await send(server, sessionId, 'GET', '/api/tenors/catalogue');
        expect(response.json().tenors[0].code).toBe('3M');
        expect(response.json().conventions[0].code).toBe('SWAP');
        expect(response.json().schedules[0].code).toBe('IMM');
    });

    it('reads the six pick lists', async () => {
        const rows = (field: string): unknown => ({ result: ok, [field]: [{ code: 'A' }] });
        const { server, sessionId } = buildTestServer({
            'refdata.v1.tenor_kinds.list': rows('kinds'),
            'refdata.v1.tenor_units.list': rows('units'),
            'refdata.v1.tenor_anchors.list': rows('anchors'),
            'refdata.v1.tenor_resolution_algorithms.list': rows('algorithms'),
            'refdata.v1.business_day_convention_types.list': rows('types'),
            'refdata.v1.calendars.list': rows('calendars'),
        });
        const response = await send(server, sessionId, 'GET', '/api/tenors/pick-lists');
        expect(Object.keys(response.json()).sort()).toEqual([
            'algorithms',
            'anchors',
            'calendars',
            'kinds',
            'rollRules',
            'units',
        ]);
        expect(response.json().calendars[0].code).toBe('A');
    });

    it("reads one convention's set by its code", async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.tenor_convention_resolutions.list_by_convention_code': {
                result: ok,
                tenor_convention_resolutions: [
                    { convention_code: 'SWAP', tenor_code: '3M', version: 1 },
                ],
            },
        });
        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/tenors/conventions/SWAP/resolutions',
        );
        expect(response.json().rows).toHaveLength(1);
        expect(calls[0]?.body).toMatchObject({ convention_code: 'SWAP' });
    });

    it('writes a tenor as new or against its version, and a convention likewise', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.tenors.put': { result: ok, tenor: { code: '3M', version: 1 } },
            'refdata.v1.tenor_conventions.put': {
                result: ok,
                tenor_convention: { code: 'SWAP', version: 3 },
            },
        });
        const tenor = await send(server, sessionId, 'PUT', '/api/tenors/tenors', {
            intent: INTENT,
            version: null,
            write: { code: '3M' },
        });
        expect(tenor.json().tenor.version).toBe(1);
        const convention = await send(server, sessionId, 'PUT', '/api/tenors/conventions', {
            intent: INTENT,
            version: 2,
            write: { code: 'SWAP' },
        });
        expect(convention.json().convention.version).toBe(3);
        expect(calls[0]?.body).toMatchObject({
            change: { precondition: { kind: 'must_not_exist' } },
        });
        expect(calls[1]?.body).toMatchObject({
            change: { precondition: { kind: 'must_match_version', version: 2 } },
        });
    });

    it('links and unlinks a tenor in a convention by both codes', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.tenor_convention_resolutions.put': { result: ok },
            'refdata.v1.tenor_convention_resolutions.delete': { result: ok },
        });
        await send(server, sessionId, 'PUT', '/api/tenors/conventions/SWAP/resolutions/3M', {
            intent: INTENT,
            version: null,
            write: { anchor_override: 'SPOT' },
        });
        await send(server, sessionId, 'DELETE', '/api/tenors/conventions/SWAP/resolutions/3M', {
            intent: INTENT,
            version: 4,
        });
        expect(calls[0]?.body).toMatchObject({
            change: {
                write: { convention_code: 'SWAP', tenor_code: '3M', anchor_override: 'SPOT' },
            },
        });
        expect(calls[1]?.body).toMatchObject({
            removal: {
                key: { convention_code: 'SWAP', tenor_code: '3M' },
                precondition: { kind: 'must_match_version', version: 4 },
            },
        });
    });

    it('returns the refusal a write gives', async () => {
        const { server, sessionId } = buildTestServer({
            'refdata.v1.tenor_conventions.put': {
                result: { outcome: 'conflict', code: 'stale_version', message: 'Stale.' },
                tenor_convention: null,
            },
        });
        const response = await send(server, sessionId, 'PUT', '/api/tenors/conventions', {
            intent: INTENT,
            version: 1,
            write: { code: 'SWAP' },
        });
        expect(response.json().result.code).toBe('stale_version');
    });

    it('answers a drafted rule with the dates it resolves to', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.ops.preview_tenor_resolution': {
                result: ok,
                anchor_date: '2026-01-31',
                date_before_roll: '2026-02-28',
                adjusted_date: '2026-02-27',
                roll_rule: 'ModifiedFollowing',
                calendar: 'TARGET',
            },
        });
        const response = await send(server, sessionId, 'POST', '/api/tenors/preview-resolution', {
            tenor: { code: '1M', kind: 'PERIOD', unit: 'MONTH', multiplier: 1 },
            convention: {
                code: 'RATES_SPOT_FORWARD',
                measured_from: 'SPOT',
                resolution_algorithm: 'ANCHOR_OFFSET',
                business_day_convention_type: 'ModifiedFollowing',
            },
            resolution: { convention_code: 'RATES_SPOT_FORWARD', tenor_code: '1M' },
            calendar_code: 'TARGET',
            horizon: '2026-01-15',
            spot: '2026-01-31',
        });

        expect(response.statusCode).toBe(200);
        expect(response.json().adjusted_date).toBe('2026-02-27');
        expect(response.json().date_before_roll).toBe('2026-02-28');
        expect(calls[0]?.subject).toBe('refdata.v1.ops.preview_tenor_resolution');
        expect(calls[0]?.body).toMatchObject({ calendar_code: 'TARGET', horizon: '2026-01-15' });
    });

    it('hands a refusal back as an answer rather than a failed request', async () => {
        const { server, sessionId } = buildTestServer({
            'refdata.v1.ops.preview_tenor_resolution': {
                result: {
                    outcome: 'invalid',
                    code: 'missing_spot_date',
                    message: 'The rule did not resolve.',
                    fields: [
                        { field: 'spot', code: 'missing_spot_date', message: 'No spot date.' },
                    ],
                },
                anchor_date: '',
                date_before_roll: '',
                adjusted_date: '',
                roll_rule: 'ModifiedFollowing',
                calendar: 'TARGET',
            },
        });
        const response = await send(server, sessionId, 'POST', '/api/tenors/preview-resolution', {
            tenor: { code: '1M', kind: 'PERIOD', unit: 'MONTH', multiplier: 1 },
            convention: { code: 'RATES_SPOT_FORWARD' },
            resolution: { convention_code: 'RATES_SPOT_FORWARD', tenor_code: '1M' },
            calendar_code: 'TARGET',
            horizon: '2026-01-15',
        });

        // The refusal is the answer to the question that was asked, so the
        // request succeeded and the caller reads why from the payload.
        expect(response.statusCode).toBe(200);
        expect(response.json().result.code).toBe('missing_spot_date');
        expect(response.json().result.fields[0].field).toBe('spot');
    });

    it('resolves the rules as they are stored, without drafting them', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.ops.resolve_tenor': {
                result: ok,
                anchor_date: '2026-03-16',
                date_before_roll: '2026-06-16',
                adjusted_date: '2026-06-16',
                roll_rule: 'ModifiedFollowing',
                calendar: 'Argentina',
            },
        });
        const response = await send(server, sessionId, 'POST', '/api/tenors/resolve', {
            tenor_code: '3M',
            convention_code: 'RATES_SPOT_FORWARD',
            calendar_code: 'Argentina',
            horizon: '2026-03-16',
        });

        expect(response.statusCode).toBe(200);
        expect(response.json().adjusted_date).toBe('2026-06-16');
        expect(response.json().calendar).toBe('Argentina');
        expect(calls[0]?.subject).toBe('refdata.v1.ops.resolve_tenor');
        expect(calls[0]?.body).toMatchObject({
            tenor_code: '3M',
            convention_code: 'RATES_SPOT_FORWARD',
            calendar_code: 'Argentina',
            horizon: '2026-03-16',
            spot: null,
        });
    });

    it('refuses a question that names no tenor, before it reaches the server', async () => {
        const { server, sessionId, calls } = buildTestServer({});

        const response = await send(server, sessionId, 'POST', '/api/tenors/resolve', {
            convention_code: 'RATES_SPOT_FORWARD',
            calendar_code: 'Argentina',
            horizon: '2026-03-16',
        });

        expect(response.statusCode).toBe(400);
        expect(calls).toHaveLength(0);
    });

    it('hands a refusal of the stored rules back as an answer', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.ops.resolve_tenor': {
                result: {
                    outcome: 'invalid',
                    code: 'tenor_not_in_convention',
                    message: 'The convention does not resolve this tenor.',
                    fields: [
                        {
                            field: 'tenor_code',
                            code: 'tenor_not_in_convention',
                            message: 'No rule resolves 3M.',
                        },
                    ],
                },
                anchor_date: '',
                date_before_roll: '',
                adjusted_date: '',
                roll_rule: 'ModifiedFollowing',
                calendar: 'Argentina',
            },
        });
        const response = await send(server, sessionId, 'POST', '/api/tenors/resolve', {
            tenor_code: '3M',
            convention_code: 'RATES_SPOT_FORWARD',
            calendar_code: 'Argentina',
            horizon: '2026-03-16',
        });

        expect(response.statusCode).toBe(200);
        expect(response.json().adjusted_date).toBe('');
        expect(response.json().result.code).toBe('tenor_not_in_convention');
        expect(response.json().result.fields[0].field).toBe('tenor_code');
        expect(calls[0]?.subject).toBe('refdata.v1.ops.resolve_tenor');
    });
});
