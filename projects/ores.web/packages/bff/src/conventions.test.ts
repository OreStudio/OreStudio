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
import { CONVENTION_FAMILIES } from './conventions.js';
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

const ID = '55555555-5555-4555-8555-555555555555';
const INTENT = { reason_code: 'common.correction', commentary: '' };

describe('convention routes', () => {
    it('counts every family, and reads a count it cannot as null', async () => {
        const replies: Record<string, unknown> = {};
        for (const family of CONVENTION_FAMILIES) {
            replies[`refdata.v1.${family.prefix}.list`] = { result: ok, total: 2 };
        }
        replies['refdata.v1.fra_conventions.list'] = {
            result: { outcome: 'denied', code: '', message: 'No.' },
            total: 0,
        };
        const { server, sessionId } = buildTestServer(replies);
        const response = await send(server, sessionId, 'GET', '/api/conventions/families');
        const families = response.json().families;
        expect(families).toHaveLength(CONVENTION_FAMILIES.length);
        expect(families.find((f: { key: string }) => f.key === 'swap').count).toBe(2);
        expect(families.find((f: { key: string }) => f.key === 'fra').count).toBeNull();
        expect(
            families
                .filter((f: { writable: boolean }) => f.writable)
                .map((f: { key: string }) => f.key),
        ).toEqual(['deposit', 'swap']);
    });

    it('lists the rows of one family under its own subject', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.swap_conventions.list': {
                result: ok,
                total: 1,
                swap_conventions: [{ id: ID, version: 3, index: 'USD-LIBOR-3M' }],
            },
        });
        const response = await send(server, sessionId, 'GET', '/api/conventions/swap');
        expect(response.json().rows[0].index).toBe('USD-LIBOR-3M');
        expect(response.json().total).toBe(1);
        expect(calls[0]?.subject).toBe('refdata.v1.swap_conventions.list');
    });

    it('refuses a family it does not know', async () => {
        const { server, sessionId } = buildTestServer({});
        const response = await send(server, sessionId, 'GET', '/api/conventions/nonsense');
        expect(response.statusCode).toBe(404);
    });

    it('reads the pick lists the terms draw from', async () => {
        const rows = (field: string): unknown => ({
            result: ok,
            [field]: [{ code: 'A', version: 1 }],
        });
        const { server, sessionId } = buildTestServer({
            'refdata.v1.calendar_names.list': rows('calendar_names'),
            'refdata.v1.business_day_convention_types.list': rows('types'),
            'refdata.v1.day_count_fraction_types.list': rows('types'),
            'refdata.v1.floating_index_types.list': rows('types'),
            'refdata.v1.payment_frequencies.list': rows('payment_frequencies'),
            'refdata.v1.sub_periods_coupon_types.list': rows('types'),
            'refdata.v1.currencies.list': rows('currencies'),
        });
        const response = await send(server, sessionId, 'GET', '/api/conventions/pick-lists');
        expect(Object.keys(response.json()).sort()).toEqual([
            'businessDayConventions',
            'calendars',
            'currencies',
            'dayCountFractions',
            'floatingIndices',
            'paymentFrequencies',
            'subPeriodsCouponTypes',
        ]);
    });

    it('writes a drawn family as new or against its version, and returns the refusal', async () => {
        const { server, sessionId, calls } = buildTestServer({
            'refdata.v1.swap_conventions.put': {
                result: { outcome: 'conflict', code: 'stale_version', message: 'Stale.' },
                swap_convention: null,
            },
        });
        const response = await send(server, sessionId, 'PUT', '/api/conventions/swap', {
            intent: INTENT,
            version: 3,
            write: { id: ID, index: 'X' },
        });
        expect(response.json().result.code).toBe('stale_version');
        expect(calls[0]?.body).toMatchObject({
            change: { precondition: { kind: 'must_match_version', version: 3 } },
        });
    });

    it('refuses a write to a family whose terms are not drawn', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const response = await send(server, sessionId, 'PUT', '/api/conventions/fra', {
            intent: INTENT,
            version: null,
            write: { id: ID },
        });
        expect(response.statusCode).toBe(400);
        expect(calls).toHaveLength(0);
    });
});
