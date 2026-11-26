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
    readonly subjects: string[];
} {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const seen: string[] = [];
    const client = {
        async callAuthenticated(
            subject: string,
            _body: unknown,
            schema: { parse: (value: unknown) => unknown },
        ): Promise<unknown> {
            seen.push(subject);
            const canned = replies[subject];
            if (canned === undefined) throw new Error(`No canned reply for ${subject}`);
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
        email: 'super_admin@ores.example',
        accountId: '11111111-1111-1111-1111-111111111111',
        tenantId: 'ffffffff-ffff-ffff-ffff-ffffffffffff',
        tenantName: 'System',
        mode: 'system-administration',
        version: 'v0.0.31 (test)',
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
        subjects: seen,
    };
}

const ok = { result: { outcome: 'ok', code: '', message: '', fields: [] } };
const SUBJECT = 'marketdata.v1.ops.list_fixing_series_summaries';

function replies(): Record<string, unknown> {
    return {
        [SUBJECT]: {
            ...ok,
            series: [
                {
                    series_id: 's1',
                    oresmd_uri: 'oresmd://ir/USD?type=fixing&index=ibor&name=SOFR&tenor=1M',
                    asset_class: 'ir',
                    index_label: 'USD SOFR 1M',
                    source: 'derived-a',
                    provider_code: 'derived',
                    latest_value: '0.039436',
                    latest_date: '2026-11-26',
                    days: 520,
                },
                {
                    series_id: 's2',
                    oresmd_uri: 'oresmd://ir/CHF?type=fixing&index=ibor&name=SARON&tenor=3M',
                    asset_class: 'ir',
                    index_label: 'CHF SARON 3M',
                    source: '',
                    provider_code: 'derived',
                    latest_value: '',
                    latest_date: null,
                    days: 0,
                },
            ],
        },
    };
}

describe('the fixing observations route', () => {
    it('answers every series the service states, an empty one included', async () => {
        const { server, sessionId } = buildTestServer(replies());
        const response = await server.inject({
            method: 'GET',
            url: '/api/marketdata/fixing-observations',
            cookies: { [SESSION_COOKIE]: sessionId },
        });
        expect(response.statusCode).toBe(200);
        const body = response.json();
        expect(body.series).toHaveLength(2);
        expect(body.series[0]).toMatchObject({
            index_label: 'USD SOFR 1M',
            latest_value: '0.039436',
            latest_date: '2026-11-26',
            days: 520,
        });
        // A series that holds no day is a row, not a gap in the answer.
        expect(body.series[1]).toMatchObject({ index_label: 'CHF SARON 3M', days: 0 });
        expect(body.series[1].latest_date).toBeNull();
    });

    it('passes the three filters to the service and nothing else', async () => {
        const { server, sessionId, subjects } = buildTestServer(replies());
        const response = await server.inject({
            method: 'GET',
            url: '/api/marketdata/fixing-observations?asset_class=ir&currency=USD&source=derived-a',
            cookies: { [SESSION_COOKIE]: sessionId },
        });
        expect(response.statusCode).toBe(200);
        expect(subjects).toEqual([SUBJECT]);
    });
});
