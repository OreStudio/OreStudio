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
    readonly calls: { subject: string; body: unknown }[];
}

function buildTestServer(replies: Readonly<Record<string, unknown>>): Harness {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const calls: { subject: string; body: unknown }[] = [];
    const client = {
        async callAuthenticated(subject: string, body: unknown): Promise<unknown> {
            calls.push({ subject, body });
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
        version: 'v0.0.30 (test)',
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

function post(
    harness: Harness,
    url: string,
    payload: Record<string, unknown>,
): ReturnType<ReturnType<typeof buildServer>['inject']> {
    return harness.server.inject({
        method: 'POST',
        url,
        payload,
        cookies: { [SESSION_COOKIE]: harness.sessionId },
    });
}

const source = {
    provider_code: 'derived',
    provider_kind: 'derived',
    series_id: 's-1',
    as_of: '2026-11-13 10:00:00Z',
    age_seconds: 900,
    stale: false,
};

const INSTRUMENTS = {
    success: true,
    message: '',
    total: 1,
    truncated: false,
    instruments: [
        {
            instrument_id: 'i-1',
            kind: 'curve',
            name: 'USD-SOFR',
            currency: 'USD',
            base_currency: '',
            quote_currency: '',
            instrument_type: 'discount',
            quote_type: 'rate',
            forward_start: '',
            axes: [{ field: 'term', values: ['1M', '3M'] }],
            sources: [source],
            party_id: 'p-1',
            stale: false,
            as_of: '2026-11-13 10:00:00Z',
        },
    ],
};

const LADDER = {
    success: true,
    message: '',
    currency: 'USD',
    instrument_type: 'discount',
    forward_start: '',
    as_of: '2026-11-13 23:59:59Z',
    provider_count: 1,
    tenor_convention: 'RATES_SPOT_FORWARD',
    day_count_convention: 'A365F',
    rungs: [
        {
            tenor: '1M',
            maturity_date: '2026-12-14',
            year_fraction: 0.0833,
            quotes: [
                {
                    provider_code: 'ore',
                    value: 0.0432,
                    as_of: '2026-11-13 10:00:00Z',
                    age_seconds: 60,
                    stale: false,
                    series_id: 's-2',
                },
            ],
            consensus: { value: 0.0432, method: 'mean', providers_used: 1 },
        },
    ],
};

const MONTAGE = {
    success: true,
    message: '',
    base_currency: 'EUR',
    as_of: '2026-11-13 23:59:59Z',
    change_since: '2026-11-13 00:00:00Z',
    pairs: [
        {
            pair_name: 'EUR/USD',
            base_currency: 'EUR',
            quote_currency: 'USD',
            spot_by_provider: [
                {
                    provider_code: 'ore',
                    value: 1.085,
                    as_of: '2026-11-13 10:00:00Z',
                    age_seconds: 60,
                    stale: false,
                    series_id: 's-3',
                },
            ],
            consensus: 1.085,
            change_pct: 0.12,
            day_high: 1.09,
            day_low: 1.08,
        },
    ],
};

const ANALYTICS = {
    success: true,
    message: '',
    instrument_id: 'c-1',
    name: 'USD-SOFR',
    as_of: '2026-11-13 23:59:59Z',
    stale: false,
    conventions: {
        index_name: 'USD-SOFR',
        day_count: 'A365F',
        compounding: 'continuous',
        interpolation: 'flat_forward_then_log_linear',
        calendar: 'UnitedStates.Settlement',
        spot_lag_days: 2,
        pillar_count: 1,
    },
    market_pillars: [
        {
            tenor: '1M',
            maturity_date: '2026-12-14',
            year_fraction: 0.0833,
            rate: 0.0432,
            provider_code: 'ore',
            series_id: 's-4',
            as_of: '2026-11-13 10:00:00Z',
        },
    ],
    bootstrapped: [
        {
            tenor: '1M',
            maturity_date: '2026-12-14',
            year_fraction: 0.0833,
            discount_factor: 0.9964,
            zero_rate: 0.0432,
            implied_forward: 0.0432,
            shift_bps: 0,
        },
    ],
};

const REPLIES = {
    'marketdata.v1.ops.list_instruments': INSTRUMENTS,
    'marketdata.v1.ops.get_rate_ladder': LADDER,
    'marketdata.v1.ops.get_fx_montage': MONTAGE,
    'marketdata.v1.ops.get_curve_analytics': ANALYTICS,
};

describe('the market read routes', () => {
    it('answers the catalogue of the session party', async () => {
        const harness = buildTestServer(REPLIES);
        const response = await post(harness, '/api/market/instruments', { query: 'USD' });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({ total: 1, truncated: false });
        expect(response.json().instruments[0]).toMatchObject({
            kind: 'curve',
            name: 'USD-SOFR',
            axes: [{ field: 'term', values: ['1M', '3M'] }],
        });
        // The party is the session's, so the browser never names it.
        expect(harness.calls[0]?.body).toMatchObject({ party_id: '', query: 'USD' });
    });

    it('forwards the ladder as of the instant the screen states', async () => {
        const harness = buildTestServer(REPLIES);
        const response = await post(harness, '/api/market/rate-ladder', {
            currency: 'USD',
            as_of: '2026-11-13 23:59:59Z',
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({
            currency: 'USD',
            tenor_convention: 'RATES_SPOT_FORWARD',
            rungs: [{ tenor: '1M', maturity_date: '2026-12-14' }],
        });
        // The route's own status is not part of the answer it serves.
        expect(response.json()).not.toHaveProperty('success');
    });

    it('says the instant a montage measures its change from', async () => {
        const harness = buildTestServer(REPLIES);
        const response = await post(harness, '/api/market/fx-montage', {
            base_currency: 'EUR',
            as_of: '2026-11-13 23:59:59Z',
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({
            base_currency: 'EUR',
            change_since: '2026-11-13 00:00:00Z',
        });
        expect(response.json().pairs[0]).toMatchObject({
            pair_name: 'EUR/USD',
            consensus: 1.085,
            change_pct: 0.12,
        });
    });

    it('keeps an absent change absent rather than zero', async () => {
        const harness = buildTestServer({
            ...REPLIES,
            'marketdata.v1.ops.get_fx_montage': {
                ...MONTAGE,
                pairs: [{ ...MONTAGE.pairs[0], change_pct: null, day_high: null, day_low: null }],
            },
        });
        const response = await post(harness, '/api/market/fx-montage', {
            base_currency: 'EUR',
            as_of: '2026-11-13 23:59:59Z',
        });

        expect(response.json().pairs[0]).toMatchObject({
            change_pct: null,
            day_high: null,
            day_low: null,
        });
    });

    it('answers a curve with its conventions and both of its sides', async () => {
        const harness = buildTestServer(REPLIES);
        const response = await post(harness, '/api/market/curve-analytics', {
            instrument_id: 'c0000000-0000-0000-0000-000000000001',
            as_of: '2026-11-13 23:59:59Z',
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({
            name: 'USD-SOFR',
            conventions: { day_count: 'A365F', pillar_count: 1 },
        });
        expect(response.json().market_pillars[0]).toMatchObject({ rate: 0.0432 });
        expect(response.json().bootstrapped[0]).toMatchObject({ discount_factor: 0.9964 });
    });

    it('carries a refusal in the server words', async () => {
        const harness = buildTestServer({
            ...REPLIES,
            'marketdata.v1.ops.get_rate_ladder': {
                ...LADDER,
                success: false,
                message: 'The party holds no USD ladder.',
            },
        });
        const response = await post(harness, '/api/market/rate-ladder', {
            currency: 'USD',
            as_of: '2026-11-13 23:59:59Z',
        });

        expect(response.statusCode).toBe(422);
        expect(response.json()).toMatchObject({
            message: 'The party holds no USD ladder.',
        });
    });

    it('refuses a request that states no currency', async () => {
        const harness = buildTestServer(REPLIES);
        const response = await post(harness, '/api/market/rate-ladder', {
            as_of: '2026-11-13 23:59:59Z',
        });

        expect(response.statusCode).toBe(400);
        expect(harness.calls).toHaveLength(0);
    });
});
