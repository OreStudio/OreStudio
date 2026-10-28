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
import { ServerError, type OresClient } from '@ores/wire-protocol';
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
    readonly calls: { subject: string; body: unknown }[];
    readonly publish: (subject: string, message: unknown) => void;
} {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const calls: { subject: string; body: unknown }[] = [];
    const listeners = new Map<string, (message: unknown) => void>();
    const client = {
        subscribeToMessages(
            subject: string,
            schema: { parse: (value: unknown) => unknown },
            onMessage: (message: unknown) => void,
        ): () => void {
            listeners.set(subject, (message) => onMessage(schema.parse(message)));
            return () => listeners.delete(subject);
        },
        async callAuthenticated(
            subject: string,
            body: unknown,
            schema: { parse: (value: unknown) => unknown },
        ): Promise<unknown> {
            calls.push({ subject, body });
            if (!(subject in replies)) {
                throw new Error(`No canned reply for ${subject}`);
            }
            const canned = replies[subject];
            if (canned instanceof Error) {
                throw canned;
            }
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
        publish: (subject, message) => listeners.get(subject)?.(message),
    };
}

function send(
    server: ReturnType<typeof buildServer>,
    sessionId: string | undefined,
    method: 'GET' | 'POST' | 'PUT',
    url: string,
    payload?: unknown,
): ReturnType<ReturnType<typeof buildServer>['inject']> {
    return server.inject({
        method,
        url,
        ...(sessionId === undefined ? {} : { cookies: { [SESSION_COOKIE]: sessionId } }),
        ...(payload === undefined ? {} : { payload: payload as Record<string, unknown> }),
    });
}

const GET_SUBJECT = 'synthetic.v1.ops.get_market_regime';
const PIN_SUBJECT = 'synthetic.v1.ops.pin_market_regime';
const TRACE_SUBJECT = 'synthetic.v1.ops.get_market_trace';
const CONFIGURE_SUBJECT = 'synthetic.v1.ops.configure_market';

/** What the service answers with when nothing has moved the heat. */
const NO_HEAT = { heat: 0, jump: 0.5, decay: 20, samples: [] };

const ACTIVE = {
    success: true,
    message: '',
    state: 'active',
    multiplier: 2,
    pinned: false,
    ...NO_HEAT,
};
const CALM = {
    success: true,
    message: '',
    state: 'calm',
    multiplier: 0.5,
    pinned: false,
    ...NO_HEAT,
};

describe('the market regime', () => {
    it('reads the state in force, the multiplier and whether it is pinned', async () => {
        const { server, sessionId } = buildTestServer({ [GET_SUBJECT]: ACTIVE });

        const response = await send(server, sessionId, 'GET', '/api/synthetic/market-regime');

        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({
            state: 'active',
            multiplier: 2,
            pinned: false,
            heat: 0,
            jump: 0.5,
            decay: 20,
            samples: [],
        });
    });

    it('reports a service that will not answer as unavailable rather than as an empty state', async () => {
        const { server, sessionId } = buildTestServer({
            [GET_SUBJECT]: { success: false, message: 'the regime is not running' },
        });

        const response = await send(server, sessionId, 'GET', '/api/synthetic/market-regime');

        expect(response.statusCode).toBe(502);
        expect(response.json().message).toBe('the regime is not running');
    });

    it('reports a refused pin as forbidden rather than as the service being down', async () => {
        const { server, sessionId } = buildTestServer({
            [PIN_SUBJECT]: new ServerError('forbidden'),
        });

        const response = await send(server, sessionId, 'POST', '/api/synthetic/market-regime', {
            mode: 'active',
        });

        expect(response.statusCode).toBe(403);
    });

    it('needs a session, because the read is attributable', async () => {
        const { server } = buildTestServer({ [GET_SUBJECT]: ACTIVE });

        const response = await send(server, undefined, 'GET', '/api/synthetic/market-regime');

        expect(response.statusCode).toBe(401);
    });

    it('carries the heat, the two numbers that move it, and its readings', async () => {
        const { server, sessionId } = buildTestServer({
            [GET_SUBJECT]: {
                ...ACTIVE,
                heat: 0.4,
                jump: 0.5,
                decay: 20,
                samples: [{ at: '2026-10-26T12:00:00Z', heat: 0.4 }],
            },
        });

        const response = await send(server, sessionId, 'GET', '/api/synthetic/market-regime');

        expect(response.json()).toMatchObject({
            state: 'active',
            multiplier: 2,
            pinned: false,
            heat: 0.4,
            jump: 0.5,
            decay: 20,
            samples: [{ at: '2026-10-26T12:00:00Z', heat: 0.4 }],
        });
    });

    it('pins a state, and reports the state the service says is in force', async () => {
        const { server, sessionId, calls } = buildTestServer({
            [PIN_SUBJECT]: { ...CALM, pinned: true },
        });

        const response = await send(server, sessionId, 'POST', '/api/synthetic/market-regime', {
            mode: 'calm',
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({
            state: 'calm',
            multiplier: 0.5,
            pinned: true,
            heat: 0,
            jump: 0.5,
            decay: 20,
            samples: [],
        });
        expect(calls).toEqual([{ subject: PIN_SUBJECT, body: { mode: 'calm' } }]);
    });

    it('hands the state back to the chain when the mode is auto', async () => {
        const { server, sessionId, calls } = buildTestServer({
            [PIN_SUBJECT]: { ...ACTIVE, pinned: false },
        });

        const response = await send(server, sessionId, 'POST', '/api/synthetic/market-regime', {
            mode: 'auto',
        });

        expect(response.statusCode).toBe(200);
        expect(response.json().pinned).toBe(false);
        expect(calls).toEqual([{ subject: PIN_SUBJECT, body: { mode: 'auto' } }]);
    });

    it('refuses a mode the service does not know, before the service hears of it', async () => {
        const { server, sessionId, calls } = buildTestServer({});

        const response = await send(server, sessionId, 'POST', '/api/synthetic/market-regime', {
            mode: 'panic',
        });

        expect(response.statusCode).toBe(400);
        expect(calls).toEqual([]);
    });

    it('lists a row for every running feed, with the times its ticks arrived in the window', async () => {
        const { server, sessionId, publish } = buildTestServer({
            'synthetic.v1.feed_configs.list': {
                success: true,
                running_source_names: ['live.eurusd', 'live.usdsofr'],
            },
            'synthetic.v1.ops.list_folder_feeds': {
                success: true,
                total: 2,
                feeds: [
                    {
                        label: 'EUR/USD',
                        source_name: 'live.eurusd',
                        asset_class_code: 'fx',
                        ticks_per_hour: 3600,
                    },
                    {
                        label: 'USD SOFR',
                        source_name: 'live.usdsofr',
                        asset_class_code: 'interest_rates',
                        ticks_per_hour: 60,
                    },
                ],
            },
        });
        const now = Date.now();
        const at = (secondsAgo: number): string => new Date(now - secondsAgo * 1000).toISOString();

        await send(server, sessionId, 'GET', '/api/synthetic/feed-raster?seconds=60');
        publish('synthetic.v1.ops.feed_tick.live.eurusd', {
            value: '1.1',
            observation_time: at(90),
        });
        publish('synthetic.v1.ops.feed_tick.live.eurusd', {
            value: '1.2',
            observation_time: at(20),
        });
        publish('synthetic.v1.ops.feed_tick.live.eurusd', {
            value: '1.3',
            observation_time: at(5),
        });
        publish('synthetic.v1.ops.feed_tick.live.usdsofr', {
            value: '4.1',
            observation_time: at(10),
        });

        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/synthetic/feed-raster?seconds=60',
        );

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            seconds: 60,
            feeds: [
                {
                    id: '',
                    kind: '',
                    sourceName: 'live.eurusd',
                    label: 'EUR/USD',
                    assetClass: 'fx',
                    running: true,
                    ticks: [at(20), at(5)],
                    expectedPerSecond: 1,
                },
                {
                    id: '',
                    kind: '',
                    sourceName: 'live.usdsofr',
                    label: 'USD SOFR',
                    assetClass: 'interest_rates',
                    running: true,
                    ticks: [at(10)],
                    expectedPerSecond: 1 / 60,
                },
            ],
        });
    });

    it('stops listening to a feed the run no longer carries', async () => {
        const replies: Record<string, unknown> = {
            'synthetic.v1.feed_configs.list': {
                success: true,
                running_source_names: ['live.eurusd'],
            },
            'synthetic.v1.ops.list_folder_feeds': {
                success: true,
                total: 1,
                feeds: [
                    {
                        label: 'EUR/USD',
                        source_name: 'live.eurusd',
                        asset_class_code: 'fx',
                        ticks_per_hour: 3600,
                    },
                ],
            },
        };
        const { server, sessionId, publish } = buildTestServer(replies);
        await send(server, sessionId, 'GET', '/api/synthetic/feed-raster');
        publish('synthetic.v1.ops.feed_tick.live.eurusd', {
            value: '1.1',
            observation_time: new Date().toISOString(),
        });

        replies['synthetic.v1.feed_configs.list'] = {
            success: true,
            running_source_names: ['live.gbpusd'],
        };
        replies['synthetic.v1.ops.list_folder_feeds'] = {
            success: true,
            total: 2,
            feeds: [
                {
                    label: 'EUR/USD',
                    source_name: 'live.eurusd',
                    asset_class_code: 'fx',
                    ticks_per_hour: 3600,
                },
                {
                    label: 'GBP/USD',
                    source_name: 'live.gbpusd',
                    asset_class_code: 'fx',
                    ticks_per_hour: 3600,
                },
            ],
        };
        await send(server, sessionId, 'GET', '/api/synthetic/feed-raster');
        // The listener for the stopped feed is gone, so a publish for it is not
        // heard; the new feed's listener answers on the next read.
        publish('synthetic.v1.ops.feed_tick.live.eurusd', {
            value: '1.2',
            observation_time: new Date().toISOString(),
        });
        const response = await send(server, sessionId, 'GET', '/api/synthetic/feed-raster');

        expect(response.json()).toEqual({
            seconds: 120,
            feeds: [
                // The feed that runs comes first, and the one that stopped keeps
                // its row: a picture that dropped it could not show it is silent.
                {
                    id: '',
                    kind: '',
                    sourceName: 'live.gbpusd',
                    label: 'GBP/USD',
                    assetClass: 'fx',
                    running: true,
                    ticks: [],
                    expectedPerSecond: 1,
                },
                {
                    id: '',
                    kind: '',
                    sourceName: 'live.eurusd',
                    label: 'EUR/USD',
                    assetClass: 'fx',
                    running: false,
                    ticks: [],
                    expectedPerSecond: 1,
                },
            ],
        });
    });

    it('keeps the row of a feed that is not running, and marks it stopped', async () => {
        const replies: Record<string, unknown> = {
            'synthetic.v1.feed_configs.list': {
                success: true,
                running_source_names: ['live.eurusd'],
            },
            'synthetic.v1.ops.list_folder_feeds': {
                success: true,
                total: 1,
                feeds: [
                    {
                        label: 'EUR/USD',
                        source_name: 'live.eurusd',
                        asset_class_code: 'fx',
                        ticks_per_hour: 3600,
                    },
                ],
            },
        };
        const { server, sessionId } = buildTestServer(replies);

        replies['synthetic.v1.feed_configs.list'] = { success: true, running_source_names: [] };
        const response = await send(server, sessionId, 'GET', '/api/synthetic/feed-raster');

        expect(response.json()).toEqual({
            seconds: 120,
            feeds: [
                {
                    id: '',
                    kind: '',
                    sourceName: 'live.eurusd',
                    label: 'EUR/USD',
                    assetClass: 'fx',
                    running: false,
                    ticks: [],
                    expectedPerSecond: 1,
                },
            ],
        });
    });
});

describe('the market settings', () => {
    const SETTINGS = {
        wakeRate: 0.002,
        calmRate: 0.004,
        jump: 0.5,
        decay: 20,
        speed: 2880,
        tailLoading: 1.5,
    };

    it('sends the five numbers and reports the regime the service holds after', async () => {
        const { server, sessionId, calls } = buildTestServer({
            [CONFIGURE_SUBJECT]: {
                ...CALM,
                speed: 2880,
                wake_rate: 0.002,
                calm_rate: 0.004,
                tail_loading: 1.5,
            },
        });

        const response = await send(
            server,
            sessionId,
            'PUT',
            '/api/synthetic/market-regime/settings',
            SETTINGS,
        );

        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({
            speed: 2880,
            wakeRate: 0.002,
            calmRate: 0.004,
            tailLoading: 1.5,
        });
        expect(calls).toEqual([
            {
                subject: CONFIGURE_SUBJECT,
                body: {
                    wake_rate: 0.002,
                    calm_rate: 0.004,
                    jump: 0.5,
                    decay: 20,
                    speed: 2880,
                    tail_loading: 1.5,
                },
            },
        ]);
    });

    it('reports numbers the service refuses as a bad request, in the service’s words', async () => {
        const message = "operational_clock: the heat's loop gain, jump / decay, is 1.25";
        const { server, sessionId } = buildTestServer({
            [CONFIGURE_SUBJECT]: { success: false, message },
        });

        const response = await send(
            server,
            sessionId,
            'PUT',
            '/api/synthetic/market-regime/settings',
            { ...SETTINGS, jump: 25 },
        );

        expect(response.statusCode).toBe(400);
        expect(response.json().message).toBe(message);
    });

    it('refuses settings without a tail loading, so an old client cannot reset it', async () => {
        const { server, sessionId, calls } = buildTestServer({});
        const { tailLoading: _left, ...older } = SETTINGS;

        const response = await send(
            server,
            sessionId,
            'PUT',
            '/api/synthetic/market-regime/settings',
            older,
        );

        expect(response.statusCode).toBe(400);
        expect(calls).toEqual([]);
    });

    it('refuses a speed that is not positive before the service hears of it', async () => {
        const { server, sessionId, calls } = buildTestServer({});

        const response = await send(
            server,
            sessionId,
            'PUT',
            '/api/synthetic/market-regime/settings',
            { ...SETTINGS, speed: 0 },
        );

        expect(response.statusCode).toBe(400);
        expect(calls).toEqual([]);
    });
});

describe('the market trace', () => {
    const TRACE = {
        success: true,
        message: '',
        run_seed: 42,
        ticks_per_second: 31.5,
        heap_size: 99,
        lateness_seconds: 0.0004,
        max_lateness_seconds: 0.002,
        series: 99,
        events: [
            {
                kind: 'transition',
                source_name: '',
                at: '2026-10-27T18:00:00Z',
                tau: 30.6,
                operational_gap: 0,
                wall_gap: 0,
                heat: 1.2,
                state: 'active',
            },
            {
                kind: 'tick',
                source_name: 'synthetic.demo.eurusd',
                at: '2026-10-27T18:00:01Z',
                tau: 32.6,
                operational_gap: 1,
                wall_gap: 0.49,
                heat: 1.7,
                state: 'active',
            },
        ],
    };

    it('serves the decisions and the loop numbers the service reports', async () => {
        const { server, sessionId } = buildTestServer({ [TRACE_SUBJECT]: TRACE });

        const response = await send(server, sessionId, 'GET', '/api/synthetic/market-trace');

        expect(response.statusCode).toBe(200);
        const body = response.json();
        expect(body).toMatchObject({
            runSeed: 42,
            ticksPerSecond: 31.5,
            heapSize: 99,
            series: 99,
        });
        expect(body.events).toHaveLength(2);
        expect(body.events[1]).toEqual({
            kind: 'tick',
            sourceName: 'synthetic.demo.eurusd',
            at: '2026-10-27T18:00:01Z',
            tau: 32.6,
            operationalGap: 1,
            wallGap: 0.49,
            compensatedGap: 0,
            heat: 1.7,
            multiplier: 1,
            pace: 1,
            state: 'active',
        });
        expect(body.feeds).toEqual([]);
    });

    it('asks the service for one feed and a window', async () => {
        const { server, sessionId, calls } = buildTestServer({ [TRACE_SUBJECT]: TRACE });

        await send(
            server,
            sessionId,
            'GET',
            '/api/synthetic/market-trace?source=synthetic.demo.eurusd&seconds=30&limit=100',
        );

        expect(calls).toEqual([
            {
                subject: TRACE_SUBJECT,
                body: { source_name: 'synthetic.demo.eurusd', seconds: 30, limit: 100 },
            },
        ]);
    });

    it('refuses a window it would not read', async () => {
        const { server, sessionId } = buildTestServer({ [TRACE_SUBJECT]: TRACE });

        const response = await send(
            server,
            sessionId,
            'GET',
            '/api/synthetic/market-trace?seconds=100000',
        );

        expect(response.statusCode).toBe(400);
    });

    it('carries the pace, tau, the enrolled rate, the loop gain and the seed on the regime read', async () => {
        const { server, sessionId } = buildTestServer({
            [GET_SUBJECT]: {
                ...ACTIVE,
                pace: 2.06,
                tau: 128.6,
                population_rate: 31.8,
                loop_gain: 0.025,
                run_seed: 42,
            },
        });

        const response = await send(server, sessionId, 'GET', '/api/synthetic/market-regime');

        expect(response.json()).toMatchObject({
            pace: 2.06,
            tau: 128.6,
            populationRate: 31.8,
            loopGain: 0.025,
            runSeed: 42,
        });
    });
});

describe('the market trace feeds', () => {
    it("carries each feed's law, rates, phase and excess, and each tick's pace parts", async () => {
        const { server, sessionId } = buildTestServer({
            ['synthetic.v1.ops.get_market_trace']: {
                success: true,
                message: '',
                run_seed: 42,
                ticks_per_second: 1,
                heap_size: 1,
                lateness_seconds: 0,
                max_lateness_seconds: 0,
                series: 1,
                events: [
                    {
                        kind: 'tick',
                        source_name: 'synthetic.demo.eurusd',
                        at: '2026-10-27T18:00:01Z',
                        tau: 3,
                        operational_gap: 1,
                        wall_gap: 0.5,
                        compensated_gap: 1,
                        heat: 0.5,
                        multiplier: 2,
                        pace: 2.02,
                        state: 'active',
                    },
                ],
                series_states: [
                    {
                        source_name: 'synthetic.demo.eurusd',
                        mode: 'periodic',
                        rate: 1,
                        sensitivity: 1,
                        effective_rate: 2.02,
                        phase: 0.37,
                        excess: 0,
                    },
                ],
            },
        });

        const body = (await send(server, sessionId, 'GET', '/api/synthetic/market-trace')).json();

        expect(body.events[0]).toMatchObject({ compensatedGap: 1, multiplier: 2, pace: 2.02 });
        expect(body.feeds).toEqual([
            {
                sourceName: 'synthetic.demo.eurusd',
                mode: 'periodic',
                rate: 1,
                sensitivity: 1,
                effectiveRate: 2.02,
                phase: 0.37,
                excess: 0,
                branchingRatio: 0,
                decay: 0,
            },
        ]);
    });
});
