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
import {
    fromWireTimestamp,
    toWireTimestamp,
    type OresClient,
    type SessionMode,
} from '@ores/wire-protocol';
import type { Config } from './config.js';
import { busStreamNames, busWindow, logWindow, secondsSinceReport } from './operations.js';
import { resolveBroker } from './broker.js';
import { buildServer, sessionCookieName } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

/**
 * The operations routes: the services roster, and who may read it.
 *
 * What is checked is what the BFF decides: that it asks the session's client
 * once for the roster, that it marks each row's age from the deployment's
 * clock, and that a session which does not act on the deployment is refused
 * before any call goes out.
 */

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

const SYSTEM_TENANT = 'ffffffff-ffff-4fff-8fff-ffffffffffff';
const ACCOUNT = '11111111-1111-4111-8111-111111111111';
const INSTANCE = '91b0f33d-4b32-4f65-8809-2d3e4f506172';

/** One roster slot as the read answers it. */
function wireSlot(overrides: Record<string, unknown> = {}): Record<string, unknown> {
    return {
        service_name: 'ores.iam.service',
        display_name: 'IAM Service',
        description: '',
        service_account: null,
        slot: 1,
        state: 'running',
        instance_id: INSTANCE,
        host_id: null,
        version: 'v0.0.25',
        sampled_at: toWireTimestamp(new Date(Date.now() - 5_000)),
        ...overrides,
    };
}

const HOST = '3c7b12d5-6d54-4b87-8a2b-4f5061728394';
const UNKNOWN_HOST = 'ad55e110-0000-4000-8000-000000000099';

/** One host as the registry answers it. */
function wireHost(overrides: Record<string, unknown> = {}): Record<string, unknown> {
    return {
        version: 1,
        tenant_id: SYSTEM_TENANT,
        id: HOST,
        external_id: 'grid-01.example.com',
        display_name: 'Grid 01',
        location: 'ldn',
        cpu_count: 8,
        ram_mb: 32768,
        gpu_type: '',
        credit_total: 100,
        modified_by: 'sysadmin',
        performed_by: 'sysadmin',
        change_reason_code: 'new',
        change_commentary: '',
        recorded_at: '2026-10-04 14:31:02Z',
        ...overrides,
    };
}

/** The grid reply: the stored counters and the newest sample of every node. */
function wireGridStats(overrides: Record<string, unknown> = {}): Record<string, unknown> {
    return {
        success: true,
        message: '',
        total_hosts: 2,
        online_hosts: 1,
        idle_hosts: 0,
        total_workunits: 3,
        total_batches: 1,
        active_batches: 1,
        outcomes_success: 4,
        outcomes_client_error: 1,
        outcomes_no_reply: 0,
        sampled_at: '2026-10-04 14:31:02Z',
        node_summaries: [
            {
                host_id: HOST,
                tasks_completed: 1284,
                tasks_failed: 2,
                tasks_since_last: 12,
                avg_task_duration_ms: 42_000,
                max_task_duration_ms: 51_000,
                input_bytes_fetched: 1_288_490_188,
                output_bytes_uploaded: 230_686_720,
                seconds_since_hb: 8,
            },
            {
                host_id: UNKNOWN_HOST,
                tasks_completed: 57,
                tasks_failed: 0,
                tasks_since_last: 2,
                avg_task_duration_ms: 51_000,
                max_task_duration_ms: 60_000,
                input_bytes_fetched: 18_874_368,
                output_bytes_uploaded: 3_145_728,
                seconds_since_hb: 31,
            },
        ],
        ...overrides,
    };
}

function buildTestServer(
    mode: SessionMode,
    slots: readonly Record<string, unknown>[],
    grid: Record<string, unknown> = wireGridStats(),
    hosts: readonly Record<string, unknown>[] = [wireHost()],
    serverSamples: readonly Record<string, unknown>[] = [],
    streamSamples: readonly Record<string, unknown>[] = [],
    logEntries: readonly Record<string, unknown>[] = [],
) {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const calls: { subject: string; body: unknown }[] = [];
    const client = {
        async serviceRoster(): Promise<readonly Record<string, unknown>[]> {
            calls.push({ subject: 'telemetry.v1.ops.get_service_roster', body: {} });
            return slots;
        },
        async gridStats(): Promise<Record<string, unknown>> {
            calls.push({ subject: 'compute.v1.ops.get_grid_stats', body: {} });
            return grid;
        },
        async listHosts(): Promise<readonly Record<string, unknown>[]> {
            calls.push({ subject: 'compute.v1.hosts.list', body: {} });
            return hosts;
        },
        async natsServerSamples(
            input: Record<string, unknown>,
        ): Promise<readonly Record<string, unknown>[]> {
            calls.push({ subject: 'telemetry.v1.nats_server_samples.list', body: input });
            return serverSamples;
        },
        async natsStreamSamples(input: {
            readonly streamName: string;
        }): Promise<readonly Record<string, unknown>[]> {
            calls.push({ subject: 'telemetry.v1.nats_stream_samples.list', body: input });
            return streamSamples.filter((row) => row['stream_name'] === input.streamName);
        },
        async listLogs(input: { readonly offset?: number; readonly limit?: number }): Promise<{
            readonly entries: readonly Record<string, unknown>[];
            readonly totalCount: number;
        }> {
            calls.push({ subject: 'telemetry.v1.logs.list', body: input });
            const offset = input.offset ?? 0;
            const limit = input.limit ?? 100;
            return {
                entries: logEntries.slice(offset, offset + limit),
                totalCount: logEntries.length,
            };
        },
        async close(): Promise<void> {
            return undefined;
        },
    } as unknown as OresClient;
    const session = sessions.create({
        client,
        session: null,
        username: 'sysadmin',
        email: 'sysadmin@acme.example',
        accountId: ACCOUNT,
        tenantId: SYSTEM_TENANT,
        tenantName: 'System',
        mode,
        version: 'v0.0.25 (test)',
        availableParties: [
            {
                id: SYSTEM_TENANT,
                name: 'System',
                partyCategory: 'Operational',
                businessCenterCode: 'GBLO',
            },
        ],
        accessLifetimeSeconds: 1800,
        passwordResetRequired: false,
        sessionId: '77777777-7777-4777-8777-777777777777',
    });
    const server = buildServer({
        config,
        site: siteConfiguration(),
        sessions,
        createClient: () => ({ client, connect: async () => undefined }),
    });
    return { server, cookies: { [SESSION_COOKIE]: session.id }, calls };
}

describe('GET /api/operations/services', () => {
    it('answers the roster with each row dated from the deployment clock', async () => {
        const { server, cookies, calls } = buildTestServer('system-administration', [
            wireSlot(),
            wireSlot({
                service_name: 'ores.analytics.service',
                state: 'missing',
                instance_id: null,
                version: null,
                sampled_at: null,
            }),
        ]);

        const response = await server.inject({
            method: 'GET',
            url: '/api/operations/services',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        const rows = (response.json() as { rows: Record<string, unknown>[] }).rows;
        expect(rows.map((row) => row['service_name'])).toEqual([
            'ores.iam.service',
            'ores.analytics.service',
        ]);
        // The running row is about five seconds old; the missing slot never
        // reported, so it carries no age at all rather than a made-up one.
        expect(typeof rows[0]?.['age_seconds']).toBe('number');
        expect(rows[1]?.['age_seconds']).toBeNull();
        expect(rows[1]?.['sampled_at']).toBeNull();
        // One read, sent with the empty request the roster operation declares.
        expect(calls).toEqual([{ subject: 'telemetry.v1.ops.get_service_roster', body: {} }]);
    });

    it('refuses a query field the read does not take', async () => {
        const { server, cookies, calls } = buildTestServer('system-administration', [wireSlot()]);

        const response = await server.inject({
            method: 'GET',
            url: '/api/operations/services?scope=all',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(400);
        expect(response.json()).toMatchObject({ code: 'invalid-request' });
        // Refused at the boundary: nothing was read from the deployment.
        expect(calls).toEqual([]);
    });

    it('refuses a session that does not act on the deployment', async () => {
        const { server, cookies, calls } = buildTestServer('tenant-administration', [wireSlot()]);

        const response = await server.inject({
            method: 'GET',
            url: '/api/operations/services',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(403);
        expect(response.json()).toMatchObject({ code: 'forbidden' });
        // Refused at the boundary: nothing was read from the deployment.
        expect(calls).toEqual([]);
    });

    it('refuses a caller with no session at all', async () => {
        const { server } = buildTestServer('system-administration', [wireSlot()]);

        const response = await server.inject({ method: 'GET', url: '/api/operations/services' });
        await server.close();

        expect(response.statusCode).toBe(401);
    });
});

/** The grid view as the browser reads it. */
interface GridBody {
    readonly sampled_at: string | null;
    readonly total_hosts: number;
    readonly nodes: readonly {
        readonly host_id: string;
        readonly host: string | null;
        readonly instance_id: string | null;
        readonly state: string;
        readonly version: string | null;
        readonly tasks_failed: number;
        readonly max_task_duration_ms: number;
    }[];
}

describe('GET /api/operations/grid', () => {
    /* The registry's service name; the screen calls the agent a runner. */
    const RUNNER = 'ores.compute.wrapper';

    /** One runner slot as the roster answers it. */
    function runnerSlot(overrides: Record<string, unknown> = {}): Record<string, unknown> {
        return wireSlot({
            service_name: RUNNER,
            display_name: 'Compute runner',
            slot: 1,
            host_id: HOST,
            ...overrides,
        });
    }

    it('answers the summary, names the nodes, and folds each runner onto its node', async () => {
        const { server, cookies, calls } = buildTestServer('system-administration', [
            runnerSlot(),
            runnerSlot({
                slot: 2,
                instance_id: null,
                state: 'missing',
                host_id: null,
                version: null,
                sampled_at: null,
            }),
            // A slot of another service on the same host must not be folded in.
            wireSlot({ host_id: HOST, state: 'lost', version: 'v0.0.1' }),
        ]);

        const response = await server.inject({
            method: 'GET',
            url: '/api/operations/grid',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        const body = response.json() as GridBody;
        // The counters travel as stored, with the sample time beside them.
        expect(body.sampled_at).toBe('2026-10-04 14:31:02Z');
        expect(body.total_hosts).toBe(2);
        // The node the host registry knows is named by its display name; the one
        // it does not keeps its row with no name, so its id is never printed as
        // if it were one.
        expect(body.nodes.map((node) => node.host)).toEqual(['Grid 01', null]);
        expect(body.nodes[1]?.host_id).toBe(UNKNOWN_HOST);
        expect(body.nodes[0]?.tasks_failed).toBe(2);
        expect(body.nodes[0]?.max_task_duration_ms).toBe(51_000);
        // The runner is folded onto the node it reports for, so the node its
        // host id names carries that instance's state, version and id.
        expect(body.nodes.map((node) => node.state)).toEqual(['running', 'missing']);
        expect(body.nodes[0]?.instance_id).toBe(INSTANCE);
        expect(body.nodes[0]?.version).toBe('v0.0.25');
        // A node no runner reports for keeps its row with nothing to name.
        expect(body.nodes[1]?.instance_id).toBeNull();
        expect(body.nodes[1]?.version).toBeNull();
        expect(calls).toEqual([
            { subject: 'compute.v1.ops.get_grid_stats', body: {} },
            { subject: 'compute.v1.hosts.list', body: {} },
            { subject: 'telemetry.v1.ops.get_service_roster', body: {} },
        ]);
    });

    it('names a node by its registered display name, not by its external id', async () => {
        const { server, cookies } = buildTestServer(
            'system-administration',
            [runnerSlot()],
            wireGridStats(),
            [wireHost({ external_id: HOST, display_name: 'quiet-yak-4f2a' })],
        );

        const response = await server.inject({
            method: 'GET',
            url: '/api/operations/grid',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        const body = response.json() as GridBody;
        expect(body.nodes[0]?.host).toBe('quiet-yak-4f2a');
        expect(body.nodes[0]?.host).not.toBe(HOST);
        // The runner is placed on the node the host registry names.
        expect(body.nodes[0]?.state).toBe('running');
    });

    it('falls back to the external id when a host has no display name', async () => {
        const { server, cookies } = buildTestServer(
            'system-administration',
            [runnerSlot()],
            wireGridStats(),
            [wireHost({ display_name: null })],
        );

        const response = await server.inject({
            method: 'GET',
            url: '/api/operations/grid',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect((response.json() as GridBody).nodes[0]?.host).toBe('grid-01.example.com');
    });

    it('answers no sample time, rather than a zeroed one, when none is stored', async () => {
        const { server, cookies } = buildTestServer(
            'system-administration',
            [runnerSlot()],
            wireGridStats({ sampled_at: '' }),
        );

        const response = await server.inject({
            method: 'GET',
            url: '/api/operations/grid',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect((response.json() as GridBody).sampled_at).toBeNull();
    });

    it('refuses a query field the read does not take', async () => {
        const { server, cookies, calls } = buildTestServer('system-administration', [runnerSlot()]);

        const response = await server.inject({
            method: 'GET',
            url: '/api/operations/grid?tenant=acme',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(400);
        expect(response.json()).toMatchObject({ code: 'invalid-request' });
        // Refused at the boundary: nothing was read from the deployment.
        expect(calls).toEqual([]);
    });

    it('refuses a session that does not act on the deployment', async () => {
        const { server, cookies, calls } = buildTestServer('tenant-administration', [runnerSlot()]);

        const response = await server.inject({
            method: 'GET',
            url: '/api/operations/grid',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(403);
        expect(response.json()).toMatchObject({ code: 'forbidden' });
        // Refused at the boundary: nothing was read from the deployment.
        expect(calls).toEqual([]);
    });

    it('refuses a caller with no session at all', async () => {
        const { server } = buildTestServer('system-administration', [runnerSlot()]);

        const response = await server.inject({ method: 'GET', url: '/api/operations/grid' });
        await server.close();

        expect(response.statusCode).toBe(401);
    });
});

describe('the age a roster row is marked with', () => {
    const NOW = Date.UTC(2026, 9, 4, 14, 32, 5);

    it('is the seconds since the instance reported', () => {
        expect(secondsSinceReport('2026-10-04 14:32:00Z', NOW)).toBe(5);
        expect(secondsSinceReport('2026-10-04 14:27:05Z', NOW)).toBe(300);
    });

    it('is nothing when the slot never reported, or its time is unreadable', () => {
        expect(secondsSinceReport(null, NOW)).toBeNull();
        expect(secondsSinceReport('', NOW)).toBeNull();
        expect(secondsSinceReport('not a time', NOW)).toBeNull();
    });

    it('is never negative when the report time is ahead of the clock', () => {
        expect(secondsSinceReport('2026-10-04 14:32:10Z', NOW)).toBe(0);
    });
});

/** One NATS server sample as the read answers it. */
function wireServerSample(overrides: Record<string, unknown> = {}): Record<string, unknown> {
    return {
        sampled_at: '2026-10-04 14:31:45Z',
        in_msgs: 1_240_512,
        out_msgs: 3_410_882,
        in_bytes: 220_200_960,
        out_bytes: 1_181_167_616,
        connections: 23,
        mem_bytes: 88_080_384,
        slow_consumers: 0,
        ...overrides,
    };
}

/** One stream sample as the read answers it. */
function wireStreamSample(overrides: Record<string, unknown> = {}): Record<string, unknown> {
    return {
        sampled_at: '2026-10-04 14:31:45Z',
        stream_name: 'ores_dev_test_workflow',
        messages: 12_004,
        bytes: 88_080_384,
        consumer_count: 2,
        ...overrides,
    };
}

const BUS_SITE = siteConfiguration();
const BUS_PREFIX = resolveBroker(BUS_SITE.configuration, BUS_SITE.environment).subjectPrefix;
const BUS_STREAMS = busStreamNames(BUS_PREFIX);

describe('GET /api/operations/bus', () => {
    it('answers the newest sample as the vitals and one row per stream', async () => {
        const samples = [
            wireServerSample(),
            wireServerSample({
                sampled_at: '2026-10-04 14:01:00Z',
                in_msgs: 1_228_110,
                out_msgs: 3_392_011,
                in_bytes: 208_666_624,
                out_bytes: 1_135_515_648,
                connections: 21,
                mem_bytes: 84_934_656,
            }),
        ];
        const streams = [
            wireStreamSample({ stream_name: BUS_STREAMS[3], messages: 12_004, bytes: 88_080_384 }),
            // A stream the deployment does not declare could never be read, so
            // it must not reach the table.
            wireStreamSample({ stream_name: 'a stream this deployment does not declare' }),
        ];
        const { server, cookies, calls } = buildTestServer(
            'system-administration',
            [wireSlot()],
            wireGridStats(),
            [wireHost()],
            samples,
            streams,
        );

        const response = await server.inject({
            method: 'GET',
            url: '/api/operations/bus?range=1h',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        const body = response.json() as {
            readonly sampled_at: string | null;
            readonly samples: readonly { readonly sampled_at: string }[];
            readonly streams: readonly { readonly stream_name: string }[];
        };
        // The newest sample dates the reading; the samples travel whole and
        // newest first, exactly as the read orders them.
        expect(body.sampled_at).toBe('2026-10-04 14:31:45Z');
        expect(body.samples.map((sample) => sample.sampled_at)).toEqual([
            '2026-10-04 14:31:45Z',
            '2026-10-04 14:01:00Z',
        ]);
        // One row per declared stream, and only one for the stream that has a
        // sample in the range.
        expect(body.streams.map((row) => row.stream_name)).toEqual([BUS_STREAMS[3]]);

        // One server read, then one stream read per declared stream, each with
        // both bounds of the same window and no overlap at the boundary.
        expect(calls[0]?.subject).toBe('telemetry.v1.nats_server_samples.list');
        const serverCall = calls[0]?.body as {
            readonly startTime: string;
            readonly endTime: string;
        };
        const windowSeconds =
            (fromWireTimestamp(serverCall.endTime).getTime() -
                fromWireTimestamp(serverCall.startTime).getTime()) /
            1000;
        expect(windowSeconds).toBe(3600);
        expect(calls).toHaveLength(1 + BUS_STREAMS.length);
        for (const [index, name] of BUS_STREAMS.entries()) {
            expect(calls[index + 1]).toMatchObject({
                subject: 'telemetry.v1.nats_stream_samples.list',
                body: { streamName: name },
            });
        }
    });

    it('answers an empty range as an empty list rather than a failure', async () => {
        const { server, cookies } = buildTestServer('system-administration', [wireSlot()]);

        const response = await server.inject({
            method: 'GET',
            url: '/api/operations/bus?range=15m',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(response.json()).toMatchObject({ sampled_at: null, samples: [], streams: [] });
    });

    it('refuses a range the read does not know', async () => {
        const { server, cookies, calls } = buildTestServer('system-administration', [wireSlot()]);

        const response = await server.inject({
            method: 'GET',
            url: '/api/operations/bus?range=1y',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(400);
        expect(response.json()).toMatchObject({ code: 'invalid-request' });
        expect(calls).toEqual([]);
    });

    it('refuses a query field the read does not take', async () => {
        const { server, cookies, calls } = buildTestServer('system-administration', [wireSlot()]);

        const response = await server.inject({
            method: 'GET',
            url: '/api/operations/bus?range=1h&tenant=acme',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(400);
        expect(response.json()).toMatchObject({ code: 'invalid-request' });
        expect(calls).toEqual([]);
    });

    it('refuses a session that does not act on the deployment', async () => {
        const { server, cookies, calls } = buildTestServer('tenant-administration', [wireSlot()]);

        const response = await server.inject({
            method: 'GET',
            url: '/api/operations/bus?range=1h',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(403);
        expect(response.json()).toMatchObject({ code: 'forbidden' });
        expect(calls).toEqual([]);
    });

    it('refuses a caller with no session at all', async () => {
        const { server } = buildTestServer('system-administration', [wireSlot()]);

        const response = await server.inject({
            method: 'GET',
            url: '/api/operations/bus?range=1h',
        });
        await server.close();

        expect(response.statusCode).toBe(401);
    });
});

describe('the window a bus range names', () => {
    const NOW = Date.UTC(2026, 9, 4, 14, 32, 0);

    it('is open at its end, so adjacent windows tile without overlapping', () => {
        const hour = busWindow('1h', NOW);
        expect(hour.start).toBe(toWireTimestamp(new Date(NOW - 60 * 60 * 1000)));
        expect(hour.end).toBe(toWireTimestamp(new Date(NOW)));
    });

    it('is as long as the preset it names', () => {
        for (const [range, minutes] of [
            ['15m', 15],
            ['1h', 60],
            ['6h', 360],
        ] as const) {
            const window = busWindow(range, NOW);
            const seconds =
                (fromWireTimestamp(window.end).getTime() -
                    fromWireTimestamp(window.start).getTime()) /
                1000;
            expect(seconds).toBe(minutes * 60);
        }
    });
});

describe('the stream names a bus window reads', () => {
    it('are the deployment’s own, from the broker prefix', () => {
        expect(busStreamNames('ores.dev.local2')).toEqual([
            'ores_dev_local2_marketdata_ticks',
            'ores_dev_local2_synthetic_ticks',
            'ores_dev_local2_synthetic_sandbox_ticks',
            'ores_dev_local2_workflow',
            'ores_dev_local2_compute_assignments',
        ]);
    });
});

/** One log entry as the read answers it. Every stored entry is source server. */
function wireLogEntry(overrides: Record<string, unknown> = {}): Record<string, unknown> {
    return {
        id: '11111111-1111-4111-8111-111111111111',
        timestamp: '2026-10-04 14:31:02Z',
        source: 'server',
        source_name: 'ores.compute.service',
        session_id: null,
        account_id: null,
        level: 'error',
        component: 'ores.compute.poller',
        message: 'fetch failed, retrying',
        tag: 'compute.fetch',
        recorded_at: '2026-10-04 14:31:03Z',
        ...overrides,
    };
}

/** The log read's answer as the browser reads it. */
interface LogsBody {
    readonly read_at: string | null;
    readonly entries: readonly { readonly id: string; readonly level: string }[];
    readonly total: number;
    readonly limit: number;
    readonly offset: number;
}

describe('GET /api/operations/logs', () => {
    it('carries every filter to one read, and answers the page with its total', async () => {
        const { server, cookies, calls } = buildTestServer(
            'system-administration',
            [wireSlot()],
            wireGridStats(),
            [wireHost()],
            [],
            [],
            [
                wireLogEntry(),
                wireLogEntry({
                    id: '22222222-2222-4222-8222-222222222222',
                    level: 'warn',
                }),
            ],
        );

        const query = new URLSearchParams({
            range: '1h',
            level: 'error',
            source: 'server',
            component: 'ores.compute.poller',
            tag: 'compute.fetch',
            message: 'fetch',
        });
        const response = await server.inject({
            method: 'GET',
            url: `/api/operations/logs?${query.toString()}`,
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        const body = response.json() as LogsBody;
        // The count is the whole set the filter matches, not the page.
        expect(body.total).toBe(2);
        expect(body.entries).toHaveLength(2);
        expect(body.limit).toBe(100);
        expect(body.offset).toBe(0);
        // The read time is the deployment's, in the wire spelling the screens
        // label UTC.
        expect(body.read_at).toMatch(/^\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}Z$/);

        // One call, carrying every filter at once: the read combines them with
        // AND, so the BFF narrows nothing itself.
        expect(calls).toHaveLength(1);
        expect(calls[0]?.subject).toBe('telemetry.v1.logs.list');
        expect(calls[0]?.body).toMatchObject({
            level: 'error',
            source: 'server',
            component: 'ores.compute.poller',
            tag: 'compute.fetch',
            messageContains: 'fetch',
            offset: 0,
            limit: 100,
        });
        const call = calls[0]?.body as { readonly startTime: string; readonly endTime: string };
        const windowSeconds =
            (fromWireTimestamp(call.endTime).getTime() -
                fromWireTimestamp(call.startTime).getTime()) /
            1000;
        expect(windowSeconds).toBe(3600);
    });

    it('sends nothing for the filters the person left off', async () => {
        const { server, cookies, calls } = buildTestServer('system-administration', [wireSlot()]);

        const response = await server.inject({
            method: 'GET',
            url: '/api/operations/logs?range=15m',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        const body = response.json() as LogsBody;
        expect(body.offset).toBe(0);
        expect(body.limit).toBe(100);
        expect(calls[0]?.body).toMatchObject({
            level: null,
            source: null,
            component: null,
            tag: null,
            messageContains: null,
            offset: 0,
            limit: 100,
        });
        const call = calls[0]?.body as { readonly startTime: string; readonly endTime: string };
        const windowSeconds =
            (fromWireTimestamp(call.endTime).getTime() -
                fromWireTimestamp(call.startTime).getTime()) /
            1000;
        expect(windowSeconds).toBe(900);
    });

    it('pages by the limit and offset the screen asked for', async () => {
        const { server, cookies, calls } = buildTestServer(
            'system-administration',
            [wireSlot()],
            wireGridStats(),
            [wireHost()],
            [],
            [],
            [wireLogEntry()],
        );

        const response = await server.inject({
            method: 'GET',
            url: '/api/operations/logs?range=1h&offset=100&limit=50',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        const body = response.json() as LogsBody;
        expect(body.offset).toBe(100);
        expect(body.limit).toBe(50);
        expect(body.total).toBe(1);
        // The page past the end is empty, and the total still says what the
        // filter matches.
        expect(body.entries).toEqual([]);
        expect(calls[0]?.body).toMatchObject({ offset: 100, limit: 50 });
    });

    it('refuses the client source, which the store can never hold', async () => {
        const { server, cookies, calls } = buildTestServer('system-administration', [wireSlot()]);

        const response = await server.inject({
            method: 'GET',
            url: '/api/operations/logs?range=1h&source=client',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(400);
        expect(response.json()).toMatchObject({ code: 'invalid-request' });
        expect(calls).toEqual([]);
    });

    it('refuses a range the read does not know', async () => {
        const { server, cookies, calls } = buildTestServer('system-administration', [wireSlot()]);

        const response = await server.inject({
            method: 'GET',
            url: '/api/operations/logs?range=1y',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(400);
        expect(response.json()).toMatchObject({ code: 'invalid-request' });
        expect(calls).toEqual([]);
    });

    it('refuses a query field the read does not take', async () => {
        const { server, cookies, calls } = buildTestServer('system-administration', [wireSlot()]);

        const response = await server.inject({
            method: 'GET',
            url: '/api/operations/logs?range=1h&tenant=acme',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(400);
        expect(response.json()).toMatchObject({ code: 'invalid-request' });
        expect(calls).toEqual([]);
    });

    it('refuses a session that does not act on the deployment', async () => {
        const { server, cookies, calls } = buildTestServer('tenant-administration', [wireSlot()]);

        const response = await server.inject({
            method: 'GET',
            url: '/api/operations/logs?range=1h',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(403);
        expect(response.json()).toMatchObject({ code: 'forbidden' });
        expect(calls).toEqual([]);
    });

    it('refuses a caller with no session at all', async () => {
        const { server } = buildTestServer('system-administration', [wireSlot()]);

        const response = await server.inject({
            method: 'GET',
            url: '/api/operations/logs?range=1h',
        });
        await server.close();

        expect(response.statusCode).toBe(401);
    });
});

describe('the window a logs range names', () => {
    const NOW = Date.UTC(2026, 9, 4, 14, 32, 0);

    it('is open at its end, so adjacent windows tile without overlapping', () => {
        const day = logWindow('24h', NOW);
        expect(day.start).toBe(toWireTimestamp(new Date(NOW - 24 * 60 * 60 * 1000)));
        expect(day.end).toBe(toWireTimestamp(new Date(NOW)));
    });

    it('is as long as the preset it names', () => {
        for (const [range, minutes] of [
            ['15m', 15],
            ['1h', 60],
            ['6h', 360],
            ['24h', 1440],
        ] as const) {
            const window = logWindow(range, NOW);
            const seconds =
                (fromWireTimestamp(window.end).getTime() -
                    fromWireTimestamp(window.start).getTime()) /
                1000;
            expect(seconds).toBe(minutes * 60);
        }
    });
});
