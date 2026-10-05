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

/*
 * PROTOTYPE. Kept on main as the design record; nothing outside the prototype
 * routes imports it.
 *
 * Every row here is a fixture. The shapes follow the replies the operations
 * send today, minus the fields those replies drop, so the screens show what a
 * deployment would really state:
 *
 * - the service instances join the registry (projects/modeling/
 *   service_registry.org, which states the replicas each service expects) with
 *   telemetry.v1.services.list, which keeps the latest sample of each instance
 *   among the rows of the last five minutes; the interval is the instance id,
 *   a UUID the heartbeat publisher generates at startup;
 * - the grid summary is the compute.v1.telemetry.get_grid_stats reply, whose
 *   node summaries carry no failure counts;
 * - the bus samples are the nats_samples reply, whose counters run since the
 *   NATS server started;
 * - the log entries are telemetry.v1.logs.list rows; every one is source
 *   server, because nothing publishes a client line;
 * - the versions are the client build stamp, the server build string, and the
 *   database row, which the login answer must learn to carry.
 */

/*
 * The state of one expected instance, as the services screen needs it.
 *
 * The state comes from the installation's own service manager, the one
 * `compass services status` reports with the same words: running, stopped,
 * failed, missing. No operation serves it today; the read of the samples can
 * say only who reported. The fixture carries the states the design needs.
 */
export interface PrototypeServiceInstance {
    readonly serviceName: string;
    readonly instanceId: string | undefined;
    readonly state: 'running' | 'stopped' | 'missing';
    readonly version: string | undefined;
    readonly lastHeartbeatSeconds: number | undefined;
}

/**
 * The registry's replicas per service, and the instances that answer them.
 *
 * 20 services, 24 expected instances: the compute wrapper is the one service
 * the registry gives more than one replica. Two are not running: the reporting
 * service is stopped, and one wrapper replica is missing.
 *
 * The wrapper instances belong to the grid screen: a wrapper runs on a node
 * and reports for it, so the services screen leaves them out.
 */
export const serviceInstances: readonly PrototypeServiceInstance[] = [
    {
        serviceName: 'ores.analytics.service',
        instanceId: 'a3f81c02-6d44-4b0e-9c21-7f5e0d8a1b34',
        state: 'running',
        version: 'v0.0.25',
        lastHeartbeatSeconds: 6,
    },
    {
        serviceName: 'ores.assets.service',
        instanceId: '5d21b7e4-90c1-4a37-b8f4-2e6d9c05a7b1',
        state: 'running',
        version: 'v0.0.25',
        lastHeartbeatSeconds: 9,
    },
    {
        serviceName: 'ores.compute.service',
        instanceId: '6f2c19a1-3e7d-4c58-a1b2-0d4f8e6c9a23',
        state: 'running',
        version: 'v0.0.24',
        lastHeartbeatSeconds: 4,
    },
    {
        serviceName: 'ores.compute.wrapper',
        instanceId: '1a90fe12-5b3c-4d6e-8f70-91a2b3c4d5e6',
        state: 'running',
        version: 'v0.0.25',
        lastHeartbeatSeconds: 5,
    },
    {
        serviceName: 'ores.compute.wrapper',
        instanceId: '84b36cd1-7c2e-4a09-93b1-2c3d4e5f6a7b',
        state: 'running',
        version: 'v0.0.25',
        lastHeartbeatSeconds: 7,
    },
    {
        serviceName: 'ores.compute.wrapper',
        instanceId: 'f27a03be-9d4f-4b21-84c3-5e6f7a8b9c0d',
        state: 'running',
        version: 'v0.0.25',
        lastHeartbeatSeconds: 6,
    },
    {
        serviceName: 'ores.compute.wrapper',
        instanceId: '5c18e4a9-1e0f-4c32-95d5-8a9b0c1d2e3f',
        state: 'running',
        version: 'v0.0.25',
        lastHeartbeatSeconds: 8,
    },
    {
        serviceName: 'ores.compute.wrapper',
        instanceId: undefined,
        state: 'missing',
        version: undefined,
        lastHeartbeatSeconds: undefined,
    },
    {
        serviceName: 'ores.dq.service',
        instanceId: '77c0e5a3-2f10-4d43-a6e7-0b1c2d3e4f50',
        state: 'running',
        version: 'v0.0.25',
        lastHeartbeatSeconds: 11,
    },
    {
        serviceName: 'ores.http.server',
        instanceId: '0be2a911-3a21-4e54-b7f8-1c2d3e4f5061',
        state: 'running',
        version: 'v0.0.25',
        lastHeartbeatSeconds: 3,
    },
    {
        serviceName: 'ores.iam.service',
        instanceId: '91b0f33d-4b32-4f65-8809-2d3e4f506172',
        state: 'running',
        version: 'v0.0.25',
        lastHeartbeatSeconds: 12,
    },
    {
        serviceName: 'ores.marketdata.service',
        instanceId: 'e14d9077-5c43-4a76-991a-3e4f50617283',
        state: 'running',
        version: 'v0.0.25',
        lastHeartbeatSeconds: 8,
    },
    {
        serviceName: 'ores.ore.service',
        instanceId: '3c7b12d5-6d54-4b87-8a2b-4f5061728394',
        state: 'running',
        version: 'v0.0.25',
        lastHeartbeatSeconds: 14,
    },
    {
        serviceName: 'ores.refdata.service',
        instanceId: 'b6e0a4c8-7e65-4c98-9b3c-5061728394a5',
        state: 'running',
        version: 'v0.0.25',
        lastHeartbeatSeconds: 7,
    },
    {
        serviceName: 'ores.reporting.service',
        instanceId: 'c9a3f10b-8f76-4da9-8c4d-61728394a5b6',
        state: 'stopped',
        version: undefined,
        lastHeartbeatSeconds: undefined,
    },
    {
        serviceName: 'ores.scheduler.service',
        instanceId: '8f4a1139-9087-4eba-9d5e-728394a5b6c7',
        state: 'running',
        version: 'v0.0.25',
        lastHeartbeatSeconds: 10,
    },
    {
        serviceName: 'ores.storage.service',
        instanceId: '2d9710fe-a198-4fcb-8e6f-8394a5b6c7d8',
        state: 'running',
        version: 'v0.0.25',
        lastHeartbeatSeconds: 13,
    },
    {
        serviceName: 'ores.synthetic.service',
        instanceId: '51ba6cc3-b2a9-40dc-9f70-94a5b6c7d8e9',
        state: 'running',
        version: 'v0.0.25',
        lastHeartbeatSeconds: 16,
    },
    {
        serviceName: 'ores.telemetry.service',
        instanceId: '2a9977f6-c3ba-41ed-8071-a5b6c7d8e9f0',
        state: 'running',
        version: 'v0.0.25',
        lastHeartbeatSeconds: 2,
    },
    {
        serviceName: 'ores.trading.service',
        instanceId: 'd4c8b210-d4cb-42fe-9182-b6c7d8e9f0a1',
        state: 'running',
        version: 'v0.0.25',
        lastHeartbeatSeconds: 15,
    },
    {
        serviceName: 'ores.variability.service',
        instanceId: '9e07ab45-e5dc-430f-8293-c7d8e9f0a1b2',
        state: 'running',
        version: 'v0.0.25',
        lastHeartbeatSeconds: 6,
    },
    {
        serviceName: 'ores.web.service',
        instanceId: '4021d7aa-f6ed-4410-93a4-d8e9f0a1b2c3',
        state: 'running',
        version: 'v0.0.25',
        lastHeartbeatSeconds: 1,
    },
    {
        serviceName: 'ores.workflow.service',
        instanceId: '66d5e2f1-a7fe-4521-84b5-e9f0a1b2c3d4',
        state: 'running',
        version: 'v0.0.25',
        lastHeartbeatSeconds: 12,
    },
    {
        serviceName: 'ores.workspace.service',
        instanceId: 'c13f8b96-b80f-4632-95c6-f0a1b2c3d4e5',
        state: 'running',
        version: 'v0.0.25',
        lastHeartbeatSeconds: 9,
    },
];

/** The one service whose instances run on the grid's nodes. */
export const computeWrapperServiceName = 'ores.compute.wrapper';

/** compute.v1.telemetry.get_grid_stats: the summary and one row per known node. */
export interface PrototypeNodeSummary {
    readonly hostId: string;
    readonly host: string | undefined;
    readonly tasksCompleted: number;
    readonly tasksSinceLast: number;
    readonly avgTaskDurationMs: number | undefined;
    readonly inputBytesFetched: number;
    readonly outputBytesUploaded: number;
    readonly secondsSinceHeartbeat: number;
}

export interface PrototypeGridStats {
    readonly sampledAt: string;
    readonly totalHosts: number;
    readonly onlineHosts: number;
    readonly idleHosts: number;
    readonly resultsInactive: number;
    readonly resultsUnsent: number;
    readonly resultsInProgress: number;
    readonly resultsDone: number;
    readonly totalWorkunits: number;
    readonly totalBatches: number;
    readonly activeBatches: number;
    readonly outcomesSuccess: number;
    readonly outcomesClientError: number;
    readonly outcomesNoReply: number;
    readonly nodes: readonly PrototypeNodeSummary[];
}

export const gridStats: PrototypeGridStats = {
    sampledAt: '14:31:02',
    totalHosts: 6,
    onlineHosts: 5,
    idleHosts: 2,
    resultsInactive: 4,
    resultsUnsent: 9,
    resultsInProgress: 3,
    resultsDone: 112,
    totalWorkunits: 128,
    totalBatches: 9,
    activeBatches: 3,
    outcomesSuccess: 412,
    outcomesClientError: 3,
    outcomesNoReply: 1,
    nodes: [
        {
            hostId: '9e0f33aa',
            host: 'grid-01.example.com',
            tasksCompleted: 1284,
            tasksSinceLast: 12,
            avgTaskDurationMs: 42_000,
            inputBytesFetched: 1_288_490_188,
            outputBytesUploaded: 230_686_720,
            secondsSinceHeartbeat: 8,
        },
        {
            hostId: '41c87b02',
            host: 'grid-02.example.com',
            tasksCompleted: 903,
            tasksSinceLast: 4,
            avgTaskDurationMs: 38_000,
            inputBytesFetched: 838_860_800,
            outputBytesUploaded: 146_800_640,
            secondsSinceHeartbeat: 12,
        },
        {
            hostId: 'ad55e110',
            host: 'grid-05.example.com',
            tasksCompleted: 101,
            tasksSinceLast: 0,
            avgTaskDurationMs: undefined,
            inputBytesFetched: 41_943_040,
            outputBytesUploaded: 2_097_152,
            secondsSinceHeartbeat: 11_520,
        },
        {
            hostId: 'c30b9d47',
            host: undefined,
            tasksCompleted: 57,
            tasksSinceLast: 2,
            avgTaskDurationMs: 51_000,
            inputBytesFetched: 18_874_368,
            outputBytesUploaded: 3_145_728,
            secondsSinceHeartbeat: 31,
        },
    ],
};

/** telemetry.v1.nats.server-samples.list: the counters run since the server started. */
export interface PrototypeNatsServerSample {
    readonly sampledAt: string;
    readonly inMsgs: number;
    readonly outMsgs: number;
    readonly inBytes: number;
    readonly outBytes: number;
    readonly connections: number;
    readonly memBytes: number;
    readonly slowConsumers: number;
}

export const natsServerSamples: readonly PrototypeNatsServerSample[] = [
    {
        sampledAt: '14:01:00',
        inMsgs: 1_228_110,
        outMsgs: 3_392_011,
        inBytes: 208_666_624,
        outBytes: 1_135_515_648,
        connections: 21,
        memBytes: 84_934_656,
        slowConsumers: 0,
    },
    {
        sampledAt: '14:11:00',
        inMsgs: 1_234_002,
        outMsgs: 3_398_144,
        inBytes: 209_715_200,
        outBytes: 1_140_228_096,
        connections: 22,
        memBytes: 85_983_232,
        slowConsumers: 0,
    },
    {
        sampledAt: '14:21:00',
        inMsgs: 1_239_884,
        outMsgs: 3_404_447,
        inBytes: 210_763_776,
        outBytes: 1_145_290_752,
        connections: 22,
        memBytes: 85_983_232,
        slowConsumers: 0,
    },
    {
        sampledAt: '14:31:45',
        inMsgs: 1_240_512,
        outMsgs: 3_410_882,
        inBytes: 220_200_960,
        outBytes: 1_181_167_616,
        connections: 23,
        memBytes: 88_080_384,
        slowConsumers: 0,
    },
];

/** telemetry.v1.nats.stream-samples.list: one row per stream per sample. */
export interface PrototypeNatsStreamSample {
    readonly streamName: string;
    readonly messages: number;
    readonly bytes: number;
    readonly consumerCount: number;
}

export const natsStreamSamples: readonly PrototypeNatsStreamSample[] = [
    { streamName: 'ORES_TRADES', messages: 12_004, bytes: 88_080_384, consumerCount: 2 },
    { streamName: 'ORES_RESULTS', messages: 3_201, bytes: 20_971_520, consumerCount: 1 },
];

/** telemetry.v1.logs.list: every stored entry is source server today. */
export interface PrototypeLogEntry {
    readonly id: number;
    readonly time: string;
    readonly level: 'ERROR' | 'WARN' | 'INFO' | 'DEBUG';
    readonly source: 'server' | 'client';
    readonly sourceName: string;
    readonly component: string;
    readonly message: string;
    readonly tag: string;
    readonly sessionId: string | undefined;
}

export const logEntries: readonly PrototypeLogEntry[] = [
    {
        id: 2431,
        time: '14:31:02.114',
        level: 'ERROR',
        source: 'server',
        sourceName: 'ores.compute.service',
        component: 'ores.compute.poller',
        message: 'fetch failed, retrying',
        tag: 'compute.fetch',
        sessionId: undefined,
    },
    {
        id: 2430,
        time: '14:30:58.902',
        level: 'WARN',
        source: 'server',
        sourceName: 'ores.compute.service',
        component: 'ores.compute.poller',
        message: 'retrying fetch after timeout',
        tag: 'compute.fetch',
        sessionId: undefined,
    },
    {
        id: 2429,
        time: '14:30:44.201',
        level: 'INFO',
        source: 'server',
        sourceName: 'ores.iam.service',
        component: 'ores.iam.auth',
        message: 'session opened',
        tag: 'iam.session',
        sessionId: 'e8f1a7c2',
    },
    {
        id: 2428,
        time: '14:30:41.550',
        level: 'DEBUG',
        source: 'server',
        sourceName: 'ores.telemetry.service',
        component: 'ores.telemetry.ingest',
        message: 'stored 18 service samples',
        tag: 'telemetry.ingest',
        sessionId: undefined,
    },
    {
        id: 2427,
        time: '14:30:30.008',
        level: 'INFO',
        source: 'server',
        sourceName: 'ores.telemetry.service',
        component: 'ores.telemetry.service.app.nats_poller',
        message: 'sampled the NATS server',
        tag: 'nats.sample',
        sessionId: undefined,
    },
    {
        id: 2426,
        time: '14:30:12.731',
        level: 'WARN',
        source: 'server',
        sourceName: 'ores.iam.service',
        component: 'ores.iam.auth',
        message: 'sign-in rejected: unknown account',
        tag: 'iam.signin',
        sessionId: undefined,
    },
    {
        id: 2425,
        time: '14:29:59.440',
        level: 'INFO',
        source: 'server',
        sourceName: 'ores.reporting.service',
        component: 'ores.reporting.queue',
        message: 'batch queued for the grid',
        tag: 'reporting.queue',
        sessionId: 'c02d55b9',
    },
    {
        id: 2424,
        time: '14:29:47.020',
        level: 'ERROR',
        source: 'server',
        sourceName: 'ores.workflow.service',
        component: 'ores.workflow.engine',
        message: 'step timed out, instance paused',
        tag: 'workflow.step',
        sessionId: undefined,
    },
];

/** The client build stamp is written into the bundle at build time. */
export interface PrototypeClientVersion {
    readonly version: string;
    readonly commit: string;
    readonly dirty: boolean;
}

export const clientVersion: PrototypeClientVersion = {
    version: 'v0.0.25',
    commit: 'a1e507d',
    dirty: false,
};

/** The server build string is the login answer's version field. */
export interface PrototypeServerVersion {
    readonly version: string;
    readonly address: string;
}

export const serverVersion: PrototypeServerVersion = {
    version: 'v0.0.25 [x64-linux] (local a1e507d, 2026-10-04)',
    address: 'https://ores.example.com',
};

export interface PrototypeDatabaseState {
    readonly fingerprint: string;
    readonly environment: string;
    readonly commit: string;
    readonly created: string;
}

/**
 * The database row, as the login answer should carry it.
 *
 * The values are the row `compass db recreate` stamps: the schema fingerprint
 * every service compares at its own startup, the build environment, the commit
 * the schema was cut from, and when it was written.
 */
export const databaseState: PrototypeDatabaseState = {
    fingerprint: '1109eccab21e8fe8',
    environment: 'development',
    commit: 'a1e507d',
    created: '2026-10-04 14:02',
};

export function asGiB(bytes: number): string {
    return `${(bytes / 1024 / 1024 / 1024).toFixed(2)} GiB`;
}

export function asMiB(bytes: number): string {
    return `${Math.round(bytes / 1024 / 1024)} MB`;
}

export function asMinutes(seconds: number): string {
    if (seconds < 60) {
        return `${String(seconds)} s`;
    }
    const minutes = Math.floor(seconds / 60);
    if (minutes < 60) {
        return `${String(minutes)} m ${String(seconds % 60)} s`;
    }
    return `${String(Math.floor(minutes / 60))} h ${String(minutes % 60)} m`;
}
