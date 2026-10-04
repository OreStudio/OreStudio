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
 * PROTOTYPE. Throwaway. Delete with the branch.
 *
 * Every row here is a fixture. The shapes follow the replies the operations
 * send today, minus the fields those replies drop, so the screens show what a
 * deployment would really state:
 *
 * - the service samples come from telemetry.v1.services.list, which keeps the
 *   latest sample of each instance among the rows of the last five minutes;
 * - the grid summary is the compute.v1.telemetry.get_grid_stats reply, whose
 *   node summaries carry no failure counts;
 * - the bus samples are the nats_samples reply, whose counters run since the
 *   NATS server started;
 * - the log entries are telemetry.v1.logs.list rows; every one is source
 *   server, because nothing publishes a client line;
 * - the versions are the client build stamp, the server build string, and the
 *   database row that has no read at all.
 */

/** telemetry.v1.services.list: one row per instance that reported in the last five minutes. */
export interface PrototypeServiceSample {
    readonly serviceName: string;
    readonly instanceId: string;
    readonly version: string;
    readonly lastHeartbeatSeconds: number;
    readonly sampledAt: string;
}

export const serviceSamples: readonly PrototypeServiceSample[] = [
    { serviceName: 'ores.telemetry.service', instanceId: '2a9977f6', version: 'v0.0.25', lastHeartbeatSeconds: 2, sampledAt: '14:32:04' },
    { serviceName: 'ores.compute.service', instanceId: '6f2c19a1', version: 'v0.0.25', lastHeartbeatSeconds: 4, sampledAt: '14:32:02' },
    { serviceName: 'ores.iam.service', instanceId: '91b0f33d', version: 'v0.0.25', lastHeartbeatSeconds: 12, sampledAt: '14:31:54' },
    { serviceName: 'ores.compute.service', instanceId: 'c47e5508', version: 'v0.0.24', lastHeartbeatSeconds: 21, sampledAt: '14:31:45' },
    { serviceName: 'ores.reporting.service', instanceId: 'b8d41e70', version: 'v0.0.25', lastHeartbeatSeconds: 47, sampledAt: '14:31:19' },
    { serviceName: 'ores.nats.poller', instanceId: 'f0a112c9', version: 'v0.0.25', lastHeartbeatSeconds: 58, sampledAt: '14:31:08' },
];

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
        { hostId: '9e0f33aa', host: 'grid-01.example.com', tasksCompleted: 1284, tasksSinceLast: 12, avgTaskDurationMs: 42_000, inputBytesFetched: 1_288_490_188, outputBytesUploaded: 230_686_720, secondsSinceHeartbeat: 8 },
        { hostId: '41c87b02', host: 'grid-02.example.com', tasksCompleted: 903, tasksSinceLast: 4, avgTaskDurationMs: 38_000, inputBytesFetched: 838_860_800, outputBytesUploaded: 146_800_640, secondsSinceHeartbeat: 12 },
        { hostId: 'ad55e110', host: 'grid-05.example.com', tasksCompleted: 101, tasksSinceLast: 0, avgTaskDurationMs: undefined, inputBytesFetched: 41_943_040, outputBytesUploaded: 2_097_152, secondsSinceHeartbeat: 11_520 },
        { hostId: 'c30b9d47', host: undefined, tasksCompleted: 57, tasksSinceLast: 2, avgTaskDurationMs: 51_000, inputBytesFetched: 18_874_368, outputBytesUploaded: 3_145_728, secondsSinceHeartbeat: 31 },
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
    { sampledAt: '14:01:00', inMsgs: 1_228_110, outMsgs: 3_392_011, inBytes: 208_666_624, outBytes: 1_135_515_648, connections: 21, memBytes: 84_934_656, slowConsumers: 0 },
    { sampledAt: '14:11:00', inMsgs: 1_234_002, outMsgs: 3_398_144, inBytes: 209_715_200, outBytes: 1_140_228_096, connections: 22, memBytes: 85_983_232, slowConsumers: 0 },
    { sampledAt: '14:21:00', inMsgs: 1_239_884, outMsgs: 3_404_447, inBytes: 210_763_776, outBytes: 1_145_290_752, connections: 22, memBytes: 85_983_232, slowConsumers: 0 },
    { sampledAt: '14:31:45', inMsgs: 1_240_512, outMsgs: 3_410_882, inBytes: 220_200_960, outBytes: 1_181_167_616, connections: 23, memBytes: 88_080_384, slowConsumers: 0 },
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
    { id: 2431, time: '14:31:02.114', level: 'ERROR', source: 'server', sourceName: 'ores.compute.service', component: 'ores.compute.poller', message: 'fetch failed, retrying', tag: 'compute.fetch', sessionId: undefined },
    { id: 2430, time: '14:30:58.902', level: 'WARN', source: 'server', sourceName: 'ores.compute.service', component: 'ores.compute.poller', message: 'retrying fetch after timeout', tag: 'compute.fetch', sessionId: undefined },
    { id: 2429, time: '14:30:44.201', level: 'INFO', source: 'server', sourceName: 'ores.iam.service', component: 'ores.iam.auth', message: 'session opened', tag: 'iam.session', sessionId: 'e8f1a7c2' },
    { id: 2428, time: '14:30:41.550', level: 'DEBUG', source: 'server', sourceName: 'ores.telemetry.service', component: 'ores.telemetry.ingest', message: 'stored 18 service samples', tag: 'telemetry.ingest', sessionId: undefined },
    { id: 2427, time: '14:30:30.008', level: 'INFO', source: 'server', sourceName: 'ores.nats.poller', component: 'ores.nats.monitor', message: 'sampled the NATS server', tag: 'nats.sample', sessionId: undefined },
    { id: 2426, time: '14:30:12.731', level: 'WARN', source: 'server', sourceName: 'ores.iam.service', component: 'ores.iam.auth', message: 'sign-in rejected: unknown account', tag: 'iam.signin', sessionId: undefined },
    { id: 2425, time: '14:29:59.440', level: 'INFO', source: 'server', sourceName: 'ores.reporting.service', component: 'ores.reporting.queue', message: 'batch queued for the grid', tag: 'reporting.queue', sessionId: 'c02d55b9' },
    { id: 2424, time: '14:29:47.020', level: 'ERROR', source: 'server', sourceName: 'ores.workflow.service', component: 'ores.workflow.engine', message: 'step timed out, instance paused', tag: 'workflow.step', sessionId: undefined },
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
    readonly fingerprint: string | undefined;
    readonly environment: string | undefined;
    readonly commit: string | undefined;
    readonly created: string | undefined;
}

/** ores_database_info_tbl has no read operation, so the panel has no values. */
export const databaseState: PrototypeDatabaseState = {
    fingerprint: undefined,
    environment: undefined,
    commit: undefined,
    created: undefined,
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
