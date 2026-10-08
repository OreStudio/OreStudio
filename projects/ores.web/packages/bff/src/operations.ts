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

import type { FastifyInstance, FastifyRequest } from 'fastify';
import { z } from 'zod';
import {
    busViewSchema,
    fromWireTimestamp,
    gridStatsRequestSchema,
    gridViewSchema,
    isWireTimestamp,
    serviceRosterRequestSchema,
    serviceRosterViewSchema,
    toWireTimestamp,
    type BusView,
    type WireTimestamp,
} from '@ores/wire-protocol';
import { invalidRequest, notPermitted } from './errors.js';
import type { LiveSession } from './sessions.js';

/**
 * The one service whose instances run on the grid's nodes.
 *
 * The wire name is the protocol's, from the service registry; the screen calls
 * the agent a runner, because it is the compute service's process on a machine
 * rather than a wrapper around anything.
 *
 * Named here rather than shared with the services screen: that screen leaves
 * the runners out and this one keeps only them, so each states its own reach
 * into the roster rather than one screen's constant deciding both.
 */
const COMPUTE_RUNNER = 'ores.compute.wrapper';

/**
 * The range presets the bus screen offers, and the window each names.
 *
 * The window is computed here rather than in the browser, because the
 * deployment's clock is the one the samples were stamped against. Every
 * window is open at its end: the read answers a sample at or after the start
 * and before the end, so a window that shares a boundary with the next does
 * not double-count the sample on it.
 */
export const BUS_RANGES = ['15m', '1h', '6h'] as const;
export type BusRange = (typeof BUS_RANGES)[number];

const RANGE_SECONDS: Readonly<Record<BusRange, number>> = {
    '15m': 15 * 60,
    '1h': 60 * 60,
    '6h': 6 * 60 * 60,
};

/**
 * The five JetStream streams this installation creates, by their suffix.
 *
 * The names are the deployment's own, from
 * doc/knowledge/architecture/jetstream_streams_and_consumers.org. They are
 * declared rather than read, because no operation lists the streams that
 * exist; the screen states that gap.
 */
export const BUS_STREAM_SUFFIXES = [
    'marketdata_ticks',
    'synthetic_ticks',
    'synthetic_sandbox_ticks',
    'workflow',
    'compute_assignments',
] as const;

/**
 * The full name of every stream the installation creates, for one broker.
 *
 * A JetStream stream name is the broker's subject prefix with its dots turned
 * into underscores, then the logical suffix, matching the C++ client's
 * `make_stream_name`.
 */
export function busStreamNames(subjectPrefix: string): readonly string[] {
    const prefix = subjectPrefix.replaceAll('.', '_');
    return BUS_STREAM_SUFFIXES.map((suffix) => `${prefix}_${suffix}`);
}

/** The window one range preset names, as the wire timestamps the reads take. */
export function busWindow(
    range: BusRange,
    now: number,
): { readonly start: WireTimestamp; readonly end: WireTimestamp } {
    return {
        start: toWireTimestamp(new Date(now - RANGE_SECONDS[range] * 1000)),
        end: toWireTimestamp(new Date(now)),
    };
}

/** The one field the bus read takes: the range the person chose. */
const busRequestSchema = z.object({ range: z.enum(BUS_RANGES) }).strict();

/**
 * The operations routes: what the installation is doing, as the screens read
 * it.
 *
 * The services screen is the first of the four installation screens, so this
 * route is the pattern the grid, bus and logs screens copy: the session, then
 * the request, then one call on the session's own client, then the answer
 * parsed into the shape the browser consumes.
 */

/**
 * The seconds since one instance last reported, or nothing when it never did.
 *
 * The roster read states the report time at any age; the screen states how long
 * ago. The subtraction happens here rather than in the browser, because the
 * deployment's clock is the one the sample was stamped against, and a browser
 * subtracting from its own would state an age the deployment never measured.
 */
export function secondsSinceReport(sampledAt: string | null, now: number): number | null {
    if (sampledAt === null || !isWireTimestamp(sampledAt)) {
        return null;
    }
    return Math.max(0, Math.floor((now - fromWireTimestamp(sampledAt).getTime()) / 1000));
}

export function registerOperationsRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
    streamPrefix: string,
): void {
    /**
     * The services roster: every expected instance, with the age of its last
     * report.
     *
     * The read belongs to the context that acts on the deployment. The
     * operations area rides the system-administration menu alone, and a web
     * route is not gated by its menu entry, so this is where that reachability
     * is made true. The roster operation checks `telemetry::samples:read` of
     * its own on the server; the mode is what says the caller is reading the
     * deployment rather than a tenant's corner of it.
     */
    server.get('/api/operations/services', async (request) => {
        const session = requireSession(request);
        if (session.mode !== 'system-administration') {
            throw notPermitted('The services of a deployment are read in system administration.');
        }
        /*
         * The read takes no fields, and the request schema is strict, so a
         * query that names one is refused at the boundary rather than stripped
         * and then answered.
         */
        if (!serviceRosterRequestSchema.safeParse(request.query ?? {}).success) {
            throw invalidRequest('The services read takes no query fields.');
        }
        const now = Date.now();
        const slots = await session.client.serviceRoster();
        return serviceRosterViewSchema.parse({
            rows: slots.map((slot) => ({
                ...slot,
                age_seconds: secondsSinceReport(slot.sampled_at, now),
            })),
        });
    });

    /**
     * The compute grid: the stored summary and the nodes, each with its runner.
     *
     * The grid belongs to the installation, and the node read serves all of it;
     * the stored counters are narrower, computed by the poller for one tenant,
     * so the view carries them as they are and the screen says whose they are.
     * The names a person reads arrive from the host registry, and the runner
     * heartbeat carries the same host id as the node sample, so the agent
     * fields are folded onto the node it runs on rather than listed in a second
     * table. A node whose runner never reported keeps its row with the missing
     * state.
     */
    server.get('/api/operations/grid', async (request) => {
        const session = requireSession(request);
        if (session.mode !== 'system-administration') {
            throw notPermitted('The grid of a deployment is read in system administration.');
        }
        if (!gridStatsRequestSchema.safeParse(request.query ?? {}).success) {
            throw invalidRequest('The grid read takes no query fields.');
        }
        const stats = await session.client.gridStats();
        const hosts = await session.client.listHosts();
        const slots = await session.client.serviceRoster();
        const hostNames = new Map(
            hosts.map((host) => [host.id, host.display_name || host.external_id]),
        );
        const nameOf = (hostId: string | null): string | null =>
            hostId === null ? null : (hostNames.get(hostId) ?? null);
        const runnersByHost = new Map<string, (typeof slots)[number]>();
        for (const slot of slots) {
            if (slot.service_name === COMPUTE_RUNNER && slot.host_id !== null) {
                runnersByHost.set(slot.host_id, slot);
            }
        }
        return gridViewSchema.parse({
            sampled_at: stats.sampled_at === '' ? null : stats.sampled_at,
            total_hosts: stats.total_hosts,
            online_hosts: stats.online_hosts,
            idle_hosts: stats.idle_hosts,
            total_workunits: stats.total_workunits,
            total_batches: stats.total_batches,
            active_batches: stats.active_batches,
            outcomes_success: stats.outcomes_success,
            outcomes_client_error: stats.outcomes_client_error,
            outcomes_no_reply: stats.outcomes_no_reply,
            nodes: stats.node_summaries.map((node) => {
                const runner = runnersByHost.get(node.host_id);
                return {
                    ...node,
                    host: nameOf(node.host_id),
                    instance_id: runner?.instance_id ?? null,
                    state: runner?.state ?? 'missing',
                    version: runner?.version ?? null,
                };
            }),
        });
    });

    /**
     * The message bus: the NATS server samples over a range, and one row per
     * stream.
     *
     * The window is derived from the deployment's clock, so the browser never
     * states a time the deployment did not measure, and it is start-inclusive
     * and end-exclusive so adjacent windows tile. The server samples travel
     * whole, newest first, because the vitals and the movement across the range
     * are two readings of the same series and the counters are running totals
     * that only a subtraction over the range turns into movement.
     *
     * The stream rows come from one read per stream. The streams are named here
     * rather than discovered, because no operation lists the streams that exist
     * or have samples; the screen states that gap. A stream with no sample in
     * the range contributes no row rather than a row of zeros.
     */
    server.get('/api/operations/bus', async (request) => {
        const session = requireSession(request);
        if (session.mode !== 'system-administration') {
            throw notPermitted('The message bus of a deployment is read in system administration.');
        }
        const parsed = busRequestSchema.safeParse(request.query ?? {});
        if (!parsed.success) {
            throw invalidRequest('The message bus read takes a range of 15m, 1h or 6h.');
        }
        const window = busWindow(parsed.data.range, Date.now());
        const samples = await session.client.natsServerSamples({
            startTime: window.start,
            endTime: window.end,
        });
        const streams: BusView['streams'] = [];
        for (const streamName of busStreamNames(streamPrefix)) {
            const rows = await session.client.natsStreamSamples({
                streamName,
                startTime: window.start,
                endTime: window.end,
            });
            const newest = rows[0];
            if (newest !== undefined) {
                streams.push(newest);
            }
        }
        return busViewSchema.parse({
            sampled_at: samples[0]?.sampled_at ?? null,
            samples,
            streams,
        });
    });
}
