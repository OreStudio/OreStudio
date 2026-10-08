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
import {
    fromWireTimestamp,
    gridStatsRequestSchema,
    gridViewSchema,
    isWireTimestamp,
    serviceRosterRequestSchema,
    serviceRosterViewSchema,
} from '@ores/wire-protocol';
import { invalidRequest, notPermitted } from './errors.js';
import type { LiveSession } from './sessions.js';

/**
 * The one service whose instances run on the grid's nodes.
 *
 * Named here rather than shared with the services screen: that screen leaves
 * the wrappers out and this one keeps only them, so each states its own reach
 * into the roster rather than one screen's constant deciding both.
 */
const COMPUTE_WRAPPER = 'ores.compute.wrapper';

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
     * The compute grid: the stored summary, the nodes, and their wrappers.
     *
     * The grid belongs to the installation, and the node read serves all of it;
     * the stored counters are narrower, computed by the poller for one tenant,
     * so the view carries them as they are and the screen says whose they are.
     * The names a person reads arrive from the host registry, joined on the
     * host id the node sample and the wrapper heartbeat both carry, so a
     * wrapper is placed on the node it runs on.
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
        const now = Date.now();
        const hostNames = new Map(
            hosts.map((host) => [host.id, host.display_name || host.external_id]),
        );
        const nameOf = (hostId: string | null): string | null =>
            hostId === null ? null : (hostNames.get(hostId) ?? null);
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
            nodes: stats.node_summaries.map((node) => ({
                ...node,
                host: nameOf(node.host_id),
            })),
            wrappers: slots
                .filter((slot) => slot.service_name === COMPUTE_WRAPPER)
                .map((slot) => ({
                    ...slot,
                    age_seconds: secondsSinceReport(slot.sampled_at, now),
                    host: nameOf(slot.host_id),
                })),
        });
    });
}
