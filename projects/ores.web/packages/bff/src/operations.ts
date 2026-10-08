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
    isWireTimestamp,
    serviceRosterRequestSchema,
    serviceRosterViewSchema,
} from '@ores/wire-protocol';
import { invalidRequest, notPermitted } from './errors.js';
import type { LiveSession } from './sessions.js';

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
}
