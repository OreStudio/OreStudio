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
import { subjects } from '@ores/wire-protocol/generated/marketdata/protocol/adapter_operation_protocol';
import { HttpFailure } from './errors.js';
import type { LiveSession } from './sessions.js';

/**
 * The state of the market data service's adapters.
 *
 * An adapter is the one thing that knows what the service is publishing, so its
 * status is the truth of what a source serves. A screen that shows a feed as
 * running reads this, and never the synthetic controller's running set, because
 * a feed the service demands from an adapter runs while the controller never
 * started it.
 *
 * The service checks the permission and the shape, so the route adds no rule of
 * its own.
 */

/**
 * One adapter: which provider it serves, whether it is serving, and the counts
 * of the last demand it answered.
 */
const adapterSchema = z.looseObject({
    adapter_id: z.string(),
    provider_code: z.string(),
    state: z.string(),
    last_heartbeat_at: z.string().default(''),
    heartbeat_counter: z.number().default(0),
    served_count: z.number().default(0),
    unknown_count: z.number().default(0),
    demanded_count: z.number().default(0),
    routes_seen: z.number().default(0),
    ticks_last_minute: z.number().default(0),
    last_tick_age_ms: z.number().default(-1),
    accepted: z.array(z.looseObject({})).default([]),
    rejected: z.array(z.looseObject({})).default([]),
});

const replySchema = z.looseObject({
    adapters: z.array(adapterSchema).default([]),
    success: z.boolean(),
    message: z.string().default(''),
});

/**
 * A read the service would not answer is the service failing, not a request the
 * caller got wrong, so the browser can tell a broken service from a bad call.
 */
function unavailable(message: string): HttpFailure {
    return new HttpFailure(502, {
        code: 'upstream-unavailable',
        message: message.length > 0 ? message : 'The market data service did not answer.',
    });
}

export function registerAdapterRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    /**
     * Every adapter the service tracks, with what it serves and what it was
     * asked to serve. The dashboard and the feed tree read the state of a
     * provider's source from here.
     */
    server.get('/api/marketdata/adapters', async (request) => {
        const session = requireSession(request);
        const reply = await session.client.callAuthenticated(
            subjects.list_adapter_status_request,
            {},
            replySchema,
        );
        if (!reply.success) {
            throw unavailable(reply.message);
        }
        return {
            adapters: reply.adapters,
            success: reply.success,
            message: reply.message,
        };
    });
}
