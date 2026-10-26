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
import { subjects as feedSubjects } from '@ores/wire-protocol/generated/synthetic/protocol/feed_config_protocol';
import { HttpFailure } from './errors.js';
import { TICK_CAPACITY, type FeedTickBuffers } from './feed-ticks.js';
import { input } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/** What every operation reply says: whether the service did what was asked, and why not. */
const operationReplySchema = z.object({
    success: z.boolean(),
    message: z.string().default(''),
});

/**
 * The market-wide arrival factor: the state that scales every feed's rate, and
 * the switch that forces it.
 *
 * The factor belongs to the run rather than to a feed, so it is read and set
 * here and not on a feed's page. The two subjects are the synthetic service's
 * own, which is why they are named here rather than imported: the service
 * handles them by hand, so no generated protocol describes them.
 */
const getSubject = 'synthetic.v1.ops.get_market_regime';
const pinSubject = 'synthetic.v1.ops.pin_market_regime';

/** What a switch may force: a state, or control handed back to the chain. */
export const REGIME_MODES = ['auto', 'calm', 'active'] as const;
export type RegimeMode = (typeof REGIME_MODES)[number];

/**
 * What the service says about the factor. The three facts default rather than
 * being required, because a refusal carries the message and nothing else: the
 * refusal is read for its reason, not for a state it never had.
 */
const regimeSchema = z.object({
    success: z.boolean(),
    message: z.string().default(''),
    state: z.string().default(''),
    multiplier: z.number().default(1),
    pinned: z.boolean().default(false),
});

/**
 * The most feed rows a raster draws. A raster is a picture of what a market-wide
 * factor means, so it draws every feed that runs today and stops at a count a
 * screen can still read; the bound is on the read, not a claim about how many
 * feeds run.
 */
export const RASTER_LANES = 120;

/**
 * A factor the service would not answer for is the service failing, not a
 * request the caller got wrong, so the browser can tell the two apart.
 */
function unavailable(message: string): HttpFailure {
    return new HttpFailure(502, {
        code: 'upstream-unavailable',
        message: message.length > 0 ? message : 'The synthetic service did not answer.',
    });
}

/** The regime as the browser reads it: what is in force, and whether it was forced. */
function regimeOf(reply: z.infer<typeof regimeSchema>): {
    readonly state: string;
    readonly multiplier: number;
    readonly pinned: boolean;
} {
    return { state: reply.state, multiplier: reply.multiplier, pinned: reply.pinned };
}

export function registerMarketRegimeRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
    ticks: FeedTickBuffers,
): void {
    /** The market state in force, the multiplier every feed is scaled by, and who holds it. */
    server.get('/api/synthetic/market-regime', async (request) => {
        const session = requireSession(request);
        const reply = await session.client.callAuthenticated(getSubject, {}, regimeSchema);
        if (!reply.success) {
            throw unavailable(reply.message);
        }
        return regimeOf(reply);
    });

    /**
     * Forces the state, or hands it back to the chain. The reply is the regime
     * after the request, so the screen redraws from what the service says rather
     * than from what it asked for.
     */
    server.post('/api/synthetic/market-regime', async (request) => {
        const session = requireSession(request);
        const body = input(
            z.object({ mode: z.enum(REGIME_MODES) }).strict(),
            request.body === undefined || request.body === null ? {} : request.body,
        );
        const reply = await session.client.callAuthenticated(
            pinSubject,
            { mode: body.mode },
            regimeSchema,
        );
        if (!reply.success) {
            throw unavailable(reply.message);
        }
        return regimeOf(reply);
    });

    /**
     * The times of the ticks each running feed published in the last window,
     * oldest first, one row per feed.
     *
     * The rows are the feeds the service says run, so a stopped feed has no row
     * and the picture follows the run rather than a list on the screen. A row
     * carries times and not prices: the raster shows when ticks arrived, which is
     * what a shared arrival factor changes.
     */
    server.get('/api/synthetic/feed-raster', async (request) => {
        const session = requireSession(request);
        const query = input(
            z
                .object({
                    seconds: z.coerce.number().int().min(1).max(600).default(60),
                    lanes: z.coerce.number().int().min(1).max(RASTER_LANES).default(RASTER_LANES),
                })
                .strict(),
            request.query,
        );
        const reply = await session.client.callAuthenticated(
            feedSubjects.list_feeds_request,
            {},
            operationReplySchema.extend({ running_source_names: z.array(z.string()).default([]) }),
        );
        if (!reply.success) {
            throw unavailable(reply.message);
        }
        const sources = [...new Set(reply.running_source_names)].sort().slice(0, query.lanes);
        const since = Date.now() - query.seconds * 1000;
        return {
            seconds: query.seconds,
            feeds: sources.map((sourceName) => {
                ticks.watch(sourceName);
                return { sourceName, ticks: ticks.arrivalTimes(sourceName, since, TICK_CAPACITY) };
            }),
        };
    });
}
