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
import { input } from './refdata-calls.js';
import { runningSourceNames, upstreamFailure } from './synthetic.js';
import type { LiveSession } from './sessions.js';
import type { FeedTickBuffers } from './feed-ticks.js';

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

/** The states the chain has. A state the screen does not know is a reply it does not read. */
const stateSchema = z.enum(['calm', 'active']);

/** The factor the service reports: all three facts, because the request was answered. */
const answeredRegime = z.object({
    success: z.literal(true),
    message: z.string().default(''),
    state: stateSchema,
    multiplier: z.number(),
    pinned: z.boolean(),
});

/** A request the service would not carry out. It says why, and nothing else. */
const refusedRegime = z.object({
    success: z.literal(false),
    message: z.string().default(''),
});

const regimeReply = z.union([answeredRegime, refusedRegime]);

/**
 * The most feed rows a raster draws. A raster is a picture of what a market-wide
 * factor means, so it draws every feed that runs today and stops at a count a
 * screen can still read; the bound is on the read, not a claim about how many
 * feeds run.
 */
export const RASTER_LANES = 120;

/** The regime in force, as the screen reads it. */
interface Regime {
    readonly state: z.infer<typeof stateSchema>;
    readonly multiplier: number;
    readonly pinned: boolean;
}

/**
 * A factor the service would not answer for, or would not change, is the service
 * failing rather than a request the caller got wrong: the message is what it
 * said, so the screen shows it instead of an empty state.
 */
function answered(reply: z.infer<typeof regimeReply>): Regime {
    if (!reply.success) {
        throw upstreamFailure(reply.message || 'The synthetic service refused the regime read.');
    }
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
        return answered(await session.client.callAuthenticated(getSubject, {}, regimeReply));
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
        return answered(
            await session.client.callAuthenticated(pinSubject, { mode: body.mode }, regimeReply),
        );
    });

    /**
     * The times of the ticks each running feed published in the last window,
     * oldest first, one row per feed.
     *
     * The rows are the feeds the service says run, so a stopped feed has no row
     * and the picture follows the run rather than a list on the screen. A row
     * carries times and not prices: the raster shows when ticks arrived, which is
     * what a shared arrival factor changes.
     *
     * The read names the sources it wants and drops the ones it no longer wants,
     * so the BFF listens to the feeds that run and not to every feed that ever
     * did.
     */
    server.get('/api/synthetic/feed-raster', async (request) => {
        const session = requireSession(request);
        const query = input(
            z
                .object({
                    seconds: z.coerce.number().int().min(1).max(600).default(120),
                })
                .strict(),
            request.query,
        );
        const running = [...(await runningSourceNames(session))].sort().slice(0, RASTER_LANES);
        ticks.keepOnly(running);
        const since = Date.now() - query.seconds * 1000;
        return {
            seconds: query.seconds,
            feeds: running.map((sourceName) => {
                ticks.watch(sourceName);
                return { sourceName, ticks: ticks.arrivalTimes(sourceName, since) };
            }),
        };
    });
}
