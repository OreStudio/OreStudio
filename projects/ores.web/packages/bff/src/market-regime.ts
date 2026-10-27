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
import { subjects as feedOverviewSubjects } from '@ores/wire-protocol/generated/synthetic/protocol/feed_overview_protocol';
import { input } from './refdata-calls.js';
import { operationReplySchema, runningSourceNames, upstreamFailure } from './synthetic.js';
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
const traceSubject = 'synthetic.v1.ops.get_market_trace';

/** What a switch may force: a state, or control handed back to the chain. */
export const REGIME_MODES = ['auto', 'calm', 'active'] as const;
export type RegimeMode = (typeof REGIME_MODES)[number];

/** The states the chain has. A state the screen does not know is a reply it does not read. */
const stateSchema = z.enum(['calm', 'active']);

/** One reading of the heat, as the service took it. */
const heatSample = z.object({
    at: z.string(),
    heat: z.number(),
});

/** The factor the service reports: all of it, because the request was answered. */
const answeredRegime = z.object({
    success: z.literal(true),
    message: z.string().default(''),
    state: stateSchema,
    multiplier: z.number(),
    pinned: z.boolean(),
    heat: z.number(),
    jump: z.number(),
    decay: z.number(),
    samples: z.array(heatSample),
    pace: z.number().default(1),
    tau: z.number().default(0),
    population_rate: z.number().default(0),
    loop_gain: z.number().default(0),
    run_seed: z.coerce.number().default(0),
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

/** The facts a raster row draws from the service's own feed list. */
const feedRow = z.object({
    label: z.string(),
    source_name: z.string(),
    asset_class_code: z.string().default(''),
    ticks_per_hour: z.number().default(0),
});

const feedPage = operationReplySchema.extend({ feeds: z.array(feedRow).default([]) });

/** What the service passes for a filter that narrows nothing. */
const everyFeed = {
    folder_id: '',
    provider_code: '',
    should_run: false,
    search: '',
    state: '',
    asset_class: '',
} as const;

/** One row of the raster: a feed, whether it runs, and when its ticks arrived. */
interface RasterRow {
    readonly sourceName: string;
    readonly label: string;
    readonly assetClass: string;
    readonly running: boolean;
    readonly ticks: readonly string[];
    /** The rate the feed's own configuration asks for, in ticks a second. */
    readonly expectedPerSecond: number;
}

/** The regime in force, as the screen reads it. */
interface Regime {
    readonly state: z.infer<typeof stateSchema>;
    readonly multiplier: number;
    readonly pinned: boolean;
    /** The endogenous half: the heat now, the two numbers that move it, and its readings. */
    readonly heat: number;
    readonly jump: number;
    readonly decay: number;
    readonly samples: z.infer<typeof heatSample>[];
    /** How fast market time runs now: the multiplier plus the heat's share. */
    readonly pace: number;
    /** The market's operational time since the run started. */
    readonly tau: number;
    /** The summed mean rate of the running feeds, a second, which the heat is shared over. */
    readonly populationRate: number;
    /** Jump over decay; the heat settles while it is below one. */
    readonly loopGain: number;
    readonly runSeed: number;
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
    return {
        state: reply.state,
        multiplier: reply.multiplier,
        pinned: reply.pinned,
        heat: reply.heat,
        jump: reply.jump,
        decay: reply.decay,
        samples: reply.samples,
        pace: reply.pace,
        tau: reply.tau,
        populationRate: reply.population_rate,
        loopGain: reply.loop_gain,
        runSeed: reply.run_seed,
    };
}

/** One decision of the scheduler's loop, as the service reports it. */
const traceEvent = z.object({
    kind: z.enum(['tick', 'transition', 'pin', 'unpin', 'add', 'remove']),
    source_name: z.string().default(''),
    at: z.string(),
    tau: z.number(),
    operational_gap: z.number().default(0),
    wall_gap: z.number().default(0),
    compensated_gap: z.number().default(0),
    heat: z.number(),
    multiplier: z.number().default(1),
    pace: z.number().default(1),
    state: stateSchema,
});

/** One running feed and the state of its arrival law now. */
const seriesState = z.object({
    source_name: z.string(),
    mode: z.string(),
    rate: z.number(),
    sensitivity: z.number(),
    effective_rate: z.number(),
    phase: z.number().default(0),
    excess: z.number().default(0),
});

const answeredTrace = z.object({
    success: z.literal(true),
    message: z.string().default(''),
    run_seed: z.coerce.number().default(0),
    ticks_per_second: z.number(),
    heap_size: z.coerce.number(),
    lateness_seconds: z.number(),
    max_lateness_seconds: z.number(),
    series: z.coerce.number(),
    events: z.array(traceEvent),
    series_states: z.array(seriesState).default([]),
});

const traceReply = z.union([answeredTrace, refusedRegime]);

/** The most events one read returns; the newest are kept. */
export const TRACE_LIMIT = 20000;

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
     * The scheduler's recent decisions and its own numbers: every tick with the gap its law
     * drew and the wall gap it took, every regime turn, pin and change of the feeds, each with
     * its clock time and its market time. One feed's events and the market's, or all of them.
     */
    server.get('/api/synthetic/market-trace', async (request) => {
        const session = requireSession(request);
        const query = input(
            z
                .object({
                    source: z.string().max(256).default(''),
                    seconds: z.coerce.number().min(1).max(3600).default(120),
                    limit: z.coerce.number().int().min(1).max(TRACE_LIMIT).default(5000),
                })
                .strict(),
            request.query,
        );
        const reply = await session.client.callAuthenticated(
            traceSubject,
            { source_name: query.source, seconds: query.seconds, limit: query.limit },
            traceReply,
        );
        if (!reply.success) {
            throw upstreamFailure(reply.message || 'The synthetic service refused the trace read.');
        }
        return {
            runSeed: reply.run_seed,
            ticksPerSecond: reply.ticks_per_second,
            heapSize: reply.heap_size,
            latenessSeconds: reply.lateness_seconds,
            maxLatenessSeconds: reply.max_lateness_seconds,
            series: reply.series,
            events: reply.events.map((e) => ({
                kind: e.kind,
                sourceName: e.source_name,
                at: e.at,
                tau: e.tau,
                operationalGap: e.operational_gap,
                wallGap: e.wall_gap,
                compensatedGap: e.compensated_gap,
                heat: e.heat,
                multiplier: e.multiplier,
                pace: e.pace,
                state: e.state,
            })),
            feeds: reply.series_states.map((f) => ({
                sourceName: f.source_name,
                mode: f.mode,
                rate: f.rate,
                sensitivity: f.sensitivity,
                effectiveRate: f.effective_rate,
                phase: f.phase,
                excess: f.excess,
            })),
        };
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
     * The times of the ticks each feed published in the last window, oldest
     * first, one row per feed, the feeds that run first.
     *
     * A stopped feed keeps its row, because a picture of the market that leaves
     * out the feeds that are silent cannot show that they are: a reader sees the
     * row and the mark that says the feed is not running. A row carries times and
     * not prices: the raster shows when ticks arrived, which is what a shared
     * arrival factor changes.
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
        const [page, running] = await Promise.all([
            session.client.callAuthenticated(
                feedOverviewSubjects.list_folder_feeds_request,
                { ...everyFeed, offset: 0, limit: RASTER_LANES },
                feedPage,
            ),
            runningSourceNames(session),
        ]);
        if (!page.success) {
            throw upstreamFailure(page.message);
        }
        const rows = page.feeds
            .map((feed) => ({ ...feed, running: running.has(feed.source_name) }))
            // The feeds that run first, so a long list still opens on the market;
            // the service's label order holds inside each group.
            .sort((left, right) => Number(right.running) - Number(left.running))
            .slice(0, RASTER_LANES);
        ticks.keepOnly(rows.filter((row) => row.running).map((row) => row.source_name));
        const since = Date.now() - query.seconds * 1000;
        const feeds: RasterRow[] = rows.map((row) => {
            if (row.running) {
                ticks.watch(row.source_name);
            }
            return {
                sourceName: row.source_name,
                label: row.label,
                assetClass: row.asset_class_code,
                running: row.running,
                ticks: row.running ? ticks.arrivalTimes(row.source_name, since) : [],
                expectedPerSecond: row.ticks_per_hour / 3600,
            };
        });
        return { seconds: query.seconds, feeds };
    });
}
