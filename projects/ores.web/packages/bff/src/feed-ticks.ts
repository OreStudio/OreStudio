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

import { z } from 'zod';

/**
 * One tick as the screen plots it: when, the price as a number, and the oresmd
 * URI of the datum it is a value of. An FX feed makes one datum, and a curve
 * feed makes one for each pillar of the curve, all at the same time.
 */
export interface FeedTick {
    readonly at: string;
    readonly value: number;
    readonly uri: string;
}

/** The part of a tick the buffer keeps. The producer writes the price as decimal text. */
export const marketTickSchema = z.object({
    value: z.string(),
    observation_time: z.string(),
    oresmd_uri: z.string().default(''),
});

/** Listens on a subject for ticks, and returns the way to stop. */
export type TickSubscriber = (
    subject: string,
    onTick: (tick: z.infer<typeof marketTickSchema>) => void,
) => () => void;

/** The subject a synthetic feed publishes its ticks on for a screen to tail. */
export function tickSubjectFor(sourceName: string): string {
    return `synthetic.v1.ops.feed_tick.${sourceName}`;
}

/** How many ticks a feed's buffer keeps, the newest last. */
export const TICK_CAPACITY = 500;

/**
 * The recent ticks of every feed that is running, shared by every session.
 *
 * A feed publishes its ticks on a subject a screen can tail, and the BFF keeps
 * the newest ones for each source that a read has asked about. A buffer holds
 * ticks, not a run's state, so a read of a feed that is not running finds
 * whatever was heard while it ran.
 */
export class FeedTickBuffers {
    readonly #subscribe: TickSubscriber;
    readonly #capacity: number;
    readonly #runs = new Map<string, { readonly ticks: FeedTick[]; readonly stop: () => void }>();

    constructor(subscribe: TickSubscriber, capacity = TICK_CAPACITY) {
        this.#subscribe = subscribe;
        this.#capacity = capacity;
    }

    /** Starts listening to one source the first time a read asks for it. */
    watch(sourceName: string): void {
        if (this.#runs.has(sourceName)) return;
        const ticks: FeedTick[] = [];
        const stop = this.#subscribe(tickSubjectFor(sourceName), (tick) => {
            const value = Number(tick.value);
            if (!Number.isFinite(value)) return;
            ticks.push({ at: tick.observation_time, value, uri: tick.oresmd_uri });
            if (ticks.length > this.#capacity) ticks.splice(0, ticks.length - this.#capacity);
        });
        this.#runs.set(sourceName, { ticks, stop });
    }

    /** The newest ticks of a feed's current run, at most `limit`, oldest first. */
    ticks(sourceName: string, limit: number): readonly FeedTick[] {
        const ticks = this.#runs.get(sourceName)?.ticks ?? [];
        return ticks.slice(Math.max(0, ticks.length - limit));
    }

    /**
     * When the ticks of a feed's current run arrived, oldest first, from an
     * instant on and at most `limit` of them.
     *
     * A raster draws when ticks arrived and nothing else, so it reads times
     * rather than prices: the value of a tick says nothing about the market-wide
     * factor, and a row of prices is a chart the feed's own page already draws.
     */
    arrivalTimes(sourceName: string, since: number, limit: number): readonly string[] {
        const ticks = this.#runs.get(sourceName)?.ticks ?? [];
        return ticks
            .filter((tick) => Date.parse(tick.at) >= since)
            .slice(-limit)
            .map((tick) => tick.at);
    }

    /** How many runs are heard from, which is what a test asserts on. */
    get size(): number {
        return this.#runs.size;
    }

    /** Stops listening to everything, for a server that is closing. */
    close(): void {
        for (const run of this.#runs.values()) run.stop();
        this.#runs.clear();
    }
}
