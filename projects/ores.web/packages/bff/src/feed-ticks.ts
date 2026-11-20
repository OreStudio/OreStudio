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

/** How many ticks a feed's buffer keeps, the newest last. */
export const TICK_CAPACITY = 500;

/** One source's buffer: its ticks, the subjects it is heard on, and the way to stop each. */
interface Run {
    readonly ticks: FeedTick[];
    subjects: readonly string[];
    stops: (() => void)[];
}

/**
 * The recent ticks of every feed that is running, shared by every session.
 *
 * A feed's ticks reach the market through its adapter and the market data
 * ingest, on the vendor subjects the synthetic service names for it, and the BFF
 * keeps the newest ones for each source a read has asked about. So a screen shows
 * what the vendor published: after the feed's faults, the adapter, and the
 * ingest's conflation and refusals. A buffer holds ticks, not a run's state, so a
 * read of a feed that is not running finds whatever was heard while it ran.
 */
export class FeedTickBuffers {
    readonly #subscribe: TickSubscriber;
    readonly #capacity: number;
    readonly #runs = new Map<string, Run>();

    constructor(subscribe: TickSubscriber, capacity = TICK_CAPACITY) {
        this.#subscribe = subscribe;
        this.#capacity = capacity;
    }

    /**
     * Listens to one source on the vendor subjects its ticks land on.
     *
     * The first read that names a source starts its buffer. A source whose
     * subjects change, such as a feed started again through another adapter, is
     * heard on the new ones and keeps its ticks. A source with no subjects, a feed
     * that does not run, keeps what was heard.
     */
    watch(sourceName: string, subjects: readonly string[]): void {
        let run = this.#runs.get(sourceName);
        if (run === undefined) {
            run = { ticks: [], subjects: [], stops: [] };
            this.#runs.set(sourceName, run);
        }
        // The same set in another order is the same subscription.
        const wanted = [...subjects].sort();
        if (wanted.length === 0 || sameSubjects(wanted, run.subjects)) return;
        const ticks = run.ticks;
        // Every new subscription is made before any is kept, so one that throws leaves the
        // source on its old subjects, and the next read tries again.
        const stops: (() => void)[] = [];
        try {
            for (const subject of wanted) {
                stops.push(
                    this.#subscribe(subject, (tick) => {
                        const value = Number(tick.value);
                        if (!Number.isFinite(value)) return;
                        ticks.push({ at: tick.observation_time, value, uri: tick.oresmd_uri });
                        if (ticks.length > this.#capacity)
                            ticks.splice(0, ticks.length - this.#capacity);
                    }),
                );
            }
        } catch (error) {
            for (const stop of stops) stop();
            throw error;
        }
        for (const stop of run.stops) stop();
        run.subjects = wanted;
        run.stops = stops;
    }

    /** The newest ticks of a feed's current run, at most `limit`, oldest first. */
    ticks(sourceName: string, limit: number): readonly FeedTick[] {
        const ticks = this.#runs.get(sourceName)?.ticks ?? [];
        return ticks.slice(Math.max(0, ticks.length - limit));
    }

    /**
     * When the ticks of a feed's current run arrived, from an instant on, at most
     * `limit` of them, in the order they were heard.
     *
     * A raster draws when ticks arrived and nothing else, so it reads times
     * rather than prices: the value of a tick says nothing about the market-wide
     * factor, and a row of prices is a chart the feed's own page already draws.
     */
    arrivalTimes(sourceName: string, since: number, limit = this.#capacity): readonly string[] {
        const ticks = this.#runs.get(sourceName)?.ticks ?? [];
        return ticks
            .filter((tick) => Date.parse(tick.at) >= since)
            .slice(-limit)
            .map((tick) => tick.at);
    }

    /**
     * Stops listening to every source the caller no longer names.
     *
     * A read that watches what a run carries and nothing else keeps the BFF's
     * subscriptions at the size of the run: a source that stopped is a subject
     * nobody reads, and left watching it grows for as long as feeds come and go.
     */
    keepOnly(sourceNames: readonly string[]): void {
        const keep = new Set(sourceNames);
        for (const [sourceName, run] of this.#runs) {
            if (keep.has(sourceName)) {
                continue;
            }
            for (const stop of run.stops) stop();
            this.#runs.delete(sourceName);
        }
    }

    /** How many runs are heard from, which is what a test asserts on. */
    get size(): number {
        return this.#runs.size;
    }

    /** Stops listening to everything, for a server that is closing. */
    close(): void {
        for (const run of this.#runs.values()) for (const stop of run.stops) stop();
        this.#runs.clear();
    }
}

function sameSubjects(left: readonly string[], right: readonly string[]): boolean {
    return left.length === right.length && left.every((subject, i) => subject === right[i]);
}
