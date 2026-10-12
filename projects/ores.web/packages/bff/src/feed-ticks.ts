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

/** The subject a synthetic feed publishes its ticks on, as the C++ tick_subjects names it. */
export function tickSubjectFor(sourceName: string): string {
    return `synthetic.v1.ops.tick.${sourceName}`;
}

/** How many ticks a feed's buffer keeps, the newest last. */
export const TICK_CAPACITY = 500;

/**
 * The recent ticks of every feed that is running, shared by every session.
 *
 * A feed's ticks are published, not stored where a read can reach them without
 * the feed's series identity, so the BFF listens and keeps the newest ones, as
 * the Qt market simulator does. The synthetic service answers what runs for
 * the whole process, so every session states the same set: listening starts
 * when a feed appears in it and ends when the feed leaves it.
 *
 * A buffer holds one run and goes when the run ends. A source name is unique
 * only within a tenant, and the service lets one feed run under a name at a
 * time, so a buffer kept past its run could later be read through another
 * tenant's feed of the same name. Ending with the run closes that, and bounds
 * the buffers by the feeds running. A caller reads a buffer only after the
 * server has shown it the feed.
 */
export class FeedTickBuffers {
    readonly #subscribe: TickSubscriber;
    readonly #capacity: number;
    readonly #runs = new Map<string, { readonly ticks: FeedTick[]; readonly stop: () => void }>();

    constructor(subscribe: TickSubscriber, capacity = TICK_CAPACITY) {
        this.#subscribe = subscribe;
        this.#capacity = capacity;
    }

    /** States every feed running now: a new one is heard from, and one that left is forgotten. */
    running(sourceNames: readonly string[]): void {
        const now = new Set(sourceNames);
        for (const [name, run] of this.#runs) {
            if (!now.has(name)) {
                run.stop();
                this.#runs.delete(name);
            }
        }
        for (const name of now) {
            if (this.#runs.has(name)) continue;
            const ticks: FeedTick[] = [];
            const stop = this.#subscribe(tickSubjectFor(name), (tick) => {
                const value = Number(tick.value);
                if (!Number.isFinite(value)) return;
                ticks.push({ at: tick.observation_time, value, uri: tick.oresmd_uri });
                if (ticks.length > this.#capacity) ticks.splice(0, ticks.length - this.#capacity);
            });
            this.#runs.set(name, { ticks, stop });
        }
    }

    /** The newest ticks of a feed's current run, at most `limit`, oldest first. */
    ticks(sourceName: string, limit: number): readonly FeedTick[] {
        const ticks = this.#runs.get(sourceName)?.ticks ?? [];
        return ticks.slice(Math.max(0, ticks.length - limit));
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
