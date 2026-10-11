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

/** One tick as the screen plots it: when, and the price as a number. */
export interface FeedTick {
    readonly at: string;
    readonly value: number;
}

/** The part of a tick the buffer keeps. The producer writes the price as decimal text. */
export const marketTickSchema = z.object({
    value: z.string(),
    observation_time: z.string(),
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
 * the Qt market simulator does. Listening starts when the BFF learns a feed is
 * running and stops when it learns the feed stopped. The ticks heard are kept
 * after a stop, so a stopped feed still shows the run it made.
 *
 * Buffers are keyed by source name. A caller reads one only after the server
 * has shown it the feed, so the key is never a way to see another's feed.
 */
export class FeedTickBuffers {
    readonly #subscribe: TickSubscriber;
    readonly #capacity: number;
    readonly #ticks = new Map<string, FeedTick[]>();
    readonly #listening = new Map<string, () => void>();

    constructor(subscribe: TickSubscriber, capacity = TICK_CAPACITY) {
        this.#subscribe = subscribe;
        this.#capacity = capacity;
    }

    /** Listens to each feed in the set that is not heard yet. */
    listen(sourceNames: readonly string[]): void {
        for (const name of sourceNames) {
            if (this.#listening.has(name)) continue;
            const ticks = this.#ticks.get(name) ?? [];
            this.#ticks.set(name, ticks);
            const stop = this.#subscribe(tickSubjectFor(name), (tick) => {
                const value = Number(tick.value);
                if (!Number.isFinite(value)) return;
                ticks.push({ at: tick.observation_time, value });
                if (ticks.length > this.#capacity) ticks.splice(0, ticks.length - this.#capacity);
            });
            this.#listening.set(name, stop);
        }
    }

    /** States every feed running now: the others stop being heard. */
    running(sourceNames: readonly string[]): void {
        const now = new Set(sourceNames);
        for (const [name, stop] of this.#listening) {
            if (!now.has(name)) {
                stop();
                this.#listening.delete(name);
            }
        }
        this.listen(sourceNames);
    }

    /** The newest ticks heard from one feed, at most `limit`, oldest first. */
    ticks(sourceName: string, limit: number): readonly FeedTick[] {
        const ticks = this.#ticks.get(sourceName) ?? [];
        return ticks.slice(Math.max(0, ticks.length - limit));
    }

    /** Stops listening to everything, for a server that is closing. */
    close(): void {
        for (const stop of this.#listening.values()) stop();
        this.#listening.clear();
    }
}
