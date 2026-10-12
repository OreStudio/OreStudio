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

import type { TimelineEvent } from '@ores/wire-protocol/browser';
import { parseTimestamp } from '../ui/Time.js';

/**
 * A row of the rail: one entry, or several entries drawn as one.
 *
 * A bulk import writes a record per row, so the history of one party can hold
 * forty links created in one second by one service for one reason. Forty
 * identical cards bury the few entries a person made, so entries that are one
 * act are drawn as one row that opens to its members.
 */
export type Row =
    | { readonly kind: 'entry'; readonly event: TimelineEvent }
    | { readonly kind: 'batch'; readonly events: readonly TimelineEvent[] };

/**
 * The kinds that can be one act in many parts. A change carries a difference
 * a reader wants to read, so it never batches.
 */
const BATCHED_KINDS: ReadonlySet<string> = new Set(['raised', 'closed']);

/** The longest pause, in milliseconds, between two entries of one batch. */
export const BATCH_GAP_MS = 5000;

/** The fewest entries that make a batch. Two entries read well as they are. */
export const BATCH_MINIMUM = 3;

/** What the entries of one act have in common: who, what, which way, and why. */
function actOf(event: TimelineEvent): string {
    return [event.actor, event.entityType, event.kind, event.reasonCode, event.commentary].join(
        '\u0000',
    );
}

/**
 * Draws entries that are one act as one row.
 *
 * Entries belong together when they were created or closed by the same author,
 * for the same kind of record, with the same reason and the same comment, and
 * each follows the one before it within BATCH_GAP_MS. A run needs at least
 * BATCH_MINIMUM entries. The batch stands where its newest entry stood, and
 * entries of other acts keep their place around it. The members stay in the
 * order they arrived in, newest first.
 *
 * The stream is not changed: the chart and the counts read the entries, and a
 * batch is only how the rail draws them.
 */
export function batchesOf(events: readonly TimelineEvent[]): readonly Row[] {
    const acts = new Map<string, number[]>();
    events.forEach((event, index) => {
        if (!BATCHED_KINDS.has(event.kind) || Number.isNaN(parseTimestamp(event.at).getTime())) {
            return;
        }
        const key = actOf(event);
        acts.set(key, [...(acts.get(key) ?? []), index]);
    });

    const batches = new Map<number, readonly number[]>();
    const members = new Set<number>();
    for (const indices of acts.values()) {
        let run: number[] = [];
        const close = (): void => {
            if (run.length >= BATCH_MINIMUM) {
                batches.set(run[0] ?? 0, run);
                run.forEach((index) => members.add(index));
            }
            run = [];
        };
        for (const index of indices) {
            const last = run.at(-1);
            if (
                last !== undefined &&
                Math.abs(
                    parseTimestamp(events[last]?.at ?? '').getTime() -
                        parseTimestamp(events[index]?.at ?? '').getTime(),
                ) > BATCH_GAP_MS
            ) {
                close();
            }
            run.push(index);
        }
        close();
    }

    const rows: Row[] = [];
    events.forEach((event, index) => {
        const batch = batches.get(index);
        if (batch !== undefined) {
            rows.push({
                kind: 'batch',
                events: batch.flatMap((member) => {
                    const own = events[member];
                    return own === undefined ? [] : [own];
                }),
            });
        } else if (!members.has(index)) {
            rows.push({ kind: 'entry', event });
        }
    });
    return rows;
}
