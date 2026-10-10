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
 */

import { fromWireTimestamp, isWireTimestamp } from '@ores/wire-protocol/browser';
import type { BusView, GridView } from '@ores/wire-protocol/browser';

/**
 * What the dashboard decides from the reads: how each panel is doing, and how
 * fast the bus is moving.
 *
 * Pure, so the panels and the status at the top of the page cannot disagree:
 * both ask the same functions the same question.
 */

/** How a panel is doing: fine, wants the person, or has nothing to say yet. */
export type StatusTone = 'ok' | 'attention' | 'none';

/** The nodes that are not online, which is what the grid panel asks the person to look at. */
export function nodesNotOnline(grid: GridView): number {
    return Math.max(0, grid.total_hosts - grid.online_hosts);
}

/** The slow consumers in the newest server sample, which the queue panel asks the person to look at. */
export function slowConsumers(bus: BusView): number {
    return bus.samples[0]?.slow_consumers ?? 0;
}

/**
 * Messages per second the server received, from the counter in each sample.
 *
 * The counter runs since the server started, so a rate is the difference
 * between two samples over the seconds between them. The samples arrive newest
 * first and the series runs oldest first, as a line is drawn. A pair that goes
 * backwards is a server restart, and a pair with no time between them has no
 * rate; both are left out rather than drawn as a spike or a hole.
 */
export function throughputSeries(bus: BusView): readonly number[] {
    const samples = [...bus.samples].reverse();
    const rates: number[] = [];
    for (let index = 1; index < samples.length; index += 1) {
        const before = samples[index - 1];
        const after = samples[index];
        if (
            before === undefined ||
            after === undefined ||
            !isWireTimestamp(before.sampled_at) ||
            !isWireTimestamp(after.sampled_at)
        ) {
            continue;
        }
        const seconds =
            (fromWireTimestamp(after.sampled_at).getTime() -
                fromWireTimestamp(before.sampled_at).getTime()) /
            1000;
        const messages = after.in_msgs - before.in_msgs;
        if (seconds > 0 && messages >= 0) {
            rates.push(messages / seconds);
        }
    }
    return rates;
}

/** How many of the four panels want the person, which is what the status at the top counts. */
export function panelsNeedingAttention(tones: readonly StatusTone[]): number {
    return tones.filter((tone) => tone === 'attention').length;
}
