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

/** One tick as the BFF serves it: a wire timestamp and the price. */
export interface Tick {
    readonly at: string;
    readonly value: number;
}

/** What a window of ticks says at a glance. */
export interface TickStats {
    readonly last: Tick;
    readonly move: number;
    readonly high: number;
    readonly low: number;
    readonly count: number;
}

/** A wire timestamp, such as 2026-10-11 09:00:00Z, as milliseconds. */
export function instantOf(at: string): number {
    return Date.parse(at.replace(' ', 'T'));
}

/** The last tick, the move from the first, and the high and low; nothing for an empty window. */
export function statsOf(ticks: readonly Tick[]): TickStats | undefined {
    const first = ticks[0];
    const last = ticks[ticks.length - 1];
    if (first === undefined || last === undefined) {
        return undefined;
    }
    const values = ticks.map((tick) => tick.value);
    return {
        last,
        move: last.value - first.value,
        high: Math.max(...values),
        low: Math.min(...values),
        count: ticks.length,
    };
}

/** How many ticks the moving average spans. */
export const EMA_PERIOD = 20;

/**
 * The exponential moving average of the values, one for each, seeded with the
 * first value: each next one moves 2/(period+1) of the way to the new value.
 */
export function emaOf(values: readonly number[], period = EMA_PERIOD): readonly number[] {
    const alpha = 2 / (period + 1);
    const out: number[] = [];
    for (const value of values) {
        const previous = out[out.length - 1];
        out.push(previous === undefined ? value : previous + alpha * (value - previous));
    }
    return out;
}

/** The ticks as CSV, one row each with the feed, the instant and the price, as the BFF gave them. */
export function ticksCsv(sourceName: string, ticks: readonly Tick[]): string {
    const quote = (text: string): string =>
        /[",\n]/.test(text) ? `"${text.replaceAll('"', '""')}"` : text;
    return [
        'source_name,observation_time,value',
        ...ticks.map((tick) => [quote(sourceName), quote(tick.at), String(tick.value)].join(',')),
    ].join('\n');
}
