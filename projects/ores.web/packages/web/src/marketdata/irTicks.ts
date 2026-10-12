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

/** One tick of a curve feed: when, the rate, and the pillar's datum. */
export interface PillarTick {
    readonly at: string;
    readonly value: number;
    readonly uri: string;
}

/** One pillar of the curve, as the curve template states it. */
export interface Pillar {
    readonly start: string;
    readonly end: string;
}

/** One whole curve the feed made: the time of the batch and the rate at each pillar, in template order. */
export interface Curve {
    readonly at: string;
    readonly rates: readonly number[];
}

/** The pillars of the curve in the order the feed publishes them: by the template's sequence. */
export function pillarsOf(entries: readonly Readonly<Record<string, unknown>>[]): readonly Pillar[] {
    return [...entries]
        .sort((a, b) => Number(a['sequence_index']) - Number(b['sequence_index']))
        .map((row) => ({
            start: String(row['start_tenor_code'] ?? ''),
            end: String(row['end_tenor_code'] ?? ''),
        }));
}

/**
 * The whole curves in a run of ticks, oldest first. A feed publishes a curve as
 * one tick for each pillar, in template order, all with the time of the batch,
 * so the ticks that share a time are one curve. A batch with a tick missing is
 * not a curve: the buffer began to listen part way through it, or the run
 * ended in it. It is dropped, and not drawn with a hole.
 */
export function curvesOf(ticks: readonly PillarTick[], pillars: number): readonly Curve[] {
    if (pillars === 0) {
        return [];
    }
    const batches: { at: string; rates: number[] }[] = [];
    for (const tick of ticks) {
        const last = batches[batches.length - 1];
        if (last !== undefined && last.at === tick.at) {
            last.rates.push(tick.value);
        } else {
            batches.push({ at: tick.at, rates: [tick.value] });
        }
    }
    return batches.filter((batch) => batch.rates.length === pillars);
}

/** How far the long end sits over the short end, in basis points. */
export function slopeOf(curve: Curve): number {
    const short = curve.rates[0];
    const long = curve.rates[curve.rates.length - 1];
    return short === undefined || long === undefined ? 0 : (long - short) * 10_000;
}

/** The curves as a CSV file: one row for each curve, one column for each pillar. */
export function curvesCsv(
    sourceName: string,
    pillars: readonly Pillar[],
    curves: readonly Curve[],
): string {
    const header = ['source', 'time', ...pillars.map((pillar) => `${pillar.start}-${pillar.end}`)];
    const rows = curves.map((curve) => [sourceName, curve.at, ...curve.rates.map(String)]);
    return [header, ...rows].map((row) => row.join(',')).join('\n') + '\n';
}
