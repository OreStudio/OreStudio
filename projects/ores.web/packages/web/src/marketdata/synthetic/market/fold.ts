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

import type { HistoryResolution, MarketHistory } from '../../../api/client.js';

/**
 * How a fold cuts time: a column is a stretch, a row a slot of it. An hour of minutes by the
 * second, or ten minutes of ten-second blocks by the tenth. The rows line up with the wall
 * clock, so a row is the same second of every minute.
 */
export interface Fold {
    readonly rows: number;
    readonly columns: number;
    readonly slotSeconds: number;
}

export const FOLDS: Readonly<Record<HistoryResolution, Fold>> = {
    second: { rows: 60, columns: 60, slotSeconds: 1 },
    tenth: { rows: 100, columns: 60, slotSeconds: 0.1 },
};

/** What a fold colours a cell by: the ticks of every class, of one class, or the pace. */
export type FoldSeries = 'all' | 'pace' | { readonly group: string };

export interface FoldCell {
    readonly column: number;
    readonly row: number;
    /** Ticks a second over the slot, or the pace. */
    readonly value: number;
    readonly at: number;
    readonly active: boolean;
}

export interface FoldPicture {
    readonly cells: readonly FoldCell[];
    /** The top of the colour scale: a high percentile, so one burst does not wash out the rest. */
    readonly top: number;
    /** The share of each column the market was active, oldest column first. */
    readonly activeShares: readonly number[];
    /** When the oldest column starts, in milliseconds since the epoch. */
    readonly firstColumnAt: number;
}

/** The value at the given share of the way up, with nothing below zero. */
function percentile(values: readonly number[], share: number): number {
    if (values.length === 0) {
        return 0;
    }
    const sorted = [...values].sort((left, right) => left - right);
    return sorted[Math.min(sorted.length - 1, Math.floor(sorted.length * share))] ?? 0;
}

/**
 * The history laid out as a temporal raster. Each slot lands in the column of its stretch and
 * the row of its place in it; a slot older than the oldest column is left out.
 */
export function foldHistory(
    history: MarketHistory,
    resolution: HistoryResolution,
    series: FoldSeries,
    now: number,
): FoldPicture {
    const fold = FOLDS[resolution];
    const slotMs = fold.slotSeconds * 1000;
    const columnOf = (slot: number): number => Math.floor(slot / fold.rows);
    const nowColumn = columnOf(Math.floor(now / slotMs));
    const firstColumn = nowColumn - fold.columns + 1;
    const groupIndex = typeof series === 'object' ? history.groups.indexOf(series.group) : -1;
    const cells: FoldCell[] = [];
    const active = new Array<number>(fold.columns).fill(0);
    const seen = new Array<number>(fold.columns).fill(0);
    for (const bin of history.bins) {
        const at = Date.parse(bin.at);
        if (!Number.isFinite(at)) {
            continue;
        }
        const slot = Math.round(at / slotMs);
        const column = columnOf(slot) - firstColumn;
        if (column < 0 || column >= fold.columns) {
            continue;
        }
        // The slot that holds now is still filling, so its rate is over the part that passed.
        const span = Math.min(fold.slotSeconds, Math.max(fold.slotSeconds / 10, (now - at) / 1000));
        const ticks =
            series === 'all'
                ? bin.counts.reduce((sum, count) => sum + count, 0)
                : typeof series === 'object'
                  ? (bin.counts[groupIndex] ?? 0)
                  : 0;
        cells.push({
            column,
            row: ((slot % fold.rows) + fold.rows) % fold.rows,
            value: series === 'pace' ? bin.pace : ticks / span,
            at,
            active: bin.activeShare >= 0.5,
        });
        active[column] = (active[column] ?? 0) + bin.activeShare;
        seen[column] = (seen[column] ?? 0) + 1;
    }
    const top = Math.max(
        series === 'pace' ? 0.1 : 1,
        percentile(
            cells.map((cell) => cell.value),
            0.98,
        ),
    );
    return {
        cells,
        top,
        activeShares: active.map((sum, column) =>
            (seen[column] ?? 0) === 0 ? 0 : sum / (seen[column] ?? 1),
        ),
        firstColumnAt: firstColumn * fold.rows * slotMs,
    };
}

/** The ticks of every class in each whole second of the last @p seconds, oldest first. */
export function ticksBySecond(
    history: MarketHistory,
    seconds: number,
    now: number,
): readonly { readonly at: number; readonly ticks: number }[] {
    return (
        history.bins
            .map((bin) => ({
                at: Date.parse(bin.at),
                ticks: bin.counts.reduce((sum, count) => sum + count, 0),
            }))
            // The second that holds now is still filling, so it is left out rather than read as a drop.
            .filter(
                (bin) =>
                    Number.isFinite(bin.at) &&
                    now - bin.at <= seconds * 1000 &&
                    now - bin.at >= 1000,
            )
    );
}
