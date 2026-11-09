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

import type { FxForwardLadder, FxForwardLadderRow } from '../api/client.js';

/** Where a ladder's later tenors measure their swap points from. */
export const LADDER_STARTS = ['spot', 'today', 'tomorrow'] as const;
export type LadderStart = (typeof LADDER_STARTS)[number];

export const LADDER_MEASURES = ['outright', 'discount', 'forward', 'swap'] as const;
export type LadderMeasure = (typeof LADDER_MEASURES)[number];

const SHORT_DATES: ReadonlySet<string> = new Set(['ON', 'TN', 'SP', 'SN']);

export interface LadderRow extends FxForwardLadderRow {
    /** Whether the chosen start moved this row's near date. */
    readonly moved: boolean;
}

/** The forward rate at today and at tomorrow, which the ON row states. */
function shortForwards(ladder: FxForwardLadder): { today: number; tomorrow: number } | undefined {
    const on = ladder.rows.find((row) => row.tenor === 'ON');
    if (on === undefined) return undefined;
    return {
        today: on.forward_rate - on.swap_points * ladder.pip_factor,
        tomorrow: on.forward_rate,
    };
}

/**
 * The rows with their near dates set from the chosen start.
 *
 * The short dates keep their own two dates. Every later tenor is measured from
 * the start, from the forward rates the reply already holds, so changing the
 * start reads nothing.
 */
export function rowsFromStart(ladder: FxForwardLadder, start: LadderStart): readonly LadderRow[] {
    const short = shortForwards(ladder);
    const on = ladder.rows.find((row) => row.tenor === 'ON');
    if (start === 'spot' || short === undefined || on === undefined) {
        return ladder.rows.map((row) => ({ ...row, moved: false }));
    }
    const nearDate = start === 'today' ? on.near_date : on.value_date;
    const nearRate = start === 'today' ? short.today : short.tomorrow;
    return ladder.rows.map((row) =>
        SHORT_DATES.has(row.tenor)
            ? { ...row, moved: false }
            : {
                  ...row,
                  near_date: nearDate,
                  swap_points: (row.forward_rate - nearRate) / ladder.pip_factor,
                  moved: true,
              },
    );
}

/** The tenors a forward-forward may name: the ones that run from spot. */
export function forwardForwardTenors(rows: readonly LadderRow[]): readonly LadderRow[] {
    return rows.filter((row) => !SHORT_DATES.has(row.tenor));
}

/** The points between two tenors, or undefined when the far tenor is not after the near one. */
export function forwardForwardPoints(
    near: FxForwardLadderRow,
    far: FxForwardLadderRow,
    pipFactor: number,
): number | undefined {
    return far.value_date > near.value_date
        ? (far.forward_rate - near.forward_rate) / pipFactor
        : undefined;
}

/** Calendar days between two ISO dates. */
export function daysBetween(from: string, to: string): number {
    return Math.round((Date.parse(`${to}T00:00:00Z`) - Date.parse(`${from}T00:00:00Z`)) / 86400000);
}

/** Today in UTC, as an ISO date. */
export function todayIso(now: Date = new Date()): string {
    return now.toISOString().slice(0, 10);
}

/** The end of a day on the wire, so the read sees every value of that day. */
export function asOfInstant(date: string): string {
    return `${date} 23:59:59Z`;
}

/** The pairs the read answers: those with a USD leg. */
export function usdPairs(codes: readonly string[]): readonly string[] {
    return codes.filter((code) => code.split('/').includes('USD')).sort();
}
