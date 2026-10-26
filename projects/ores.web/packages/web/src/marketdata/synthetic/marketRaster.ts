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

/**
 * How far back the raster draws, in seconds.
 *
 * Two minutes, because every feed runs at its own rate and the slowest of them
 * publishes a tick a minute: a window of one minute would leave those rows empty
 * and read as a feed that is not running.
 */
export const RASTER_SECONDS = 120;

/**
 * Where each arrival falls in the window: 0 at the oldest edge and 1 at now.
 *
 * The marks are positions rather than times because the window is a picture, and
 * a mark outside it is not drawn at its edge: a tick older than the window has
 * left it, and one stamped after the browser's clock has not arrived yet.
 */
export function arrivalPositions(
    ticks: readonly string[],
    seconds: number,
    now: number,
): readonly number[] {
    const span = seconds * 1000;
    if (span <= 0) {
        return [];
    }
    const positions: number[] = [];
    for (const tick of ticks) {
        const at = Date.parse(tick);
        if (!Number.isFinite(at)) {
            continue;
        }
        const age = now - at;
        if (age < 0 || age > span) {
            continue;
        }
        positions.push(1 - age / span);
    }
    return positions.sort((left, right) => left - right);
}

/**
 * The multiplier as a screen writes it: the factor a feed's rate is scaled by,
 * at no more than two decimals.
 */
export function multiplierText(multiplier: number): string {
    return `×${multiplier.toLocaleString(undefined, { maximumFractionDigits: 2 })}`;
}
