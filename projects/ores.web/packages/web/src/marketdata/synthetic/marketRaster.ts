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

/** The factor the feeds were scaled by, as one read of it saw it. */
export interface FactorSample {
    /** When the read landed. */
    readonly at: number;
    /** The multiplier every feed's rate was scaled by then. */
    readonly multiplier: number;
}

/**
 * The path the factor traced over the window, as a step line, in the raster's
 * own units: 0 to 100 across and 0 to 1 up from the baseline.
 *
 * A step, because the factor holds a value and then changes it: a diagonal
 * between two readings would claim a value the market never had. The two ends of
 * the range are put at the top and the foot, so the shape of the change reads
 * even when a market only ever visits two values.
 */
export function factorPath(samples: readonly FactorSample[], seconds: number, now: number): string {
    const span = seconds * 1000;
    if (span <= 0) {
        return '';
    }
    const placed = samples
        .filter((sample) => now - sample.at <= span)
        .map((sample) => ({
            x: 100 * (1 - (now - sample.at) / span),
            value: sample.multiplier,
        }))
        .sort((left, right) => left.x - right.x);
    const first = placed[0];
    if (first === undefined) {
        return '';
    }
    const values = placed.map((point) => point.value);
    const low = Math.min(...values);
    const high = Math.max(...values);
    // Up the box is a larger factor, and the drawing's y runs down.
    const y = (value: number): number => (high === low ? 0.5 : 1 - (value - low) / (high - low));
    let path = `M ${first.x.toFixed(2)} ${y(first.value).toFixed(3)}`;
    let previous = first.value;
    for (const point of placed.slice(1)) {
        path += ` H ${point.x.toFixed(2)}`;
        if (point.value !== previous) {
            path += ` V ${y(point.value).toFixed(3)}`;
            previous = point.value;
        }
    }
    return `${path} H 100`;
}
