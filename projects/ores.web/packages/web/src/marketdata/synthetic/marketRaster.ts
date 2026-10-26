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

/** The state the market was in, as one read of it saw it. */
export interface StateSample {
    /** When the read landed. */
    readonly at: number;
    /** What the chain, or a person, held the market at then. */
    readonly state: 'calm' | 'active';
}

/** One stretch of the window the market spent in a state, in the raster's units. */
export interface StateBand {
    readonly from: number;
    readonly to: number;
    readonly state: 'calm' | 'active';
}

/**
 * The stretches a market held each state over the window, in the raster's own
 * units: 0 at the oldest edge and 100 at now.
 *
 * A band, because the state holds and then changes: drawing it as a line would
 * claim a value between the two that the chain never had. It is the background
 * the arrivals sit on, so a reader sees what the market was doing while it
 * ticked.
 */
export function stateBands(
    samples: readonly StateSample[],
    seconds: number,
    now: number,
): readonly StateBand[] {
    const span = seconds * 1000;
    if (span <= 0 || samples.length === 0) {
        return [];
    }
    const placed = samples
        .filter((sample) => now - sample.at <= span)
        .map((sample) => ({ x: 100 * (1 - (now - sample.at) / span), state: sample.state }))
        .sort((left, right) => left.x - right.x);
    if (placed.length === 0) {
        return [];
    }
    const bands: StateBand[] = [];
    const first = placed[0];
    if (first === undefined) {
        return [];
    }
    bands.push({ from: 0, to: first.x, state: first.state });
    for (let i = 0; i < placed.length; i += 1) {
        const here = placed[i];
        const next = placed[i + 1];
        if (here === undefined) {
            continue;
        }
        bands.push({ from: here.x, to: next === undefined ? 100 : next.x, state: here.state });
    }
    return bands;
}

/** One asset class's arrival rate over the window, in ticks a second per feed. */
export interface RateCurve {
    readonly assetClass: string;
    /** The rate at the middle of each bin, oldest first. */
    readonly points: readonly number[];
}

/** What the rate curves show: a line for each asset class, and the peak they reach. */
export interface RateChart {
    readonly curves: readonly RateCurve[];
    readonly peak: number;
}

/**
 * How fast each asset class's feeds ticked, bin by bin, in ticks a second per
 * feed.
 *
 * A rate, because that is the variable a market-wide factor moves: the dot rows
 * below say when each tick happened, and this says how fast they came. The mean
 * is per feed, so a class with more feeds does not look busier than one with
 * fewer.
 */
export function rateCurves(
    feeds: readonly {
        readonly assetClass: string;
        readonly running: boolean;
        readonly ticks: readonly string[];
    }[],
    seconds: number,
    now: number,
    bins = 48,
): RateChart {
    const span = seconds * 1000;
    if (span <= 0 || bins <= 0) {
        return { curves: [], peak: 0 };
    }
    const start = now - span;
    const binMs = span / bins;
    const byClass = new Map<string, { running: number; counts: number[] }>();
    for (const feed of feeds) {
        if (!feed.running) {
            continue;
        }
        const held = byClass.get(feed.assetClass) ?? {
            running: 0,
            counts: new Array<number>(bins).fill(0),
        };
        held.running += 1;
        for (const tick of feed.ticks) {
            const at = Date.parse(tick);
            if (!Number.isFinite(at) || at < start || at > now) {
                continue;
            }
            const bin = Math.min(bins - 1, Math.floor((at - start) / binMs));
            held.counts[bin] = (held.counts[bin] ?? 0) + 1;
        }
        byClass.set(feed.assetClass, held);
    }
    const curves: RateCurve[] = [];
    let peak = 0;
    for (const [assetClass, held] of byClass) {
        const points = held.counts.map((count) => count / held.running / (binMs / 1000));
        peak = Math.max(peak, ...points);
        curves.push({ assetClass, points });
    }
    return { curves, peak };
}

/** The path a curve draws in the raster's own units: 0 to 100 across, 0 to 1 up. */
export function curvePath(points: readonly number[], peak: number): string {
    if (points.length === 0) {
        return '';
    }
    const scale = peak <= 0 ? 0 : 1 / peak;
    return points
        .map((value, index) => {
            const x = ((index + 0.5) / points.length) * 100;
            const y = 1 - value * scale;
            return `${index === 0 ? 'M' : 'L'} ${x.toFixed(2)} ${y.toFixed(3)}`;
        })
        .join(' ');
}

/** One reading of the shared heat, as the service took it. */
export interface HeatSample {
    /** When the service took the reading. */
    readonly at: string;
    /** The shared heat then, in units of a rate. */
    readonly heat: number;
}

/**
 * The heat over the window, oldest first, at the service's own sample rate.
 *
 * A reading older than the window has left it, and one stamped after the clock
 * the picture is drawn at has not arrived; both are dropped, as an arrival is.
 */
export function heatSeries(
    samples: readonly HeatSample[],
    seconds: number,
    now: number,
): readonly number[] {
    const span = seconds * 1000;
    if (span <= 0) {
        return [];
    }
    return samples
        .map((sample) => ({ at: Date.parse(sample.at), heat: sample.heat }))
        .filter(
            (sample) => Number.isFinite(sample.at) && now - sample.at <= span && sample.at <= now,
        )
        .sort((left, right) => left.at - right.at)
        .map((sample) => sample.heat);
}

/** The arrivals that landed at one instant, and how many of them there were. */
export interface MarkGroup {
    readonly position: number;
    readonly count: number;
}

/**
 * The arrivals grouped by the instant they landed at, oldest first.
 *
 * One curve tick publishes a tick for every pillar of its curve, so a row can
 * receive a dozen arrivals with the same timestamp: drawn as a dozen dots they
 * are one dot overdraw, which claims one event where the market published
 * twelve. Grouping lets the picture say how many landed together.
 */
export function markGroups(
    ticks: readonly string[],
    seconds: number,
    now: number,
): readonly MarkGroup[] {
    const span = seconds * 1000;
    if (span <= 0) {
        return [];
    }
    const byInstant = new Map<string, number>();
    for (const tick of ticks) {
        const at = Date.parse(tick);
        if (!Number.isFinite(at)) {
            continue;
        }
        const age = now - at;
        if (age < 0 || age > span) {
            continue;
        }
        byInstant.set(tick, (byInstant.get(tick) ?? 0) + 1);
    }
    return [...byInstant.entries()]
        .map(([tick, count]) => ({ position: 1 - (now - Date.parse(tick)) / span, count }))
        .sort((left, right) => left.position - right.position);
}
