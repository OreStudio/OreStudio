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

import type { MarketState, TraceEvent, TraceFeed } from '../../api/client.js';
import type { StateBand } from './marketRaster.js';

/** One step of the pace, from one decision to the next, in the raster's units. */
export interface PaceStep {
    /** Where the step starts: 0 at the oldest edge of the window and 100 at now. */
    readonly from: number;
    readonly to: number;
    /** The regime's part of the pace. */
    readonly multiplier: number;
    /** The heat's part: the pace less the multiplier. */
    readonly heat: number;
}

/** The pace over the window, and the most it reached. */
export interface PaceChart {
    readonly steps: readonly PaceStep[];
    readonly peak: number;
}

/** An event's place in the window: 0 at the oldest edge and 100 at now, or nothing outside it. */
function placed(event: TraceEvent, seconds: number, now: number): number | undefined {
    const at = Date.parse(event.at);
    const span = seconds * 1000;
    if (!Number.isFinite(at) || span <= 0 || now - at > span || at > now) {
        return undefined;
    }
    return 100 * (1 - (now - at) / span);
}

/**
 * How fast market time ran over the window, split into the regime's part and the heat's.
 *
 * Every decision records the pace in force after it, so the pace is a step that holds from
 * one decision to the next. The heat decays between them, so a step is the pace as the loop
 * last saw it, which is what the feeds were scheduled at.
 */
export function paceSteps(events: readonly TraceEvent[], seconds: number, now: number): PaceChart {
    const points = events
        .map((event) => ({ x: placed(event, seconds, now), event }))
        .filter((point): point is { x: number; event: TraceEvent } => point.x !== undefined)
        .sort((left, right) => left.x - right.x);
    const steps: PaceStep[] = [];
    let peak = 0;
    for (let i = 0; i < points.length; i += 1) {
        const here = points[i];
        if (here === undefined) {
            continue;
        }
        const to = points[i + 1]?.x ?? 100;
        const heat = Math.max(0, here.event.pace - here.event.multiplier);
        peak = Math.max(peak, here.event.pace);
        steps.push({ from: here.x, to, multiplier: here.event.multiplier, heat });
    }
    return { steps, peak };
}

/**
 * The stretches the market held each state, from the scheduler's own record.
 *
 * Every decision carries the state after it, so a band runs from one decision to the next
 * and a turn is placed at the instant the chain took it.
 */
export function traceBands(
    events: readonly TraceEvent[],
    seconds: number,
    now: number,
): readonly StateBand[] {
    const points = events
        .map((event) => ({ x: placed(event, seconds, now), state: event.state }))
        .filter((point): point is { x: number; state: MarketState } => point.x !== undefined)
        .sort((left, right) => left.x - right.x);
    const bands: StateBand[] = [];
    for (let i = 0; i < points.length; i += 1) {
        const here = points[i];
        if (here === undefined) {
            continue;
        }
        const to = points[i + 1]?.x ?? 100;
        const last = bands[bands.length - 1];
        if (last !== undefined && last.state === here.state) {
            bands[bands.length - 1] = { ...last, to };
        } else {
            bands.push({ from: here.x, to, state: here.state });
        }
    }
    return bands;
}

/** The decisions a person took or the feeds made: pins and changes of the feeds, newest first. */
export function commandLog(events: readonly TraceEvent[], most = 12): readonly TraceEvent[] {
    return [...events]
        .sort((left, right) => left.tau - right.tau)
        .filter((event) => event.kind !== 'tick' && event.kind !== 'transition')
        .slice(-most)
        .reverse();
}

/** The span of market time the window covers: the first and the last tau the trace saw. */
export function tauSpan(events: readonly TraceEvent[]): { from: number; to: number } | undefined {
    if (events.length === 0) {
        return undefined;
    }
    let from = Number.POSITIVE_INFINITY;
    let to = Number.NEGATIVE_INFINITY;
    for (const event of events) {
        from = Math.min(from, event.tau);
        to = Math.max(to, event.tau);
    }
    return to > from ? { from, to } : undefined;
}

/** A feed's ticks placed by market time: 0 at the first tau of the window and 1 at the last. */
export function tauPositions(
    events: readonly TraceEvent[],
    sourceName: string,
    span: { from: number; to: number },
): readonly number[] {
    const width = span.to - span.from;
    return events
        .filter((event) => event.kind === 'tick' && event.sourceName === sourceName)
        .map((event) => (event.tau - span.from) / width)
        .filter((position) => position >= 0 && position <= 1)
        .sort((left, right) => left - right);
}

/** What a feed did over the window against what its law asks for. */
export interface FeedFit {
    readonly observed: number;
    /** The rate times the sensitivity times the market time that passed. */
    readonly expected: number;
    /** The compensated gaps, ascending; Exp(1) when the feed follows its law. */
    readonly gaps: readonly number[];
}

/**
 * A feed's ticks over the window against its law.
 *
 * Every law's long-run rate is its configured rate in operational time, and a feed's
 * operational time runs at its sensitivity times tau, so the count to expect is the rate
 * times the sensitivity times the tau the window spans. The compensated gaps skip a feed's
 * first tick in the window, whose gap began before it.
 */
export function feedFit(
    events: readonly TraceEvent[],
    feed: TraceFeed,
    span: { from: number; to: number },
): FeedFit {
    const ticks = events
        .filter((event) => event.kind === 'tick' && event.sourceName === feed.sourceName)
        .sort((left, right) => left.tau - right.tau);
    return {
        observed: ticks.length,
        expected: feed.rate * feed.sensitivity * (span.to - span.from),
        gaps: ticks
            .slice(1)
            .map((event) => event.compensatedGap)
            .sort((left, right) => left - right),
    };
}

/** Points of a QQ plot of ascending gaps against Exp(1): the theoretical then the observed. */
/** The fewest gaps a QQ plot is drawn from: fewer say nothing about a law's shape. */
export const QQ_MINIMUM_GAPS = 5;

export function exponentialQuantiles(
    gaps: readonly number[],
): readonly { readonly expected: number; readonly observed: number }[] {
    const n = gaps.length;
    return gaps.map((observed, i) => ({
        expected: -Math.log(1 - (i + 0.5) / n),
        observed,
    }));
}

/** A feed's law in a few words: its kind, its rate, its sensitivity, and the rate it runs at now. */
export function lawText(
    feed: TraceFeed,
    t: (key: string, values?: Readonly<Record<string, string | number>>) => string,
): string {
    const base = t('marketdata.marketRegime.law', {
        mode: t(`marketdata.marketRegime.modes.${feed.mode}`),
        rate: feed.rate.toFixed(2),
        sensitivity: feed.sensitivity.toFixed(2),
        now: feed.effectiveRate.toFixed(2),
    });
    if (feed.mode === 'periodic') {
        return `${base} ${t('marketdata.marketRegime.phase', { phase: feed.phase.toFixed(2) })}`;
    }
    if (feed.mode === 'hawkes') {
        return `${base} ${t('marketdata.marketRegime.excess', { excess: feed.excess.toFixed(2) })}`;
    }
    return base;
}
