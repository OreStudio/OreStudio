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

import type { ReactNode } from 'react';
import { fromWireTimestamp, isWireTimestamp } from '@ores/wire-protocol/browser';

/**
 * A chart of columns on a regular time axis.
 *
 * The data arrives the way the grid series read serves it: one array per
 * counter, bucket i starting at the axis start plus i buckets, and a `samples`
 * column that says which buckets hold a sample at all. The chart reads the
 * columns as they are and draws no row objects between them and the picture.
 *
 * A bucket with no sample is a gap and is never a zero. The lines and areas
 * break at it, the bars leave it blank, and a faint band marks it, so a quiet
 * stretch cannot be read as an idle grid. A value of null is a bucket whose
 * figure is unknown, such as the change since a bucket that was not sampled,
 * and it is treated as a gap too.
 */

export type ChartTone = 'accent' | 'warn' | 'muted' | 'up' | 'down' | 'neutral';
export type ChartKind = 'stacked-area' | 'lines' | 'stacked-bars';

export interface ChartSeries {
    readonly key: string;
    readonly label: string;
    readonly values: readonly (number | null)[];
    readonly tone: ChartTone;
}

export interface ChartAxis {
    /** The wire timestamp at which bucket 0 starts. */
    readonly startTime: string;
    readonly bucketSeconds: number;
    readonly count: number;
}

const WIDTH = 640;
const HEIGHT = 168;
const PAD_LEFT = 44;
const PAD_RIGHT = 8;
const PAD_TOP = 8;
const PAD_BOTTOM = 22;
const PLOT_WIDTH = WIDTH - PAD_LEFT - PAD_RIGHT;
const PLOT_HEIGHT = HEIGHT - PAD_TOP - PAD_BOTTOM;

const TONE_CLASS: Readonly<Record<ChartTone, string>> = {
    accent: 'text-accent',
    warn: 'text-warn',
    muted: 'text-ink-faint',
    up: 'text-up',
    down: 'text-down',
    neutral: 'text-ink-muted',
};

/** Whether bucket i can be drawn: it holds a sample and every series has a figure for it. */
export function drawable(
    samples: readonly number[],
    series: readonly ChartSeries[],
    index: number,
): boolean {
    return (samples[index] ?? 0) > 0 && series.every((entry) => entry.values[index] != null);
}

/** The runs of consecutive drawable buckets, as inclusive [first, last] index pairs. */
export function drawableRuns(
    samples: readonly number[],
    series: readonly ChartSeries[],
    count: number,
): readonly (readonly [number, number])[] {
    const runs: (readonly [number, number])[] = [];
    let first = -1;
    for (let index = 0; index < count; index += 1) {
        if (drawable(samples, series, index)) {
            if (first < 0) {
                first = index;
            }
        } else if (first >= 0) {
            runs.push([first, index - 1]);
            first = -1;
        }
    }
    if (first >= 0) {
        runs.push([first, count - 1]);
    }
    return runs;
}

/** The top of the plot: the tallest stack, or the tallest single line, in the drawable buckets. */
export function chartMaximum(
    kind: ChartKind,
    samples: readonly number[],
    series: readonly ChartSeries[],
    count: number,
): number {
    let top = 0;
    for (let index = 0; index < count; index += 1) {
        if (!drawable(samples, series, index)) {
            continue;
        }
        const figures = series.map((entry) => entry.values[index] ?? 0);
        const height =
            kind === 'lines'
                ? Math.max(...figures)
                : figures.reduce((sum, value) => sum + value, 0);
        top = Math.max(top, height);
    }
    return top;
}

/** The x of the middle of bucket i, in the chart's own units. */
export function bucketCentre(index: number, count: number): number {
    return PAD_LEFT + ((index + 0.5) * PLOT_WIDTH) / Math.max(count, 1);
}

function yOf(value: number, maximum: number): number {
    const share = maximum <= 0 ? 0 : Math.min(value / maximum, 1);
    return PAD_TOP + PLOT_HEIGHT - share * PLOT_HEIGHT;
}

function fixed(value: number): string {
    return value.toFixed(1);
}

/**
 * The outline of one stacked layer over a run of buckets.
 *
 * The layer sits between the running total below it and the total with itself
 * added. A run of one bucket has no width to draw between, so it is drawn as a
 * short slab around the bucket's centre.
 */
export function layerPath(
    run: readonly [number, number],
    lower: readonly number[],
    upper: readonly number[],
    count: number,
    maximum: number,
): string {
    const [first, last] = run;
    const points: { readonly x: number; readonly index: number }[] = [];
    if (first === last) {
        const half = (0.3 * PLOT_WIDTH) / Math.max(count, 1);
        const centre = bucketCentre(first, count);
        points.push({ x: centre - half, index: first }, { x: centre + half, index: first });
    } else {
        for (let index = first; index <= last; index += 1) {
            points.push({ x: bucketCentre(index, count), index });
        }
    }
    const top = points.map(
        ({ x, index }) => `${fixed(x)} ${fixed(yOf(upper[index] ?? 0, maximum))}`,
    );
    const bottom = points
        .map(({ x, index }) => `${fixed(x)} ${fixed(yOf(lower[index] ?? 0, maximum))}`)
        .reverse();
    return `M ${top.join(' L ')} L ${bottom.join(' L ')} Z`;
}

/** The line of one series over a run of buckets; a single bucket is a short stroke. */
export function linePath(
    run: readonly [number, number],
    values: readonly (number | null)[],
    count: number,
    maximum: number,
): string {
    const [first, last] = run;
    if (first === last) {
        const half = (0.3 * PLOT_WIDTH) / Math.max(count, 1);
        const centre = bucketCentre(first, count);
        const y = fixed(yOf(values[first] ?? 0, maximum));
        return `M ${fixed(centre - half)} ${y} L ${fixed(centre + half)} ${y}`;
    }
    const points: string[] = [];
    for (let index = first; index <= last; index += 1) {
        points.push(
            `${fixed(bucketCentre(index, count))} ${fixed(yOf(values[index] ?? 0, maximum))}`,
        );
    }
    return `M ${points.join(' L ')}`;
}

/** The start of bucket i as an instant. */
export function bucketInstant(axis: ChartAxis, index: number): Date {
    const start = isWireTimestamp(axis.startTime)
        ? fromWireTimestamp(axis.startTime).getTime()
        : Number.NaN;
    return new Date(start + index * axis.bucketSeconds * 1000);
}

/**
 * The label under the axis: the time in UTC, with the date when the buckets are
 * half an hour or wider, because such a range spans days.
 */
export function axisLabel(axis: ChartAxis, index: number): string {
    const instant = bucketInstant(axis, index);
    if (Number.isNaN(instant.getTime())) {
        return '';
    }
    const iso = instant.toISOString();
    const time = iso.slice(11, 16);
    return axis.bucketSeconds >= 1800 ? `${iso.slice(5, 10)} ${time}` : time;
}

/** The gap bands: one faint rectangle per run of buckets that cannot be drawn. */
function gapBands(
    runs: readonly (readonly [number, number])[],
    count: number,
): readonly (readonly [number, number])[] {
    const bands: (readonly [number, number])[] = [];
    let cursor = 0;
    for (const [first, last] of runs) {
        if (first > cursor) {
            bands.push([cursor, first - 1]);
        }
        cursor = last + 1;
    }
    if (cursor < count) {
        bands.push([cursor, count - 1]);
    }
    return bands;
}

function bandX(first: number, last: number, count: number): { x: number; width: number } {
    const step = PLOT_WIDTH / Math.max(count, 1);
    return { x: PAD_LEFT + first * step, width: (last - first + 1) * step };
}

/** The legend: one swatch and name per series. */
function Legend({ series }: { readonly series: readonly ChartSeries[] }): ReactNode {
    return (
        <ul className="flex flex-wrap gap-x-4 gap-y-1 text-xs text-ink-muted">
            {series.map((entry) => (
                <li key={entry.key} className="flex items-center gap-1.5">
                    <span
                        aria-hidden="true"
                        className={`inline-block h-2 w-3 rounded-sm bg-current ${TONE_CLASS[entry.tone]}`}
                    />
                    {entry.label}
                </li>
            ))}
        </ul>
    );
}

/**
 * One chart. It stacks areas or bars, or draws lines, over the buckets that hold
 * a sample, and marks the buckets that hold none.
 */
export function SeriesChart({
    kind,
    axis,
    samples,
    series,
    caption,
    format = String,
}: {
    readonly kind: ChartKind;
    readonly axis: ChartAxis;
    readonly samples: readonly number[];
    readonly series: readonly ChartSeries[];
    readonly caption: string;
    /** How the top of the scale is written; a count by default. */
    readonly format?: (value: number) => string;
}): ReactNode {
    const count = axis.count;
    const runs = drawableRuns(samples, series, count);
    const maximum = chartMaximum(kind, samples, series, count);
    const bands = gapBands(runs, count);

    // Running totals, bottom layer first, for the stacked kinds.
    const totals: number[][] = [];
    let running: number[] = Array.from({ length: count }, () => 0);
    for (const entry of series) {
        totals.push(running);
        running = running.map((below, index) => below + (entry.values[index] ?? 0));
    }
    const tops = series.map((_, layer) => {
        const below = totals[layer] ?? [];
        const own = series[layer]?.values ?? [];
        return below.map((value, index) => value + (own[index] ?? 0));
    });

    const barWidth = (0.8 * PLOT_WIDTH) / Math.max(count, 1);
    const midpoint = Math.floor((count - 1) / 2);

    return (
        <div className="space-y-2">
            <svg viewBox={`0 0 ${WIDTH} ${HEIGHT}`} className="h-44 w-full" role="img">
                <title>{caption}</title>
                {bands.map(([first, last]) => {
                    const { x, width } = bandX(first, last, count);
                    return (
                        <rect
                            key={`gap-${first}`}
                            data-gap=""
                            x={fixed(x)}
                            y={PAD_TOP}
                            width={fixed(width)}
                            height={PLOT_HEIGHT}
                            className="fill-current text-ink-faint"
                            opacity="0.08"
                        />
                    );
                })}
                <line
                    x1={PAD_LEFT}
                    x2={WIDTH - PAD_RIGHT}
                    y1={PAD_TOP + PLOT_HEIGHT}
                    y2={PAD_TOP + PLOT_HEIGHT}
                    className="stroke-current text-line"
                    strokeWidth="1"
                />
                <text
                    x={PAD_LEFT - 6}
                    y={PAD_TOP + 8}
                    textAnchor="end"
                    className="fill-current text-ink-faint"
                    fontSize="10"
                >
                    {format(maximum)}
                </text>
                <text
                    x={PAD_LEFT - 6}
                    y={PAD_TOP + PLOT_HEIGHT}
                    textAnchor="end"
                    className="fill-current text-ink-faint"
                    fontSize="10"
                >
                    0
                </text>
                {kind === 'stacked-area' &&
                    series.map((entry, layer) =>
                        runs.map((run) => (
                            <path
                                key={`${entry.key}-${run[0]}`}
                                d={layerPath(
                                    run,
                                    totals[layer] ?? [],
                                    tops[layer] ?? [],
                                    count,
                                    maximum,
                                )}
                                className={`fill-current ${TONE_CLASS[entry.tone]}`}
                                opacity="0.55"
                            />
                        )),
                    )}
                {kind === 'lines' &&
                    series.map((entry) =>
                        runs.map((run) => (
                            <path
                                key={`${entry.key}-${run[0]}`}
                                d={linePath(run, entry.values, count, maximum)}
                                fill="none"
                                className={`stroke-current ${TONE_CLASS[entry.tone]}`}
                                strokeWidth="2"
                            />
                        )),
                    )}
                {kind === 'stacked-bars' &&
                    series.map((entry, layer) =>
                        runs.flatMap(([first, last]) =>
                            Array.from({ length: last - first + 1 }, (_, offset) => {
                                const index = first + offset;
                                const bottom = totals[layer]?.[index] ?? 0;
                                const top = tops[layer]?.[index] ?? 0;
                                if (top <= bottom) {
                                    return null;
                                }
                                return (
                                    <rect
                                        key={`${entry.key}-${index}`}
                                        x={fixed(bucketCentre(index, count) - barWidth / 2)}
                                        y={fixed(yOf(top, maximum))}
                                        width={fixed(barWidth)}
                                        height={fixed(yOf(bottom, maximum) - yOf(top, maximum))}
                                        className={`fill-current ${TONE_CLASS[entry.tone]}`}
                                        opacity="0.85"
                                    />
                                );
                            }),
                        ),
                    )}
                {[0, midpoint, count - 1].map((index, position) => (
                    <text
                        key={`tick-${position}`}
                        x={fixed(bucketCentre(Math.max(index, 0), count))}
                        y={HEIGHT - 6}
                        textAnchor={position === 0 ? 'start' : position === 2 ? 'end' : 'middle'}
                        className="fill-current text-ink-faint"
                        fontSize="10"
                    >
                        {axisLabel(axis, Math.max(index, 0))}
                    </text>
                ))}
            </svg>
            <Legend series={series} />
        </div>
    );
}
