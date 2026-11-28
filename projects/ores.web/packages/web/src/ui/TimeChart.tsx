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
 * One measurement plotted against the time it was taken.
 *
 * The chart is a reading and not a decoration: the time axis states the window
 * the read covered, the value axis states the scale, and every point draws a mark
 * so the sampling cadence is visible. A line through points that are hours apart
 * is a slope the data does not have, and a reader who cannot see where the marks
 * are cannot tell that.
 *
 * The window comes from the read and never from the browser's clock. A window the
 * read did not state is not drawn at all: guessing one would put the axis in a
 * place the installation never measured.
 */

export interface TimeValue {
    /** The instant the measurement was taken, as the wire states it. */
    readonly at: string;
    readonly value: number;
}

/** The window a series covers, as the read stated it. */
export interface TimeWindow {
    readonly from: string;
    readonly to: string;
}

const WIDTH = 720;
const HEIGHT = 240;
const PAD_LEFT = 68;
const PAD_RIGHT = 12;
const PAD_TOP = 14;
const PAD_BOTTOM = 26;
const PLOT_WIDTH = WIDTH - PAD_LEFT - PAD_RIGHT;
const PLOT_HEIGHT = HEIGHT - PAD_TOP - PAD_BOTTOM;

/** How many labels the value axis carries, zero included. */
export const VALUE_TICKS = 5;

/** How many labels the time axis carries, the window's ends included. */
export const TIME_TICKS = 4;

/** The instant a wire timestamp states, in milliseconds, or NaN when it states none. */
export function instantOf(at: string): number {
    return isWireTimestamp(at) ? fromWireTimestamp(at).getTime() : Number.NaN;
}

/** Whether a window can be drawn: two instants, the first before the second. */
export function drawableWindow(window: TimeWindow): boolean {
    const from = instantOf(window.from);
    const to = instantOf(window.to);
    return Number.isFinite(from) && Number.isFinite(to) && to > from;
}

/**
 * The x of an instant inside the plot, or null when it falls outside the window.
 *
 * A mark outside the window is not drawn and not moved to the edge: a point at the
 * edge would read as a measurement the window holds when the window does not hold
 * it.
 */
export function xOf(
    at: number,
    window: { readonly from: number; readonly to: number },
): number | null {
    if (!Number.isFinite(at) || at < window.from || at > window.to) {
        return null;
    }
    return PAD_LEFT + ((at - window.from) / (window.to - window.from)) * PLOT_WIDTH;
}

/** The y of a value, zero at the foot of the plot and the top value at its head. */
export function yOf(value: number, top: number): number {
    const share = top <= 0 ? 0 : Math.min(Math.max(value, 0) / top, 1);
    return PAD_TOP + PLOT_HEIGHT - share * PLOT_HEIGHT;
}

/** The highest measurement, which is the top of the value axis. */
export function chartTop(points: readonly TimeValue[]): number {
    return points.reduce((top, point) => Math.max(top, point.value), 0);
}

/**
 * The path through the points the window holds, oldest first.
 *
 * The caller states the points in time order, so the path is drawn in that order
 * and never re-sorted here: a series states its own sequence.
 */
export function seriesPath(
    points: readonly TimeValue[],
    window: { readonly from: number; readonly to: number },
    top: number,
): string {
    const drawn = points
        .map((point) => ({ x: xOf(instantOf(point.at), window), y: yOf(point.value, top) }))
        .filter((point): point is { readonly x: number; readonly y: number } => point.x !== null);
    if (drawn.length < 2) {
        return '';
    }
    return drawn
        .map(
            (point, index) =>
                `${index === 0 ? 'M' : 'L'} ${point.x.toFixed(1)} ${point.y.toFixed(1)}`,
        )
        .join(' ');
}

/** The instants the time axis labels, from the window's start to its end. */
export function timeTicks(window: TimeWindow, count = TIME_TICKS): readonly Date[] {
    const from = instantOf(window.from);
    const to = instantOf(window.to);
    if (!Number.isFinite(from) || !Number.isFinite(to) || to < from) {
        return [];
    }
    return Array.from(
        { length: count },
        (_, index) => new Date(from + ((to - from) * index) / (count - 1)),
    );
}

/**
 * The label under the time axis: the date and the time in UTC.
 *
 * Every window the screen offers spans at least a day, so the date is always part
 * of the label; a time alone would name the same hour four times.
 */
export function timeLabel(instant: Date): string {
    if (Number.isNaN(instant.getTime())) {
        return '';
    }
    return instant.toISOString().slice(5, 16).replace('T', ' ');
}

/** One label on each of the time axis' ticks. */
function timeLabels(window: TimeWindow): readonly string[] {
    return timeTicks(window).map(timeLabel);
}

export function TimeChart({
    window,
    points,
    format,
    caption,
}: {
    /** The window the read covered. */
    readonly window: TimeWindow;
    readonly points: readonly TimeValue[];
    /** How a value on the scale is written, such as a size in bytes. */
    readonly format: (value: number) => string;
    /** What the chart is, for a reader who cannot see it. */
    readonly caption: string;
}): ReactNode {
    if (!drawableWindow(window)) {
        return null;
    }
    const from = instantOf(window.from);
    const to = instantOf(window.to);
    const window_ = { from, to };
    const top = chartTop(points);
    const path = seriesPath(points, window_, top);
    const marks = points
        .map((point) => ({ x: xOf(instantOf(point.at), window_), y: yOf(point.value, top) }))
        .filter((mark): mark is { readonly x: number; readonly y: number } => mark.x !== null);
    const labels = timeLabels(window);

    return (
        <svg
            viewBox={`0 0 ${WIDTH} ${HEIGHT}`}
            className="h-60 w-full"
            role="img"
            aria-label={caption}
        >
            <title>{caption}</title>
            {Array.from({ length: VALUE_TICKS }, (_, index) => {
                const share = index / (VALUE_TICKS - 1);
                const y = PAD_TOP + PLOT_HEIGHT - share * PLOT_HEIGHT;
                return (
                    <g key={`value-${index}`}>
                        <line
                            x1={PAD_LEFT}
                            x2={WIDTH - PAD_RIGHT}
                            y1={y}
                            y2={y}
                            className="stroke-current text-line-subtle"
                            strokeWidth="1"
                        />
                        <text
                            x={PAD_LEFT - 6}
                            y={y + 3}
                            textAnchor="end"
                            className="fill-current text-ink-faint"
                            fontSize="10"
                        >
                            {format(top * share)}
                        </text>
                    </g>
                );
            })}
            {labels.map((label, index) => (
                <text
                    key={`time-${index}`}
                    x={PAD_LEFT + (PLOT_WIDTH * index) / (labels.length - 1)}
                    y={HEIGHT - 6}
                    textAnchor={
                        index === 0 ? 'start' : index === labels.length - 1 ? 'end' : 'middle'
                    }
                    className="fill-current text-ink-faint"
                    fontSize="10"
                >
                    {label}
                </text>
            ))}
            {path !== '' && (
                <path d={path} fill="none" className="stroke-current text-accent" strokeWidth="2" />
            )}
            {marks.map((mark, index) => (
                <circle
                    key={`mark-${index}`}
                    cx={mark.x.toFixed(1)}
                    cy={mark.y.toFixed(1)}
                    r="2.5"
                    className="fill-current text-accent"
                />
            ))}
        </svg>
    );
}
