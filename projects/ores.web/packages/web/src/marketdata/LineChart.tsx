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

const WIDTH = 600;
const HEIGHT = 240;
const TOP = 8;
const BOTTOM = 24;
/** The margin on the side that carries the value labels, and on the other side. */
const AXIS_MARGIN = 64;
const BARE_MARGIN = 8;
/** How many value levels a gridded chart draws and labels, the low and high ends included. */
const GRID_LEVELS = 5;

/** The colours a chart draws its series in, in turn: the theme's, so both themes read. */
export const SERIES_COLOURS = [
    'var(--color-accent)',
    'var(--color-up)',
    'var(--color-warn)',
    'var(--color-down)',
    'var(--color-roadmap)',
    'var(--color-accent-bright)',
    'var(--color-ink-muted)',
] as const;

export interface ChartSeries {
    readonly points: readonly number[];
    readonly colour: string;
    readonly dashed?: boolean;
    readonly strong?: boolean;
    /** Shades the area under the line, for the one series a chart is about. */
    readonly fill?: boolean;
}

/** Where a value falls between the low and high ends, in chart units; a flat range sits in the middle. */
function scale(value: number, low: number, high: number, from: number, to: number): number {
    return high === low ? (from + to) / 2 : from + ((value - low) / (high - low)) * (to - from);
}

/**
 * Lines drawn over one shared x range, with the low and high values on the
 * y axis and the first and last x labels under it. Every series has as many
 * points as `xs`, or as the first series when `xs` is not given.
 *
 * A price chart puts the values on the right, as a trading screen does, and
 * draws a grid of labelled levels across it with the middle x label too.
 */
export function LineChart({
    series,
    xs,
    format,
    xFormat,
    label,
    price = false,
}: {
    readonly series: readonly ChartSeries[];
    readonly xs?: readonly number[];
    readonly format: (value: number) => string;
    readonly xFormat?: (value: number) => string;
    /** What the chart shows, for a reader that does not see it. */
    readonly label: string;
    readonly price?: boolean;
}): ReactNode {
    const length = xs?.length ?? series[0]?.points.length ?? 0;
    if (length < 2) {
        return null;
    }
    const left = price ? BARE_MARGIN : AXIS_MARGIN;
    const right = price ? AXIS_MARGIN : BARE_MARGIN;
    const xAt = xs ?? Array.from({ length }, (_, i) => i);
    const values = series.flatMap((one) => one.points);
    const low = Math.min(...values);
    const high = Math.max(...values);
    const xLow = xAt[0] ?? 0;
    const xHigh = xAt[length - 1] ?? 1;
    const xOf = (i: number): number => scale(xAt[i] ?? i, xLow, xHigh, left, WIDTH - right);
    const yOf = (value: number): number => scale(value, low, high, HEIGHT - BOTTOM, TOP);
    const pathOf = (points: readonly number[]): string =>
        points
            .map(
                (value, i) =>
                    `${i === 0 ? 'M' : 'L'} ${xOf(i).toFixed(1)} ${yOf(value).toFixed(1)}`,
            )
            .join(' ');
    const areaOf = (points: readonly number[]): string =>
        `${pathOf(points)} L ${xOf(points.length - 1).toFixed(1)} ${String(HEIGHT - BOTTOM)} L ${xOf(0).toFixed(1)} ${String(HEIGHT - BOTTOM)} Z`;
    const showX = xFormat ?? ((value: number) => String(value));
    const levels = price
        ? Array.from(
              { length: GRID_LEVELS },
              (_, i) => low + ((high - low) * i) / (GRID_LEVELS - 1),
          )
        : [low, high];
    const axisX = price ? WIDTH - right : left;
    return (
        <svg
            role="img"
            aria-label={label}
            className="h-auto w-full"
            viewBox={`0 0 ${String(WIDTH)} ${String(HEIGHT)}`}
        >
            {price &&
                levels.map((level) => (
                    <line
                        key={level}
                        x1={left}
                        x2={WIDTH - right}
                        y1={yOf(level)}
                        y2={yOf(level)}
                        stroke="var(--color-line-subtle)"
                        vectorEffect="non-scaling-stroke"
                    />
                ))}
            <line
                x1={axisX}
                x2={axisX}
                y1={TOP}
                y2={HEIGHT - BOTTOM}
                stroke="var(--color-line)"
                vectorEffect="non-scaling-stroke"
            />
            <line
                x1={left}
                x2={WIDTH - right}
                y1={HEIGHT - BOTTOM}
                y2={HEIGHT - BOTTOM}
                stroke="var(--color-line)"
                vectorEffect="non-scaling-stroke"
            />
            <g fill="var(--color-ink-faint)" fontSize="11" fontFamily="monospace">
                {levels.map((level) => (
                    <text
                        key={level}
                        x={price ? axisX + 6 : axisX - 6}
                        y={Math.min(Math.max(yOf(level) + 4, TOP + 10), HEIGHT - BOTTOM)}
                        textAnchor={price ? 'start' : 'end'}
                    >
                        {format(level)}
                    </text>
                ))}
                <text x={left} y={HEIGHT - 6}>
                    {showX(xLow)}
                </text>
                {price && (
                    <text x={(left + WIDTH - right) / 2} y={HEIGHT - 6} textAnchor="middle">
                        {showX((xLow + xHigh) / 2)}
                    </text>
                )}
                <text x={WIDTH - right} y={HEIGHT - 6} textAnchor="end">
                    {showX(xHigh)}
                </text>
            </g>
            {series.map((one, i) =>
                one.fill === true ? (
                    <path
                        key={`fill-${String(i)}`}
                        d={areaOf(one.points)}
                        fill={one.colour}
                        fillOpacity={0.08}
                        stroke="none"
                    />
                ) : null,
            )}
            {series.map((one, i) => (
                <path
                    key={i}
                    d={pathOf(one.points)}
                    fill="none"
                    stroke={one.colour}
                    strokeWidth={one.strong === true ? 2.4 : 1.4}
                    {...(one.dashed === true ? { strokeDasharray: '4 3' } : {})}
                    vectorEffect="non-scaling-stroke"
                />
            ))}
        </svg>
    );
}
