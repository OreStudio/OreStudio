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
const LEFT = 64;
const RIGHT = 8;
const TOP = 8;
const BOTTOM = 24;

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
}

/** Where a value falls between the low and high ends, in chart units; a flat range sits in the middle. */
function scale(value: number, low: number, high: number, from: number, to: number): number {
    return high === low ? (from + to) / 2 : from + ((value - low) / (high - low)) * (to - from);
}

/**
 * Lines drawn over one shared x range, with the low and high values on the
 * y axis and the first and last x labels under it. Every series has as many
 * points as `xs`, or as the first series when `xs` is not given.
 */
export function LineChart({
    series,
    xs,
    format,
    xFormat,
    label,
}: {
    readonly series: readonly ChartSeries[];
    readonly xs?: readonly number[];
    readonly format: (value: number) => string;
    readonly xFormat?: (value: number) => string;
    /** What the chart shows, for a reader that does not see it. */
    readonly label: string;
}): ReactNode {
    const length = xs?.length ?? series[0]?.points.length ?? 0;
    if (length < 2) {
        return null;
    }
    const xAt = xs ?? Array.from({ length }, (_, i) => i);
    const values = series.flatMap((one) => one.points);
    const low = Math.min(...values);
    const high = Math.max(...values);
    const xLow = xAt[0] ?? 0;
    const xHigh = xAt[length - 1] ?? 1;
    const pathOf = (points: readonly number[]): string =>
        points
            .map((value, i) => {
                const x = scale(xAt[i] ?? i, xLow, xHigh, LEFT, WIDTH - RIGHT);
                const y = scale(value, low, high, HEIGHT - BOTTOM, TOP);
                return `${i === 0 ? 'M' : 'L'} ${x.toFixed(1)} ${y.toFixed(1)}`;
            })
            .join(' ');
    const showX = xFormat ?? ((value: number) => String(value));
    return (
        <svg
            role="img"
            aria-label={label}
            className="h-auto w-full"
            viewBox={`0 0 ${String(WIDTH)} ${String(HEIGHT)}`}
        >
            <line
                x1={LEFT}
                x2={LEFT}
                y1={TOP}
                y2={HEIGHT - BOTTOM}
                stroke="var(--color-line)"
                vectorEffect="non-scaling-stroke"
            />
            <line
                x1={LEFT}
                x2={WIDTH - RIGHT}
                y1={HEIGHT - BOTTOM}
                y2={HEIGHT - BOTTOM}
                stroke="var(--color-line)"
                vectorEffect="non-scaling-stroke"
            />
            <g fill="var(--color-ink-faint)" fontSize="11" fontFamily="monospace">
                <text x={LEFT - 6} y={TOP + 10} textAnchor="end">
                    {format(high)}
                </text>
                <text x={LEFT - 6} y={HEIGHT - BOTTOM} textAnchor="end">
                    {format(low)}
                </text>
                <text x={LEFT} y={HEIGHT - 6}>
                    {showX(xLow)}
                </text>
                <text x={WIDTH - RIGHT} y={HEIGHT - 6} textAnchor="end">
                    {showX(xHigh)}
                </text>
            </g>
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
