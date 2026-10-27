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

import { useEffect, useState, type ReactNode } from 'react';

/** The width drawn before the chart has measured its box, and on a server render. */
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
    /**
     * The values, one for each x. A null is a gap: the series does not hold that
     * point, and the line breaks there rather than falling to zero, because a
     * point that is absent and a point at zero are different facts.
     */
    readonly points: readonly (number | null)[];
    readonly colour: string;
    readonly dashed?: boolean;
    readonly strong?: boolean;
    /** Shades the area under the line, for the one series a chart is about. */
    readonly fill?: boolean;
    /** How opaque the series is, from 0 to 1; a curve fades with age. */
    readonly opacity?: number;
    /** Draws a dot at each point, as a chart of a few points does. */
    readonly dots?: boolean;
    /** The width of the line, when neither the default nor `strong` fits. */
    readonly width?: number;
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
    categories,
    padding = 0,
}: {
    readonly series: readonly ChartSeries[];
    readonly xs?: readonly number[];
    readonly format: (value: number) => string;
    readonly xFormat?: (value: number) => string;
    /** What the chart shows, for a reader that does not see it. */
    readonly label: string;
    readonly price?: boolean;
    /**
     * Names the points of a chart whose x axis is a list and not a scale, such
     * as the tenors of a curve: every name is drawn under its point, and the
     * points are spaced evenly.
     */
    readonly categories?: readonly string[];
    /** How far past the low and the high values the y axis reaches, as a share of their range. */
    readonly padding?: number;
}): ReactNode {
    const width = useWidth();
    const length = categories?.length ?? xs?.length ?? series[0]?.points.length ?? 0;
    if (length < 2) {
        return null;
    }
    const left = price ? BARE_MARGIN : AXIS_MARGIN;
    const right = price ? AXIS_MARGIN : BARE_MARGIN;
    const xAt = xs ?? Array.from({ length }, (_, i) => i);
    const values = series.flatMap((one) => one.points).filter((value) => value !== null);
    const lowest = Math.min(...values);
    const highest = Math.max(...values);
    const room =
        highest === lowest
            ? Math.max(Math.abs(highest) * 0.0005, 1e-6)
            : (highest - lowest) * padding;
    const low = lowest - room;
    const high = highest + room;
    const xLow = xAt[0] ?? 0;
    const xHigh = xAt[length - 1] ?? 1;
    const xOf = (i: number): number => scale(xAt[i] ?? i, xLow, xHigh, left, width.value - right);
    const yOf = (value: number): number => scale(value, low, high, HEIGHT - BOTTOM, TOP);
    // A gap starts a new subpath, so the line breaks where the series holds no
    // point and joins nothing across it.
    const pathOf = (points: readonly (number | null)[]): string =>
        points
            .map((value, i) =>
                value === null
                    ? ''
                    : `${i === 0 || points[i - 1] === null ? 'M' : 'L'} ${xOf(i).toFixed(1)} ${yOf(value).toFixed(1)}`,
            )
            .filter((part) => part !== '')
            .join(' ');
    const areaOf = (points: readonly (number | null)[]): string =>
        `${pathOf(points)} L ${xOf(points.length - 1).toFixed(1)} ${String(HEIGHT - BOTTOM)} L ${xOf(0).toFixed(1)} ${String(HEIGHT - BOTTOM)} Z`;
    const showX = xFormat ?? ((value: number) => String(value));
    const levels = price
        ? Array.from(
              { length: GRID_LEVELS },
              (_, i) => low + ((high - low) * i) / (GRID_LEVELS - 1),
          )
        : [low, high];
    const axisX = price ? width.value - right : left;
    return (
        <svg
            ref={width.ref}
            role="img"
            aria-label={label}
            className="w-full"
            height={HEIGHT}
            viewBox={`0 0 ${String(width.value)} ${String(HEIGHT)}`}
        >
            {price &&
                levels.map((level) => (
                    <line
                        key={level}
                        x1={left}
                        x2={width.value - right}
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
                x2={width.value - right}
                y1={HEIGHT - BOTTOM}
                y2={HEIGHT - BOTTOM}
                stroke="var(--color-line)"
                vectorEffect="non-scaling-stroke"
            />
            <g fill="var(--color-ink-faint)" className="font-mono text-xs">
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
                {categories === undefined && (
                    <text x={left} y={HEIGHT - 6}>
                        {showX(xLow)}
                    </text>
                )}
                {categories?.map((name, i) => (
                    <text
                        key={`${name}-${String(i)}`}
                        x={xOf(i)}
                        y={HEIGHT - 6}
                        textAnchor="middle"
                    >
                        {name}
                    </text>
                ))}
                {categories === undefined && price && (
                    <text x={(left + width.value - right) / 2} y={HEIGHT - 6} textAnchor="middle">
                        {showX((xLow + xHigh) / 2)}
                    </text>
                )}
                {categories === undefined && (
                    <text x={width.value - right} y={HEIGHT - 6} textAnchor="end">
                        {showX(xHigh)}
                    </text>
                )}
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
                    strokeWidth={one.width ?? (one.strong === true ? 2.4 : 1.4)}
                    {...(one.opacity === undefined ? {} : { strokeOpacity: one.opacity })}
                    {...(one.dashed === true ? { strokeDasharray: '4 3' } : {})}
                    vectorEffect="non-scaling-stroke"
                />
            ))}
            {series.map((one, i) =>
                one.dots === true
                    ? one.points.map((value, j) =>
                          value === null ? null : (
                              <circle
                                  key={`dot-${String(i)}-${String(j)}`}
                                  cx={xOf(j)}
                                  cy={yOf(value)}
                                  r={one.strong === true ? 3.5 : 2.5}
                                  fill={one.colour}
                                  {...(one.opacity === undefined
                                      ? {}
                                      : { fillOpacity: one.opacity })}
                              />
                          ),
                      )
                    : null,
            )}
        </svg>
    );
}

/**
 * The chart's own width in pixels, so the chart is drawn at the size it is
 * shown and its labels keep the page's type size at any width.
 */
function useWidth(): {
    readonly ref: (element: SVGSVGElement | null) => void;
    readonly value: number;
} {
    const [element, setElement] = useState<SVGSVGElement | null>(null);
    const [value, setValue] = useState(WIDTH);
    useEffect(() => {
        if (element === null || typeof ResizeObserver === 'undefined') {
            return undefined;
        }
        const observer = new ResizeObserver(([entry]) => {
            const measured = entry?.contentRect.width ?? 0;
            if (measured > 0) {
                setValue(Math.round(measured));
            }
        });
        observer.observe(element);
        return () => observer.disconnect();
    }, [element]);
    return { ref: setElement, value };
}
