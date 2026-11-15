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
import type { ReactNode } from 'react';

/**
 * A shape with no axes: a series drawn small enough to sit in a table cell.
 *
 * It states the shape and nothing else. The values are the row's own, so a
 * person comparing rows sees which pair moved and which stood still; the
 * numbers are in the columns beside it.
 */
export function Sparkline({
    points,
    label,
    width = 80,
    height = 22,
}: {
    readonly points: readonly number[];
    /** What the shape shows, for a reader that does not see it. */
    readonly label: string;
    readonly width?: number;
    readonly height?: number;
}): ReactNode {
    if (points.length < 2) return null;
    const lowest = Math.min(...points);
    const highest = Math.max(...points);
    const span = highest - lowest;
    const x = (index: number): number => 2 + (index * (width - 4)) / (points.length - 1);
    // A flat day sits on the middle line rather than dividing by zero.
    const y = (value: number): number =>
        span === 0 ? height / 2 : 2 + ((highest - value) / span) * (height - 4);
    const path = points
        .map((value, index) => `${x(index).toFixed(1)},${y(value).toFixed(1)}`)
        .join(' ');
    return (
        <svg
            width={width}
            height={height}
            viewBox={`0 0 ${width} ${height}`}
            role="img"
            aria-label={label}
        >
            <polyline points={path} fill="none" stroke="currentColor" strokeWidth="1.5" />
        </svg>
    );
}
