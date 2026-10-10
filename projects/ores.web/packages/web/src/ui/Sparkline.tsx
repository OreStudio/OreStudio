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

const WIDTH = 300;
const HEIGHT = 40;
const PAD = 3;

/**
 * The path of a line through the points, scaled to the box.
 *
 * The lowest point sits at the foot and the highest at the top, so the line
 * shows the shape of the movement and not its size; the figure beside it states
 * the size. A flat series draws a flat line through the middle.
 */
export function sparklinePath(points: readonly number[]): string {
    if (points.length < 2) {
        return '';
    }
    const low = Math.min(...points);
    const high = Math.max(...points);
    const span = high - low;
    const step = WIDTH / (points.length - 1);
    return points
        .map((value, index) => {
            const y =
                span === 0
                    ? HEIGHT / 2
                    : HEIGHT - PAD - ((value - low) / span) * (HEIGHT - 2 * PAD);
            return `${index === 0 ? 'M' : 'L'} ${(index * step).toFixed(1)} ${y.toFixed(1)}`;
        })
        .join(' ');
}

/** A line through the points with the area beneath it tinted; nothing for fewer than two. */
export function Sparkline({ points }: { readonly points: readonly number[] }): ReactNode {
    const path = sparklinePath(points);
    if (path === '') {
        return null;
    }
    return (
        <svg
            aria-hidden="true"
            className="h-12 w-full overflow-visible text-accent"
            viewBox={`0 0 ${WIDTH} ${HEIGHT}`}
            preserveAspectRatio="none"
        >
            <path
                d={`${path} L ${WIDTH} ${HEIGHT} L 0 ${HEIGHT} Z`}
                fill="currentColor"
                opacity="0.1"
            />
            <path
                d={path}
                fill="none"
                stroke="currentColor"
                strokeWidth="2"
                vectorEffect="non-scaling-stroke"
            />
        </svg>
    );
}
