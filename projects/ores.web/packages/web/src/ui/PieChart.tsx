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

/**
 * A pie of named slices.
 *
 * The chart states shares of one whole, so the slices are drawn from the values
 * as they arrive and never re-normalised: if the values add up to less than the
 * whole, the caller passes the remainder as its own slice, because a chart that
 * silently scaled them would draw a share nobody measured.
 *
 * A slice of zero is not drawn, and a value that is not a number is not drawn:
 * the legend states the figure either way, so an absent slice still says nothing
 * is there.
 */

export interface PieSlice {
    readonly key: string;
    readonly label: string;
    readonly value: number;
}

const TONES = [
    'text-accent',
    'text-warn',
    'text-up',
    'text-down',
    'text-ink-muted',
    'text-ink-faint',
] as const;

const SIZE = 220;
const RADIUS = SIZE / 2 - 4;
const CENTRE = SIZE / 2;

/** The angle of each slice, in radians, in the order they were given. */
export function sliceAngles(slices: readonly PieSlice[]): readonly (readonly [number, number])[] {
    const total = slices.reduce((sum, slice) => sum + Math.max(slice.value, 0), 0);
    if (total <= 0) {
        return [];
    }
    const angles: (readonly [number, number])[] = [];
    let from = -Math.PI / 2;
    for (const slice of slices) {
        const share = Math.max(slice.value, 0) / total;
        const to = from + share * 2 * Math.PI;
        angles.push([from, to]);
        from = to;
    }
    return angles;
}

/** The share of the whole one value holds, from 0 to 1, or 0 when the whole is empty. */
export function shareOf(value: number, total: number): number {
    if (!Number.isFinite(value) || !Number.isFinite(total) || total <= 0) {
        return 0;
    }
    return Math.max(value, 0) / total;
}

/** The path of one slice: an arc from the centre, out and round, and back. */
export function slicePath(from: number, to: number): string {
    if (!Number.isFinite(from) || !Number.isFinite(to) || to <= from) {
        return '';
    }
    /* One slice that holds everything is a circle, which an arc cannot draw. */
    if (to - from >= 2 * Math.PI - 1e-9) {
        return `M ${CENTRE} ${CENTRE - RADIUS} A ${RADIUS} ${RADIUS} 0 1 1 ${CENTRE - 1e-6} ${CENTRE - RADIUS} Z`;
    }
    const x = (angle: number): number => CENTRE + RADIUS * Math.cos(angle);
    const y = (angle: number): number => CENTRE + RADIUS * Math.sin(angle);
    const large = to - from > Math.PI ? 1 : 0;
    return [
        `M ${CENTRE} ${CENTRE}`,
        `L ${x(from).toFixed(2)} ${y(from).toFixed(2)}`,
        `A ${RADIUS} ${RADIUS} 0 ${large} 1 ${x(to).toFixed(2)} ${y(to).toFixed(2)}`,
        'Z',
    ].join(' ');
}

/** The tone a slice wears, cycling through the palette for slices past its end. */
export function toneOf(index: number): string {
    return TONES[index % TONES.length] ?? TONES[0];
}

export function PieChart({
    slices,
    format,
    caption,
}: {
    readonly slices: readonly PieSlice[];
    /** How a slice's value is written in the legend. */
    readonly format: (value: number) => string;
    readonly caption: string;
}): ReactNode {
    const angles = sliceAngles(slices);
    if (angles.length === 0) {
        return null;
    }
    const total = slices.reduce((sum, slice) => sum + Math.max(slice.value, 0), 0);
    return (
        <div className="flex flex-wrap items-center gap-6">
            <svg
                viewBox={`0 0 ${SIZE} ${SIZE}`}
                className="h-52 w-52"
                role="img"
                aria-label={caption}
            >
                <title>{caption}</title>
                {slices.map((slice, index) => {
                    const angle = angles[index];
                    if (angle === undefined) {
                        return null;
                    }
                    const path = slicePath(angle[0], angle[1]);
                    if (path === '') {
                        return null;
                    }
                    return (
                        <path
                            key={slice.key}
                            d={path}
                            className={`fill-current ${toneOf(index)}`}
                            opacity={index === 0 ? 0.9 : 0.7}
                        />
                    );
                })}
            </svg>
            <ul className="space-y-1 text-sm">
                {slices.map((slice, index) => (
                    <li key={slice.key} className="flex items-center gap-2">
                        <span
                            aria-hidden="true"
                            className={`inline-block h-2 w-3 rounded-sm bg-current ${toneOf(index)}`}
                        />
                        <span className="text-ink">{slice.label}</span>
                        <span className="tabular-nums text-ink-muted">{format(slice.value)}</span>
                        <span className="tabular-nums text-ink-faint">
                            {Math.round(shareOf(slice.value, total) * 100)}%
                        </span>
                    </li>
                ))}
            </ul>
        </div>
    );
}
