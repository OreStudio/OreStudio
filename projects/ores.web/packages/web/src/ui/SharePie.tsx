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
 * A pie of how a whole divides between named parts.
 *
 * It draws what it is given. The parts are sorted largest first, the slices
 * beyond the palette are folded into one "other" slice, and a part with no
 * share is left out, so a part that did nothing is never drawn as a sliver.
 */

export interface PieItem {
    readonly key: string;
    readonly label: string;
    readonly value: number;
    /**
     * A colour class that the part carries by its meaning, such as green for a success. A
     * part without one takes the next colour of the categorical palette.
     */
    readonly swatch?: string;
}

export interface PieSlice extends PieItem {
    /** This slice's share of the whole, from 0 to 1. */
    readonly fraction: number;
    /** The Tailwind colour class of the slice and its legend swatch. */
    readonly swatch: string;
}

/**
 * The colours of the named slices. They are categories and carry no status, so the palette
 * leaves out the up, down and warn colours that the rest of the screen uses for good and bad.
 */
const SWATCHES: readonly string[] = [
    'text-accent',
    'text-roadmap',
    'text-flow',
    'text-accent-bright',
    'text-ink-muted',
];

/** The colour of the folded slice, which no named slice uses. */
const OTHER_SWATCH = 'text-ink-faint';

/** The most slices drawn, counting the folded "other" slice. */
export const MAX_SLICES = SWATCHES.length + 1;

/** The slices of the parts: positive parts only, largest first, the tail folded into one. */
export function pieSlices(items: readonly PieItem[], otherLabel: string): readonly PieSlice[] {
    const parts = items.filter((item) => item.value > 0).sort((a, b) => b.value - a.value);
    const total = parts.reduce((sum, part) => sum + part.value, 0);
    if (total <= 0) {
        return [];
    }
    const shown = parts.length > SWATCHES.length ? parts.slice(0, SWATCHES.length) : parts;
    const rest = parts.slice(shown.length);
    const slices: PieSlice[] = shown.map((part, index) => ({
        ...part,
        fraction: part.value / total,
        swatch: part.swatch ?? SWATCHES[index] ?? OTHER_SWATCH,
    }));
    if (rest.length > 0) {
        const value = rest.reduce((sum, part) => sum + part.value, 0);
        slices.push({
            key: 'other',
            label: otherLabel,
            value,
            fraction: value / total,
            swatch: OTHER_SWATCH,
        });
    }
    return slices;
}

const RADIUS = 60;
const CENTRE = 64;

function point(angle: number): string {
    const x = CENTRE + RADIUS * Math.sin(angle);
    const y = CENTRE - RADIUS * Math.cos(angle);
    return `${x.toFixed(2)} ${y.toFixed(2)}`;
}

/** The SVG path of the slice that starts at fraction `from` and spans `fraction`. */
export function slicePath(from: number, fraction: number): string {
    const start = from * 2 * Math.PI;
    const end = (from + fraction) * 2 * Math.PI;
    const large = fraction > 0.5 ? 1 : 0;
    return `M ${CENTRE} ${CENTRE} L ${point(start)} A ${RADIUS} ${RADIUS} 0 ${large} 1 ${point(end)} Z`;
}

/** A share as a whole percentage, with a floor of "<1%" for a slice that is not empty. */
export function percent(fraction: number): string {
    const value = fraction * 100;
    return value < 1 ? '<1%' : `${Math.round(value)}%`;
}

export function SharePie({
    items,
    otherLabel,
    label,
    caption,
}: {
    readonly items: readonly PieItem[];
    readonly otherLabel: string;
    /** What the pie shows, read out by a screen reader. */
    readonly label: string;
    /** A line drawn above the pie, so a screen with two pies says what each one is. */
    readonly caption?: string;
}): ReactNode {
    const slices = pieSlices(items, otherLabel);
    if (slices.length === 0) {
        return null;
    }
    let from = 0;
    return (
        <figure className="space-y-2">
            {caption !== undefined && (
                <figcaption className="text-xs text-ink-faint">{caption}</figcaption>
            )}
            <div className="flex flex-wrap items-center gap-6">
                <svg
                    viewBox="0 0 128 128"
                    role="img"
                    aria-label={label}
                    className="h-40 w-40 shrink-0"
                >
                    {slices.length === 1 ? (
                        <circle
                            cx={CENTRE}
                            cy={CENTRE}
                            r={RADIUS}
                            fill="currentColor"
                            className={slices[0]?.swatch ?? OTHER_SWATCH}
                        />
                    ) : (
                        slices.map((slice) => {
                            const path = slicePath(from, slice.fraction);
                            from += slice.fraction;
                            return (
                                <path
                                    key={slice.key}
                                    d={path}
                                    fill="currentColor"
                                    strokeWidth="1"
                                    className={`stroke-surface-raised ${slice.swatch}`}
                                />
                            );
                        })
                    )}
                </svg>
                <ul className="space-y-1 text-sm">
                    {slices.map((slice) => (
                        <li key={slice.key} className="flex items-center gap-2">
                            <span
                                aria-hidden="true"
                                className={`inline-block h-3 w-3 rounded-sm bg-current ${slice.swatch}`}
                            />
                            <span>{slice.label}</span>
                            <span className="font-mono text-ink-muted">
                                {percent(slice.fraction)}
                            </span>
                        </li>
                    ))}
                </ul>
            </div>
        </figure>
    );
}
