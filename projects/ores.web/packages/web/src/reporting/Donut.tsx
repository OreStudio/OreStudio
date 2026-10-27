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

/** One part of a whole, as a name and a size. */
export interface Share {
    readonly name: string;
    readonly value: number;
}

/** Where each part starts and ends on the circle, in turn. */
export interface Arc extends Share {
    readonly from: number;
    readonly to: number;
}

/**
 * The arcs of a whole, in the order given.
 *
 * The parts are stated as fractions of the whole rather than of the sum, so a
 * caller can hand in figures that do not add up and see the gap as a smaller
 * ring rather than as a wrong share.
 */
export function arcsOf(parts: readonly Share[], total?: number): readonly Arc[] {
    const whole = total ?? parts.reduce((sum, part) => sum + Math.max(0, part.value), 0);
    if (whole <= 0) {
        return [];
    }
    let swept = 0;
    return parts.map((part) => {
        const from = swept;
        const sweep = (Math.max(0, part.value) / whole) * 360;
        swept += sweep;
        return { ...part, from, to: from + sweep };
    });
}

/** The point on the circle at an angle, measured from the top. */
function pointAt(cx: number, cy: number, radius: number, angle: number): [number, number] {
    const radians = ((angle - 90) * Math.PI) / 180;
    return [cx + radius * Math.cos(radians), cy + radius * Math.sin(radians)];
}

/** One arc as a filled path, or a full circle when it is the whole. */
function pathOf(arc: Arc, cx: number, cy: number, radius: number, hole: number): string {
    if (arc.to - arc.from >= 359.999) {
        return [
            `M ${cx} ${cy - radius}`,
            `A ${radius} ${radius} 0 1 1 ${cx - 0.01} ${cy - radius}`,
            `M ${cx} ${cy - hole}`,
            `A ${hole} ${hole} 0 1 0 ${cx + 0.01} ${cy - hole}`,
            'Z',
        ].join(' ');
    }
    const [x1, y1] = pointAt(cx, cy, radius, arc.from);
    const [x2, y2] = pointAt(cx, cy, radius, arc.to);
    const [x3, y3] = pointAt(cx, cy, hole, arc.to);
    const [x4, y4] = pointAt(cx, cy, hole, arc.from);
    const large = arc.to - arc.from > 180 ? 1 : 0;
    return [
        `M ${x1} ${y1}`,
        `A ${radius} ${radius} 0 ${large} 1 ${x2} ${y2}`,
        `L ${x3} ${y3}`,
        `A ${hole} ${hole} 0 ${large} 0 ${x4} ${y4}`,
        'Z',
    ].join(' ');
}

/**
 * A whole in a few parts.
 *
 * The form is the prototype's rule: one whole in not more than about six parts
 * is a donut, and every part carries its exact figure in the legend beside it,
 * so the ring says the shape and the numbers say the value.
 */
export function Donut({
    title,
    caption,
    parts,
    format,
}: {
    readonly title: string;
    readonly caption: string;
    readonly parts: readonly Share[];
    readonly format: (value: number) => string;
}): ReactNode {
    const arcs = arcsOf(parts);
    const whole = parts.reduce((sum, part) => sum + Math.max(0, part.value), 0);
    const colours = ['#7aa2f7', '#9ece6a', '#e0af68', '#f7768e', '#bb9af7', '#7dcfff'];
    return (
        <figure className="space-y-2">
            <figcaption className="text-sm">
                <span className="font-medium">{title}</span>
                <span className="block text-xs text-ink-faint">{caption}</span>
            </figcaption>
            <div className="flex flex-wrap items-center gap-6">
                <svg
                    viewBox="0 0 120 120"
                    width={120}
                    height={120}
                    role="img"
                    aria-label={`${title}: ${parts
                        .map((part) => `${part.name} ${format(part.value)}`)
                        .join(', ')}`}
                >
                    {arcs.map((arc, index) => (
                        <path
                            key={arc.name}
                            d={pathOf(arc, 60, 60, 56, 34)}
                            fill={colours[index % colours.length]}
                            opacity={0.9}
                        />
                    ))}
                    <text
                        x={60}
                        y={62}
                        textAnchor="middle"
                        className="fill-ink text-[10px]"
                        style={{ fontSize: 10 }}
                    >
                        {format(whole)}
                    </text>
                </svg>
                <ul className="space-y-1 text-sm">
                    {parts.map((part, index) => (
                        <li key={part.name} className="flex items-center gap-2">
                            <span
                                aria-hidden
                                className="inline-block h-2 w-2 rounded-full"
                                style={{ background: colours[index % colours.length] }}
                            />
                            <span>{part.name}</span>
                            <span className="font-mono text-xs text-ink-muted">
                                {format(part.value)}
                                {whole > 0 ? ` · ${((part.value / whole) * 100).toFixed(0)}%` : ''}
                            </span>
                        </li>
                    ))}
                </ul>
            </div>
        </figure>
    );
}
