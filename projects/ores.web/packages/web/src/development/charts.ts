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

import type { Charts } from '@ores/contracts';

/** The drawing area of every chart, in SVG units. */
export const CHART = { width: 360, height: 180, top: 14, right: 10, bottom: 30, left: 30 } as const;

const INNER_WIDTH = CHART.width - CHART.left - CHART.right;
const INNER_HEIGHT = CHART.height - CHART.top - CHART.bottom;

export interface Bar {
    readonly label: string;
    readonly value: number;
    readonly x: number;
    readonly y: number;
    readonly width: number;
    readonly height: number;
}

/** The bars of a column chart. The tallest value fills the height, and an all-zero chart is flat. */
export function barsOf(data: readonly { readonly label: string; readonly value: number }[]): Bar[] {
    const max = Math.max(1, ...data.map((entry) => entry.value));
    const slot = INNER_WIDTH / Math.max(1, data.length);
    return data.map((entry, index) => {
        const height = (entry.value / max) * INNER_HEIGHT;
        return {
            ...entry,
            x: CHART.left + index * slot + slot * 0.15,
            y: CHART.top + INNER_HEIGHT - height,
            width: slot * 0.7,
            height,
        };
    });
}

/** The label of a sprint on the velocity axis: `sprint_27` is `s27`. */
export function sprintLabel(sprint: string): string {
    return sprint.replace(/^sprint_/, 's');
}

/** The last `count` sprints of the velocity series. */
export function recentVelocity(charts: Charts, count = 8): Charts['velocity'] {
    return charts.velocity.slice(-count);
}

export interface Line {
    /** The path of the step line, or an empty string with no points. */
    readonly path: string;
    /** The height of the scope line, or null when there is no scope. */
    readonly scopeY: number | null;
    readonly first: string;
    readonly last: string;
    /** The largest value on the vertical axis. */
    readonly max: number;
}

const DAY_MS = 86_400_000;

/**
 * The burn-up as a step line over the days that tasks were done, against a
 * flat scope line. The axis tops out at the larger of the scope and the total.
 */
export function burnupLine(charts: Charts): Line {
    const points = charts.burnup;
    const max = Math.max(1, charts.scope, ...points.map((point) => point.done));
    const y = (value: number): number => CHART.top + INNER_HEIGHT - (value / max) * INNER_HEIGHT;
    const scopeY = charts.scope > 0 ? y(charts.scope) : null;
    const first = points[0];
    const last = points[points.length - 1];
    if (first === undefined || last === undefined) {
        return { path: '', scopeY, first: '', last: '', max };
    }
    const from = Date.parse(`${first.date}T00:00:00Z`);
    const span = Math.max(Date.parse(`${last.date}T00:00:00Z`) - from, DAY_MS);
    const x = (date: string): number =>
        CHART.left + ((Date.parse(`${date}T00:00:00Z`) - from) / span) * INNER_WIDTH;
    let path = `M ${x(first.date)} ${y(0)}`;
    let before = 0;
    for (const point of points) {
        path += ` L ${x(point.date)} ${y(before)} L ${x(point.date)} ${y(point.done)}`;
        before = point.done;
    }
    return { path, scopeY, first: first.date, last: last.date, max };
}
