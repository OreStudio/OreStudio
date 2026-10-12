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

import type { Observation } from '@ores/contracts';

/** One observation as a point of a chart: its instant in milliseconds and its value. */
export interface Point {
    readonly at: number;
    readonly value: number;
}

/**
 * The points a chart can draw from the observations. An observation whose
 * instant or value does not read as a number is left out, because a chart of a
 * made-up zero would say something no source said. The order of the observations
 * is kept.
 */
export function pointsOf(observations: readonly Observation[]): readonly Point[] {
    const points: Point[] = [];
    for (const observation of observations) {
        const at = Date.parse(observation.at.replace(' ', 'T'));
        const value = Number(observation.value);
        if (Number.isFinite(at) && observation.value.trim() !== '' && Number.isFinite(value)) {
            points.push({ at, value });
        }
    }
    return points;
}

/** An instant for a label: the day and the minute, in UTC, as the observations carry it. */
export function instantLabel(at: number): string {
    return new Date(at).toISOString().slice(0, 16).replace('T', ' ');
}

/** The newest `count` observations, newest first, for the table under the chart. */
export function latestFirst(
    observations: readonly Observation[],
    count: number,
): readonly Observation[] {
    return observations.slice(-count).reverse();
}
