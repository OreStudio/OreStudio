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
import type { RecordRow } from '../api/client.js';
import type { TitledIdentity } from '../oresmd/title.js';

/**
 * The instant as ISO text. The wire writes UTC with a Z, and a text with no zone is
 * read as UTC too, so the viewer's own zone never shifts a point.
 */
function instantText(text: string): string {
    const iso = text.trim().replace(' ', 'T');
    return /(Z|[+-]\d\d:?\d\d)$/.test(iso) ? iso : `${iso}Z`;
}

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
        const at = Date.parse(instantText(observation.at));
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

/**
 * The decimal places a series' values are shown with, or nothing to keep the
 * plain format. Only an FX spot rate has them: they come from the convention of
 * its currency pair, whose code is the unit currency over the quoted currency.
 */
export function placesOf(
    identity: TitledIdentity,
    conventions: readonly RecordRow[],
): number | undefined {
    if (identity.instrument_type !== 'fx_spot' || identity.quote_type !== 'rate') return undefined;
    const quoted = identity.fields.find((field) => field.name === 'ccy')?.text;
    if (quoted === undefined) return undefined;
    const row = conventions.find((c) => c['pair_code'] === `${identity.scope}/${quoted}`);
    const places = Number(row?.['decimal_places']);
    return Number.isInteger(places) && places >= 0 && places <= 10 ? places : undefined;
}

/** A value with a fixed number of places, or in the plain format of up to seven digits when none is known. */
export function formatValue(value: number, places: number | undefined): string {
    return places === undefined ? String(Number(value.toPrecision(7))) : value.toFixed(places);
}

/** A stored value text in the same format, or the text itself when it does not read as a number. */
export function formatText(text: string, places: number | undefined): string {
    const value = Number(text);
    return places === undefined || text.trim() === '' || !Number.isFinite(value)
        ? text
        : value.toFixed(places);
}
