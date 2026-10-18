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

import {
    DEFAULT_SERIES_WINDOW,
    SERIES_WINDOWS,
    type Observation,
    type SeriesWindow,
} from '@ores/contracts';
import { fromWireTimestamp } from '@ores/wire-protocol/browser';
import type { RecordRow } from '../api/client.js';
import type { TitledIdentity } from '../oresmd/title.js';

/** One observation as a point of a chart: its instant in milliseconds and its value. */
export interface Point {
    readonly at: number;
    readonly value: number;
}

/**
 * The points a chart can draw from the observations. An observation whose
 * value does not read as a number is left out, because a chart of a made-up
 * zero would say something no source said. The instant is the wire
 * protocol's own conversion, and the schema that parsed the observation has
 * already refused anything that is not the wire shape. The order of the
 * observations is kept.
 */
export function pointsOf(observations: readonly Observation[]): readonly Point[] {
    const points: Point[] = [];
    for (const observation of observations) {
        const at = fromWireTimestamp(observation.at).getTime();
        const value = Number(observation.value);
        if (observation.value.trim() !== '' && Number.isFinite(value)) {
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
 * The window an address names, or the default one when it names none the page
 * offers. An address is a person's text, so a window this page does not offer
 * draws the page as it opens rather than an empty chart.
 */
export function windowOf(requested: string | null): SeriesWindow {
    return SERIES_WINDOWS.find((window) => window === requested) ?? DEFAULT_SERIES_WINDOW;
}

/** The most decimal places a convention may ask for; more than this is a data error, not a format. */
export const MAX_PLACES = 10;

/** Whether a series is an FX spot rate, the one kind of series that has conventions to format by. */
export function isSpotRate(identity: TitledIdentity): boolean {
    return identity.instrument_type === 'fx_spot' && identity.quote_type === 'rate';
}

/**
 * The decimal places a series' values are shown with, or nothing to keep the
 * plain format.
 *
 * The pair store keeps a pair in one direction only: the seed writes one
 * currency_pairs row per instrument, and the convention follows it under the
 * same code. The reverse code is not a row, so the lookup answers for the
 * direction the series quotes and nothing else. Borrowing the reverse pair's
 * places would apply figures calibrated for one magnitude to its reciprocal,
 * which is a different number entirely.
 */
export function placesOf(
    identity: TitledIdentity,
    conventions: readonly RecordRow[],
): number | undefined {
    if (!isSpotRate(identity)) return undefined;
    const quoted = identity.fields.find((field) => field.name === 'ccy')?.text;
    if (quoted === undefined) return undefined;
    const row = conventions.find((c) => c['pair_code'] === `${identity.scope}/${quoted}`);
    const places = Number(row?.['decimal_places']);
    return Number.isInteger(places) && places >= 0 && places <= MAX_PLACES ? places : undefined;
}

/**
 * Whether a spot rate's pair has no convention to format by, so the page says so
 * instead of showing the plain format in silence. A pair quoted the other way
 * round than the pair store keeps it, and a pair the store does not hold at all,
 * both read as no convention.
 */
export function lacksConvention(
    identity: TitledIdentity,
    conventions: readonly RecordRow[],
): boolean {
    return isSpotRate(identity) && placesOf(identity, conventions) === undefined;
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
