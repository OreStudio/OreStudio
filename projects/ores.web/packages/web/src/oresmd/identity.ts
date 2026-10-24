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

import type {
    CurveSeriesList,
    ListedSeries,
    OresmdParseResult,
    OresmdSchema,
} from '@ores/contracts';
import { canBeShown, type IdentityForm } from '../market/evolutionView.js';

/** The series an oresmd URL names, as the form of the Evolution page states it, or why it names none. */
export type SeriesOfUrl =
    | { readonly ok: true; readonly form: IdentityForm }
    | {
          readonly ok: false;
          readonly reason: 'refused' | 'fixing' | 'unknown';
          readonly message: string;
      };

/**
 * The series a parsed URL belongs to. A series URL names its own series. A quote
 * URL names the series of its datum, which holds the identity fields of the row
 * and not the coordinate, so a coordinate the quote states is left out. A fixing
 * names no series.
 *
 * The words of the identity follow the series list's own: the asset class, the
 * path as the scope, the instrument type, the quote type, and the remaining
 * identity fields as `name=text` lines. The party and the provider are left empty, which
 * asks across the parties the person may see and across their providers.
 */
export function seriesOfUrl(schema: OresmdSchema, parts: OresmdParseResult): SeriesOfUrl {
    if (!parts.success) return { ok: false, reason: 'refused', message: parts.message };
    if (parts.kind === 'fixing') return { ok: false, reason: 'fixing', message: '' };
    const instrument = schema.instruments.find((candidate) => candidate.code === parts.instrument);
    if (instrument === undefined)
        return { ok: false, reason: 'unknown', message: parts.instrument };
    const identity = new Set(
        instrument.fields
            .filter((field) => field.role === 'identity' && field.name !== instrument.subject)
            .map((field) => field.name),
    );
    const lines = parts.fields
        .filter((field) => identity.has(field.name) && field.text !== '')
        .map((field) => `${field.name}=${field.text}`);
    return {
        ok: true,
        form: {
            asset: parts.asset,
            scope: parts.subject,
            instrumentType: parts.instrument,
            quoteType: parts.quote,
            fields: lines.join('\n'),
            partyId: '',
            providerCode: '',
        },
    };
}

/** The address of the Evolution page for a URL. The page reads the URL and opens its series. */
export function evolutionRoute(uri: string): string {
    return `/market-data/evolution?uri=${encodeURIComponent(uri)}`;
}

/** The address of the page of a series with no axis. The page reads the URL and draws the values. */
export function seriesRoute(uri: string): string {
    return `/market-data/series?uri=${encodeURIComponent(uri)}`;
}

/** The Evolution page for a series the party holds, by its id. */
export function evolutionIdRoute(seriesId: string): string {
    return `/market-data/evolution?series=${encodeURIComponent(seriesId)}`;
}

/** The series page for a series the party holds, by its id. */
export function seriesIdRoute(seriesId: string): string {
    return `/market-data/series?series=${encodeURIComponent(seriesId)}`;
}

/** The page that shows a listed series: Evolution when it has an axis, else the series page. */
export function seriesPageOf(listed: ListedSeries): string {
    return canBeShown(listed)
        ? evolutionIdRoute(listed.series_id)
        : seriesIdRoute(listed.series_id);
}

/** Whether a listed series is the one a form names: the same four words and the same identity fields. */
function isTheSeries(listed: ListedSeries, form: IdentityForm): boolean {
    const { asset, scope, instrument_type, quote_type, fields } = listed.identity;
    if (
        asset !== form.asset ||
        scope !== form.scope ||
        instrument_type !== form.instrumentType ||
        quote_type !== form.quoteType
    ) {
        return false;
    }
    const held = fields.map((field) => `${field.name}=${field.text}`).sort();
    const wanted = form.fields
        .split('\n')
        .map((line) => line.trim())
        .filter((line) => line !== '')
        .sort();
    return held.length === wanted.length && held.every((line, index) => line === wanted[index]);
}

/**
 * What the Evolution page can do with the series a form names, given the series
 * the party can see.
 *
 * It can draw the series when the party has it and it declares an axis, because
 * only a series with a shape has a grid of instants by nodes. A series with no axis,
 * such as one spot rate, is a single value at each instant and has nothing to
 * draw. A series the party does not have is not there to open. The page and the
 * link both ask, so a link is offered only where the page can answer.
 */
export type SeriesVerdict =
    | { readonly kind: 'shown'; readonly listed: ListedSeries }
    | { readonly kind: 'no-shape'; readonly listed: ListedSeries }
    | { readonly kind: 'not-listed' };

export function seriesVerdict(list: CurveSeriesList, form: IdentityForm): SeriesVerdict {
    const listed = list.series.find((candidate) => isTheSeries(candidate, form));
    if (listed === undefined) return { kind: 'not-listed' };
    return canBeShown(listed) ? { kind: 'shown', listed } : { kind: 'no-shape', listed };
}

/**
 * Where the text of a URL points: the page of the series it names, or nowhere.
 *
 * A series with an axis opens on the Evolution page, which draws it. A series with
 * no axis is a single value at each instant and opens on the series page. A URL
 * that names no series, a fixing, a series the party does not have and a URL the
 * codec refuses are all nowhere, and the caller draws plain text: a link that ends
 * in an apology is worse than no link.
 *
 * This is the one rule the detail view of a record and a list of records share, so
 * a URL behaves one way on both. They differ only in how they ask the codec: a
 * record asks about its one URL, and a list asks about its rows together.
 */
export function seriesTargetOf(
    uri: string,
    schema: OresmdSchema,
    parts: OresmdParseResult,
    list: CurveSeriesList,
): string | undefined {
    const named = seriesOfUrl(schema, parts);
    if (!named.ok) return undefined;
    switch (seriesVerdict(list, named.form).kind) {
        case 'shown':
            return evolutionRoute(uri);
        case 'no-shape':
            return seriesRoute(uri);
        case 'not-listed':
            return undefined;
    }
}
