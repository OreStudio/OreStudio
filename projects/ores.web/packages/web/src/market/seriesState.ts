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
import { seriesOfUrl, seriesVerdict, type SeriesOfUrl } from '../oresmd/identity.js';
import { canBeShown } from './evolutionView.js';

/**
 * One read of the page, as the page can use it: its answer, the reason it has
 * none, or neither yet.
 *
 * The waiting kind is a value rather than a flag, so the state cannot be worked
 * out from an answer that has not arrived.
 */
export type SeriesRead<T> =
    | { readonly kind: 'answer'; readonly data: T }
    | { readonly kind: 'error'; readonly message: string }
    | { readonly kind: 'waiting' };

/** The queries of the series page, with the address that names the series. */
export interface SeriesPageReads {
    /** The URL the address carries as `uri`, empty when it carries none. */
    readonly uri: string;
    /** The series id the address carries as `series`, empty when it carries none. */
    readonly seriesId: string;
    readonly parsed: SeriesRead<OresmdParseResult>;
    readonly schema: SeriesRead<OresmdSchema>;
    readonly list: SeriesRead<CurveSeriesList>;
}

/**
 * What the series page draws, one kind per situation it can be in.
 *
 * A URL the codec refuses and a series that has values are kinds of their own.
 * A refused URL is not an error of the page: the codec answered, and its words
 * say why. A series with values is the only kind that draws a chart.
 */
export type SeriesPageState =
    | { readonly kind: 'no-uri' }
    | { readonly kind: 'error'; readonly message: string }
    | { readonly kind: 'loading' }
    | { readonly kind: 'refused'; readonly message: string }
    | { readonly kind: 'fixing' }
    | { readonly kind: 'unknown'; readonly name: string }
    | { readonly kind: 'not-listed' }
    | { readonly kind: 'has-shape'; readonly listed: ListedSeries }
    | { readonly kind: 'values'; readonly listed: ListedSeries };

/**
 * The situation the page is in, from the answers of its reads.
 *
 * An error outranks loading and says the first failure, so a page with one
 * failed read does not wait for the other two. A URL the codec reads outranks
 * the series id, because an address that carries both names its series twice
 * and the URL is the more specific of the two. A series the party does not have
 * is not there to draw, and a series with an axis is drawn by the Evolution page
 * instead.
 */
export function seriesPageState(reads: SeriesPageReads): SeriesPageState {
    const { uri, seriesId, parsed, schema, list } = reads;
    if (uri === '' && seriesId === '') {
        return { kind: 'no-uri' };
    }
    if (parsed.kind === 'error') {
        return { kind: 'error', message: parsed.message };
    }
    if (schema.kind === 'error') {
        return { kind: 'error', message: schema.message };
    }
    if (list.kind === 'error') {
        return { kind: 'error', message: list.message };
    }
    if (list.kind !== 'answer') {
        return { kind: 'loading' };
    }
    const named =
        parsed.kind === 'answer' && schema.kind === 'answer'
            ? seriesOfUrl(schema.data, parsed.data)
            : undefined;
    if (seriesId === '') {
        if (named === undefined) {
            return { kind: 'loading' };
        }
        if (!named.ok) {
            return refusalOf(named);
        }
        const verdict = seriesVerdict(list.data, named.form);
        switch (verdict.kind) {
            case 'shown':
                return { kind: 'has-shape', listed: verdict.listed };
            case 'no-shape':
                return { kind: 'values', listed: verdict.listed };
            case 'not-listed':
                return { kind: 'not-listed' };
        }
    }
    if (named !== undefined && !named.ok) {
        return refusalOf(named);
    }
    const listed = list.data.series.find((one) => one.series_id === seriesId);
    if (listed === undefined) {
        return { kind: 'not-listed' };
    }
    return canBeShown(listed) ? { kind: 'has-shape', listed } : { kind: 'values', listed };
}

/** The page's words for a URL that names no series: the codec's, a fixing's, or a type's. */
function refusalOf(refused: Extract<SeriesOfUrl, { readonly ok: false }>): SeriesPageState {
    switch (refused.reason) {
        case 'refused':
            return { kind: 'refused', message: refused.message };
        case 'fixing':
            return { kind: 'fixing' };
        case 'unknown':
            return { kind: 'unknown', name: refused.message };
    }
}
