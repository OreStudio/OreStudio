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

import { keepPreviousData, queryOptions } from '@tanstack/react-query';
import { ORESMD_MAX_BATCH, type OresmdComposeRequest, type SeriesWindow } from '@ores/contracts';
import { api } from '../api/client.js';
import { curves } from '../api/curves.js';
import { series } from '../api/series.js';
import { oresmd } from '../api/oresmd.js';
import { refdataStore } from '../refdata/store.js';
import { keyOf } from './editor.js';

/**
 * The reads of the oresmd editor. Each is quiet: the editor states a failure
 * itself, where the person is typing, so the app's banner would only repeat it.
 */
const QUIET = { quiet: true } as const;

/** The schema is the codec's table and changes with a release, so it is kept for the session. */
const SCHEMA_STALE_MS = 5 * 60 * 1000;

/**
 * The URIs of one read, without a repeat, in one order whatever order the rows
 * name them in, split into the sets the codec's batch limit allows.
 *
 * The order is part of the read: the answer arrives in the order asked and a
 * caller holds the URIs it asked about beside their parts. Sorting the set keeps
 * a set of rows that the server returned in another order on the same answer.
 */
export function chunksOf(uris: readonly string[]): readonly (readonly string[])[] {
    const distinct = [...new Set(uris.filter((uri) => uri !== ''))].sort();
    const chunks: string[][] = [];
    for (let start = 0; start < distinct.length; start += ORESMD_MAX_BATCH) {
        chunks.push(distinct.slice(start, start + ORESMD_MAX_BATCH));
    }
    return chunks;
}

/** The read that answers for a set of URIs: the set itself, so a new set is a new read. */
export function parseManyKey(uris: readonly string[]): readonly unknown[] {
    return ['oresmd-parse-many', chunksOf(uris)];
}

export const oresmdQueries = {
    /**
     * The currencies the system keeps, for the suggestions of a currency field.
     *
     * They are read from the refdata store by name. The editor opens over screens
     * that wrap their parts in another store, such as the synthetic feeds, and the
     * store of the screen has no currencies. A missing list of suggestions is no
     * reason for a banner, so the read is quiet.
     */
    currencies: () =>
        queryOptions({
            queryKey: ['oresmd-currencies'],
            queryFn: () => refdataStore.records('currencies'),
            staleTime: SCHEMA_STALE_MS,
            meta: QUIET,
        }),

    /**
     * The currency pair conventions, for the decimal places of a spot rate. They are
     * read from the refdata store by name and quietly, so a failed read leaves a
     * rate in its plain format and raises no banner.
     */
    pairConventions: () =>
        queryOptions({
            queryKey: ['oresmd-pair-conventions'],
            queryFn: () => refdataStore.records('currency-pair-conventions'),
            staleTime: SCHEMA_STALE_MS,
            meta: QUIET,
        }),

    /**
     * The series the party can see, with the axes each declares. It is the list the
     * Evolution page reads, under the same key, so the two share one answer.
     */
    seriesList: (partyId: string) =>
        queryOptions({
            queryKey: ['market-series-list', partyId],
            queryFn: () => curves.seriesList(),
            retry: false,
            meta: QUIET,
        }),

    /** The observations of one series over a window, oldest first. */
    observations: (seriesId: string, limit: number, window: SeriesWindow) =>
        queryOptions({
            queryKey: ['series-observations', seriesId, limit, window],
            queryFn: () => series.observations(seriesId, limit, window),
            retry: false,
            meta: QUIET,
        }),

    dayCounters: () =>
        queryOptions({
            queryKey: ['oresmd-day-counters'],
            queryFn: () => api.classificationRows('day-counter'),
            staleTime: SCHEMA_STALE_MS,
            meta: QUIET,
        }),

    schema: () =>
        queryOptions({
            queryKey: ['oresmd-schema'],
            queryFn: () => oresmd.schema(),
            staleTime: SCHEMA_STALE_MS,
            meta: QUIET,
        }),

    parse: (uri: string) =>
        queryOptions({
            queryKey: ['oresmd-parse', uri],
            queryFn: () => oresmd.parse(uri),
            meta: QUIET,
        }),

    /**
     * The parts of each URI, in the order given, as one read for the whole set.
     *
     * A URI's parts are the codec's answer and change with a release, not with a
     * tick, so the answer is kept for as long as the schema. A list that asks again
     * every few seconds asks about the same URIs, and a fresh read each time would
     * cost the server a call a row for an answer that cannot have changed.
     */
    parseMany: (uris: readonly string[]) =>
        queryOptions({
            queryKey: parseManyKey(uris),
            queryFn: async () =>
                (await Promise.all(chunksOf(uris).map((chunk) => oresmd.parseMany(chunk)))).flat(),
            enabled: chunksOf(uris).length > 0,
            staleTime: SCHEMA_STALE_MS,
            meta: QUIET,
        }),

    /** The URI the parts write. The previous answer stays while the next arrives, so the preview does not flash. */
    compose: (parts: OresmdComposeRequest | null) =>
        queryOptions({
            queryKey: ['oresmd-compose', keyOf(parts)],
            queryFn: () => oresmd.compose(parts as OresmdComposeRequest),
            enabled: parts !== null,
            placeholderData: keepPreviousData,
            meta: QUIET,
        }),
};
