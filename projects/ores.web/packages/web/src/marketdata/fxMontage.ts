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
import { queryOptions } from '@tanstack/react-query';
import { DEFAULT_OBSERVATIONS } from '@ores/contracts';
import { api, type FxMontagePair, type MontageSpot } from '../api/client.js';
import { series } from '../api/series.js';
import { asOfInstant } from './readScreen.js';

/**
 * What the FX montage reads and how it chooses the spot it draws.
 *
 * The reads live here, beside the screen rather than inside it, so the screen's
 * test drives the same query the page does: a test that rebuilt the key by hand
 * would keep passing while the page asked for another one.
 */

/**
 * The montage of one base at an instant, over the providers the person asked
 * for.
 *
 * An empty set asks for every provider, which is what the screen shows when the
 * address names none. Each provider travels as one body member, so the server
 * filters the crosses and the consensus is the one those providers make.
 */
export function montageQuery(base: string, asOf: string, providers: readonly string[]) {
    return queryOptions({
        queryKey: ['fx-montage', base, asOf, providers.join(',')] as const,
        queryFn: () =>
            api.fxMontage({
                baseCurrency: base,
                asOf: asOfInstant(asOf),
                providers,
            }),
        retry: false,
    });
}

/**
 * The chosen pair's day: the observations of its provider's series, over the
 * twenty four hours that end at @p asOf.
 *
 * The window ends at the same instant the montage answered for, so the path
 * describes the day under the table rather than whatever the clock has done
 * since the table was read.
 */
export function chosenDayQuery(seriesId: string, asOf: string) {
    return queryOptions({
        queryKey: ['series-observations', seriesId, 'day', asOf] as const,
        queryFn: () =>
            series.observations(seriesId, DEFAULT_OBSERVATIONS, 'day', asOfInstant(asOf)),
        enabled: seriesId !== '',
        retry: false,
    });
}

/**
 * The order in which a pair's spots are the freshest: the youngest age first,
 * and the provider's code when two are equally fresh.
 */
function byAgeThenCode(a: MontageSpot, b: MontageSpot): number {
    return a.age_seconds - b.age_seconds || a.provider_code.localeCompare(b.provider_code);
}

/** The freshest spot a pair has, whoever quotes it. */
export function newestSpot(pair: FxMontagePair): MontageSpot | undefined {
    return [...pair.spot_by_provider].sort(byAgeThenCode)[0];
}

/**
 * The spot the day is drawn from: the freshest one whose series is named, so
 * the read has an identity to ask for.
 *
 * The choice is made once and used for both the read and the caption. A screen
 * that read one provider's series and named another's would draw a path under
 * the wrong name.
 */
export function drawnSpot(pair: FxMontagePair | undefined): MontageSpot | undefined {
    if (pair === undefined) return undefined;
    return [...pair.spot_by_provider]
        .filter((spot) => spot.series_id !== '')
        .sort(byAgeThenCode)[0];
}
