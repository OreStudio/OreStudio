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

import { show, useRecords } from '../../refdata/records.js';

/**
 * The source names of the synthetic feeds, by the naming scheme
 * synthetic.<source>.<asset_class>.<instrument>.<subject>, spelled from oresmd. The
 * publish spells the same names in SQL (synthetic_feed_names_create.sql), and the service
 * spells a vendor view's in C++.
 */

const lower = (text: string): string => text.toLowerCase();

/** A spot: synthetic.bloomberg.fx.fx_spot.audusd. */
export function fxSpotName(source: string, base: string, quote: string): string {
    return lower(`synthetic.${source}.fx.fx_spot.${base}${quote}`);
}

/**
 * A yield curve, the index before the currency so the curves of one index sort together, and
 * the index tenor last when it has one: synthetic.bloomberg.ir.yield_curve.sofr.usd.
 */
export function yieldCurveName(
    source: string,
    indexFamily: string,
    currency: string,
    tenor: string,
): string {
    const term = tenor.replaceAll(' ', '');
    return lower(
        `synthetic.${source}.ir.yield_curve.${indexFamily}.${currency}${term === '' ? '' : `.${term}`}`,
    );
}

/** A vol surface: synthetic.latent.fx.fx_vol_surface.audusd. */
export function fxVolSurfaceName(source: string, base: string, quote: string): string {
    return lower(`synthetic.${source}.fx.fx_vol_surface.${base}${quote}`);
}

/** One vendor's quotes of a surface, in the vendor's namespace: synthetic.bloomberg.fx.fx_option.audusd. */
export function fxOptionViewName(provider: string, base: string, quote: string): string {
    return lower(`synthetic.${provider}.fx.fx_option.${base}${quote}`);
}

/**
 * A collection's source: its provider, or its name without spaces when it names none, as the
 * publish takes it.
 */
export function sourceOfCollection(
    collection: Readonly<Record<string, unknown>> | undefined,
): string {
    const provider = show(collection?.['provider_code']);
    return provider !== '' ? provider : lower(show(collection?.['name']).replaceAll(' ', ''));
}

/** The source of each readable collection, by its id. */
export function useSourceOf(): (collectionId: string) => string {
    const collections = useRecords('collections');
    const rows = collections.data ?? [];
    return (collectionId) =>
        sourceOfCollection(rows.find((row) => show(row['id']) === collectionId));
}
