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

/**
 * The colour an asset class is drawn in, wherever a screen draws one.
 *
 * One scheme, stated here and nowhere else: the colours are data colours, named
 * for the class and carrying no other meaning, so a series is never mistaken for
 * a control, a warning or an error. The tokens live in =styles.css=; a class this
 * scheme does not name is drawn in the accent, so a new class appears at once
 * rather than not at all.
 */
export interface AssetClassColour {
    /** The fill of a mark, such as an arrival's dot. */
    readonly dot: string;
    /** The same colour for text and for a line, which strokes in currentColor. */
    readonly text: string;
}

/** The scheme, by the code the asset class carries. */
const SCHEME: Readonly<Record<string, AssetClassColour>> = {
    fx: { dot: 'bg-class-fx', text: 'text-class-fx' },
    interest_rates: { dot: 'bg-class-interest-rates', text: 'text-class-interest-rates' },
    equity: { dot: 'bg-class-equity', text: 'text-class-equity' },
    credit: { dot: 'bg-class-credit', text: 'text-class-credit' },
    commodity: { dot: 'bg-class-commodity', text: 'text-class-commodity' },
    bond: { dot: 'bg-class-bond', text: 'text-class-bond' },
    inflation: { dot: 'bg-class-inflation', text: 'text-class-inflation' },
};

/** What a class the scheme does not name is drawn in. */
export const UNKNOWN_ASSET_CLASS: AssetClassColour = {
    dot: 'bg-accent',
    text: 'text-accent',
};

/** The colour of @p assetClass, or the accent's for a class the scheme does not name. */
export function assetClassColour(assetClass: string): AssetClassColour {
    return SCHEME[assetClass] ?? UNKNOWN_ASSET_CLASS;
}

/** The codes the scheme names, which is what a test and a legend read. */
export const NAMED_ASSET_CLASSES: readonly string[] = Object.keys(SCHEME);
