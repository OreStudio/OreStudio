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
 * What the screen knows about each convention kind that the server does not
 * say: the asset class it serves, and whether the party or the tenant holds it.
 *
 * The model carries neither. The grouping is the screen's, so a kind that the
 * registry adds and this table lacks still shows, under Other, and a test
 * names it.
 */

/** The asset classes of the product model that a convention kind serves. */
export const ASSET_CLASSES = [
    'interest-rates',
    'fx',
    'inflation',
    'credit',
    'commodity',
    'bond',
] as const;

export type AssetClass = (typeof ASSET_CLASSES)[number];

const CLASS_OF: Readonly<Record<string, AssetClass>> = {
    'average-ois': 'interest-rates',
    'bma-basis-swap': 'interest-rates',
    'cms-spread-option': 'interest-rates',
    deposit: 'interest-rates',
    fra: 'interest-rates',
    future: 'interest-rates',
    'ibor-index': 'interest-rates',
    ois: 'interest-rates',
    'overnight-index': 'interest-rates',
    swap: 'interest-rates',
    'swap-index': 'interest-rates',
    tenor: 'interest-rates',
    'tenor-basis-swap': 'interest-rates',
    'tenor-basis-two-swap': 'interest-rates',
    zero: 'interest-rates',
    'cross-currency-basis': 'fx',
    'cross-currency-fix-float': 'fx',
    'currency-pair': 'fx',
    'fx-option': 'fx',
    'inflation-swap': 'inflation',
    'zero-inflation-index': 'inflation',
    cds: 'credit',
    'commodity-forward': 'commodity',
    'commodity-future': 'commodity',
    'intraday-power-load': 'commodity',
    'bond-yield': 'bond',
};

/** The kinds that carry no party: held once for the tenant, and read by every party. */
const SHARED: ReadonlySet<string> = new Set([
    'currency-pair',
    'ibor-index',
    'overnight-index',
    'tenor',
]);

/** The asset class a kind serves, or undefined for a kind this table does not know. */
export function assetClassOf(family: string): AssetClass | undefined {
    return CLASS_OF[family];
}

/** Whether every party reads the kind's documents, because the tenant holds them once. */
export function isShared(family: string): boolean {
    return SHARED.has(family);
}
