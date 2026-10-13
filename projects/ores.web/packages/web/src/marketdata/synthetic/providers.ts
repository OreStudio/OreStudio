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

import type { FeedFolder } from '../../api/client.js';
import { totalsOf, type Totals } from './totals.js';

/** One provider and the collections that stand for it. */
export interface ProviderEntry {
    readonly code: string;
    readonly collections: readonly FeedFolder[];
    readonly totals: Totals;
    /** Whether any of its collections is bound, so its ticks reach the parties that subscribe. */
    readonly bound: boolean;
}

/** The providers of the given collections, in the order of their codes. */
export function providersOf(collections: readonly FeedFolder[]): readonly ProviderEntry[] {
    const byCode = new Map<string, FeedFolder[]>();
    for (const collection of collections) {
        byCode.set(collection.provider_code, [
            ...(byCode.get(collection.provider_code) ?? []),
            collection,
        ]);
    }
    return [...byCode.entries()]
        .sort(([a], [b]) => a.localeCompare(b))
        .map(([code, held]) => ({
            code,
            collections: held,
            totals: totalsOf(held),
            bound: held.some((collection) => collection.binding_mode === 'bound'),
        }));
}

/** What the catalogue says about a provider. */
export interface ProviderFacts {
    readonly name: string;
    readonly kind: string;
    readonly description: string;
}

/** A provider's name for a screen: the catalogue's name, else its code with a capital. */
export function providerName(code: string, catalogue: ReadonlyMap<string, ProviderFacts>): string {
    return catalogue.get(code)?.name ?? `${code.charAt(0).toUpperCase()}${code.slice(1)}`;
}
