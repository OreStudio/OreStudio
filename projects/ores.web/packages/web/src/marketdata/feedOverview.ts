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

import type { FeedFolder } from '../api/client.js';

/** Who a collection's feeds are for: the installation's own, or a tenant's or party's test feeds. */
export type OverviewScope = 'system' | 'test';

/** The provider code a collection stands for none of. The catalogue calls it generic. */
export const GENERIC_PROVIDER = 'generic';

/** What runs under a folder: running, stopped or some of its feeds. */
export type RunState = 'running' | 'stopped' | 'partial' | 'empty';

/** The feeds under a node, the sum of the folders it stands for. */
export interface Totals {
    readonly fx: number;
    readonly ir: number;
    readonly running: number;
}

/** The feeds a node holds in all. */
export function totalOf(totals: Totals): number {
    return totals.fx + totals.ir;
}

/** Running when all run, stopped when none runs, and partial between; empty with no feeds. */
export function stateOf(totals: Totals): RunState {
    const all = totalOf(totals);
    if (all === 0) {
        return 'empty';
    }
    if (totals.running === 0) {
        return 'stopped';
    }
    return totals.running >= all ? 'running' : 'partial';
}

function sum(folders: readonly FeedFolder[]): Totals {
    return folders.reduce<Totals>(
        (out, folder) => ({
            fx: out.fx + folder.fx_feeds,
            ir: out.ir + folder.ir_feeds,
            running: out.running + folder.running_feeds,
        }),
        { fx: 0, ir: 0, running: 0 },
    );
}

/** A collection is the installation's when its scope says system, and a test feed's otherwise. */
export function scopeOf(collection: FeedFolder): OverviewScope {
    return collection.scope === 'system' ? 'system' : 'test';
}

/** The provider a collection stands for, or generic when it stands for none. */
export function providerOf(collection: FeedFolder): string {
    return collection.provider_code === '' ? GENERIC_PROVIDER : collection.provider_code;
}

/** One entry of the scope column. */
export interface ScopeEntry {
    readonly scope: OverviewScope;
    readonly collections: readonly FeedFolder[];
    readonly totals: Totals;
}

/** The two scopes, system first, each with the collections that belong to it. */
export function scopesOf(collections: readonly FeedFolder[]): readonly ScopeEntry[] {
    return (['system', 'test'] as const).map((scope) => {
        const inScope = collections.filter((collection) => scopeOf(collection) === scope);
        return { scope, collections: inScope, totals: sum(inScope) };
    });
}

/** One entry of the provider column: a provider and the collections that stand for it. */
export interface ProviderEntry {
    readonly code: string;
    readonly collections: readonly FeedFolder[];
    readonly totals: Totals;
    /** Whether any of its collections is bound, so its ticks reach the parties that subscribe. */
    readonly bound: boolean;
}

/** The providers of one scope, in name order of their codes, each with its collections. */
export function providersOf(collections: readonly FeedFolder[]): readonly ProviderEntry[] {
    const byCode = new Map<string, FeedFolder[]>();
    for (const collection of collections) {
        const code = providerOf(collection);
        byCode.set(code, [...(byCode.get(code) ?? []), collection]);
    }
    return [...byCode.entries()]
        .sort(([a], [b]) => a.localeCompare(b))
        .map(([code, held]) => ({
            code,
            collections: held,
            totals: sum(held),
            bound: held.some((collection) => collection.binding_mode === 'bound'),
        }));
}

/** The totals of the given folders. */
export function totalsOf(folders: readonly FeedFolder[]): Totals {
    return sum(folders);
}

/**
 * A provider's name for a screen: the catalogue's name when it has one, else
 * its code with a capital. Every provider is simulated, so the name carries
 * the synthetic mark where the screen draws it.
 */
export function providerName(code: string, catalogue: ReadonlyMap<string, string>): string {
    return catalogue.get(code) ?? `${code.charAt(0).toUpperCase()}${code.slice(1)}`;
}
