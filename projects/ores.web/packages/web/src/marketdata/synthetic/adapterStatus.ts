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

import { useQuery } from '@tanstack/react-query';
import { api, type AdapterStatus } from '../../api/client.js';
import { totalOf, type Totals } from './totals.js';

/** The key of every read of the market data service's adapters, so one refresh renews them all. */
export const ADAPTERS_KEY = ['marketdata-adapters'] as const;

/** How often the screens ask again, since an adapter's state changes on its own. */
const ADAPTERS_EVERY_MS = 5_000;

/**
 * The sources of one provider, as the market data service reports them.
 *
 * A source is one adapter of a provider, and a provider can have several. The
 * service demands a set of ids of each source, and a restart of the service
 * sends the same set again, so `demanded` is what should be running and
 * `notRunning` is the part of it that is not.
 *
 * A feed names no source, so these counts are the truth of a provider and not
 * of one feed: telling which source serves one feed needs the vendor ids of the
 * generators, which the adapter status does not carry.
 */
export interface Source {
    /** Whether at least one source of the provider publishes what it was asked for. */
    readonly serving: boolean;
    /** How many ids the service demanded of the provider's sources. */
    readonly demanded: number;
    /** How many of those ids were demanded of a source that does not serve them. */
    readonly notRunning: number;
}

/** Only an adapter that is serving publishes what it was asked for. */
export function isServing(adapter: AdapterStatus): boolean {
    return adapter.state === 'serving';
}

/** Whether one adapter serves the demand it was given. */
function adapterServes(adapter: AdapterStatus): boolean {
    return isServing(adapter) && adapter.demanded_count > 0;
}

/** The sources, by the provider each one serves. */
export function sourcesOf(adapters: readonly AdapterStatus[]): ReadonlyMap<string, Source> {
    const sources = new Map<string, Source>();
    for (const adapter of adapters) {
        const held = sources.get(adapter.provider_code);
        sources.set(adapter.provider_code, {
            serving: (held?.serving ?? false) || adapterServes(adapter),
            demanded: (held?.demanded ?? 0) + adapter.demanded_count,
            notRunning:
                (held?.notRunning ?? 0) + (adapterServes(adapter) ? 0 : adapter.demanded_count),
        });
    }
    return sources;
}

/**
 * Whether the feeds of a provider are served: a source of it publishes what it
 * was asked for. A provider with several sources reads as serving while any of
 * them serves; the demand none of them serves shows in its count.
 */
export function servesFeeds(source: Source | undefined): boolean {
    return source !== undefined && source.serving && source.demanded > 0;
}

/** How many ids the service demanded of a provider and does not get served. */
export function demandedNotRunning(source: Source | undefined): number {
    return source?.notRunning ?? 0;
}

/** The ids the service demanded of every provider and does not get served. */
export function demandedNotRunningAll(sources: ReadonlyMap<string, Source>): number {
    return [...sources.values()].reduce((count, source) => count + demandedNotRunning(source), 0);
}

/**
 * What runs under a node once its source has the last word: every feed it holds
 * runs while its provider's source serves, and none runs while it does not.
 */
export function servedTotals(totals: Totals, source: Source | undefined): Totals {
    return { ...totals, running: servesFeeds(source) ? totalOf(totals) : 0 };
}

/** The sources of every adapter the service tracks, or undefined until it answers. */
export function useSources(): ReadonlyMap<string, Source> | undefined {
    const adapters = useQuery({
        queryKey: ADAPTERS_KEY,
        queryFn: api.adapterStatuses,
        refetchInterval: ADAPTERS_EVERY_MS,
    });
    return adapters.data === undefined ? undefined : sourcesOf(adapters.data);
}
