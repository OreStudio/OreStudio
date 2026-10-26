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

/** The key of every read of the market data service's adapters, so one refresh renews them all. */
export const ADAPTERS_KEY = ['marketdata-adapters'] as const;

/** How often the screens ask again, since an adapter's state changes on its own. */
export const ADAPTERS_EVERY_MS = 5_000;

/**
 * The sources of one provider, as the market data service reports them.
 *
 * A source is one adapter of a provider, and a provider can have several. The
 * service demands a set of ids of each source, and a restart of the service
 * sends the same set again, so `demanded` is what should be running and
 * `notRunning` is the part of it that is not.
 *
 * A source is a provider's, so these counts are the truth of a provider and not
 * of one feed: whether one feed's generator runs is the feed's own `running`,
 * which the feed reads say and this status does not.
 */
export interface Source {
    /** Whether at least one source of the provider publishes what it was asked for. */
    readonly serving: boolean;
    /** How many ids the service demanded of the provider's sources. */
    readonly demanded: number;
    /** How many ticks the provider's sources published in the last minute. */
    readonly ticksPerMinute: number;
}

/** Only an adapter that is serving publishes what it was asked for. */
export function isServing(adapter: AdapterStatus): boolean {
    return adapter.state === 'serving';
}

/** The sources, by the provider each one serves. */
export function sourcesOf(adapters: readonly AdapterStatus[]): ReadonlyMap<string, Source> {
    const sources = new Map<string, Source>();
    for (const adapter of adapters) {
        const held = sources.get(adapter.provider_code);
        sources.set(adapter.provider_code, {
            serving: (held?.serving ?? false) || isServing(adapter),
            demanded: (held?.demanded ?? 0) + adapter.demanded_count,
            ticksPerMinute: (held?.ticksPerMinute ?? 0) + adapter.ticks_last_minute,
        });
    }
    return sources;
}

/**
 * The one rule of the screen: a feed serves when its provider's source serves
 * and the service demands at least one id of it. A provider with several sources
 * reads as serving while any of them serves.
 */
export function servesFeeds(source: Source | undefined): boolean {
    return source !== undefined && source.serving && source.demanded > 0;
}

/**
 * The ids the service demanded of a provider and does not get served: all of
 * them while the provider does not serve, and none while it does.
 */
export function demandedNotServed(source: Source | undefined): number {
    if (source === undefined || servesFeeds(source)) {
        return 0;
    }
    return source.demanded;
}

/** The ids the service demanded of every provider and does not get served. */
export function demandedNotServedAll(sources: ReadonlyMap<string, Source>): number {
    return [...sources.values()].reduce((count, source) => count + demandedNotServed(source), 0);
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
