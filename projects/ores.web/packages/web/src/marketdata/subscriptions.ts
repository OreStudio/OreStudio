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
import {
    api,
    type MarketDataCatalogue,
    type MarketDataSource,
    type RouteRow,
    type SubscriptionRow,
} from '../api/client.js';

/**
 * What a party takes from the market data service, from
 * doc/knowledge/journeys/market_data/journey_manage-market-data-subscriptions.org.
 *
 * A subscription names a provider and its rights. A route names the one source
 * that serves the party for a provider. Neither names the party: the service
 * states it from the session, so the screen reads the party of each row and
 * never writes one.
 */

/** The reads the screen makes, so one refresh renews all of them. */
export const SUBSCRIPTIONS_KEY = ['marketdata-subscriptions'] as const;
export const ROUTES_KEY = ['marketdata-routes'] as const;
export const CATALOGUE_KEY = ['marketdata-catalogue'] as const;

/** The three rights a subscription carries, in the order the vendor licences name them. */
export const RIGHTS = ['display', 'non_display', 'derived'] as const;
export type Right = (typeof RIGHTS)[number];

/** The rights one subscription holds. */
export function rightsOf(subscription: SubscriptionRow): readonly Right[] {
    return RIGHTS.filter((right) => subscription[right]);
}

/** Whether a row belongs to the party the session acts in. */
export function heldBy(subscription: { readonly party_id: string }, partyId: string): boolean {
    return subscription.party_id === partyId;
}

/** The name of one provider, or its code when the catalogue does not name it. */
export function providerName(catalogue: MarketDataCatalogue | undefined, code: string): string {
    return catalogue?.providers.find((provider) => provider.code === code)?.name ?? code;
}

/** The source one route names, or nothing when the catalogue does not hold it. */
export function routeSource(
    catalogue: MarketDataCatalogue | undefined,
    route: RouteRow,
): MarketDataSource | undefined {
    return catalogue?.sources.find((source) => source.id === route.source_id);
}

/** The name of one source, or its id when the catalogue does not name it. */
export function sourceName(catalogue: MarketDataCatalogue | undefined, route: RouteRow): string {
    return routeSource(catalogue, route)?.name ?? route.source_id;
}

/** The name of one party, or the id the service stated when the parties list does not hold it. */
export function partyName(parties: ReadonlyMap<string, string>, partyId: string): string {
    return parties.get(partyId) ?? partyId;
}

/** The providers a party may still take: those the catalogue names and its subscriptions do not. */
export function providersToTake(
    catalogue: MarketDataCatalogue | undefined,
    held: readonly SubscriptionRow[],
    partyId: string,
): readonly { readonly code: string; readonly name: string }[] {
    const taken = new Set(
        held
            .filter((subscription) => subscription.party_id === partyId)
            .map((row) => row.provider_code),
    );
    return (catalogue?.providers ?? [])
        .filter((provider) => !taken.has(provider.code))
        .map((provider) => ({ code: provider.code, name: provider.name }));
}

/** The sources that may serve a party for one provider: the provider's own, its default first. */
export function sourcesForProvider(
    catalogue: MarketDataCatalogue | undefined,
    providerCode: string,
): readonly MarketDataSource[] {
    return (catalogue?.sources ?? [])
        .filter((source) => source.provider_code === providerCode)
        .sort((left, right) => Number(right.is_default) - Number(left.is_default));
}

/** The providers a party holds a subscription to and has not routed yet. */
export function providersToRoute(
    subscriptions: readonly SubscriptionRow[],
    routes: readonly RouteRow[],
    partyId: string,
): readonly string[] {
    const routed = new Set(
        routes.filter((route) => route.party_id === partyId).map((route) => route.provider_code),
    );
    return subscriptions
        .filter((row) => row.party_id === partyId && !routed.has(row.provider_code))
        .map((row) => row.provider_code);
}

/** Every subscription the session may read. */
export function useSubscriptions() {
    return useQuery({ queryKey: SUBSCRIPTIONS_KEY, queryFn: api.marketDataSubscriptions });
}

/** Every route the session may read. */
export function useRoutes() {
    return useQuery({ queryKey: ROUTES_KEY, queryFn: api.marketDataRoutes });
}

/** The providers and the sources both tables choose from. */
export function useCatalogue() {
    return useQuery({ queryKey: CATALOGUE_KEY, queryFn: api.marketDataCatalogue });
}

/** The names of the session's tenant parties, by the id a subscription states. */
export function usePartyNames() {
    return useQuery({
        queryKey: ['marketdata-party-names'],
        queryFn: async () => {
            const page = await api.parties({ offset: 0, limit: 1000 });
            return new Map(page.parties.map((party) => [party.id, party.name]));
        },
    });
}
