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

import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import { describe, expect, it } from 'vitest';
import type { MarketDataCatalogue, RouteRow, SubscriptionRow } from '../api/client.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { SubscriptionsPage } from './SubscriptionsPage.js';
import {
    CATALOGUE_KEY,
    ROUTES_KEY,
    SUBSCRIPTIONS_KEY,
    heldBy,
    partyName,
    providerName,
    providersToRoute,
    providersToTake,
    rightsOf,
    sourceName,
    sourcesForProvider,
} from './subscriptions.js';

/**
 * The subscriptions screen, as a party administrator reads it.
 *
 * What is checked is what the screen promises: the rights a subscription holds
 * are the service's three, a route names a source the catalogue knows, and the
 * selectors offer only what the party may still take.
 */

const PARTY = '3f1e2d4c-0000-4000-8000-000000000010';
const OTHER = '3f1e2d4c-0000-4000-8000-000000000011';
const BLOOMBERG_SOURCE = '0a42775e-3da7-4c61-a732-6b62cb9f948f';
const LSEG_SOURCE = 'c0f53bf1-a03a-4557-9906-7213091fcd82';

function subscription(overrides: Partial<SubscriptionRow> = {}): SubscriptionRow {
    return {
        version: 2,
        id: '55555555-5555-4555-8555-555555555555',
        party_id: PARTY,
        provider_code: 'bloomberg',
        display: true,
        non_display: true,
        derived: false,
        modified_by: 'tenant_admin',
        performed_by: 'tenant_admin',
        change_reason_code: 'system.new_record',
        change_commentary: '',
        recorded_at: '2026-10-26 09:00:00Z',
        ...overrides,
    };
}

function route(overrides: Partial<RouteRow> = {}): RouteRow {
    return {
        version: 1,
        id: '66666666-6666-4666-8666-666666666666',
        party_id: PARTY,
        provider_code: 'bloomberg',
        source_id: BLOOMBERG_SOURCE,
        modified_by: 'tenant_admin',
        performed_by: 'tenant_admin',
        change_reason_code: 'system.new_record',
        change_commentary: '',
        recorded_at: '2026-10-26 09:00:00Z',
        ...overrides,
    };
}

const CATALOGUE: MarketDataCatalogue = {
    providers: [
        {
            code: 'bloomberg',
            name: 'Bloomberg',
            kind: 'data_vendor',
            description: '',
            display_order: 1,
        },
        { code: 'lseg', name: 'LSEG', kind: 'data_vendor', description: '', display_order: 2 },
        { code: 'ore', name: 'ORE', kind: 'reference', description: '', display_order: 3 },
    ],
    sources: [
        {
            id: BLOOMBERG_SOURCE,
            name: 'bloomberg-a',
            provider_code: 'bloomberg',
            kind: 'simulated',
            is_default: false,
            description: '',
        },
        {
            id: LSEG_SOURCE,
            name: 'lseg-a',
            provider_code: 'lseg',
            kind: 'simulated',
            is_default: true,
            description: '',
        },
    ],
};

describe('the rights of a subscription', () => {
    it('are the three the vendor licences name, and only those held', () => {
        expect(rightsOf(subscription())).toEqual(['display', 'non_display']);
        expect(rightsOf(subscription({ display: false, derived: true }))).toEqual([
            'non_display',
            'derived',
        ]);
        expect(rightsOf(subscription({ display: false, non_display: false }))).toEqual([]);
    });

    it('decide which party may remove a row', () => {
        expect(heldBy(subscription(), PARTY)).toBe(true);
        expect(heldBy(subscription(), OTHER)).toBe(false);
    });
});

describe('the selectors', () => {
    it('offer the providers the party has not taken yet', () => {
        expect(providersToTake(CATALOGUE, [subscription()], PARTY).map((row) => row.code)).toEqual([
            'lseg',
            'ore',
        ]);
    });

    it('offer a subscription another party holds, because this party does not hold it', () => {
        const other = subscription({ party_id: OTHER, provider_code: 'lseg' });

        expect(providersToTake(CATALOGUE, [other], PARTY).map((row) => row.code)).toEqual([
            'bloomberg',
            'lseg',
            'ore',
        ]);
    });

    it('offer the sources of one provider, its default first', () => {
        expect(sourcesForProvider(CATALOGUE, 'lseg').map((source) => source.name)).toEqual([
            'lseg-a',
        ]);
        expect(sourcesForProvider(CATALOGUE, 'bloomberg').map((source) => source.name)).toEqual([
            'bloomberg-a',
        ]);
    });

    it('route a provider the party holds and has not routed', () => {
        const held = [subscription(), subscription({ provider_code: 'lseg' })];

        expect(providersToRoute(held, [], PARTY)).toEqual(['bloomberg', 'lseg']);
        expect(providersToRoute(held, [route()], PARTY)).toEqual(['lseg']);
    });

    it('name a provider and a source, and fall back to the code when the catalogue is silent', () => {
        expect(providerName(CATALOGUE, 'bloomberg')).toBe('Bloomberg');
        expect(providerName(CATALOGUE, 'demo')).toBe('demo');
        expect(sourceName(CATALOGUE, route())).toBe('bloomberg-a');
        expect(sourceName(undefined, route())).toBe(BLOOMBERG_SOURCE);
        expect(partyName(new Map([[PARTY, 'Acme']]), PARTY)).toBe('Acme');
        expect(partyName(new Map(), OTHER)).toBe(OTHER);
    });
});

/** The screen, with the reads it makes already answered. */
function render(permissions: readonly string[]): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['my-access'], {
        roles: [
            {
                roleId: 'r',
                name: 'Party administrator',
                description: '',
                permissionCodes: [...permissions],
                givenBy: 'system',
                givenAt: '2026-10-26 09:30:00Z',
                reasonCode: 'access.initial',
                commentary: '',
            },
        ],
    });
    client.setQueryData(SUBSCRIPTIONS_KEY, [
        subscription(),
        subscription({
            id: '55555555-5555-4555-8555-555555555556',
            party_id: OTHER,
            provider_code: 'lseg',
            display: true,
            non_display: false,
        }),
    ]);
    client.setQueryData(ROUTES_KEY, [route()]);
    client.setQueryData(CATALOGUE_KEY, CATALOGUE);
    client.setQueryData(['marketdata-party-names'], new Map([[PARTY, 'Acme']]));
    client.setQueryData(['reference-data-reasons', 'delete'], []);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter>
                    <SubscriptionsPage partyId={PARTY} />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the subscriptions screen', () => {
    it('lists the party subscriptions with their rights, and the routes with their source', () => {
        const html = render([]);

        expect(html).toContain('Subscriptions');
        expect(html).toContain('Bloomberg');
        expect(html).toContain('display');
        expect(html).toContain('non-display');
        expect(html).toContain('bloomberg-a');
        expect(html).toContain('simulated');
    });

    it('draws Add and Delete only for a person who may write and delete', () => {
        const reader = render([]);
        expect(reader).not.toContain('>Add<');
        expect(reader).not.toContain('>Add a route<');
        expect(reader).not.toContain('>Delete<');

        const writer = render([
            'marketdata::subscriptions:write',
            'marketdata::subscriptions:delete',
            'marketdata::routes:write',
            'marketdata::routes:delete',
        ]);
        expect(writer).toContain('>Add<');
        expect(writer).toContain('>Add a route<');
    });

    it('offers Delete on the rows of the party the session acts in, and on no other', () => {
        const writer = render(['marketdata::subscriptions:delete', 'marketdata::routes:delete']);
        const deletes = writer.split('>Delete<').length - 1;

        expect(deletes).toBe(2);
    });

    it('states the gaps the service and the web still leave open', () => {
        const html = render([]);

        expect(html).toContain('Suspending a subscription');
        expect(html).toContain('The history of a subscription');
        expect(html).toContain('The usage and the cost');
    });
});
