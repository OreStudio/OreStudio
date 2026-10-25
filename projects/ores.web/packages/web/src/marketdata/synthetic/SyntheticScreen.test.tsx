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

import { describe, expect, it, vi } from 'vitest';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter, Route, Routes } from 'react-router';
import type { AccountAccess } from '@ores/wire-protocol/browser';
import type {
    AdapterStatus,
    FeedFolder,
    FolderFeed,
    SyntheticResourceView,
} from '../../api/client.js';
import { TranslationProvider } from '../../i18n/Provider.js';
import { ADAPTERS_KEY } from './adapterStatus.js';
import { FEEDS_KEY } from './selectionRuns.js';
import { SyntheticScreen } from './SyntheticScreen.js';
import { FOLDERS_KEY } from './useCollections.js';
import { HELD_KEY } from './useProviderFeeds.js';

vi.mock('../../session/SessionProvider.js', () => ({ useActingPartyId: () => 'party-1' }));

function access(codes: string[]): AccountAccess {
    return {
        roles: [
            {
                roleId: '33333333-3333-3333-3333-333333333333' as AccountAccess['roles'][number]['roleId'],
                name: 'Quant',
                description: '',
                permissionCodes: codes,
                givenBy: 'priya',
                givenAt: '2026-10-04',
                reasonCode: 'access.new_joiner',
                commentary: '',
            },
        ],
    };
}

function resource(key: string, name: string): SyntheticResourceView {
    return {
        key,
        entityType: `ores.synthetic.${name}`,
        keyField: 'id',
        writable: true,
        readOnlyBecause: null,
        search: true,
        sortable: [],
        readPermission: `synthetic::${name}s:read`,
        writePermission: `synthetic::${name}s:write`,
        deletePermission: `synthetic::${name}s:delete`,
    };
}

const REGISTRY = [
    resource('fx-feeds', 'fx_spot_generation_config'),
    resource('ir-curve-feeds', 'ir_curve_generation_config'),
];

const ROOT: FeedFolder = {
    id: 'r',
    parent_id: '',
    name: 'Synthetic',
    kind: 'root',
    provider_code: '',
    fx_feeds: 26,
    ir_feeds: 4,
    has_children: true,
};

function collection(
    id: string,
    name: string,
    provider: string,
    fx: number,
    ir: number,
): FeedFolder {
    return {
        id,
        parent_id: 'r',
        name,
        kind: 'collection',
        provider_code: provider,
        fx_feeds: fx,
        ir_feeds: ir,
        has_children: true,
    };
}

const BLOOMBERG = collection('b', 'Bloomberg', 'bloomberg', 13, 0);
const ORE = collection('o', '2016 ORE Samples', 'ore', 13, 4);
const BLOOMBERG_EXTRA = collection('b2', 'Bloomberg Extra', 'bloomberg', 0, 0);

/** One adapter of the market data service, as it reports itself. */
function adapter(provider: string, state: string, demanded: number): AdapterStatus {
    return {
        adapter_id: `${provider}-sim`,
        provider_code: provider,
        state,
        last_heartbeat_at: '2026-10-25 15:00:00Z',
        heartbeat_counter: 42,
        served_count: 26,
        unknown_count: 0,
        demanded_count: demanded,
        routes_seen: 3,
        ticks_last_minute: 60,
        last_tick_age_ms: 200,
        accepted: [],
        rejected: [],
    };
}

/** Bloomberg's source serves its feeds, and the ORE source is demanded 17 ids it does not serve. */
const ADAPTERS = [adapter('bloomberg', 'serving', 13), adapter('ore', 'ready', 17)];

const PROVIDERS = [
    {
        version: 1,
        code: 'bloomberg',
        name: 'Bloomberg',
        kind: 'data_vendor',
        description: 'Simulates Bloomberg composite quotes.',
    },
    {
        version: 1,
        code: 'ore',
        name: 'ORE',
        kind: 'reference',
        description: 'Simulates the ORE examples.',
    },
];

/** The feeds each provider holds, counted from the feeds themselves. */
const HELD = new Map([
    ['bloomberg', { fx: 13, ir: 0 }],
    ['ore', { fx: 13, ir: 4 }],
]);

function feed(
    id: string,
    kind: 'fx_spot' | 'ir_curve',
    label: string,
    underlying: string,
    patch: Partial<FolderFeed> = {},
): FolderFeed {
    return {
        id,
        kind,
        label,
        source_name: `synthetic.${id}`,
        oresmd_uri: '',
        folder_id: 'b',
        enabled: true,
        provider_code: 'bloomberg',
        asset_class_code: kind === 'fx_spot' ? 'fx' : 'interest_rates',
        underlying,
        engine: kind === 'fx_spot' ? 'geometric' : 'VASICEK',
        ticks_per_hour: kind === 'fx_spot' ? 1800 : 60,
        auto_start: true,
        ...patch,
    };
}

const EURUSD = feed('eurusd', 'fx_spot', 'EUR/USD', 'EUR/USD');
const GBPUSD = feed('gbpusd', 'fx_spot', 'GBP/USD', 'GBP/USD');
const SOFR = feed('usdsofr', 'ir_curve', 'USD SOFR', 'USD', { enabled: false });

/** The words of the page, with the markup taken out, so a figure sits beside its label. */
function textOf(html: string): string {
    return html
        .replace(/<[^>]*>/g, ' ')
        .replace(/\s+/g, ' ')
        .trim();
}

function render(path: string, seed: (client: QueryClient) => void): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['registry', 'synthetic'], REGISTRY);
    client.setQueryData([...FOLDERS_KEY, 'roots'], [ROOT]);
    client.setQueryData([...FOLDERS_KEY, 'collections', ['r']], [BLOOMBERG, ORE]);
    client.setQueryData(ADAPTERS_KEY, ADAPTERS);
    client.setQueryData([...HELD_KEY, ['bloomberg', 'ore']], HELD);
    client.setQueryData(['records', 'providers'], PROVIDERS);
    seed(client);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[path]}>
                    <Routes>
                        <Route path="/market-data/synthetic" element={<SyntheticScreen />} />
                    </Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the Synthetic dashboard', () => {
    it('names the two tabs and lists each provider with what it simulates', () => {
        const html = render('/market-data/synthetic', (client) => {
            client.setQueryData(['my-access'], access(['synthetic::*']));
        });
        expect(html).toContain('>Dashboard<');
        expect(html).toContain('>Provider<');
        expect(html).toContain('Bloomberg');
        expect(html).toContain('Simulates Bloomberg composite quotes.');
        expect(html).toContain('Simulates the ORE examples.');
        expect(html).toContain('13 FX · 0 IR');
        expect(html).toContain('>Serving<');
        expect(html).not.toContain('(synthetic)');
        expect(html).not.toContain('Generic');
    });

    it('counts the five figures by the one rule, from the feeds each provider holds', () => {
        const html = render('/market-data/synthetic', (client) => {
            client.setQueryData(['my-access'], access(['synthetic::*']));
        });
        const text = textOf(html);
        expect(text).toContain('Providers 2');
        expect(text).toContain('Feeds running 13');
        expect(text).toContain('Feeds stopped 17');
        expect(text).toContain('Ids demanded 30');
        expect(text).toContain('Ticks per minute 120');
    });

    it('counts no feed as serving for a provider whose source was demanded nothing', () => {
        const html = render('/market-data/synthetic', (client) => {
            client.setQueryData(['my-access'], access(['synthetic::*']));
            client.setQueryData(ADAPTERS_KEY, [
                adapter('bloomberg', 'serving', 13),
                adapter('ore', 'serving', 0),
            ]);
            client.setQueryData([...HELD_KEY, ['bloomberg', 'ore']], HELD);
        });
        const text = textOf(html);
        expect(text).toContain('Feeds running 13');
        expect(text).toContain('Feeds stopped 17');
        expect(html).toContain('>Stopped<');
        expect(html).not.toContain('demanded ids are not being served');
    });

    it('counts every feed as stopped while its provider does not serve', () => {
        const html = render('/market-data/synthetic', (client) => {
            client.setQueryData(['my-access'], access(['synthetic::*']));
            client.setQueryData(ADAPTERS_KEY, [
                adapter('bloomberg', 'ready', 13),
                adapter('ore', 'ready', 17),
            ]);
        });
        const text = textOf(html);
        expect(text).toContain('Feeds running 0');
        expect(text).toContain('Feeds stopped 30');
        expect(text).toContain('30 demanded ids are not being served.');
    });

    it('counts every feed as serving while every source serves', () => {
        const html = render('/market-data/synthetic', (client) => {
            client.setQueryData(['my-access'], access(['synthetic::*']));
            client.setQueryData(ADAPTERS_KEY, [
                adapter('bloomberg', 'serving', 13),
                adapter('ore', 'serving', 17),
            ]);
        });
        const text = textOf(html);
        expect(text).toContain('Feeds running 30');
        expect(text).toContain('Feeds stopped 0');
        expect(html).not.toContain('demanded ids are not being served');
    });

    it('alerts only for demanded ids that are not being served', () => {
        const html = render('/market-data/synthetic', (client) => {
            client.setQueryData(['my-access'], access(['synthetic::*']));
        });
        expect(html).toContain('17 demanded ids are not being served.');
        expect(html).not.toContain('13 demanded ids are not being served.');
    });

    it('opens the Provider screen on the provider of a row', () => {
        const html = render('/market-data/synthetic', (client) => {
            client.setQueryData(['my-access'], access(['synthetic::*']));
        });
        expect(html).toContain('href="/market-data/synthetic?tab=provider&amp;provider=bloomberg"');
    });

    it('offers no start or stop to a person who may write the feeds but not run them', () => {
        const html = render('/market-data/synthetic', (client) => {
            client.setQueryData(
                ['my-access'],
                access([
                    'synthetic::fx_spot_generation_configs:read',
                    'synthetic::fx_spot_generation_configs:write',
                ]),
            );
        });
        expect(html).toContain('Bloomberg');
        expect(html).not.toContain('>Start<');
        expect(html).not.toContain('>Stop<');
    });

    it('offers start and stop, in their words, to a person who holds the run permission', () => {
        const html = render('/market-data/synthetic', (client) => {
            client.setQueryData(
                ['my-access'],
                access(['synthetic::fx_spot_generation_configs:read', 'synthetic::feeds:run']),
            );
        });
        expect(html).toContain('>Start<');
        expect(html).toContain('>Stop<');
    });
});

describe('the Synthetic Provider screen', () => {
    function seeded(client: QueryClient, codes: string[]): void {
        client.setQueryData(['my-access'], access(codes));
        client.setQueryData([...FEEDS_KEY, '{"provider":"bloomberg"}', 0, 15], {
            total: 3,
            feeds: [EURUSD, GBPUSD, SOFR],
        });
    }

    it('lists the feeds of a provider with the columns the prototype signs off', () => {
        const html = render('/market-data/synthetic?tab=provider', (client) =>
            seeded(client, ['synthetic::*']),
        );
        expect(html).toContain('>State<');
        expect(html).toContain('>Enabled<');
        expect(html).toContain('>Name<');
        expect(html).toContain('>Asset class<');
        expect(html).toContain('>Underlying<');
        expect(html).toContain('>Engine<');
        expect(html).toContain('>Frequency<');
        expect(html).toContain('EUR/USD');
        expect(html).toContain('USD SOFR');
        expect(html).toContain('>Geometric<');
        expect(html).toContain('>Vasicek<');
        expect(html).toContain('Every 2 s');
        expect(html).toContain('Every 1 min');
        expect(html).toContain('>Interest rate<');
        expect(html).toContain('>Enabled<');
        expect(html).toContain('>Disabled<');
        expect(html).toContain('href="/market-data/synthetic/fx/eurusd"');
        expect(html).toContain('href="/market-data/synthetic/ir/usdsofr"');
    });

    it('has no oresmd URI column, no search and no folder tree', () => {
        const html = render('/market-data/synthetic?tab=provider', (client) =>
            seeded(client, ['synthetic::*']),
        );
        expect(html).not.toContain('oresmd URI');
        expect(html).not.toContain('Search every provider');
        expect(html).not.toContain('role="tree"');
        expect(html).not.toContain('All providers');
    });

    it('groups the feeds by asset class, its default', () => {
        const html = render('/market-data/synthetic?tab=provider', (client) =>
            seeded(client, ['synthetic::*']),
        );
        expect(html).toContain('FX · 2');
        expect(html).toContain('Interest rate · 1');
    });

    it('groups the feeds by instrument when the person asks for it', () => {
        const html = render('/market-data/synthetic?tab=provider&group=underlying', (client) =>
            seeded(client, ['synthetic::*']),
        );
        expect(html).toContain('EUR/USD · 1');
        expect(html).toContain('GBP/USD · 1');
        expect(html).toContain('USD · 1');
        expect(html).not.toContain('FX · 2');
    });

    it('groups the feeds by engine when the person asks for it', () => {
        const html = render('/market-data/synthetic?tab=provider&group=engine', (client) =>
            seeded(client, ['synthetic::*']),
        );
        expect(html).toContain('Geometric · 2');
        expect(html).toContain('Vasicek · 1');
        expect(html).not.toContain('FX · 2');
    });

    it('shows the collection filter only for a provider that holds several collections', () => {
        const one = render('/market-data/synthetic?tab=provider', (client) =>
            seeded(client, ['synthetic::*']),
        );
        expect(one).not.toContain('All collections');
        const two = render('/market-data/synthetic?tab=provider', (client) => {
            client.setQueryData(['my-access'], access(['synthetic::*']));
            client.setQueryData(
                [...FOLDERS_KEY, 'collections', ['r']],
                [BLOOMBERG, BLOOMBERG_EXTRA, ORE],
            );
            client.setQueryData([...FEEDS_KEY, '{"provider":"bloomberg"}', 0, 15], {
                total: 3,
                feeds: [EURUSD, GBPUSD, SOFR],
            });
        });
        expect(two).toContain('All collections');
        expect(two).toContain('Bloomberg Extra');
    });

    it('reads each feed as running from its provider source', () => {
        const serving = render('/market-data/synthetic?tab=provider', (client) =>
            seeded(client, ['synthetic::*']),
        );
        expect(serving).toContain('>Running<');
        expect(serving).toContain('>Serving<');
        const stopped = render('/market-data/synthetic?tab=provider', (client) => {
            seeded(client, ['synthetic::*']);
            client.setQueryData(ADAPTERS_KEY, [adapter('bloomberg', 'ready', 13)]);
        });
        expect(stopped).toContain('>Stopped<');
        expect(stopped).not.toContain('>Running<');
        expect(stopped).not.toContain('>Serving<');
    });

    it('offers start and stop in the header to a person who holds the run permission', () => {
        const html = render('/market-data/synthetic?tab=provider', (client) =>
            seeded(client, ['synthetic::*']),
        );
        expect(html).toContain('>Start<');
        expect(html).toContain('>Stop<');
    });

    it('offers a reader the rows and no start or stop', () => {
        const html = render('/market-data/synthetic?tab=provider', (client) =>
            seeded(client, ['synthetic::fx_spot_generation_configs:read']),
        );
        expect(html).toContain('EUR/USD');
        expect(html).not.toContain('>Start<');
        expect(html).not.toContain('Add feed');
    });

    it('pages with the standard pager', () => {
        const html = render('/market-data/synthetic?tab=provider', (client) =>
            seeded(client, ['synthetic::*']),
        );
        expect(html).toContain('1 to 3 of 3');
        expect(html).toContain('>Previous<');
        expect(html).toContain('>Next<');
    });
});
