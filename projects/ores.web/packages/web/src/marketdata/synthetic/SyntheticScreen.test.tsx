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
import type { CurveSeriesList, OresmdParseResult } from '@ores/contracts';
import type {
    AdapterStatus,
    FeedFolder,
    FolderFeed,
    SyntheticResourceView,
} from '../../api/client.js';
import { parseManyKey } from '../../oresmd/queries.js';
import { SCHEMA } from '../../oresmd/testSchema.js';
import { TranslationProvider } from '../../i18n/Provider.js';
import { ADAPTERS_KEY } from './adapterStatus.js';
import { FEEDS_KEY } from './selectionRuns.js';
import { SyntheticScreen } from './SyntheticScreen.js';
import { FOLDERS_KEY } from './useCollections.js';

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

/** Bloomberg's source serves its feeds, and the ORE source was demanded nothing. */
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
        folder_id: 'f',
        enabled: true,
        provider_code: 'bloomberg',
        asset_class_code: kind === 'fx_spot' ? 'fx' : 'interest_rates',
        underlying,
        engine: kind === 'fx_spot' ? 'geometric' : 'VASICEK',
        ticks_per_hour: kind === 'fx_spot' ? 1800 : 60,
        auto_start: true,
        running: false,
        ...patch,
    };
}

function render(path: string, seed: (client: QueryClient) => void): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['registry', 'synthetic'], REGISTRY);
    client.setQueryData([...FOLDERS_KEY, 'roots'], [ROOT]);
    client.setQueryData([...FOLDERS_KEY, 'collections', ['r']], [BLOOMBERG, ORE]);
    client.setQueryData(ADAPTERS_KEY, ADAPTERS);
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
    it('lists each provider with what it simulates and what runs', () => {
        const html = render('/market-data/synthetic', (client) => {
            client.setQueryData(['my-access'], access(['synthetic::*']));
        });
        expect(html).toContain('>Dashboard<');
        expect(html).toContain('>Feeds<');
        expect(html).toContain('Bloomberg');
        expect(html).toContain('Simulates Bloomberg composite quotes.');
        expect(html).toContain('Simulates the ORE examples.');
        expect(html).toContain('13 FX · 0 IR');
        expect(html).toContain('>Running<');
        expect(html).not.toContain('(synthetic)');
        expect(html).not.toContain('Generic');
    });

    it('counts the feeds that should run and do not, and offers to start them', () => {
        const html = render('/market-data/synthetic', (client) => {
            client.setQueryData(['my-access'], access(['synthetic::*']));
        });
        expect(html).toContain('17 demanded ids are not being served.');
        expect(html).toContain('Start them');
        expect(html).toContain('href="/market-data/synthetic?tab=tree&amp;shouldRun=true"');
    });

    it('says nothing of demanded feeds when every source serves them', () => {
        const html = render('/market-data/synthetic', (client) => {
            client.setQueryData(['my-access'], access(['synthetic::*']));
            client.setQueryData(ADAPTERS_KEY, [
                adapter('bloomberg', 'serving', 13),
                adapter('ore', 'serving', 17),
            ]);
        });
        expect(html).not.toContain('demanded ids are not being served');
    });

    it('reads a provider as running from its source, not from its feed counts', () => {
        const html = render('/market-data/synthetic', (client) => {
            client.setQueryData(['my-access'], access(['synthetic::*']));
            // Bloomberg's source serves the ids it was demanded, and the ORE source
            // serves nothing because it was demanded nothing.
            client.setQueryData([...FOLDERS_KEY, 'collections', ['r']], [BLOOMBERG, ORE]);
            client.setQueryData(ADAPTERS_KEY, [
                adapter('bloomberg', 'serving', 13),
                adapter('ore', 'ready', 0),
            ]);
        });
        expect(html).toContain('Simulates Bloomberg composite quotes.');
        expect(html).toContain('>Running<');
        expect(html).toContain('>Stopped<');
        expect(html).not.toContain('demanded ids are not being served');
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
        expect(html).not.toContain('Start them');
    });

    it('offers start and stop to a person who holds the run permission', () => {
        const html = render('/market-data/synthetic', (client) => {
            client.setQueryData(
                ['my-access'],
                access(['synthetic::fx_spot_generation_configs:read', 'synthetic::feeds:run']),
            );
        });
        expect(html).toContain('>Start<');
        expect(html).toContain('Start them');
    });

    it('offers a reader the figures but no start or stop', () => {
        const html = render('/market-data/synthetic', (client) => {
            client.setQueryData(
                ['my-access'],
                access(['synthetic::fx_spot_generation_configs:read']),
            );
        });
        expect(html).toContain('Bloomberg');
        expect(html).not.toContain('>Start<');
        expect(html).not.toContain('Start them');
    });
});

describe('the Synthetic feed tree', () => {
    const FX = feed('eurusd', 'fx_spot', 'EUR/USD', 'EUR/USD', { running: true });
    const IR = feed('usdsofr', 'ir_curve', 'USD SOFR', 'USD', {
        enabled: false,
        provider_code: 'ore',
    });

    function seeded(client: QueryClient, codes: string[]): void {
        client.setQueryData(['my-access'], access(codes));
        client.setQueryData([...FEEDS_KEY, '{}', 0, 15], { total: 2, feeds: [FX, IR] });
    }

    it('shows a feed as serving while its provider is demanded, though the controller never started it', () => {
        const html = render('/market-data/synthetic?tab=tree', (client) => {
            client.setQueryData(['my-access'], access(['synthetic::*']));
            client.setQueryData([...FEEDS_KEY, '{}', 0, 15], {
                total: 1,
                feeds: [{ ...FX, running: false }],
            });
            client.setQueryData(ADAPTERS_KEY, [adapter('bloomberg', 'serving', 13)]);
        });
        expect(html).toContain('EUR/USD');
        expect(html).toContain('>Running<');
    });

    it('shows a feed as stopped when its provider is demanded nothing, though the controller ran it', () => {
        const html = render('/market-data/synthetic?tab=tree', (client) => {
            client.setQueryData(['my-access'], access(['synthetic::*']));
            client.setQueryData([...FEEDS_KEY, '{}', 0, 15], {
                total: 1,
                feeds: [{ ...FX, running: true }],
            });
            client.setQueryData(ADAPTERS_KEY, [adapter('bloomberg', 'ready', 0)]);
        });
        expect(html).toContain('EUR/USD');
        expect(html).not.toContain('>Running<');
    });

    it('lists the feeds with the pair, the engine and the frequency', () => {
        const html = render('/market-data/synthetic?tab=tree', (client) =>
            seeded(client, ['synthetic::*']),
        );
        expect(html).toContain('EUR/USD');
        expect(html).toContain('USD SOFR');
        expect(html).toContain('>Geometric<');
        expect(html).toContain('>Vasicek<');
        expect(html).toContain('Every 2 s');
        expect(html).toContain('Every 1 min');
        expect(html).toContain('>Interest rate<');
        expect(html).toContain('>Running<');
        expect(html).toContain('>Stopped<');
        expect(html).toContain('>Enabled<');
        expect(html).toContain('>Disabled<');
        expect(html).toContain('href="/market-data/synthetic/fx/eurusd"');
        expect(html).toContain('href="/market-data/synthetic/ir/usdsofr"');
    });

    it('shows every provider in the tree and filters by state and asset class', () => {
        const html = render('/market-data/synthetic?tab=tree', (client) =>
            seeded(client, ['synthetic::*']),
        );
        expect(html).toContain('>Started<');
        expect(html).toContain('>Stopped<');
        expect(html).toContain('All asset classes');
        expect(html).toContain('>Interest rate<');
        expect(html).toContain('Search every provider');
        expect(html).not.toContain('Running only');
    });

    it('offers a writer a pick for each row and Add feed, and no button on the rows', () => {
        const html = render('/market-data/synthetic?tab=tree', (client) =>
            seeded(client, ['synthetic::*']),
        );
        expect(html).toContain('aria-label="Select EUR/USD"');
        expect(html).toContain('aria-label="Select this page"');
        expect(html).toContain('Add feed');
        expect(html).not.toContain('>Open<');
    });

    it('offers a reader the rows and no pick or Add feed', () => {
        const html = render('/market-data/synthetic?tab=tree', (client) =>
            seeded(client, ['synthetic::fx_spot_generation_configs:read']),
        );
        expect(html).toContain('EUR/USD');
        expect(html).not.toContain('aria-label="Select EUR/USD"');
        expect(html).not.toContain('Add feed');
    });

    it('pages with the standard pager', () => {
        const html = render('/market-data/synthetic?tab=tree', (client) =>
            seeded(client, ['synthetic::*']),
        );
        expect(html).toContain('1 to 2 of 2');
        expect(html).toContain('>Previous<');
        expect(html).toContain('>Next<');
    });

    it('names the filter that shows the feeds that should run', () => {
        const filter = JSON.stringify({ state: 'stopped', shouldRun: true });
        const html = render(
            '/market-data/synthetic?tab=tree&shouldRun=true&state=stopped',
            (client) => {
                client.setQueryData(['my-access'], access(['synthetic::*']));
                client.setQueryData([...FEEDS_KEY, filter, 0, 15], { total: 1, feeds: [IR] });
            },
        );
        expect(html).toContain('Demand not served');
        expect(html).toContain('USD SOFR');
    });
});

describe('the oresmd URI column of the feed tree', () => {
    const FX_URI = 'oresmd://ir/EUR?type=quote&instrument=capfloor&quote=rate&cap_floor=Cap';
    const SERIES_URI = 'oresmd://ir/GBP?type=quote&instrument=capfloor&quote=rate&cap_floor=Cap';
    const FX = feed('eurusd', 'fx_spot', 'EUR/USD', 'EUR/USD', { oresmd_uri: FX_URI });
    const IR = feed('usdsofr', 'ir_curve', 'USD SOFR', 'USD', { oresmd_uri: SERIES_URI });

    /** A URI as the markup carries it, which escapes the separators of a query. */
    const escaped = (uri: string): string => uri.replaceAll('&', '&amp;');

    const parts = (subject: string): OresmdParseResult => ({
        success: true,
        message: '',
        kind: 'quote',
        instrument: 'capfloor',
        quote: 'rate',
        asset: 'ir',
        subject,
        fields: [{ name: 'cap_floor', text: 'Cap' }],
    });

    /** The party's series: the EUR one declares an axis, the GBP one does not. */
    const LIST: CurveSeriesList = {
        truncated: false,
        series: [
            {
                series_id: 'eur',
                axes: ['term'],
                identity: {
                    asset: 'ir',
                    instrument_type: 'capfloor',
                    quote_type: 'rate',
                    scope: 'EUR',
                    party_id: 'party-1',
                    fields: [{ name: 'cap_floor', text: 'Cap' }],
                },
            },
            {
                series_id: 'gbp',
                axes: [],
                identity: {
                    asset: 'ir',
                    instrument_type: 'capfloor',
                    quote_type: 'rate',
                    scope: 'GBP',
                    party_id: 'party-1',
                    fields: [{ name: 'cap_floor', text: 'Cap' }],
                },
            },
        ],
    };

    /**
     * The page as the app reads it: one answer about both URIs, in the order the
     * read asks them, and one series list. No answer is seeded a row, so a column
     * that asked about a URI a row would draw plain text and fail the test.
     */
    function seeded(client: QueryClient): void {
        client.setQueryData(['my-access'], access(['synthetic::*']));
        client.setQueryData([...FEEDS_KEY, '{}', 0, 15], { total: 2, feeds: [FX, IR] });
        client.setQueryData(['oresmd-schema'], SCHEMA);
        client.setQueryData(parseManyKey([FX_URI, SERIES_URI]), [parts('EUR'), parts('GBP')]);
        client.setQueryData(['market-series-list', 'party-1'], LIST);
    }

    it('names the column after the field the record page shows', () => {
        const html = render('/market-data/synthetic?tab=tree', seeded);
        expect(html).toContain('oresmd URI');
        expect(html).toContain(escaped(FX_URI));
        expect(html).toContain(escaped(SERIES_URI));
    });

    it('links each row to the page that opens its series, and its series alone', () => {
        const html = render('/market-data/synthetic?tab=tree', seeded);
        expect(html).toContain(`href="/market-data/evolution?uri=${encodeURIComponent(FX_URI)}"`);
        expect(html).toContain(`href="/market-data/series?uri=${encodeURIComponent(SERIES_URI)}"`);
    });

    it('offers Inspect on a row, as the record page does', () => {
        const html = render('/market-data/synthetic?tab=tree', seeded);
        expect(html).toContain('>Inspect<');
    });

    it('draws plain text for a row whose series the party does not have', () => {
        const html = render('/market-data/synthetic?tab=tree', (client) => {
            client.setQueryData(['my-access'], access(['synthetic::*']));
            client.setQueryData([...FEEDS_KEY, '{}', 0, 15], { total: 2, feeds: [FX, IR] });
            client.setQueryData(['oresmd-schema'], SCHEMA);
            client.setQueryData(parseManyKey([FX_URI, SERIES_URI]), [parts('EUR'), parts('GBP')]);
            client.setQueryData(['market-series-list', 'party-1'], {
                truncated: false,
                series: [],
            });
        });
        expect(html).toContain(escaped(FX_URI));
        expect(html).not.toContain('/market-data/evolution?uri=');
    });
});
