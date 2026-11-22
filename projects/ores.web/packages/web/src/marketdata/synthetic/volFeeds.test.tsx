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

import { describe, expect, it } from 'vitest';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter, Route, Routes } from 'react-router';
import type { AccountAccess } from '@ores/wire-protocol/browser';
import type { RecordRow, SyntheticResourceView } from '../../api/client.js';
import { TranslationProvider } from '../../i18n/Provider.js';
import { invalidFields } from '../../refdata/records.js';
import { feedPageOf } from './feedTree.js';
import { processName, processTab } from './processSummary.js';
import {
    SURFACE_FIELDS,
    VolFeedPage,
    completeVolFeed,
    tenorDays,
    upstreamsOf,
    volSource,
} from './volFeeds.js';

const FEED_ID = '77777777-7777-4777-8777-777777777777';
const BLOOMBERG = 'c3f0d994-efcd-88d6-456e-ad7f8837aa51';
const ICE = 'a6d66ed0-67ed-dec7-7afb-294a589a4e60';

function resource(key: string, name: string): SyntheticResourceView {
    return {
        key,
        entityType: `ores.synthetic.${name}`,
        keyField: 'id',
        writable: true,
        readOnlyBecause: null,
        search: false,
        sortable: [],
        readPermission: `synthetic::${name}s:read`,
        writePermission: `synthetic::${name}s:write`,
        deletePermission: `synthetic::${name}s:delete`,
    };
}

const REGISTRY = [
    resource('fx-vol-surface-feeds', 'fx_vol_surface_feed'),
    resource('fx-vol-surface-feed-expiries', 'fx_vol_surface_feed_expiry'),
    resource('fx-vol-surface-feed-contributors', 'fx_vol_surface_feed_contributor'),
    resource('fx-vol-contributors', 'fx_vol_contributor'),
];

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

const feed: RecordRow = {
    running: false,
    version: 2,
    id: FEED_ID,
    config_id: '88888888-8888-4888-8888-888888888888',
    base_currency_code: 'EUR',
    quote_currency_code: 'USD',
    source_name: 'synthetic.bloomberg.vol.eurusd',
    ticks_per_hour: 12,
    enabled: true,
    auto_start: true,
    folder_id: null,
    asset_class_code: 'fx',
    arrival_mode: 'periodic',
    branching_ratio: 0,
    decay: 1,
    sensitivity: 1,
    skew_reversion: 3,
    skew_volatility: 0.6,
    curvature_reversion: 3,
    curvature_volatility: 0.4,
    spot_variance_correlation: 0.3,
    spot_skew_correlation: -0.4,
    segment_correlation: 0.9,
    start_skew: -0.1,
    start_curvature: 0.8,
    spot_oresmd_uri: 'oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD',
    spot_provider_code: 'bloomberg',
    provider_code: 'bloomberg',
};

const COLLECTION = '88888888-8888-4888-8888-888888888888';
const DEMO = '99999999-9999-4999-8999-999999999999';
const SPOT_URI = 'oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD';

const collections: RecordRow[] = [
    { id: COLLECTION, name: 'Bloomberg', provider_code: 'bloomberg' },
    { id: DEMO, name: 'Uniform Volatility Demo', provider_code: 'demo' },
];

const spots: RecordRow[] = [
    {
        id: 'spot-demo',
        config_id: DEMO,
        oresmd_uri: SPOT_URI,
        source_name: 'synthetic.demo.eurusd',
    },
    {
        id: 'spot-bbg',
        config_id: COLLECTION,
        oresmd_uri: SPOT_URI,
        source_name: 'synthetic.bloomberg.eurusd',
    },
];

const curve = (
    id: string,
    config: string,
    currency: string,
    role: string,
    tenor: string,
): RecordRow => ({
    id,
    config_id: config,
    currency_code: currency,
    role,
    tenor,
    source_name: `synthetic.${id}`,
});

const curves: RecordRow[] = [
    curve('eur-projection', COLLECTION, 'EUR', 'projection', '6M'),
    curve('eur-discount', COLLECTION, 'EUR', 'discount', ''),
    curve('usd-demo', DEMO, 'USD', 'self_discounting', ''),
    curve('usd-discount', COLLECTION, 'USD', 'self_discounting', ''),
];

const expiry = (id: string, tenor: string): RecordRow => ({
    id,
    version: 1,
    feed_id: FEED_ID,
    tenor,
    atm_vol: 0.085,
    variance_reversion: 2,
    variance_volatility: 0.08,
});

const contributor = (id: string, provider: string, noise: number): RecordRow => ({
    id,
    version: 2,
    provider_code: provider,
    noise,
    wing_widening: 1,
    expiry_widening: 0.5,
    quote_step: 0.0005,
    missing_probability: 0.02,
    stale_probability: 0.05,
    outlier_probability: 0.001,
    outlier_size: 0.01,
    bias: 0,
});

function render(path: string, seed: (client: QueryClient) => void): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['registry', 'synthetic'], REGISTRY);
    client.setQueryData(['records', 'fx-vol-surface-feeds', 'key', FEED_ID], feed);
    client.setQueryData(['records', 'fx-vol-surface-feed-expiries', FEED_ID], []);
    client.setQueryData(['records', 'fx-vol-surface-feed-contributors', FEED_ID], []);
    seed(client);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[path]}>
                    <Routes>
                        <Route path="/market-data/synthetic/vol/:id" element={<VolFeedPage />} />
                    </Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

const writer = (client: QueryClient): void => {
    client.setQueryData(['my-access'], access(['synthetic::*']));
};

describe('the vol surface feed screens', () => {
    it('shows a surface feed with the tabs of the FX and IR feeds and its own', () => {
        const html = render(`/market-data/synthetic/vol/${FEED_ID}`, writer);
        expect(html).toContain('synthetic.bloomberg.vol.eurusd');
        expect(html).toContain('SSVI');
        for (const tab of ['Surface', 'Expiries', 'Contributors', 'Arrivals', 'History']) {
            expect(html).toContain(`>${tab}<`);
        }
    });

    it('shows whether a provider holds the surface and offers no Start or Stop', () => {
        for (const [running, word] of [
            [true, '>Running<'],
            [false, '>Stopped<'],
        ] as const) {
            const html = render(`/market-data/synthetic/vol/${FEED_ID}`, (client) => {
                writer(client);
                client.setQueryData(['records', 'fx-vol-surface-feeds', 'key', FEED_ID], {
                    ...feed,
                    running,
                });
            });
            expect(html).toContain(word);
            expect(html).toContain('Runs while a simulated source of a provider holds it.');
            expect(html).not.toContain('>Start</button>');
            expect(html).not.toContain('>Stop</button>');
        }
    });

    it('draws the SSVI start, dynamics and correlations, each with its own Edit', () => {
        const html = render(`/market-data/synthetic/vol/${FEED_ID}?tab=surface`, writer);
        expect(html).toContain('Start skew (ρ)');
        expect(html).toContain('Start curvature (η)');
        expect(html).toContain('Skew reversion');
        expect(html).toContain('Segment correlation');
        expect(html).toContain('>Start<');
        expect(html).toContain('>Dynamics<');
        expect(html).toContain('>Correlations<');
        expect(html.match(/>Edit</g)?.length).toBeGreaterThanOrEqual(3);
    });

    it('draws the three cards side by side, gamma at 0.5 with why, and no last change', () => {
        const html = render(`/market-data/synthetic/vol/${FEED_ID}?tab=surface`, writer);
        expect(html).toContain('lg:grid-cols-3');
        expect(html).toContain('Gamma (γ)');
        expect(html).toMatch(/>0\.5</);
        expect(html).not.toContain('fixed');
        expect(html).toContain('butterfly arbitrage');
        expect(html).not.toContain('Last changed');
        expect(html).not.toContain('name="gamma"');
    });

    it('never writes the gamma that the start card edit dialog shows', () => {
        const write = completeVolFeed({ ...feed, gamma: '0.5' });
        expect(write).not.toHaveProperty('gamma');
    });

    it('offers a reader no change to the surface', () => {
        const html = render(`/market-data/synthetic/vol/${FEED_ID}?tab=surface`, (client) => {
            client.setQueryData(['my-access'], access(['synthetic::fx_vol_surface_feeds:read']));
        });
        expect(html).toContain('Start skew (ρ)');
        expect(html).not.toContain('>Edit<');
    });

    it('lists the expiries in the order of their dates, not of their names', () => {
        const html = render(`/market-data/synthetic/vol/${FEED_ID}?tab=expiries`, (client) => {
            writer(client);
            client.setQueryData(
                ['records', 'fx-vol-surface-feed-expiries', FEED_ID],
                [expiry('e3', '10Y'), expiry('e1', '1W'), expiry('e2', '18M')],
            );
        });
        const at = (tenor: string): number => html.indexOf(`>${tenor}<`);
        expect(at('1W')).toBeGreaterThan(-1);
        expect(at('1W')).toBeLessThan(at('18M'));
        expect(at('18M')).toBeLessThan(at('10Y'));
        expect(html).toContain('8.50%');
    });

    it('lists the vendors that quote the surface, each opening its own page', () => {
        const html = render(`/market-data/synthetic/vol/${FEED_ID}?tab=contributors`, (client) => {
            writer(client);
            client.setQueryData(
                ['records', 'fx-vol-surface-feed-contributors', FEED_ID],
                [
                    { id: 'l1', version: 1, feed_id: FEED_ID, contributor_id: ICE },
                    { id: 'l2', version: 1, feed_id: FEED_ID, contributor_id: BLOOMBERG },
                ],
            );
            client.setQueryData(
                ['records', 'fx-vol-contributors'],
                [
                    contributor(BLOOMBERG, 'bloomberg', 0.002),
                    contributor(ICE, 'ice', 0.0025),
                    contributor('d', 'tradeweb', 0.003),
                ],
            );
        });
        expect(html).toContain('>bloomberg<');
        expect(html).toContain('>ice<');
        expect(html).not.toContain('>tradeweb<');
        expect(html.indexOf('>bloomberg<')).toBeLessThan(html.indexOf('>ice<'));
        expect(html).toContain('>0.0025<');
        expect(html).toContain('cursor-pointer');
        expect(html).not.toContain('Noise profile');
    });
});

describe('what a vol surface reads', () => {
    it('names the spot feed its config names by URI and provider, not the pair alone', () => {
        const [spot] = upstreamsOf(feed, spots, collections, curves);
        expect(spot?.role).toBe('spot');
        expect(spot?.row?.['id']).toBe('spot-bbg');
    });

    it("names each currency's discounting curve in the surface's own collection", () => {
        const [, base, quote] = upstreamsOf(feed, spots, collections, curves);
        expect(base?.row?.['id']).toBe('eur-discount');
        expect(quote?.row?.['id']).toBe('usd-discount');
    });

    it('fills the spot URI from the pair when the person leaves it blank', () => {
        const write = completeVolFeed({ base_currency_code: 'EUR', quote_currency_code: 'USD' });
        expect(write['spot_oresmd_uri']).toBe(SPOT_URI);
        const kept = completeVolFeed({ ...feed, spot_oresmd_uri: 'oresmd://fx/X' });
        expect(kept['spot_oresmd_uri']).toBe('oresmd://fx/X');
    });

    it('prefers the spot in the surface own collection when two collections name it', () => {
        const other = { ...spots[1], id: 'spot-other', config_id: 'elsewhere' };
        const both = [other, ...spots];
        const [spot] = upstreamsOf(
            feed,
            both,
            [...collections, { id: 'elsewhere', provider_code: 'bloomberg' }],
            curves,
        );
        expect(spot?.row?.['id']).toBe('spot-bbg');
    });

    it('says what it looked for when no feed matches', () => {
        const [spot] = upstreamsOf(
            { ...feed, spot_provider_code: 'lseg' },
            spots,
            collections,
            curves,
        );
        expect(spot?.row).toBeUndefined();
        expect(spot?.wants).toBe(`${SPOT_URI} from lseg`);
    });

    it('links the upstreams on the Calibration tab and does not call a running surface stopped', () => {
        const html = render(`/market-data/synthetic/vol/${FEED_ID}?tab=calibration`, (client) => {
            writer(client);
            client.setQueryData(['records', 'fx-vol-surface-feeds', 'key', FEED_ID], {
                ...feed,
                running: true,
            });
            client.setQueryData(['records', 'collections'], collections);
            client.setQueryData(['records', 'fx-feeds'], spots);
            client.setQueryData(['records', 'ir-curve-feeds'], curves);
        });
        expect(html).not.toContain('is not running');
        expect(html).toContain('href="/market-data/synthetic/fx/spot-bbg"');
        expect(html).toContain('href="/market-data/synthetic/ir/eur-discount"');
        expect(html).toContain('href="/market-data/synthetic/ir/usd-discount"');
        expect(html).toContain('EUR discounting curve');
        expect(html).not.toContain('GJR-GARCH');
    });
});

describe('a vol surface feed among the other feeds', () => {
    it('opens on its own page and on its Surface tab, under the SSVI name', () => {
        expect(feedPageOf({ kind: 'fx_vol_surface', id: FEED_ID })).toBe(
            `/market-data/synthetic/vol/${FEED_ID}`,
        );
        expect(feedPageOf({ kind: 'ir_curve', id: FEED_ID })).toBe(
            `/market-data/synthetic/ir/${FEED_ID}`,
        );
        expect(feedPageOf({ kind: 'fx_spot', id: FEED_ID })).toBe(
            `/market-data/synthetic/fx/${FEED_ID}`,
        );
        expect(processTab('fx_vol_surface')).toBe('surface');
        expect(processName((key) => key, { kind: 'fx_vol_surface', engine: 'ssvi' })).toBe(
            'marketdata.volFeeds.engine',
        );
    });

    it('names a new surface apart from its pair spot feed', () => {
        expect(volSource('', 'EUR', 'USD')).toBe('synthetic.vol.eurusd');
        expect(volSource('bloomberg.vol.eurusd', 'EUR', 'USD')).toBe(
            'synthetic.bloomberg.vol.eurusd',
        );
        expect(
            completeVolFeed({ base_currency_code: 'GBP', quote_currency_code: 'USD' }),
        ).toMatchObject({ source_name: 'synthetic.vol.gbpusd', asset_class_code: 'fx' });
    });

    it('refuses a surface value outside the bound its table checks, before the save', () => {
        const values = Object.fromEntries(
            SURFACE_FIELDS.map((spec) => [spec.field, String(feed[spec.field])]),
        );
        expect(invalidFields(SURFACE_FIELDS, values)).toEqual([]);
        const refused = (field: string, value: string): readonly string[] =>
            invalidFields(SURFACE_FIELDS, { ...values, [field]: value });
        expect(refused('start_curvature', '1.5')).toEqual(['start_curvature']);
        expect(refused('start_curvature', '0')).toEqual(['start_curvature']);
        expect(refused('start_skew', '-1')).toEqual(['start_skew']);
        expect(refused('spot_variance_correlation', '1')).toEqual(['spot_variance_correlation']);
        expect(refused('segment_correlation', '-0.1')).toEqual(['segment_correlation']);
        expect(refused('segment_correlation', '0')).toEqual([]);
        expect(refused('skew_volatility', '-0.1')).toEqual(['skew_volatility']);
        expect(refused('skew_volatility', '0')).toEqual([]);
    });

    it('orders tenors by the time they span', () => {
        expect(tenorDays('1W')).toBeLessThan(tenorDays('1M'));
        expect(tenorDays('18M')).toBeLessThan(tenorDays('2Y'));
        expect(tenorDays('ON')).toBe(Number.MAX_SAFE_INTEGER);
    });
});
