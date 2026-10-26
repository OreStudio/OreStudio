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
import type { ReactNode } from 'react';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import { describe, expect, it } from 'vitest';
import type { AdapterStatus } from '../api/client.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { enFlat } from '../i18n/locales/en.js';
import { createTranslator } from '../i18n/translate.js';
import { ADAPTERS_KEY } from '../marketdata/synthetic/adapterStatus.js';
import { MarketDataSourcesPage, RejectedIds } from './MarketDataSourcesPage.js';
import {
    QUIET_AFTER_MS,
    formatTickAge,
    formatVendorId,
    sourceFigures,
    sourceOf,
    sourceRows,
    stateTone,
} from './marketDataSources.js';

/**
 * The sources board, as the system administrator reads it.
 *
 * What is checked is what the screen promises: every figure is counted from the
 * one adapter status read, the quiet and stale sources are at the top of the
 * table, and a field the status does not carry is a gap rather than a guess.
 */

const t = createTranslator('en', enFlat, enFlat).t;

/** One adapter as the service reports it. */
function adapter(overrides: Partial<AdapterStatus> = {}): AdapterStatus {
    return {
        adapter_id: 'bloomberg-a',
        provider_code: 'bloomberg',
        state: 'serving',
        last_heartbeat_at: '2026-10-26 09:00:00Z',
        heartbeat_counter: 42,
        served_count: 30,
        unknown_count: 1,
        demanded_count: 26,
        routes_seen: 5,
        ticks_last_minute: 298,
        last_tick_age_ms: 400,
        accepted: [{ id_scheme: 'bbg', external_id: 'AUD AONIA', field: 'MID' }],
        rejected: [],
        ...overrides,
    };
}

const quiet = adapter({
    adapter_id: 'lseg-a',
    provider_code: 'lseg',
    last_tick_age_ms: QUIET_AFTER_MS + 1000,
    demanded_count: 4,
    accepted: [],
    ticks_last_minute: 0,
    routes_seen: 2,
    rejected: [
        {
            id: { id_scheme: 'lseg', external_id: 'GBP GILT 2030', field: 'MID' },
            reason: 'not_entitled',
        },
    ],
});

const stale = adapter({
    adapter_id: 'ore-import',
    provider_code: 'ore',
    state: 'stale',
    last_tick_age_ms: -1,
    demanded_count: 0,
    accepted: [],
    ticks_last_minute: 0,
    routes_seen: 2,
});

describe('one source', () => {
    it('counts the demanded ids the adapter did not accept', () => {
        const row = sourceOf(adapter({ demanded_count: 26, accepted: [] }));

        expect(row.notServed).toBe(26);
        expect(sourceOf(adapter()).notServed).toBe(25);
    });

    it('is quiet when it serves, is demanded, and nothing ticked inside the window', () => {
        expect(sourceOf(quiet).quiet).toBe(true);
        expect(sourceOf(adapter()).quiet).toBe(false);
        expect(sourceOf(adapter({ last_tick_age_ms: -1 })).quiet).toBe(true);
        expect(sourceOf(adapter({ demanded_count: 0, last_tick_age_ms: -1 })).quiet).toBe(false);
    });

    it('keeps the ids the adapter refused, with the reason for each', () => {
        expect(sourceOf(quiet).rejectedIds).toEqual([
            {
                id: { id_scheme: 'lseg', external_id: 'GBP GILT 2030', field: 'MID' },
                reason: 'not_entitled',
            },
        ]);
    });
});

describe('the board', () => {
    const rows = sourceRows([adapter(), quiet, stale]);

    it('stands the source that ticked longest ago first', () => {
        expect(rows.map((row) => row.name)).toEqual(['ore-import', 'lseg-a', 'bloomberg-a']);
    });

    it('counts every tile from the same rows', () => {
        expect(sourceFigures(rows)).toEqual({
            sources: 3,
            serving: 2,
            routes: 9,
            ticksPerMinute: 298,
            quiet: 1,
            notServed: 29,
            withoutDemand: 1,
            stale: 1,
        });
    });

    it('paints a state by what it means, and repeats a word it does not know', () => {
        expect(stateTone('serving')).toBe('up');
        expect(stateTone('stale')).toBe('warn');
        expect(stateTone('rejected')).toBe('down');
        expect(stateTone('announced')).toBe('muted');
        expect(stateTone('melting')).toBe('warn');
    });

    it('states an age in the units a person reads, and never for one that did not tick', () => {
        expect(formatTickAge(-1, t)).toBe('never');
        expect(formatTickAge(400, t)).toBe('400 ms');
        expect(formatTickAge(1500, t)).toBe('2 s');
        expect(formatTickAge(180_000, t)).toBe('3 m');
        expect(formatTickAge(7_200_000, t)).toBe('2 h');
        expect(sourceOf(stale).lastTickAgeMs).toBe(-1);
    });

    it('names a rejected id in the vendor\u2019s own notation', () => {
        expect(formatVendorId({ id_scheme: 'bbg', external_id: 'AUD AONIA', field: 'MID' })).toBe(
            'bbg:AUD AONIA MID',
        );
        expect(formatVendorId({ id_scheme: 'bbg', external_id: 'AUD AONIA', field: '' })).toBe(
            'bbg:AUD AONIA',
        );
    });

    it('states the kind of a source as a gap rather than inventing one', () => {
        const html = render(<MarketDataSourcesPage />);

        expect(html).toContain('The kind of a source');
        expect(html).toContain('The quality drops');
        expect(html).toContain('The parties routed to a source');
        expect(html).toContain('The history of a source');
    });
});

/** The board, with the one read it makes already answered. */
function render(element: ReactNode): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(ADAPTERS_KEY, [adapter(), quiet, stale]);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter>{element}</MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the rejected ids of a source', () => {
    it('names each id and the reason the adapter gave', () => {
        const html = render(<RejectedIds row={sourceOf(quiet)} />);

        expect(html).toContain('lseg:GBP GILT 2030 MID');
        expect(html).toContain('not_entitled');
    });

    it('says so when the adapter refused nothing', () => {
        const html = render(<RejectedIds row={sourceOf(adapter())} />);

        expect(html).toContain('The adapter refused no id in its last demand.');
    });
});

describe('the sources board on a server', () => {
    it('draws the tiles, the alert, the table and the detail panel', () => {
        const html = render(<MarketDataSourcesPage />);

        expect(html).toContain('Market data sources');
        expect(html).toContain('Sources');
        expect(html).toContain('29 demanded ids are not being served');
        expect(html).toContain('ore-import');
        expect(html).toContain('lseg-a');
        expect(html).toContain('Source · ore-import');
        expect(html).toContain('never');
    });
});
