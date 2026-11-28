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
import type { LargestRelation, LargestRelationsView } from '@ores/wire-protocol/browser';
import { TranslationProvider } from '../i18n/Provider.js';
import { HousekeepingTablesPage } from './HousekeepingTablesPage.js';
import { pieSlices, relationShare } from './tables.js';

/**
 * The tables screen: the largest relations, and whether anything names them.
 *
 * The fixtures state a database whose largest relations are mostly unnamed,
 * because that is the case the screen exists for: the housekeeping catalogue
 * names ten stores and the answer to "what fills the disk" is often none of them.
 */

const GiB = 1024 * 1024 * 1024;
const MiB = 1024 * 1024;

function relation(name: string, size: number, storeCode: string | null = null): LargestRelation {
    return { relation_name: name, size_bytes: size, store_code: storeCode };
}

const ROWS: readonly LargestRelation[] = [
    relation('ores_refdata_calendar_dates_tbl', 283 * MiB, 'refdata.calendar_dates'),
    relation('ores_geo_ip2country_tbl', 96 * MiB),
    relation('ores_dq_ip2country_artefact_tbl', 94 * MiB),
    relation('ores_marketdata_delivery_counts_tbl', 47 * MiB, 'marketdata.delivery_counts'),
    relation('ores_refdata_legal_entities_tbl', 43 * MiB),
    relation('ores_assets_images_tbl', 35 * MiB),
    relation('ores_marketdata_demand_intervals_tbl', 30 * MiB),
    relation('ores_refdata_counterparties_tbl', 28 * MiB),
];

function view(rows: readonly LargestRelation[] = ROWS): LargestRelationsView {
    return {
        success: true,
        message: '',
        rows,
        database_bytes: 2 * GiB,
        relation_count: 847,
        read_at: '2026-11-28 14:10:00.000000Z',
    };
}

function withProviders(path: string, answer: LargestRelationsView = view()): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['operations', 'housekeeping', 'tables', 200], answer);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[path]}>
                    <Routes>
                        <Route
                            path="/operations/housekeeping/tables"
                            element={<HousekeepingTablesPage />}
                        />
                        <Route path="/operations/housekeeping/:code" element={<div />} />
                    </Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the pie of the largest relations', () => {
    it('takes the largest few asked for, and folds the rest of the database into one slice', () => {
        const slices = pieSlices(ROWS, 2 * GiB, 5, 'Everything else');
        expect(slices).toHaveLength(6);
        expect(slices.slice(0, 5).map((slice) => slice.label)).toEqual([
            'ores_refdata_calendar_dates_tbl',
            'ores_geo_ip2country_tbl',
            'ores_dq_ip2country_artefact_tbl',
            'ores_marketdata_delivery_counts_tbl',
            'ores_refdata_legal_entities_tbl',
        ]);
        const head = ROWS.slice(0, 5).reduce((sum, row) => sum + row.size_bytes, 0);
        expect(slices[5]).toEqual({
            key: 'rest',
            label: 'Everything else',
            value: 2 * GiB - head,
        });
    });

    it('states no remainder when the head already covers the database', () => {
        const slices = pieSlices(ROWS, 283 * MiB, 1, 'Everything else');
        expect(slices).toHaveLength(1);
    });

    it('reads a share of the database, and nothing when the database is unknown', () => {
        expect(relationShare(283 * MiB, 2 * GiB)).toBeCloseTo(283 / 2048, 6);
        expect(relationShare(283 * MiB, 0)).toBe(0);
    });
});

describe('the tables screen', () => {
    const html = withProviders('/operations/housekeeping/tables');

    it('states the database, the relations and what names them', () => {
        expect(html).toContain('2.0 GiB');
        expect(html).toContain('847');
        expect(html).toContain('2 of 8');
    });

    it('lists the largest relations with their size and their share', () => {
        expect(html).toContain('ores_refdata_calendar_dates_tbl');
        expect(html).toContain('283.0 MiB');
        expect(html).toContain('13.8%');
    });

    it('names the store where the catalogue names one, and says so where it does not', () => {
        expect(html).toContain('refdata.calendar_dates');
        expect(html).toContain('not named');
        expect(html).toContain('href="/operations/housekeeping/marketdata.delivery_counts"');
    });

    it('draws a pie of the largest five, with the rest of the database beside them', () => {
        expect(html).toContain('<svg');
        expect(html).toContain('Everything else');
        expect(html).toContain('The largest 5, and everything else');
    });

    it('states when the read ran, because the figures are of that instant', () => {
        expect(html).toContain('Measured');
    });

    it('says what the read answered when no relation is named by a store', () => {
        const unnamed = withProviders(
            '/operations/housekeeping/tables',
            view([relation('ores_geo_ip2country_tbl', 96 * MiB)]),
        );
        expect(unnamed).toContain('The catalogue names none of the largest tables');
    });
});
