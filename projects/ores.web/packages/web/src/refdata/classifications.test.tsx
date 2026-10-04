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
import type { ClassificationList, ClassificationRow } from '@ores/wire-protocol/browser';
import { TranslationProvider } from '../i18n/Provider.js';
import { ClassificationsPage } from './ClassificationsPage.js';

/**
 * The classification screen, rendered from a seeded query cache.
 *
 * What is checked is what the screen promises: every list has a name in the
 * person's language, a list of ORE spellings cannot be changed, and the
 * columns follow the list.
 */

const TOPICS = ['currencies', 'calendars', 'parties', 'books', 'products', 'tenors', 'market-data'];

const LISTS: ClassificationList[] = [
    {
        key: 'rounding-type',
        entityType: 'ores.refdata.rounding_type',
        topic: 'currencies',
        shape: 'named',
        editable: true,
    },
    {
        key: 'day-counter',
        entityType: 'ores.refdata.day_counter',
        topic: 'products',
        shape: 'plain',
        editable: false,
    },
    {
        key: 'tenor-anchor',
        entityType: 'ores.refdata.tenor_anchor',
        topic: 'tenors',
        shape: 'ordered',
        editable: true,
    },
];

function row(code: string, name: string, order: number | null): ClassificationRow {
    return {
        code,
        name,
        description: `${code} explained`,
        displayOrder: order,
        version: 1,
        modifiedBy: 'system',
        recordedAt: '2026-10-04 09:00:00Z',
        reasonCode: 'system.new_record',
        commentary: '',
    };
}

function render(
    path: string,
    lists: ClassificationList[],
    rows: Record<string, ClassificationRow[]>,
): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['classifications'], lists);
    for (const [key, value] of Object.entries(rows)) {
        client.setQueryData(['classifications', key], value);
    }
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[path]}>
                    <Routes>
                        <Route path="/classifications" element={<ClassificationsPage />} />
                        <Route path="/classifications/:list" element={<ClassificationsPage />} />
                    </Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('ClassificationsPage', () => {
    it('names every topic and every one of the 28 lists in words', () => {
        const keys = [
            'monetary-nature',
            'rounding-type',
            'currency-market-tier',
            'currency-pair-classification',
            'calendar-type',
            'diary-entry-type',
            'calendar-name',
            'business-day-convention-type',
            'party-type',
            'party-status',
            'contact-type',
            'book-status',
            'book-purpose-type',
            'ledger-feed-type',
            'regulatory-book-type',
            'purpose-type',
            'asset-class-code',
            'leg-type',
            'floating-index-type',
            'day-counter',
            'day-count-fraction-type',
            'curve-role',
            'tenor-kind',
            'tenor-unit',
            'tenor-anchor',
            'tenor-resolution-algorithm',
            'derivation-kind',
            'series-subclass-code',
        ];
        expect(keys).toHaveLength(28);
        const lists = keys.map((key, index) => ({
            key,
            entityType: `ores.refdata.${key}`,
            topic: TOPICS[index % TOPICS.length] ?? 'currencies',
            shape: 'named' as const,
            editable: true,
        }));
        const html = render('/classifications', lists, {});
        expect(html).not.toContain('refdata.classifications.');
        expect(html).toContain('Rounding types');
        expect(html).toContain('Market data');
    });

    it('asks for a list when none is chosen', () => {
        const html = render('/classifications', LISTS, {});
        expect(html).toContain('Choose a list on the left.');
    });

    it('shows a named list with its name and order columns, and lets rows be added', () => {
        const html = render('/classifications/rounding-type', LISTS, {
            'rounding-type': [row('Up', 'Up', 10), row('Down', 'Down', 20)],
        });
        expect(html).toContain('Add row');
        expect(html).toContain('<th class="px-4 py-2 font-medium">Name</th>');
        expect(html).toContain('<th class="px-4 py-2 font-medium">Order</th>');
        expect(html).toContain('Up explained');
        expect(html).toContain('aria-label="Move up"');
    });

    it('shows a list of ORE spellings read-only, with no name, order or add', () => {
        const html = render('/classifications/day-counter', LISTS, {
            'day-counter': [row('A360', '', null)],
        });
        expect(html).toContain('Read-only');
        expect(html).toContain('ORE documents write these spellings exactly');
        expect(html).not.toContain('Add row');
        expect(html).not.toContain('<th class="px-4 py-2 font-medium">Name</th>');
        expect(html).not.toContain('<th class="px-4 py-2 font-medium">Order</th>');
        expect(html).not.toContain('aria-label="Move up"');
    });

    it('shows an ordered list with its order but no name', () => {
        const html = render('/classifications/tenor-anchor', LISTS, {
            'tenor-anchor': [row('SPOT', '', 5)],
        });
        expect(html).not.toContain('<th class="px-4 py-2 font-medium">Name</th>');
        expect(html).toContain('<th class="px-4 py-2 font-medium">Order</th>');
    });
});
