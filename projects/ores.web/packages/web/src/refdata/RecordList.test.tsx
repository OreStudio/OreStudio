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
import { MemoryRouter } from 'react-router';
import { TranslationProvider } from '../i18n/Provider.js';
import { FIRST_PAGE, RecordTable, pageKey, type ListPage, type ListSource } from './RecordList.js';

interface Pet {
    readonly name: string;
    readonly kind: string;
}

const PETS: ListSource<Pet> = {
    key: 'pets',
    scope: 'zoo',
    read: () => Promise.reject(new Error('not read in a static render')),
    search: true,
    sortable: [],
    filters: [
        {
            kind: 'choice',
            id: 'kind',
            label: 'Kind',
            all: 'All kinds',
            choices: [
                { value: 'cat', label: 'Cat' },
                { value: 'dog', label: 'Dog' },
            ],
        },
        { kind: 'toggle', id: 'old', label: 'Show old pets' },
    ],
    mayAdd: false,
};

function render(address: string, seed: (client: QueryClient) => void, opens = true): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    seed(client);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[address]}>
                    <RecordTable
                        source={PETS}
                        plural="Pets"
                        columns={[{ id: 'name', header: 'Name', cell: (pet) => pet.name }]}
                        keyOf={(pet) => pet.name}
                        {...(opens ? { pathOf: (pet: Pet) => `/pets/${pet.name}` } : {})}
                    />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

const one: ListPage<Pet> = { rows: [{ name: 'Rex', kind: 'dog' }], total: 1 };

describe('RecordTable', () => {
    it('asks for the page with the filters the address holds, and shows them chosen', () => {
        const html = render('/pets?f.kind=dog&f.old=1', (client) => {
            client.setQueryData(
                pageKey(PETS, { ...FIRST_PAGE, filters: { kind: 'dog', old: '1' } }),
                one,
            );
        });
        expect(html).toContain('Rex');
        expect(html).toMatch(/<option value="dog" selected="">Dog<\/option>/);
        expect(html).toMatch(/checked=""[^>]*\/>Show old pets/);
    });

    it('keeps one owner’s page apart from another’s', () => {
        const html = render('/pets', (client) => {
            client.setQueryData(pageKey({ key: 'pets', scope: 'farm' }, FIRST_PAGE), one);
        });
        expect(html).not.toContain('Rex');
        expect(html).toContain('Loading');
    });

    it('shows what the server said about the page above the table', () => {
        const html = render('/pets', (client) => {
            client.setQueryData(pageKey(PETS, FIRST_PAGE), {
                ...one,
                notes: [{ tone: 'info', text: '3 old pets are hidden.' }],
            });
        });
        expect(html).toContain('3 old pets are hidden.');
    });

    it('offers to clear a filter that matches nothing', () => {
        const html = render('/pets?f.kind=cat', (client) => {
            client.setQueryData(pageKey(PETS, { ...FIRST_PAGE, filters: { kind: 'cat' } }), {
                rows: [],
                total: 0,
            });
        });
        expect(html).toContain('No pets match the search.');
        expect(html).toContain('Clear');
    });

    it('opens a row only when the record has a page', () => {
        const seed = (client: QueryClient) => client.setQueryData(pageKey(PETS, FIRST_PAGE), one);
        expect(render('/pets', seed)).toContain('tabindex="0"');
        expect(render('/pets', seed, false)).not.toContain('tabindex="0"');
    });
});
