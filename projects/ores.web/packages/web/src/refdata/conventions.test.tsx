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
import type { ConventionFamilyCard } from '../api/conventions.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { ConventionDocumentsPage, ConventionKindsPage, FAMILIES_KEY } from './conventions.js';
import { FIRST_PAGE, pageKey } from './RecordList.js';

const CARDS: readonly ConventionFamilyCard[] = [
    { key: 'swap', entity: 'swap_convention', writable: true, count: 320 },
    { key: 'deposit', entity: 'deposit_convention', writable: true, count: 24 },
    { key: 'cds', entity: 'cds_convention', writable: false, count: 3 },
    { key: 'fx-option', entity: 'fx_option_convention', writable: false, count: null },
    { key: 'tenor', entity: 'tenor_convention', writable: false, count: 4 },
    { key: 'currency-pair', entity: 'currency_pair_convention', writable: false, count: 179 },
];

function render(address: string, seed: (client: QueryClient) => void = () => undefined): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(FAMILIES_KEY, CARDS);
    seed(client);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[address]}>
                    <Routes>
                        <Route path="/refdata/conventions" element={<ConventionKindsPage />} />
                        <Route
                            path="/refdata/conventions/:family"
                            element={<ConventionDocumentsPage />}
                        />
                    </Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the list of convention kinds', () => {
    it('groups the party kinds under their asset class', () => {
        const html = render('/refdata/conventions');

        expect(html).toContain('Interest rates');
        expect(html).toContain('Credit');
        expect(html).toContain('Swap');
        expect(html).toContain('Kinds 2 · Documents 344');
    });

    it('counts each tab and keeps the shared kinds out of the party tab', () => {
        const html = render('/refdata/conventions');

        expect(html).toContain('Party conventions (4)');
        expect(html).toContain('Shared conventions (2)');
        expect(html).not.toContain('Currency pair');
        expect(html).not.toContain('Every party reads these conventions.');
    });

    it('warns on the shared tab that every party reads its kinds', () => {
        const html = render('/refdata/conventions?tab=shared');

        expect(html).toContain('Every party reads these conventions.');
        expect(html).toContain('Currency pair');
        expect(html).toContain('Tenor resolution');
        expect(html).not.toContain('>Swap<');
    });

    it('shows a dash for a count that could not be read', () => {
        const html = render('/refdata/conventions');

        expect(html).toContain('title="Count unavailable"');
    });

    it('narrows to one asset class from the address', () => {
        const html = render('/refdata/conventions?class=credit');

        expect(html).toContain('CDS');
        expect(html).not.toContain('>Swap<');
    });

    it('says when nothing matches, with a way to clear', () => {
        const html = render('/refdata/conventions?q=zzz');

        expect(html).toContain('No kinds match the search.');
        expect(html).toContain('Clear');
    });

    it('falls back to the kind column for a sort the screen does not know', () => {
        const html = render('/refdata/conventions?sort=junk');

        expect(html).toContain('aria-sort="ascending"');
    });

    it('links each kind to its documents', () => {
        const html = render('/refdata/conventions');

        expect(html).toContain('href="/refdata/conventions/swap"');
    });

    it('puts Workflows beside Refresh in the header', () => {
        const html = render('/refdata/conventions');

        expect(html).toContain('Refresh');
    });
});

describe('the documents of one kind', () => {
    it('shows a page the server sent, with the key and the terms', () => {
        const html = render('/refdata/conventions/swap', (client) => {
            client.setQueryData(pageKey({ key: 'conventions:swap' }, FIRST_PAGE), {
                rows: [{ id: 'AED-EIBOR-6M-SWAP', version: 2, fixed_calendar: 'AED' }],
                total: 320,
            });
        });

        expect(html).toContain('AED-EIBOR-6M-SWAP');
        expect(html).toContain('Fixed calendar');
        expect(html).toContain('1–1 of 320');
    });

    it('waits for the kinds before it reads a page', () => {
        const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
        const html = renderToStaticMarkup(
            <QueryClientProvider client={client}>
                <TranslationProvider>
                    <MemoryRouter initialEntries={['/refdata/conventions/foo']}>
                        <Routes>
                            <Route
                                path="/refdata/conventions/:family"
                                element={<ConventionDocumentsPage />}
                            />
                        </Routes>
                    </MemoryRouter>
                </TranslationProvider>
            </QueryClientProvider>,
        );

        expect(html).toContain('Loading');
        expect(html).not.toContain('type="search"');
    });

    it('offers no search and no sort, because the server has neither for a convention', () => {
        const html = render('/refdata/conventions/swap');

        expect(html).not.toContain('type="search"');
    });

    it('does not open a row, because no record page exists yet', () => {
        const html = render('/refdata/conventions/swap', (client) => {
            client.setQueryData(pageKey({ key: 'conventions:swap' }, FIRST_PAGE), {
                rows: [{ id: 'AED-EIBOR-6M-SWAP', version: 2 }],
                total: 1,
            });
        });

        expect(html).not.toContain('tabindex="0"');
    });

    it('warns on a shared kind that every party reads it', () => {
        const html = render('/refdata/conventions/tenor');

        expect(html).toContain('Every party reads these documents.');
    });

    it('says so for a kind that does not exist, with a way back', () => {
        const html = render('/refdata/conventions/nonesuch');

        expect(html).toContain('There is no convention kind named nonesuch.');
        expect(html).toContain('Back to the conventions');
    });
});
