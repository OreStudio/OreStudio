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
import type { PartyPage } from '@ores/wire-protocol/browser';
import { TranslationProvider } from '../i18n/Provider.js';
import { PartiesPage } from './PartiesPage.js';

/**
 * The parties of the session's own tenant, as a reader sees them.
 *
 * The first page is seeded in the query cache, so the screen renders without
 * a server. What is checked is the screen: the columns, a parent named from
 * the page or said to be elsewhere, and the pager's sentence from the server
 * total.
 */

const SYSTEM = '66666666-6666-6666-6666-666666666666';

const page: PartyPage = {
    parties: [
        {
            id: SYSTEM,
            code: 'system',
            name: 'Acme System',
            category: 'System',
            type: 'Internal',
            status: 'Active',
            parentId: null,
            parentName: null,
        },
        {
            id: '77777777-7777-7777-7777-777777777777',
            code: 'acme_group',
            name: 'Acme Group',
            category: 'Operational',
            type: 'Corporate',
            status: 'Active',
            parentId: SYSTEM,
            parentName: 'Acme System',
        },
        {
            id: '88888888-8888-8888-8888-888888888888',
            code: 'acme_london',
            name: 'Acme London',
            category: 'Operational',
            type: 'Branch',
            status: 'Active',
            parentId: '99999999-9999-9999-9999-999999999999',
            parentName: null,
        },
    ],
    totalCount: 30,
};

function render(): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['parties', 0], page);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter>
                    <PartiesPage />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('PartiesPage', () => {
    it('lists the parties with their parents', () => {
        const html = render();

        expect(html).toContain('acme_group');
        expect(html).toContain('>Acme System</td>');
        expect(html).toContain('Not one you can see');
    });

    it('pages with the server total', () => {
        const html = render();

        expect(html).toContain('Showing 1–3 of 30 parties');
        expect(html).toContain('Next');
        expect(html).toMatch(/disabled=""[^>]*>Previous/);
    });
});
