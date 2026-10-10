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
import { StaffOfMyParties } from './StaffOfMyParties.js';

describe('StaffOfMyParties', () => {
    it('lists the people of the reader’s parties with their parties, and links no one', () => {
        const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
        client.setQueryData(['reporting-tree'], {
            unrooted: 0,
            parties: [
                { partyId: 'p1', name: 'Acme North plc', shortCode: 'NORTH', parentPartyId: null },
            ],
            nodes: [
                {
                    accountId: 'a1',
                    username: 'ada',
                    fullName: 'Ada Lovelace',
                    jobTitle: 'Head of Desk',
                    imageId: null,
                    reportsToAccountId: null,
                    reportsOutsideScope: false,
                    partyIds: ['p1'],
                    depth: 0,
                    directReports: 0,
                },
            ],
        });
        const html = renderToStaticMarkup(
            <QueryClientProvider client={client}>
                <TranslationProvider>
                    <MemoryRouter>
                        <StaffOfMyParties />
                    </MemoryRouter>
                </TranslationProvider>
            </QueryClientProvider>,
        );

        expect(html).toContain('Ada Lovelace');
        expect(html).toContain('Head of Desk');
        expect(html).toContain('Acme North plc');
        expect(html).not.toContain('href="/people/ada"');
    });
});
