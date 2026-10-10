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
import { PersonRef } from './PersonRef.js';

const ACCOUNT = '01a123b5-e1a7-7f7a-97a7-e0ee059bfbf5';

function render(who: string, permissionCodes: string[], seed?: (client: QueryClient) => void) {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['my-access'], { roles: [{ roleId: 'r', permissionCodes }] });
    seed?.(client);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter>
                    <PersonRef who={who} />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('PersonRef', () => {
    it('names a person by full name and username, linking them for a reader of accounts', () => {
        const html = render('adrian.vance', ['iam::accounts:read'], (client) =>
            client.setQueryData(['accounts'], {
                accounts: [{ id: ACCOUNT, username: 'adrian.vance', fullName: 'Adrian Vance' }],
                totalCount: 1,
            }),
        );
        expect(html).toContain('Adrian Vance (adrian.vance)');
        expect(html).toContain('href="/people/adrian.vance"');
    });

    it('finds a person by account id', () => {
        const html = render(ACCOUNT, ['iam::accounts:read'], (client) =>
            client.setQueryData(['accounts'], {
                accounts: [{ id: ACCOUNT, username: 'adrian.vance', fullName: 'Adrian Vance' }],
                totalCount: 1,
            }),
        );
        expect(html).toContain('Adrian Vance (adrian.vance)');
    });

    it('names a person from the organisation without linking them for a member', () => {
        const html = render('adrian.vance', ['iam::organisation:read'], (client) =>
            client.setQueryData(['reporting-tree'], {
                unrooted: 0,
                parties: [],
                nodes: [
                    {
                        accountId: ACCOUNT,
                        username: 'adrian.vance',
                        fullName: 'Adrian Vance',
                        jobTitle: '',
                        accountType: 'user',
                        imageId: null,
                        reportsToAccountId: null,
                        reportsOutsideScope: false,
                        partyIds: [],
                        depth: 0,
                        directReports: 0,
                    },
                ],
            }),
        );
        expect(html).toContain('Adrian Vance (adrian.vance)');
        expect(html).not.toContain('href=');
    });

    it('says an unresolved account id is someone outside the reader’s view', () => {
        const html = render(ACCOUNT, []);
        expect(html).toContain('Someone outside your view');
        expect(html).not.toContain(ACCOUNT);
    });

    it('does not link a service', () => {
        const html = render('ores_brave_hopper_iam_service', ['iam::accounts:read']);
        expect(html).not.toContain('href=');
    });
});
