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

import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { describe, expect, it } from 'vitest';
import { TranslationProvider } from '../i18n/Provider.js';
import { OrganisationPage } from './OrganisationPage.js';

/**
 * The organisation area draws a card for each screen the person holds the read
 * of, whoever the person is.
 */

function render(permissionCodes: readonly string[]): string {
    const client = new QueryClient();
    client.setQueryData(['my-access'], {
        roles: [
            {
                roleId: '77777777-7777-7777-7777-777777777777',
                name: 'Viewer',
                description: '',
                permissionCodes: [...permissionCodes],
                givenBy: 'system',
                givenAt: '2026-10-05 09:30:00Z',
                reasonCode: 'access.initial',
                commentary: '',
            },
        ],
    });
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter>
                    <OrganisationPage />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the organisation area', () => {
    it('draws only the screens the person may read', () => {
        const none = render([]);
        expect(none).not.toContain('href="/roles"');
        expect(none).not.toContain('href="/rescue"');
        expect(none).toContain('href="/hierarchy"');

        const some = render(['iam::roles:read']);
        expect(some).toContain('href="/roles"');
        expect(some).not.toContain('href="/rescue"');
    });

    it('draws Staff, Roles and Rescue access for a person who holds every read', () => {
        const all = render(['*']);

        for (const href of ['/staff', '/hierarchy', '/roles', '/rescue']) {
            expect(all).toContain(`href="${href}"`);
        }
    });

    it('stands under Home in the trail', () => {
        expect(render([])).toContain('Home');
    });
});
