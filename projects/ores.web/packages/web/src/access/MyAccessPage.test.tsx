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
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import { describe, expect, it } from 'vitest';
import { TranslationProvider } from '../i18n/Provider.js';
import { MyAccessPage } from './MyAccessPage.js';

/** The screen while a read fails: it keeps its breadcrumb and header, and says why. */
describe('My access while a read fails', () => {
    function render(failure: 'access' | 'catalogue' | 'none'): string {
        const client = new QueryClient({
            defaultOptions: { queries: { retry: false, retryOnMount: false } },
        });
        const fail = (key: string): void => {
            client
                .getQueryCache()
                .build(client, { queryKey: [key] })
                .setState({
                    status: 'error',
                    error: new Error(`${key} could not be read`),
                    fetchStatus: 'idle',
                });
        };
        if (failure === 'access') fail('my-access');
        if (failure === 'catalogue') fail('permissions');
        return renderToStaticMarkup(
            <QueryClientProvider client={client}>
                <TranslationProvider>
                    <MemoryRouter>
                        <MyAccessPage tenantName="Acme Corporation" />
                    </MemoryRouter>
                </TranslationProvider>
            </QueryClientProvider>,
        );
    }

    it('says the read that failed, while the other still waits, under its trail and header', () => {
        const html = render('access');

        expect(html).toContain('my-access could not be read');
        expect(html).toContain('aria-label="Breadcrumb"');
        expect(html).toContain('<h1');
        expect(html).not.toContain('Loading');
    });

    it('says the catalogue failed the same way', () => {
        expect(render('catalogue')).toContain('permissions could not be read');
    });

    it('says it is loading when nothing has failed, under the same trail', () => {
        const html = render('none');

        expect(html).toContain('Loading');
        expect(html).toContain('aria-label="Breadcrumb"');
    });
});
