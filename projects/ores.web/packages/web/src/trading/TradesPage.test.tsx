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

import { describe, expect, it, vi } from 'vitest';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import { TranslationProvider } from '../i18n/Provider.js';
import { TradesPage } from './TradesPage.js';

/** The list is read by the table, so a static render never calls it. */
vi.mock('../api/trades.js', async () => {
    const actual = await vi.importActual<typeof import('../api/trades.js')>('../api/trades.js');
    return {
        ...actual,
        trades: { page: () => Promise.reject(new Error('not read in a static render')) },
    };
});

function render(): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={['/trades']}>
                    <TradesPage />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the trade list screen', () => {
    it('draws its title, its lead and the two filters the server reads', () => {
        const html = render();
        expect(html).toContain('Trades');
        expect(html).toContain('The trades and structures of your party');
        expect(html).toContain('All states');
        expect(html).toContain('All types');
    });

    it('offers no Add, because booking is its own screen', () => {
        expect(render()).not.toContain('Add');
    });
});
