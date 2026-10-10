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
import { AreaCards } from './AreaCards.js';

/**
 * An area page tells a screen card from a workflow card at a glance: the
 * workflows stand together under their own heading, in their own hue, each with
 * a tag.
 */

function reference(permissionCodes: readonly string[]): string {
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
                    <AreaCards area="refdata" mode="application" />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the Reference data cards', () => {
    it('draw the workflows apart, in their own hue and with a tag', () => {
        const html = reference(['*']);

        expect(html).toMatch(/Workflows<\/h2>/);
        expect(html).toContain('Onboard a party');
        expect(html).toContain('Onboard a counterparty');
        expect(html).toContain('text-flow');
        expect(html).toMatch(/Workflow<\/span>/);
    });

    it('draw the screens under their topics', () => {
        const html = reference(['refdata::parties:read']);

        expect(html).toContain('href="/refdata/parties"');
        expect(html).toContain('href="/refdata/currencies"');
        expect(html).not.toContain('Onboard a party');
    });

    it('draw the workflows that need no write permission', () => {
        const html = reference([]);

        expect(html).toContain('Shape the book structure');
        expect(html).toContain('Define instrument conventions');
        expect(html).not.toContain('Onboard a counterparty');
    });
});
