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
import { textOf } from '../textOf.js';
import { AreaCards } from './AreaCards.js';
import { WorkflowMenu } from './WorkflowMenu.js';

/**
 * Which cards a person is offered, by the permissions they hold. These tests
 * read the names and the addresses a person would click, and nothing about how
 * the cards are laid out.
 */

function render(permissionCodes: readonly string[], page: 'area' | 'menu'): string {
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
                    {page === 'area' ? (
                        <AreaCards area="refdata" mode="application" />
                    ) : (
                        <WorkflowMenu on="parties" />
                    )}
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the Reference data cards', () => {
    it('offer every screen, and the workflows that have no screen of their own', () => {
        const words = textOf(render(['refdata::parties:read'], 'area'));

        for (const name of [
            'Parties',
            'Currencies',
            'Currency pairs',
            'Desk groups',
            'Holiday calendars',
            'Classifications',
            'Shape the book structure',
            'Define instrument conventions',
        ]) {
            expect(words, name).toContain(name);
        }
    });

    it('leave the party workflows to the Parties screen', () => {
        const words = textOf(render(['*'], 'area'));

        expect(words).not.toContain('Onboard a party');
        expect(words).not.toContain('Onboard a counterparty');
    });

    it('lead to the screens by their addresses under Reference data', () => {
        const html = render(['refdata::parties:read'], 'area');

        expect(html).toContain('href="/refdata/parties"');
        expect(html).toContain('href="/refdata/currencies"');
        expect(html).toContain('href="/refdata/books/structure"');
    });
});

describe('the workflows on the Parties screen', () => {
    it('are offered to a person who may write parties and counterparties', () => {
        const html = render(['refdata::parties:write', 'refdata::counterparties:write'], 'menu');
        const words = textOf(html);

        expect(words).toContain('Onboard a party');
        expect(words).toContain("Correct a party's details");
        expect(words).toContain('Onboard a counterparty');
        expect(html).toContain('href="/refdata/parties/new"');
    });

    it('offer only the workflows the person may start', () => {
        const words = textOf(render(['refdata::counterparties:write'], 'menu'));

        expect(words).toContain('Onboard a counterparty');
        expect(words).not.toContain('Onboard a party');
    });

    it('are not drawn at all for a person who may start none', () => {
        expect(render(['refdata::parties:read'], 'menu')).toBe('');
    });
});
