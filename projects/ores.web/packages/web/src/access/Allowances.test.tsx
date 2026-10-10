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
import type { HeldRole, PermissionEntry, PermissionPage } from '@ores/wire-protocol/browser';
import { TranslationProvider } from '../i18n/Provider.js';
import { CanIPanel, RolesAllow } from './Allowances.js';

const COMPONENTS = ['alpha', 'bravo', 'charlie', 'delta', 'echo', 'foxtrot', 'golf'];

const catalogue: readonly PermissionEntry[] = COMPONENTS.flatMap((component) => [
    { code: `${component}::things:read`, description: `Read ${component} things` },
    { code: `${component}::things:write`, description: `Write ${component} things` },
]);

const role: HeldRole = {
    roleId: 'r',
    name: 'Trading',
    description: '',
    permissionCodes: COMPONENTS.map((component) => `${component}::*`),
    givenBy: 'system',
    givenAt: '2026-10-05 09:30:00Z',
    reasonCode: 'access.initial',
    commentary: '',
};

function render(node: React.ReactNode): string {
    return renderToStaticMarkup(<TranslationProvider>{node}</TranslationProvider>);
}

/**
 * A page as the server answers it: the rows of one area, the total of the
 * matching rows, and the areas the account holds something in.
 */
function serverPage(area: string, count: number, total: number): PermissionPage {
    return {
        area,
        totalCount: total,
        areas: [
            { component: 'refdata', resources: 40 },
            { component: 'iam', resources: 3 },
        ],
        rows: Array.from({ length: count }, (_unused, index) => ({
            component: area,
            resource: `thing_${String(index + 1).padStart(2, '0')}`,
            actions: ['read', 'write', 'delete'],
            held: ['read'],
            roles: ['Operations'],
        })),
    };
}

function renderPanel(page: PermissionPage, everythingBy?: string): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['permission-page', 'me', '', '', 0, 15], page);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <RolesAllow
                    queryKey={['me']}
                    read={() => Promise.resolve(page)}
                    everythingBy={everythingBy}
                />
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

function shownResources(html: string): number {
    return (html.match(/>thing \d\d</g) ?? []).length;
}

describe('RolesAllow', () => {
    it('draws the page the server answered and says how many there are in all', () => {
        const html = renderPanel(serverPage('refdata', 15, 40));

        expect(shownResources(html)).toBe(15);
        expect(html).toContain('Showing 1–15 of 40 permissions');
        expect(html).toContain('40 resources');
    });

    it('offers the standard page sizes', () => {
        const html = renderPanel(serverPage('refdata', 15, 40));

        expect(html).toContain('value="25"');
        expect(html).toContain('value="100"');
    });

    it('offers only the areas the server lists, with the chosen one selected and no all areas', () => {
        const html = renderPanel(serverPage('refdata', 15, 40));

        expect(html).toContain('value="refdata"');
        expect(html).toContain('value="iam"');
        expect(html).not.toContain('value="alpha"');
        expect(html).not.toContain('All areas');
        expect(html).toMatch(/<option value="refdata" selected/);
    });

    it('asks the server for the page, not the whole catalogue', () => {
        const asked: unknown[] = [];
        const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
        void client.prefetchQuery({
            queryKey: ['permission-page', 'me', '', '', 0, 15],
            queryFn: () => {
                asked.push('read');
                return Promise.resolve(serverPage('refdata', 15, 40));
            },
        });
        expect(asked).toEqual(['read']);
    });

    it('says everything when a role grants everything, and reads nothing', () => {
        const html = renderPanel(serverPage('refdata', 15, 40), 'TenantAdmin');

        expect(html).toContain('lets you do everything');
        expect(html).not.toContain('<details');
    });
});

describe('CanIPanel', () => {
    it('is the question box, for any set of roles', () => {
        const html = render(<CanIPanel roles={[role]} catalogue={catalogue} />);

        expect(html).toContain('type="search"');
    });
});
