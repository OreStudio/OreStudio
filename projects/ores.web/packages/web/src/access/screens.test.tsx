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
import type { ReactNode } from 'react';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter, Route, Routes } from 'react-router';
import type { Account, HeldRole, RoleSummary } from '@ores/wire-protocol/browser';
import { TranslationProvider } from '../i18n/Provider.js';
import { MyAccessPage } from './MyAccessPage.js';
import { PersonPage } from './PersonPage.js';
import { RolePage } from './RolePage.js';
import { RolesPage } from './RolesPage.js';

/**
 * The access screens, rendered from a seeded query cache.
 *
 * What is checked is what each screen promises: a role that grants
 * everything says so, service roles stay out of a person's way, nobody can
 * take a role away from themselves, and every account shows its picture.
 */

const CATALOGUE = [
    { code: '*', description: 'Full access' },
    { code: 'refdata::currencies:read', description: 'View currencies' },
    { code: 'refdata::currencies:write', description: 'Create and modify currencies' },
    { code: 'iam::accounts:read', description: 'View user account details' },
];

const TRADING = '33333333-3333-3333-3333-333333333333';
const ADMIN = '44444444-4444-4444-4444-444444444444';

function held(roleId: string, name: string, codes: string[]): HeldRole {
    return {
        roleId: roleId as HeldRole['roleId'],
        name,
        description: `${name} role`,
        permissionCodes: codes,
        givenBy: 'priya',
        givenAt: '2026-10-04 09:00:00Z',
        reasonCode: 'access.new_joiner',
        commentary: 'Desk start',
    };
}

function role(id: string, name: string, codes: string[], service = false): RoleSummary {
    return {
        id: id as RoleSummary['id'],
        version: 1,
        name,
        description: `${name} role`,
        service,
        permissionCodes: codes,
    };
}

const daniel = {
    version: 1,
    id: '22222222-2222-2222-2222-222222222222',
    tenantId: 'ffffffff-ffff-ffff-ffff-ffffffffffff',
    username: 'daniel',
    fullName: 'Daniel Okafor',
    email: 'daniel@acme.example',
    accountType: 'user',
    jobTitle: 'FX trader',
    reportsToAccountId: null,
    defaultPartyId: null,
    imageId: '55555555-5555-5555-5555-555555555555',
    modifiedBy: 'priya',
    changeReasonCode: 'system.new_record',
    changeCommentary: '',
    performedBy: 'priya',
    recordedAt: '2026-10-04 09:00:00Z',
} as unknown as Account;

function render(seed: (client: QueryClient) => void, path: string, routes: ReactNode): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['permissions'], CATALOGUE);
    seed(client);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[path]}>
                    <Routes>{routes}</Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('My access', () => {
    it('names the roles held, who gave them and why, and draws only what they allow', () => {
        const html = render(
            (client) =>
                client.setQueryData(['my-access'], {
                    roles: [held(TRADING, 'Trading', ['refdata::currencies:read'])],
                }),
            '/access',
            <Route path="/access" element={<MyAccessPage tenantName="Acme" />} />,
        );

        expect(html).toContain('Trading');
        expect(html).toContain('Desk start');
        expect(html).toContain('src="/api/accounts/priya/picture"');
        expect(html).toContain('1 of 3 permissions');
        expect(html).toContain('Reference data');
        expect(html).not.toContain('Identity and access');
    });

    it('says everything rather than ticking every permission', () => {
        const html = render(
            (client) =>
                client.setQueryData(['my-access'], {
                    roles: [held(ADMIN, 'TenantAdmin', ['*'])],
                }),
            '/access',
            <Route path="/access" element={<MyAccessPage tenantName="Acme" />} />,
        );

        expect(html).toContain('Tenant administrator lets you do everything in this tenant.');
        expect(html).not.toContain('Reference data');
    });
});

describe('Roles', () => {
    it('keeps the service roles out of the list and says how many it hid', () => {
        const html = render(
            (client) =>
                client.setQueryData(
                    ['roles'],
                    [
                        role(TRADING, 'Trading', ['refdata::currencies:read']),
                        role(ADMIN, 'IamService', [], true),
                    ],
                ),
            '/roles',
            <Route path="/roles" element={<RolesPage />} />,
        );

        expect(html).toContain('Trading');
        expect(html).not.toContain('IamService');
        expect(html).toContain('1 service roles hidden');
    });

    it('does not edit a role that grants everything', () => {
        const html = render(
            (client) => {
                client.setQueryData(['roles'], [role(ADMIN, 'TenantAdmin', ['*'])]);
                client.setQueryData(['accounts'], { accounts: [], totalCount: 0 });
            },
            `/roles/${ADMIN}`,
            <Route path="/roles/:roleId" element={<RolePage />} />,
        );

        expect(html).toContain('This role grants every permission');
        expect(html).not.toContain('type="checkbox"');
    });
});

describe('A person', () => {
    function person(me: string): string {
        return render(
            (client) => {
                client.setQueryData(['account', 'daniel'], daniel);
                client.setQueryData(['account-access', daniel.id], {
                    roles: [held(TRADING, 'Trading', ['refdata::currencies:read'])],
                });
            },
            '/people/daniel',
            <Route path="/people/:username" element={<PersonPage me={me} />} />,
        );
    }

    it('shows the person picture and the roles they hold, with a way to take one away', () => {
        const html = person('priya');

        expect(html).toContain('src="/api/images/55555555-5555-5555-5555-555555555555"');
        expect(html).toContain('Daniel Okafor');
        expect(html).toMatch(/<button[^>]*>Take away<\/button>/);
        expect(html).not.toMatch(/<button[^>]*disabled=""[^>]*>Take away/);
    });

    it('does not let a person take a role away from themselves', () => {
        const html = person('daniel');

        expect(html).toMatch(/<button[^>]*disabled=""[^>]*>Take away<\/button>/);
    });
});
