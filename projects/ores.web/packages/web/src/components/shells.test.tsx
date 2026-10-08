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
import { describe, expect, it } from 'vitest';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import type { Account } from '@ores/wire-protocol/browser';
import type { EnvironmentView } from '@ores/contracts';
import { TranslationProvider } from '../i18n/Provider.js';
import { AppShell } from './AppShell.js';
import { PublicShell } from './PublicShell.js';

/**
 * A shell is a frame around one screen, so what it must do is show the frame
 * and the screen, and nothing else. The session is a prop rather than something
 * read from a provider here, which is what lets these render without a server.
 */

const brightFaraday: EnvironmentView = {
    id: 'bright_faraday',
    displayName: 'Bright Faraday',
    description: '',
    nonProduction: true,
};

const production: EnvironmentView = {
    id: 'production',
    displayName: 'Production',
    description: '',
    nonProduction: false,
};

describe('the public shell', () => {
    it('shows the application name and the screen', () => {
        const html = renderToStaticMarkup(
            <QueryClientProvider client={new QueryClient()}>
                <TranslationProvider>
                    <MemoryRouter>
                        <PublicShell>
                            <p>the screen</p>
                        </PublicShell>
                    </MemoryRouter>
                </TranslationProvider>
            </QueryClientProvider>,
        );

        expect(html).toContain('ORE Studio');
        expect(html).toContain('the screen');
    });

    it('states what the browser runs and what the deployment runs', () => {
        const html = renderToStaticMarkup(
            <QueryClientProvider client={new QueryClient()}>
                <TranslationProvider>
                    <MemoryRouter>
                        <PublicShell serverVersion="v0.0.25 [x64-linux] (local abc1234-dirty)">
                            <p>the screen</p>
                        </PublicShell>
                    </MemoryRouter>
                </TranslationProvider>
            </QueryClientProvider>,
        );

        // The client's own build is stamped into the bundle, so the test knows
        // only that a version is stated, not which one this checkout produced.
        expect(html).toContain('client v');
        expect(html).toContain('server v0.0.25 [x64-linux] (local abc1234-dirty)');
    });

    it('says the deployment has not said which build it runs', () => {
        const html = renderToStaticMarkup(
            <QueryClientProvider client={new QueryClient()}>
                <TranslationProvider>
                    <MemoryRouter>
                        <PublicShell>
                            <p>the screen</p>
                        </PublicShell>
                    </MemoryRouter>
                </TranslationProvider>
            </QueryClientProvider>,
        );

        expect(html).toContain('server version unknown');
    });

    it('states the environment it serves, and marks a non-production one', () => {
        const html = renderToStaticMarkup(
            <QueryClientProvider client={new QueryClient()}>
                <TranslationProvider>
                    <MemoryRouter>
                        <PublicShell environment={brightFaraday}>
                            <p>the screen</p>
                        </PublicShell>
                    </MemoryRouter>
                </TranslationProvider>
            </QueryClientProvider>,
        );

        expect(html).toContain('environment Bright Faraday (non-production)');
    });

    it('names a production environment without the marker', () => {
        const html = renderToStaticMarkup(
            <QueryClientProvider client={new QueryClient()}>
                <TranslationProvider>
                    <MemoryRouter>
                        <PublicShell environment={production}>
                            <p>the screen</p>
                        </PublicShell>
                    </MemoryRouter>
                </TranslationProvider>
            </QueryClientProvider>,
        );

        expect(html).toContain('environment Production');
        expect(html).not.toContain('non-production');
    });

    it('says it does not know the environment when the site has not answered', () => {
        const html = renderToStaticMarkup(
            <QueryClientProvider client={new QueryClient()}>
                <TranslationProvider>
                    <MemoryRouter>
                        <PublicShell>
                            <p>the screen</p>
                        </PublicShell>
                    </MemoryRouter>
                </TranslationProvider>
            </QueryClientProvider>,
        );

        expect(html).toContain('environment unknown');
    });
});

/*
 * The signed-in shell: the areas of the session's mode, the tenant beside the
 * brand, and the person's picture with the menu behind it.
 */
describe('the application shell', () => {
    function renderAppShell(
        partyName: string,
        mode: 'system-administration' | 'tenant-administration' | 'application' = 'application',
        width?: 'column' | 'workspace',
        permissionCodes: readonly string[] = [],
        self?: Account | null,
        unread = 0,
        environment?: EnvironmentView,
    ): string {
        const client = new QueryClient();
        client.setQueryData(['unread-notifications'], unread);
        client.setQueryData(['notifications'], { items: [], total: 0 });
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
                        <AppShell
                            username="admin"
                            tenantName="Acme Corporation"
                            partyName={partyName}
                            mode={mode}
                            {...(width !== undefined && { width })}
                            {...(self !== undefined && { self })}
                            {...(environment !== undefined && { environment })}
                            onSignOut={() => undefined}
                        >
                            <p>the screen</p>
                        </AppShell>
                    </MemoryRouter>
                </TranslationProvider>
            </QueryClientProvider>,
        );
    }

    it('shows the session it is in, the way out of it, and the screen', () => {
        const html = renderAppShell('Acme Operations');

        expect(html).toContain('ORE Studio');
        expect(html).toContain('admin');
        expect(html).toContain('Acme Corporation');
        expect(html).toContain('Acme Operations');
        expect(html).toContain('Sign out');
        expect(html).toContain('the screen');
    });

    /*
     * The header holds the picture alone; who the person is, where they work,
     * their own pages and the way out sit in the menu, hidden until opened.
     */
    it('puts the person behind their picture, with their pages and the way out', () => {
        const html = renderAppShell('Acme Operations');

        expect(html).toContain('aria-label="Your account"');
        expect(html).toContain('src="/api/accounts/admin/picture"');
        expect(html).toMatch(/role="menu" hidden=""/);
        expect(html).toContain('href="/access"');
        expect(html).toContain('href="/security"');
        expect(html).not.toContain('admin · Acme Corporation');
    });

    /*
     * The header names the person from their own account once the shell has
     * read it, so an administrator is not listed by a handle; the username
     * stays under the name as the handle it is.
     */
    it('names the person from their own account once the shell has read it', () => {
        const html = renderAppShell('Acme Operations', 'application', undefined, [], {
            version: 1,
            id: '33333333-3333-4333-8333-333333333333',
            tenantId: '22222222-2222-4222-8222-222222222222',
            username: 'admin',
            fullName: 'Super Admin',
            email: 'admin@example.com',
            accountType: 'user',
            jobTitle: '',
            reportsToAccountId: null,
            defaultPartyId: null,
            imageId: null,
            modifiedBy: 'admin',
            changeReasonCode: 'common.non_material_update',
            changeCommentary: '',
            performedBy: 'admin',
            recordedAt: '2026-10-05 09:30:00Z',
        });

        expect(html).toContain('Super Admin');
        expect(html).toContain('@admin');
    });

    it('names the party it is working in', () => {
        expect(renderAppShell('Northwind Trading')).toContain('Northwind Trading');
    });

    /*
     * A journey that stands something up draws the same banner the public shell
     * draws, and a banner fills the width it is given. The public shell bounds
     * its column and the signed-in shell did not, so the same journey drew a
     * wall on a wide display once somebody had signed in.
     */
    it('bounds the screen it wraps, as the public shell bounds its own', () => {
        expect(renderAppShell('Acme Operations')).toContain('max-w-[1100px]');
    });

    /*
     * One bound for every screen was wrong in both directions: a rail or a form
     * wants the column, and a list bounded at that width leaves a dead margin
     * on either side of nothing. A screen that has not said is a column, so the
     * harder mistake, a form across a wide display, is the one that needs the
     * screen to ask for.
     */
    it('gives a list-shaped screen the wider bound it asks for', () => {
        const html = renderAppShell('Acme Operations', 'application', 'workspace');

        expect(html).toContain('max-w-[1600px]');
        expect(html).not.toContain('max-w-[1100px]');
    });

    it('states the mode the session runs in', () => {
        expect(renderAppShell('Acme Operations', 'system-administration')).toContain(
            'System administration',
        );
    });

    it('states the environment the deployment serves', () => {
        const html = renderAppShell(
            'Acme Operations',
            'application',
            undefined,
            [],
            undefined,
            0,
            brightFaraday,
        );

        expect(html).toContain('environment Bright Faraday (non-production)');
    });

    /*
     * The menu is the areas of the mode, and a mode whose journeys no group has
     * implemented has no areas. It is asserted as an absence because an empty
     * menu is not the same defect as a menu that names an area nobody can open.
     */
    it('offers the areas of its mode, and no area of another', () => {
        const system = renderAppShell('Acme Operations', 'system-administration');
        expect(system).toContain('Tenants');
        expect(system).toContain('>Accounts<');
        expect(system).toContain('href="/people"');

        const application = renderAppShell('Acme Operations', 'application');
        expect(application).not.toContain('Tenants');
    });

    it('offers People only to a person who may read the accounts', () => {
        expect(renderAppShell('Acme', 'application')).not.toContain('href="/people"');
        expect(renderAppShell('Acme', 'application', undefined, ['iam::accounts:read'])).toContain(
            'href="/people"',
        );
    });

    /*
     * The bell sits in the header on every screen, and the count is the one
     * thing it must say before anybody opens it: a bell that only tells a
     * person what happened after they look is a bell nobody looks at.
     */
    it('carries the notification bell, saying how much is unread', () => {
        const html = renderAppShell('Acme Operations', 'application', undefined, [], undefined, 4);

        expect(html).toContain('aria-label="Notifications, 4 unread"');
        expect(html).toContain('>4</span>');
    });
});
