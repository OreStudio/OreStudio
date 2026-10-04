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
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import { TranslationProvider } from '../i18n/Provider.js';
import { AppShell } from './AppShell.js';
import { PublicShell } from './PublicShell.js';

/**
 * A shell is a frame around one screen, so what it must do is show the frame
 * and the screen, and nothing else. The session is a prop rather than something
 * read from a provider here, which is what lets these render without a server.
 */

describe('the public shell', () => {
    it('shows the application name and the screen', () => {
        const html = renderToStaticMarkup(
            <TranslationProvider>
                <MemoryRouter>
                    <PublicShell>
                        <p>the screen</p>
                    </PublicShell>
                </MemoryRouter>
            </TranslationProvider>,
        );

        expect(html).toContain('ORE Studio');
        expect(html).toContain('the screen');
    });

    it('states what the browser runs and what the deployment runs', () => {
        const html = renderToStaticMarkup(
            <TranslationProvider>
                <MemoryRouter>
                    <PublicShell serverVersion="v0.0.25 [x64-linux] (local abc1234-dirty)">
                        <p>the screen</p>
                    </PublicShell>
                </MemoryRouter>
            </TranslationProvider>,
        );

        // The client's own build is stamped into the bundle, so the test knows
        // only that a version is stated, not which one this checkout produced.
        expect(html).toContain('client v');
        expect(html).toContain('server v0.0.25 [x64-linux] (local abc1234-dirty)');
    });

    it('says the deployment has not said which build it runs', () => {
        const html = renderToStaticMarkup(
            <TranslationProvider>
                <MemoryRouter>
                    <PublicShell>
                        <p>the screen</p>
                    </PublicShell>
                </MemoryRouter>
            </TranslationProvider>,
        );

        expect(html).toContain('server version unknown');
    });
});

/*
 * A system administrator inside a tenant reads another tenant's data. Every
 * screen says so and offers the way out, and a session in its own tenant
 * carries no such banner.
 */
describe('the application shell', () => {
    function renderAppShell(
        partyName: string,
        mode: 'system-administration' | 'tenant-administration' | 'application' = 'application',
        width?: 'column' | 'workspace',
    ): string {
        return renderToStaticMarkup(
            <TranslationProvider>
                <MemoryRouter>
                    <AppShell
                        username="admin"
                        tenantName="Acme Corporation"
                        partyName={partyName}
                        mode={mode}
                        {...(width !== undefined && { width })}
                        onSignOut={() => undefined}
                    >
                        <p>the screen</p>
                    </AppShell>
                </MemoryRouter>
            </TranslationProvider>,
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

    /*
     * The menu is the areas of the mode, and a mode whose journeys no group has
     * implemented has no areas. It is asserted as an absence because an empty
     * menu is not the same defect as a menu that names an area nobody can open.
     */
    it('offers the areas of its mode, and no area of another', () => {
        const system = renderAppShell('Acme Operations', 'system-administration');
        expect(system).toContain('Tenants');

        const application = renderAppShell('Acme Operations', 'application');
        expect(application).not.toContain('Tenants');
    });
});
