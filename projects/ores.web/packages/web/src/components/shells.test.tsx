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
});

describe('the application shell', () => {
    function renderAppShell(partyName: string): string {
        return renderToStaticMarkup(
            <TranslationProvider>
                <MemoryRouter>
                    <AppShell
                        username="admin"
                        tenantName="Acme Corporation"
                        partyName={partyName}
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
});
