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
import { TranslationProvider } from './i18n/Provider.js';
import { AppRoutes, type AppRoutesProps } from './AppRoutes.js';
import type { BootstrapState } from './session/BootstrapProvider.js';
import type { SessionState } from './session/SessionProvider.js';
import type { SessionView } from '@ores/wire-protocol/browser';

const party = {
    id: '3f1e2d4c-0000-4000-8000-000000000010',
    name: 'Acme Operations',
    partyCategory: 'Operational',
    businessCenterCode: 'GBLO',
};

const session: SessionView = {
    username: 'admin',
    email: 'admin@acme.test',
    accountId: '3f1e2d4c-0000-4000-8000-000000000001',
    tenantId: '3f1e2d4c-0000-4000-8000-000000000002',
    tenantName: 'Acme Corporation',
    party,
    availableParties: [party],
    accessLifetimeSeconds: 3600,
    passwordResetRequired: false,
};

const inBootstrap: BootstrapState = {
    status: 'ready',
    inBootstrapMode: true,
    message: 'This deployment has not been provisioned.',
};
const ready: BootstrapState = { status: 'ready', inBootstrapMode: false, message: '' };
const anonymous: SessionState = { status: 'anonymous' };
const authenticated: SessionState = { status: 'authenticated', session };

function render(
    path: string,
    gate: BootstrapState,
    sessionState: SessionState,
    overrides: Partial<AppRoutesProps> = {},
): string {
    return renderToStaticMarkup(
        <TranslationProvider>
            <MemoryRouter initialEntries={[path]}>
                <AppRoutes
                    gate={gate}
                    session={sessionState}
                    onSignIn={async () => ({ outcome: 'active' })}
                    onChooseParty={async () => undefined}
                    onSignOut={() => undefined}
                    onRetryBootstrap={() => undefined}
                    onCreateAdministrator={async () => undefined}
                    {...overrides}
                />
            </MemoryRouter>
        </TranslationProvider>,
    );
}

describe('the bootstrap gate', () => {
    it('renders the setup page, and no sign-in, for any path while the flag is set', () => {
        const html = render('/iam/account', inBootstrap, anonymous);

        expect(html).toContain('Set up this installation');
        expect(html).toContain('does not have an administrator account yet');
        expect(html).toContain('This deployment has not been provisioned.');
        // The setup form asks for a new password; the sign-in form is the one
        // that would ask for an existing one, and it is not offered.
        expect(html).toContain('new-password');
        expect(html).not.toContain('current-password');
    });

    it('offers the action that closes bootstrap mode', () => {
        const html = render('/', inBootstrap, anonymous);

        expect(html).toContain('Create administrator');
        expect(html).toContain('Administrator username');
        expect(html).toContain('Administrator email');
    });

    it('carries the banner, so the first screen of an installation looks like the product', () => {
        const html = render('/', inBootstrap, anonymous);

        expect(html).toContain('ore-studio-splash');
    });

    it('renders the setup page at the sign-in path too, so signing in is never offered', () => {
        const html = render('/login', inBootstrap, anonymous);

        expect(html).toContain('Set up this installation');
        expect(html).not.toContain('current-password');
    });
});

describe('the route table once the flag is clear', () => {
    it('offers the sign-in form to a visitor', () => {
        const html = render('/login', ready, anonymous);

        expect(html).toContain('Sign in');
        expect(html).toContain('Username');
        expect(html).toContain('type="password"');
    });

    it('does not render the application to a visitor', () => {
        const html = render('/', ready, anonymous);

        expect(html).not.toContain('Signed in');
        expect(html).not.toContain('Sign out');
    });

    it('renders the application, with the session in it, to a signed-in person', () => {
        const html = render('/', ready, authenticated);

        expect(html).toContain('Signed in');
        expect(html).toContain('Acme Corporation');
        expect(html).toContain('Acme Operations');
        expect(html).toContain('admin@acme.test');
        expect(html).toContain('Sign out');
    });
});

describe('a server that does not answer', () => {
    it('says so and offers to ask again, rather than guessing a shell', () => {
        const html = render('/', ready, anonymous, {
            gate: { status: 'unreachable', reason: '503: no broker' },
        });

        expect(html).toContain('The server did not answer');
        expect(html).toContain('503: no broker');
        expect(html).toContain('Try again');
        expect(html).not.toContain('Create administrator');
    });
});
