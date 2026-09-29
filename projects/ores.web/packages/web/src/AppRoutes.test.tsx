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
    version: 'v0.0.25 (test)',
    party,
    availableParties: [party],
    accessLifetimeSeconds: 3600,
    passwordResetRequired: false,
};

const inBootstrap: BootstrapState = {
    status: 'ready',
    inBootstrapMode: true,
    hasTenant: false,
    message: 'This deployment has not been provisioned.',
    version: 'v0.0.25 (test)',
};
const ready: BootstrapState = {
    status: 'ready',
    inBootstrapMode: false,
    hasTenant: true,
    message: '',
    version: 'v0.0.25 (test)',
};
/** An installation whose administrator exists and whose first tenant does not. */
const tenantless: BootstrapState = { ...ready, hasTenant: false };
const anonymous: SessionState = { status: 'anonymous' };
const authenticated: SessionState = { status: 'authenticated', session };

/**
 * The journey stands in for itself here: this file is about the route table,
 * and the journey reaches the server, which a rendered-to-string page cannot.
 * The journey's own screens are asserted in its own file.
 */
const journey = <p>First run journey</p>;
const newTenantJourney = <p>New tenant journey</p>;
const newPartyJourney = <p>New party journey</p>;

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
                    journey={journey}
                    newTenantJourney={newTenantJourney}
                    newPartyJourney={newPartyJourney}
                    journeyInProgress={false}
                    onSignIn={async () => ({ outcome: 'active', passwordResetRequired: false })}
                    onChooseParty={async () => undefined}
                    onSignOut={() => undefined}
                    onRetryBootstrap={() => undefined}
                    {...overrides}
                />
            </MemoryRouter>
        </TranslationProvider>,
    );
}

describe('the bootstrap gate', () => {
    it('renders the journey, and no sign-in, for any path while the flag is set', () => {
        const html = render('/iam/account', inBootstrap, anonymous);

        expect(html).toContain('First run journey');
        // The sign-in form is the one that would ask for an existing password,
        // and it is not offered: the installation has nobody to sign in as.
        expect(html).not.toContain('current-password');
    });

    it('renders the journey at the sign-in path too, so signing in is never offered', () => {
        const html = render('/login', inBootstrap, anonymous);

        expect(html).toContain('First run journey');
        expect(html).not.toContain('current-password');
    });
});

describe('a journey that has begun', () => {
    it('keeps the browser on it after the flag clears', () => {
        // Creating the administrator closes bootstrap mode, and the person is
        // half way through the journey when it does.
        const html = render('/setup', ready, authenticated, { journeyInProgress: true });

        expect(html).toContain('First run journey');
        expect(html).not.toContain('Sign out');
    });

    it('hands the browser to the ordinary routes once it finishes', () => {
        const html = render('/', ready, authenticated, { journeyInProgress: false });

        expect(html).not.toContain('First run journey');
        expect(html).toContain('Acme Corporation');
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

    it('offers the new tenant journey to a signed-in person', () => {
        // The journey is a route, not the gate: the installation is set up, and
        // this is one of the screens it serves.
        const html = render('/tenants/new', ready, authenticated);

        expect(html).toContain('New tenant journey');
        expect(html).toContain('Sign out');
    });

    it('sends a visitor to sign in rather than to a journey they cannot run', () => {
        const html = render('/tenants/new', ready, anonymous);

        // The guard is a redirect, and a redirect renders nothing of its own:
        // what matters is that the journey is not one of them.
        expect(html).not.toContain('New tenant journey');
    });

    it('offers the new party journey to a signed-in person', () => {
        const html = render('/parties/new', ready, authenticated);

        expect(html).toContain('New party journey');
        expect(html).toContain('Sign out');
    });

    it('sends a visitor to sign in rather than to the party journey', () => {
        const html = render('/parties/new', ready, anonymous);

        expect(html).not.toContain('New party journey');
    });

    it('offers the way into the journey from the home screen', () => {
        const html = render('/', ready, authenticated);

        expect(html).toContain('New tenant');
        expect(html).toContain('/tenants/new');
        expect(html).toContain('New party');
        expect(html).toContain('/parties/new');
    });
});

describe('an installation that has no tenant of its own', () => {
    it('holds a person whose deployment has not been set up, signed in or not', () => {
        const visitor = render('/iam/account', tenantless, anonymous);
        const signedIn = render('/', tenantless, authenticated);

        expect(visitor).toContain('First run journey');
        expect(signedIn).toContain('First run journey');
        expect(signedIn).not.toContain('Sign out');
    });

    it('hands the browser over as soon as there is one', () => {
        const html = render('/', ready, authenticated);

        expect(html).not.toContain('First run journey');
        expect(html).toContain('Signed in');
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
        expect(html).not.toContain('First run journey');
    });
});
