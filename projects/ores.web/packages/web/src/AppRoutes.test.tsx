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
import { TranslationProvider } from './i18n/Provider.js';
import { AppRoutes, type AppRoutesProps } from './AppRoutes.js';
import type { BootstrapState } from './session/BootstrapProvider.js';
import type { SessionState } from './session/SessionProvider.js';
import type { SessionView } from '@ores/wire-protocol/browser';
import type { EnvironmentView } from '@ores/contracts';

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
    mode: 'application',
    version: 'v0.0.25 (test)',
    party,
    availableParties: [party],
    accessLifetimeSeconds: 3600,
    passwordResetRequired: false,
    actingIn: null,
};

const inBootstrap: BootstrapState = {
    status: 'ready',
    inBootstrapMode: true,
    hasTenant: false,
    onboardingComplete: false,
    message: 'This deployment has not been provisioned.',
    version: 'v0.0.25 (test)',
};
const ready: BootstrapState = {
    status: 'ready',
    inBootstrapMode: false,
    hasTenant: true,
    onboardingComplete: true,
    message: '',
    version: 'v0.0.25 (test)',
};
/** An installation whose administrator exists and whose first tenant does not. */
const tenantless: BootstrapState = { ...ready, hasTenant: false, onboardingComplete: false };
/** A system-only installation that finished its wizard, so it has no user tenant. */
const systemOnlyComplete: BootstrapState = { ...tenantless, onboardingComplete: true };
const anonymous: SessionState = { status: 'anonymous' };
const authenticated: SessionState = { status: 'authenticated', session };

const environment: EnvironmentView = {
    id: 'bright_faraday',
    displayName: 'Bright Faraday',
    description: '',
    nonProduction: true,
};

/**
 * The journey stands in for itself here: this file is about the route table,
 * and the journey reaches the server, which a rendered-to-string page cannot.
 * The journey's own screens are asserted in its own file.
 */
const journey = <p>First run journey</p>;
const newTenantJourney = <p>New tenant journey</p>;
const newPartyJourney = <p>New party journey</p>;
const signUpJourney = <p>Registration door</p>;

function render(
    path: string,
    gate: BootstrapState,
    sessionState: SessionState,
    overrides: Partial<AppRoutesProps> = {},
    permissionCodes: readonly string[] = [],
): string {
    const client = new QueryClient();
    /*
     * What the person holds, which is what decides the menu: the screens the
     * session's mode holds, less the ones gated on a permission.
     */
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
                <MemoryRouter initialEntries={[path]}>
                    <AppRoutes
                        gate={gate}
                        session={sessionState}
                        journey={journey}
                        newTenantJourney={newTenantJourney}
                        newPartyJourney={newPartyJourney}
                        signUpJourney={signUpJourney}
                        journeyInProgress={false}
                        onSignIn={async () => ({ outcome: 'active', passwordResetRequired: false })}
                        onChooseParty={async () => undefined}
                        onSignOut={() => undefined}
                        onEnterTenant={async () => undefined}
                        onLeaveTenant={() => undefined}
                        onRetryBootstrap={() => undefined}
                        {...overrides}
                    />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
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

    it('states the environment the deployment serves in the public shell', () => {
        const html = render('/login', ready, anonymous, { environment });

        expect(html).toContain('environment Bright Faraday (non-production)');
    });

    it('states the environment the deployment serves in the signed-in shell', () => {
        const html = render('/', ready, authenticated, { environment });

        expect(html).toContain('environment Bright Faraday (non-production)');
    });

    it('carries the banner on the sign-in dialog, above the work', () => {
        const html = render('/login', ready, anonymous);

        // The same element the setup journeys carry, at the ratio nothing may
        // crop, and it stands above the form rather than inside it.
        expect(html).toContain('width="964"');
        expect(html).toContain('height="323"');
        expect(html.indexOf('width="964"')).toBeLessThan(html.indexOf('current-password'));
    });

    it('offers the way to the registration door from the sign-in dialog', () => {
        const html = render('/login', ready, anonymous);

        expect(html).toContain('/signup');
    });

    it('does not render the application to a visitor', () => {
        const html = render('/', ready, anonymous);

        expect(html).not.toContain('Welcome, admin');
        expect(html).not.toContain('Sign out');
    });

    it('renders the application, with the session in it, to a signed-in person', () => {
        const html = render('/', ready, authenticated);

        expect(html).toContain('Welcome, admin');
        expect(html).toContain('Acme Corporation');
        expect(html).toContain('Working for Acme Operations');
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

    /*
     * A party user's home offers their own screens. A tenant is created from
     * Tenants, which only system administration has, so this home does not
     * offer one.
     */
    it("offers a party user's own screens from home, and no new tenant", () => {
        const html = render('/', ready, authenticated);

        expect(html).not.toContain('/tenants/new');
        expect(html).toContain('href="/security"');
    });
});

describe('the registration door', () => {
    it('is offered to a visitor at its own route', () => {
        const html = render('/signup', ready, anonymous);

        expect(html).toContain('Registration door');
    });

    it('is not offered to somebody who already has an account to sign in with', () => {
        const html = render('/signup', ready, authenticated);

        expect(html).not.toContain('Registration door');
    });

    it('is not offered while the installation has nobody in it', () => {
        const html = render('/signup', inBootstrap, anonymous);

        expect(html).toContain('First run journey');
        expect(html).not.toContain('Registration door');
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

    it('releases the browser once the wizard recorded that it finished', () => {
        /*
         * A system-only installation keeps no tenant of its own, so "has no
         * tenant" is true forever; the wizard's flag is what says the setup job
         * is done and the browser may leave the rail.
         */
        const visitor = render('/login', systemOnlyComplete, anonymous);

        expect(visitor).not.toContain('First run journey');
        expect(visitor).toContain('Sign in');
    });

    it('hands the browser over as soon as there is one', () => {
        const html = render('/', ready, authenticated);

        expect(html).not.toContain('First run journey');
        expect(html).toContain('Welcome, admin');
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

/*
 * The requests queue is the administrator's, and the menu says so: the screen
 * offers the door only to somebody who holds the permission the kind names to
 * decide it. The server checks the same permission again on every call.
 */
describe('the requests queue', () => {
    it('is offered in the menu to somebody who may assign roles', () => {
        const html = render('/', ready, authenticated, {}, ['iam::roles:assign']);

        expect(html).toContain('href="/requests"');
        expect(html).toContain('>Requests</a>');
    });

    it('is left out of the menu for somebody who may not', () => {
        const html = render('/', ready, authenticated, {}, ['refdata::currencies:read']);

        expect(html).not.toContain('href="/requests"');
    });

    it('renders the queue for a signed-in person at its own route, not the sign-in form', () => {
        const html = render('/requests', ready, authenticated, {}, ['iam::roles:assign']);

        expect(html).toContain('Sign out');
        expect(html).not.toContain('current-password');
    });

    it('sends a visitor to sign in rather than to the queue', () => {
        const html = render('/requests', ready, anonymous);

        expect(html).not.toContain('Roles people in this tenant have asked for, oldest first.');
        expect(html).not.toContain('Sign out');
    });
});
