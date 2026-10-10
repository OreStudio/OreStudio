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
    tenantBootstrapping: false,
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
    onboardingTenantComplete: false,
    // The installation has nobody to sign in as, so the flags were read for
    // no account and the request presented no session.
    accountId: '',
    sessionPresent: false,
    message: 'This deployment has not been provisioned.',
    version: 'v0.0.25 (test)',
};
const ready: BootstrapState = {
    status: 'ready',
    inBootstrapMode: false,
    hasTenant: true,
    onboardingComplete: true,
    onboardingTenantComplete: true,
    // The answer names the account whose session it was read through.
    accountId: session.accountId,
    sessionPresent: true,
    message: '',
    version: 'v0.0.25 (test)',
};
/** An installation whose administrator exists and whose first tenant does not. */
const tenantless: BootstrapState = { ...ready, hasTenant: false, onboardingComplete: false };
/** A system-only installation that finished its wizard, so it has no user tenant. */
const systemOnlyComplete: BootstrapState = { ...tenantless, onboardingComplete: true };
/*
 * The same answers as a visitor with no session reads them. The two setup flags
 * are settings, so a request with no session cannot read them and the answer
 * names no account. A screen must not act on that answer once somebody signs
 * in, which is why the signed-in fixtures above are not weakened to this.
 */
const readyAnonymous: BootstrapState = {
    ...ready,
    onboardingComplete: false,
    onboardingTenantComplete: false,
    accountId: '',
    sessionPresent: false,
};
const tenantlessAnonymous: BootstrapState = {
    ...tenantless,
    accountId: '',
    sessionPresent: false,
};
const systemOnlyAnonymous: BootstrapState = {
    ...systemOnlyComplete,
    accountId: '',
    sessionPresent: false,
};
const anonymous: SessionState = { status: 'anonymous' };
const authenticated: SessionState = { status: 'authenticated', session };
/** A super administrator, whose session acts on the deployment itself. */
const systemAdmin: SessionState = {
    status: 'authenticated',
    session: { ...session, mode: 'system-administration' },
};

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
const tenantSetupJourney = <p>Tenant setup screen</p>;
const newTenantJourney = <p>New tenant journey</p>;
const newPartyJourney = <p>New party journey</p>;
const counterpartyJourney = <p>Counterparty onboarding</p>;
const partyDetailsJourney = <p>Party details</p>;
const bookStructureJourney = <p>Book structure</p>;
const conventionJourney = <p>Instrument conventions</p>;
const signUpJourney = <p>Registration door</p>;

function render(
    path: string,
    gate: BootstrapState,
    sessionState: SessionState,
    overrides: Partial<AppRoutesProps> = {},
    permissionCodes: readonly string[] = [],
    seed?: (client: QueryClient) => void,
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
    seed?.(client);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[path]}>
                    <AppRoutes
                        gate={gate}
                        session={sessionState}
                        journey={journey}
                        tenantSetupJourney={tenantSetupJourney}
                        newTenantJourney={newTenantJourney}
                        newPartyJourney={newPartyJourney}
                        counterpartyJourney={counterpartyJourney}
                        partyDetailsJourney={partyDetailsJourney}
                        bookStructureJourney={bookStructureJourney}
                        conventionJourney={conventionJourney}
                        signUpJourney={signUpJourney}
                        journeyInProgress={false}
                        onSignIn={async () => ({ outcome: 'active', passwordResetRequired: false })}
                        onChooseParty={async () => undefined}
                        onSwitchParty={async () => undefined}
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
        // The rail is the one public screen a signed-in person stands on, so it
        // carries the way out now; what says this is the rail and not the
        // application is that the application's home is not rendered.
        expect(html).not.toContain('Welcome, admin');
    });

    it('hands the browser to the ordinary routes once it finishes', () => {
        const html = render('/', ready, authenticated, { journeyInProgress: false });

        expect(html).not.toContain('First run journey');
        expect(html).toContain('Acme Corporation');
    });
});

describe('the route table once the flag is clear', () => {
    it('offers the sign-in form to a visitor', () => {
        const html = render('/login', readyAnonymous, anonymous);

        expect(html).toContain('Sign in');
        expect(html).toContain('Username');
        expect(html).toContain('type="password"');
    });

    it('states the environment the deployment serves in the public shell', () => {
        const html = render('/login', readyAnonymous, anonymous, { environment });

        expect(html).toContain('Environment: Bright Faraday');
    });

    it('states the environment the deployment serves in the signed-in shell', () => {
        const html = render('/', ready, authenticated, { environment });

        expect(html).toContain('Environment: Bright Faraday');
    });

    it('carries the banner on the sign-in dialog, above the work', () => {
        const html = render('/login', readyAnonymous, anonymous);

        // The same element the setup journeys carry, at the ratio nothing may
        // crop, and it stands above the form rather than inside it.
        expect(html).toContain('width="964"');
        expect(html).toContain('height="323"');
        expect(html.indexOf('width="964"')).toBeLessThan(html.indexOf('current-password'));
    });

    it('offers the way to the registration door from the sign-in dialog', () => {
        const html = render('/login', readyAnonymous, anonymous);

        expect(html).toContain('/signup');
    });

    it('does not render the application to a visitor', () => {
        const html = render('/', readyAnonymous, anonymous);

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
        const html = render('/tenants/new', readyAnonymous, anonymous);

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
        const html = render('/parties/new', readyAnonymous, anonymous);

        expect(html).not.toContain('New party journey');
    });

    it('offers the counterparty onboarding screen to a signed-in person', () => {
        const html = render('/counterparties/onboard', ready, authenticated);

        expect(html).toContain('Counterparty onboarding');
    });

    it('offers the party details screen to a signed-in person', () => {
        const html = render('/parties/details', ready, authenticated);

        expect(html).toContain('Party details');
    });

    it('offers the book structure screen to a signed-in person', () => {
        const html = render('/books/structure', ready, authenticated);

        expect(html).toContain('Book structure');
    });

    it('offers the convention screen to a signed-in person', () => {
        const html = render('/conventions', ready, authenticated);

        expect(html).toContain('Instrument conventions');
    });

    it('sends a visitor to sign in rather than to the convention screen', () => {
        const html = render('/conventions', readyAnonymous, anonymous);

        expect(html).not.toContain('Instrument conventions');
    });

    it('sends a visitor to sign in rather than to the book structure screen', () => {
        const html = render('/books/structure', readyAnonymous, anonymous);

        expect(html).not.toContain('Book structure');
    });

    it('sends a visitor to sign in rather than to the party details screen', () => {
        const html = render('/parties/details', readyAnonymous, anonymous);

        expect(html).not.toContain('Party details');
    });

    it('sends a visitor to sign in rather than to the counterparty screen', () => {
        const html = render('/counterparties/onboard', readyAnonymous, anonymous);

        expect(html).not.toContain('Counterparty onboarding');
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
        expect(html).toContain('href="/where-i-work"');
    });

    it('offers the staff list and the hierarchy as two cards on the Organisation page', () => {
        const html = render('/organisation', ready, authenticated, {}, [], (client) =>
            client.setQueryData(['my-access'], {
                roles: [{ roleId: 'r', permissionCodes: ['iam::accounts:read'] }],
            }),
        );

        expect(html).toContain('href="/staff"');
        expect(html).toContain('href="/hierarchy"');
        expect(html).toContain('>Staff<');
        expect(html).toContain('>Hierarchy<');
    });

    it('opens the staff list and the hierarchy at their own routes', () => {
        const staff = render('/staff', ready, authenticated, {}, [], (client) =>
            client.setQueryData(['records', 'people', '', 'page', 0], {
                accounts: [],
                totalCount: 0,
            }),
        );
        expect(staff).toContain('>Staff<');

        const tree = render('/hierarchy', ready, authenticated, {}, [], (client) =>
            client.setQueryData(['reporting-tree'], {
                unrooted: 0,
                nodes: [
                    {
                        accountId: '11111111-1111-1111-1111-111111111111',
                        username: 'ada.lovelace',
                        fullName: 'Ada Lovelace',
                        jobTitle: 'Head of Desk',
                        imageId: null,
                        reportsToAccountId: null,
                        reportsOutsideScope: false,
                        depth: 0,
                        directReports: 0,
                        partyIds: [],
                    },
                ],
                parties: [],
            }),
        );
        expect(tree).toContain('>Hierarchy<');
        expect(tree).toContain('Ada Lovelace');
    });

    it('opens Where I work at its own route, and names the parties there', () => {
        const html = render('/where-i-work', ready, authenticated, {}, [], (client) =>
            client.setQueryData(['my-parties'], {
                defaultPartyId: '33333333-3333-3333-3333-333333333333',
                parties: [
                    {
                        partyId: '33333333-3333-3333-3333-333333333333',
                        name: 'ACME Corporation US Inc',
                        shortCode: 'ACCOUS',
                        partyCategory: 'Operational',
                        businessCenterCode: 'USNY',
                    },
                ],
            }),
        );

        expect(html).toContain('Where I work');
        expect(html).toContain('ACME Corporation US Inc');
    });
});

describe('the registration door', () => {
    it('is offered to a visitor at its own route', () => {
        const html = render('/signup', readyAnonymous, anonymous);

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
    it('does not hold a browser that has no session, so the sign-in screen shows', () => {
        /*
         * An installation may keep only the system tenant, so "no tenant" is
         * true forever; it is not a reason to hold the rail, and the wizard
         * signs the person out, so this is the state it leaves behind.
         */
        const visitor = render('/login', tenantlessAnonymous, anonymous);

        expect(visitor).not.toContain('First run journey');
        expect(visitor).toContain('Sign in');
    });

    it('holds a system administrator whose wizard has not recorded its finish', () => {
        const signedIn = render('/', tenantless, systemAdmin);

        expect(signedIn).toContain('First run journey');
        // A signed-in rail carries the way out, so it cannot be told from the
        // application by the absence of one; the home screen is the difference.
        expect(signedIn).not.toContain('Welcome, admin');
    });

    it('releases the browser once the wizard recorded that it finished', () => {
        /*
         * A system-only installation keeps no tenant of its own, so "has no
         * tenant" is true forever; the wizard's flag is what says the setup job
         * is done and the browser may leave the rail.
         */
        const visitor = render('/login', systemOnlyAnonymous, anonymous);

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
        const html = render('/', readyAnonymous, anonymous, {
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
        const html = render('/requests', readyAnonymous, anonymous);

        expect(html).not.toContain('Roles people in this tenant have asked for, oldest first.');
        expect(html).not.toContain('Sign out');
    });
});
