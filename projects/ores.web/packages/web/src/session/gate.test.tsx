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
import { TranslationProvider } from '../i18n/Provider.js';
import { AppRoutes } from '../AppRoutes.js';
import type { BootstrapState } from './BootstrapProvider.js';
import type { SessionState } from './SessionProvider.js';
import type { SessionView } from '@ores/wire-protocol/browser';

/**
 * The gate, pinned to the persona that holds each rail.
 *
 * A first-run installation that keeps only the system tenant finishes with no
 * tenant of its own, and the wizard signs the person out as its last act. The
 * flag that says the wizard finished can only be read through a session, so a
 * browser with none reads it as false. That must not return the setup rail: the
 * rail is the login screen's. Once the deployment is set up, a tenant that has
 * not finished its own run holds that tenant's administrator on the tenant
 * setup screen, and a flag read as finished releases them. This file pins each
 * branch of both rules.
 */

const party = {
    id: '3f1e2d4c-0000-4000-8000-000000000010',
    name: 'Acme Operations',
    partyCategory: 'Operational',
    businessCenterCode: 'GBLO',
};

/** The account every signed-in fixture in this file holds. */
const ACCOUNT_ID = '3f1e2d4c-0000-4000-8000-000000000001';

function sessionIn(mode: SessionView['mode']): SessionView {
    return {
        username: 'admin',
        email: 'admin@acme.test',
        accountId: ACCOUNT_ID,
        tenantId: '3f1e2d4c-0000-4000-8000-000000000002',
        tenantName: 'Acme Corporation',
        mode,
        version: 'v0.0.25 (test)',
        party,
        availableParties: [party],
        accessLifetimeSeconds: 3600,
        passwordResetRequired: false,
        actingIn: null,
    };
}

/**
 * What the deployment answers after a Bare-system bootstrap finished, read
 * through the session of the account above.
 */
const afterBareSystem: BootstrapState = {
    status: 'ready',
    inBootstrapMode: false,
    hasTenant: false,
    onboardingComplete: false,
    onboardingTenantComplete: false,
    accountId: ACCOUNT_ID,
    sessionPresent: true,
    message: '',
    version: 'v0.0.25 (test)',
};

/**
 * The same answer as a visitor with no session reads it.
 *
 * The two flags are settings, so a request with no session cannot read them
 * and the answer names no account. The signed-in fixture above is not weakened
 * to this: a screen that acted on this answer for a signed-in person would put
 * the wizard in front of somebody who has finished it.
 */
const afterBareSystemAnonymous: BootstrapState = {
    ...afterBareSystem,
    accountId: '',
    sessionPresent: false,
};

/** What the deployment answers once the first run finished and made a tenant. */
const afterFirstRun: BootstrapState = {
    ...afterBareSystem,
    hasTenant: true,
    onboardingComplete: true,
};

/** A rail drawn while the deployment is still in bootstrap mode. */
const inBootstrapAnonymous: BootstrapState = {
    ...afterBareSystemAnonymous,
    inBootstrapMode: true,
};

const anonymous: SessionState = { status: 'anonymous' };

/** The header of a rendered page, where a rail offers what it offers. */
function header(html: string): string {
    return html.slice(html.indexOf('<header'), html.indexOf('</header>'));
}

function render(path: string, gate: BootstrapState, session: SessionState): string {
    const client = new QueryClient();
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[path]}>
                    <AppRoutes
                        gate={gate}
                        session={session}
                        journey={<p>First run journey</p>}
                        tenantSetupJourney={<p>Tenant setup screen</p>}
                        newTenantJourney={<p>New tenant journey</p>}
                        newPartyJourney={<p>New party journey</p>}
                        signUpJourney={<p>Registration door</p>}
                        journeyInProgress={false}
                        onSignIn={async () => ({ outcome: 'active', passwordResetRequired: false })}
                        onChooseParty={async () => undefined}
                        onSignOut={() => undefined}
                        onRetryBootstrap={() => undefined}
                    />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the bootstrap rail the deployment answers with', () => {
    it('does not return for a browser with no session, and the login screen shows', () => {
        const html = render('/login', afterBareSystemAnonymous, anonymous);

        expect(html).not.toContain('First run journey');
        expect(html).toContain('Sign in');
    });

    it('holds a system administrator whose named account has an unfinished wizard flag', () => {
        // The answer names the account in hand and says the wizard has not
        // finished, so the account rule lets the rail stand: the rule is about
        // whose answer it is, not a reason to disable a legitimate rail.
        const html = render('/', afterBareSystem, {
            status: 'authenticated',
            session: sessionIn('system-administration'),
        });

        expect(html).toContain('First run journey');
        expect(html).not.toContain('Tenant setup screen');
    });

    it('holds a tenant administrator whose tenant setup has not finished', () => {
        const html = render('/login', afterFirstRun, {
            status: 'authenticated',
            session: sessionIn('application'),
        });

        expect(html).toContain('Tenant setup screen');
        expect(html).not.toContain('First run journey');
    });

    it('releases a tenant administrator once the tenant setup recorded its finish', () => {
        const html = render(
            '/',
            { ...afterFirstRun, onboardingTenantComplete: true },
            {
                status: 'authenticated',
                session: sessionIn('application'),
            },
        );

        expect(html).not.toContain('Tenant setup screen');
        expect(html).not.toContain('First run journey');
    });

    it('decides nothing from an answer that names no account while somebody is signed in', () => {
        // The flags are settings read through a session, and this answer was
        // read through none. Acting on it would put the wizard in front of a
        // person who holds a session, so nothing is decided until an answer
        // that names their account arrives.
        const html = render('/', afterBareSystemAnonymous, {
            status: 'authenticated',
            session: sessionIn('system-administration'),
        });

        expect(html).toContain('Loading...');
        expect(html).not.toContain('First run journey');
    });

    it('offers the way out of a rail while somebody is signed in, and only then', () => {
        const signedIn = render('/', afterBareSystem, {
            status: 'authenticated',
            session: sessionIn('system-administration'),
        });
        const visitor = render('/', inBootstrapAnonymous, anonymous);

        expect(header(signedIn)).toContain('Sign out');
        expect(header(visitor)).not.toContain('Sign out');
    });
});
