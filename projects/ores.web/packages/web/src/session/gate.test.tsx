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
 * The gate, pinned to the persona that holds the rail.
 *
 * A first-run installation that keeps only the system tenant finishes with no
 * tenant of its own, and the wizard signs the person out as its last act. The
 * flag that says the wizard finished can only be read through a session, so a
 * browser with none reads it as false. That must not return the setup rail: the
 * rail is the login screen's, and this file pins each branch of that rule.
 */

const party = {
    id: '3f1e2d4c-0000-4000-8000-000000000010',
    name: 'Acme Operations',
    partyCategory: 'Operational',
    businessCenterCode: 'GBLO',
};

function sessionIn(mode: SessionView['mode']): SessionView {
    return {
        username: 'admin',
        email: 'admin@acme.test',
        accountId: '3f1e2d4c-0000-4000-8000-000000000001',
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

/** What the deployment answers after a Bare-system bootstrap finished. */
const afterBareSystem: BootstrapState = {
    status: 'ready',
    inBootstrapMode: false,
    hasTenant: false,
    onboardingComplete: false,
    message: '',
    version: 'v0.0.25 (test)',
};

const anonymous: SessionState = { status: 'anonymous' };

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

describe('the bootstrap rail after a Bare-system run', () => {
    it('does not return for a browser with no session, and the login screen shows', () => {
        const html = render('/login', afterBareSystem, anonymous);

        expect(html).not.toContain('First run journey');
        expect(html).toContain('Sign in');
    });

    it('holds a system administrator whose wizard flag reads unfinished', () => {
        const html = render('/', afterBareSystem, {
            status: 'authenticated',
            session: sessionIn('system-administration'),
        });

        expect(html).toContain('First run journey');
    });

    it('does not hold a session in an ordinary tenant', () => {
        const html = render('/login', afterBareSystem, {
            status: 'authenticated',
            session: sessionIn('application'),
        });

        expect(html).not.toContain('First run journey');
    });
});
