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
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import type { InboxRequestView } from '@ores/wire-protocol/browser';
import { AppRoutes } from '../AppRoutes.js';
import { TranslationProvider } from '../i18n/Provider.js';
import type { BootstrapState } from '../session/BootstrapProvider.js';
import type { SessionState } from '../session/SessionProvider.js';

/**
 * The requests route, reached the way a person reaches it.
 *
 * The screen's own promises are asserted in the inbox's test file; what this
 * adds is the wiring, because a screen nobody can get to is a screen nobody
 * has. The queue is drawn under the shell, with the menu the mode and the
 * person's permissions produce.
 */

const REQUEST = 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa';
const ROLE = 'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb';

const request = {
    id: REQUEST,
    version: 1,
    kindCode: 'iam.role_grant',
    stateCode: 'waiting',
    requestedBy: 'daniel',
    requestedAt: '2026-10-04 09:00:00Z',
    reason: 'I price the FX book and cannot read currencies.',
    expiresAt: '',
    roles: [{ roleId: ROLE, name: 'Trading', description: 'Trading desk access' }],
    decision: null,
} as unknown as InboxRequestView;

const party = {
    id: '3f1e2d4c-0000-4000-8000-000000000010',
    name: 'Acme Operations',
    partyCategory: 'Operational',
    businessCenterCode: 'GBLO',
};

const session: SessionState = {
    status: 'authenticated',
    session: {
        username: 'priya',
        email: 'priya@acme.test',
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
    },
};

const gate: BootstrapState = {
    status: 'ready',
    inBootstrapMode: false,
    hasTenant: true,
    onboardingComplete: true,
    message: '',
    version: 'v0.0.25 (test)',
};

function render(path: string, permissionCodes: readonly string[]): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['request-queue'], { items: [request], total: 1, answered: [] });
    client.setQueryData(
        ['permissions'],
        [{ code: 'refdata::currencies:read', description: 'View currencies' }],
    );
    client.setQueryData(['unread-notifications'], 0);
    client.setQueryData(['notifications'], { items: [], total: 0 });
    client.setQueryData(['my-access'], {
        roles: [
            {
                roleId: ROLE,
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

describe('the requests route', () => {
    it('draws the queue under the shell, in the menu for a decider', () => {
        const html = render('/requests', ['iam::roles:assign']);

        expect(html).toContain('Roles people in this tenant have asked for, oldest first.');
        expect(html).toContain('daniel');
        expect(html).toContain('Trading');
        expect(html).toContain('href="/requests"');
    });

    it('draws the queue even when the menu left its door out', () => {
        // The menu is the client's structure, not the gate: a person who
        // reaches the route still gets the screen, and the server decides on
        // every call what they may actually do.
        const html = render('/requests', []);

        expect(html).toContain('Trading');
        expect(html).not.toContain('href="/requests"');
    });
});
