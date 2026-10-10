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
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import { describe, expect, it } from 'vitest';
import type { Scenario } from '@ores/contracts';
import { AppRoutes } from '../AppRoutes.js';
import { TranslationProvider } from '../i18n/Provider.js';
import type { BootstrapState } from '../session/BootstrapProvider.js';
import type { SessionState } from '../session/SessionProvider.js';
import { QA_SCENARIOS_KEY } from './TestsQueue.js';
import { summary } from './testSummary.js';

/**
 * The development routes, reached the way a person reaches them.
 *
 * The screens' own promises are asserted beside them. What this adds is the
 * wiring: the menu offers the area to a person who holds no permission, and
 * the queue and the scenario page are drawn under the shell.
 */

const ID = '22222222-2222-4222-8222-222222222222';

const party = {
    id: '3f1e2d4c-0000-4000-8000-000000000010',
    name: 'Acme Operations',
    partyCategory: 'Operational',
    businessCenterCode: 'GBLO',
};

const session: SessionState = {
    status: 'authenticated',
    session: {
        username: 'trader',
        email: 'trader@acme.test',
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
    },
};

const gate: BootstrapState = {
    status: 'ready',
    inBootstrapMode: false,
    hasTenant: true,
    onboardingComplete: true,
    onboardingTenantComplete: true,
    accountId: session.session.accountId,
    sessionPresent: true,
    message: '',
    version: 'v0.0.25 (test)',
};

const scenario: Scenario = {
    id: ID,
    title: 'Retake the currency screenshots',
    description: 'Use the runner to retake four screenshots.',
    state: 'PENDING',
    target: 'CurrencyDetailDialog',
    story: null,
    task: null,
    clients: [],
    beforeYouStart: [],
    steps: [
        {
            client: null,
            title: 'Open the General tab',
            instructions: [],
            status: 'PENDING',
            notes: '',
        },
        {
            client: null,
            title: 'Open the Rounding tab',
            instructions: [],
            status: 'PENDING',
            notes: '',
        },
    ],
    run: { status: '', completedAt: '', branch: '', commit: '', worktree: '' },
};

function render(path: string): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(QA_SCENARIOS_KEY, [summary({ id: ID, title: scenario.title })]);
    client.setQueryData(['qa-scenario', ID], scenario);
    client.setQueryData(['permissions'], []);
    client.setQueryData(['unread-notifications'], 0);
    client.setQueryData(['notifications'], { items: [], total: 0 });
    client.setQueryData(['my-access'], { roles: [] });
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
                        onSwitchParty={async () => undefined}
                        onSignOut={() => undefined}
                        onRetryBootstrap={() => undefined}
                    />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the development routes', () => {
    it('offers the area in the menu to a person who holds no permission', () => {
        const html = render('/');
        expect(html).toContain('href="/development"');
    });

    it('draws the queue under the shell', () => {
        const html = render('/development');
        expect(html).toContain('Retake the currency screenshots');
        expect(html).toContain('CurrencyDetailDialog');
        expect(html).toContain(`href="/development/tests/${ID}"`);
    });

    it('draws the scenario a row opens, with its steps, linked to the area from the menu and one crumb', () => {
        const html = render(`/development/tests/${ID}`);
        expect(html).toContain('Retake the currency screenshots');
        expect(html).toContain('Open the General tab');
        expect(html).toContain('Open the Rounding tab');
        expect(html).toContain('not built yet');
        expect(html.match(/href="\/development"/g)?.length).toBe(2);
    });
});
