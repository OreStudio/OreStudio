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
import type { QaDoc, Scenario } from '@ores/contracts';
import { ApiFailure } from '../api/transport.js';
import { docKey } from './DocPanel.js';
import { AppRoutes } from '../AppRoutes.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { SiteProvider, SITE_QUERY_KEY } from '../session/SiteProvider.js';
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
    created: '2026-10-11',
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

function render(path: string, seed: (client: QueryClient) => void = () => undefined): string {
    const client = new QueryClient({
        defaultOptions: { queries: { retry: false, retryOnMount: false } },
    });
    client.setQueryData(QA_SCENARIOS_KEY, [summary({ id: ID, title: scenario.title })]);
    client.setQueryData(['qa-scenario', ID], scenario);
    client.setQueryData(SITE_QUERY_KEY, {
        appName: 'ORE Studio',
        environment: {
            id: 'eager_maxwell',
            displayName: 'Eager Maxwell',
            description: '',
            nonProduction: true,
        },
        developerTools: true,
        developerAccounts: [],
    });
    client.setQueryData(['permissions'], []);
    client.setQueryData(['unread-notifications'], 0);
    client.setQueryData(['notifications'], { items: [], total: 0 });
    client.setQueryData(['my-access'], { roles: [] });
    seed(client);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <SiteProvider>
                    <MemoryRouter initialEntries={[path]}>
                        <AppRoutes
                            gate={gate}
                            session={session}
                            journey={<p>First run journey</p>}
                            newTenantJourney={<p>New tenant journey</p>}
                            newPartyJourney={<p>New party journey</p>}
                            signUpJourney={<p>Registration door</p>}
                            journeyInProgress={false}
                            onSignIn={async () => ({
                                outcome: 'active',
                                passwordResetRequired: false,
                            })}
                            onChooseParty={async () => undefined}
                            onSwitchParty={async () => undefined}
                            onSignOut={() => undefined}
                            onRetryBootstrap={() => undefined}
                        />
                    </MemoryRouter>
                </SiteProvider>
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

    it('draws the runner for the scenario a row opens, linked to the area from the menu and one crumb', () => {
        const html = render(`/development/tests/${ID}`);
        expect(html).toContain('Retake the currency screenshots');
        expect(html).toContain('Step 1 of 2');
        expect(html).toContain('Open the General tab');
        expect(html).toContain('>Pass</button>');
        expect(html).toContain('>Fail</button>');
        expect(html).toContain('>Save</button>');
        expect(html.match(/href="\/development"/g)?.length).toBe(3);
    });
});

const STORY = 'FE07BF4D-054D-4A69-AF3C-D70D10493370';
const TASK = '8DC4ABA3-B053-4C40-B575-6EDFCF3C86DE';
const withContext: Scenario = {
    ...scenario,
    story: { id: STORY, title: 'Refresh the manual' },
    task: { id: TASK, title: 'Retake the shots' },
};
const storyDoc: QaDoc = {
    id: STORY,
    title: 'Story: Refresh the manual',
    type: 'story',
    path: 'sprint_27/refresh/story.org',
    text: `* Goal\n\nRefresh the manual. See [[id:${TASK}][the task]].`,
};
const taskDoc: QaDoc = {
    id: TASK,
    title: 'Task: Retake the shots',
    type: 'task',
    path: 'sprint_27/refresh/task.org',
    text: '* Goal\n\nRetake four screenshots.',
};

function seeded(client: QueryClient): void {
    client.setQueryData(['qa-scenario', ID], withContext);
    client.setQueryData(docKey(STORY), storyDoc);
    client.setQueryData(docKey(TASK), taskDoc);
}

describe('the story and the task beside the steps', () => {
    it('offers Steps, Story and Task, with Steps showing first', () => {
        const html = render(`/development/tests/${ID}`, seeded);
        for (const tab of ['Steps', 'Story', 'Task']) expect(html).toContain(`>${tab}</button>`);
        expect(html).not.toMatch(
            /<div hidden=""><div class="space-y-4"><section class="card space-y-3/,
        );
        expect(html).toContain('Open the General tab');
        expect(html).not.toContain('Story: Refresh the manual');
    });

    it('shows the rendered story on the Story tab, with a link to the task', () => {
        const html = render(`/development/tests/${ID}?tab=story`, seeded);
        expect(html).toContain('Story: Refresh the manual');
        expect(html).toContain('Refresh the manual. See');
        expect(html).toContain(`href="/development/docs/${TASK}"`);
    });

    it('shows the rendered task on the Task tab', () => {
        const html = render(`/development/tests/${ID}?tab=task`, seeded);
        expect(html).toContain('Task: Retake the shots');
        expect(html).toContain('Retake four screenshots.');
    });

    it('opens an open scenario on its steps, and offers the report beside them', () => {
        const html = render(`/development/tests/${ID}`, seeded);
        const tabs = html
            .match(/role="tab"[^>]*>([^<]+)</g)
            ?.map((t) => t.replace(/^.*>/, '').replace('<', ''));
        expect(tabs).toEqual(['Steps', 'Report', 'Story', 'Task']);
        expect(html).not.toContain('Steps at a glance');
    });

    it('opens a closed scenario on its report, where a reviewer reads it', () => {
        const closed = (client: QueryClient) =>
            client.setQueryData(['qa-scenario', ID], {
                ...withContext,
                state: 'PASSED',
                steps: withContext.steps.map((step) => ({
                    ...step,
                    status: 'PASS',
                    notes: 'It worked.',
                })),
            });
        const html = render(`/development/tests/${ID}`, closed);
        const tabs = html
            .match(/role="tab"[^>]*>([^<]+)</g)
            ?.map((t) => t.replace(/^.*>/, '').replace('<', ''));
        expect(tabs).toEqual(['Report', 'Steps', 'Story', 'Task']);
        expect(html).toContain('Steps at a glance');
        expect(html).toContain('It worked.');
        expect(html).toContain('<div hidden="">');
    });

    it('shows the report of an open scenario when its tab is asked for', () => {
        const html = render(`/development/tests/${ID}?tab=report`, seeded);
        expect(html).toContain('Steps at a glance');
        expect(html).toContain('<div hidden="">');
    });

    it('keeps the runner mounted but hidden on another tab, so its marks survive', () => {
        const html = render(`/development/tests/${ID}?tab=story`, seeded);
        expect(html).toContain('<div hidden="">');
        expect(html).toContain('Open the General tab');
    });

    it('says plainly when the scenario names no story or no task', () => {
        const none = (client: QueryClient) => client.setQueryData(['qa-scenario', ID], scenario);
        expect(render(`/development/tests/${ID}?tab=story`, none)).toContain(
            'This scenario names no story.',
        );
        const task = render(`/development/tests/${ID}?tab=task`, none);
        expect(task).toContain('This scenario names no task.');
        expect(task).not.toContain('role="alert"');
    });

    it('says a doc the server cannot find does not resolve', () => {
        const dead = (client: QueryClient) => {
            client.setQueryData(['qa-scenario', ID], withContext);
            const query = client.getQueryCache().build(client, { queryKey: docKey(STORY) });
            query.setState({
                ...query.state,
                status: 'error',
                error: new ApiFailure(404, { code: 'not-found', message: 'No story.' }),
                fetchStatus: 'idle',
            });
        };
        expect(render(`/development/tests/${ID}?tab=story`, dead)).toContain(
            `No story, task or scenario has the id ${STORY}.`,
        );
    });
});

describe('the doc route', () => {
    it('opens the doc an id link names, under the shell', () => {
        const html = render(`/development/docs/${TASK}`, seeded);
        expect(html).toContain('Task: Retake the shots');
        expect(html).toContain('Retake four screenshots.');
        expect(html).toContain('>Doc</span>');
    });

    it('says a link that does not resolve does not', () => {
        const html = render('/development/docs/CCCCCCCC-CCCC-4CCC-8CCC-CCCCCCCCCCCC', (client) => {
            const query = client.getQueryCache().build(client, {
                queryKey: docKey('CCCCCCCC-CCCC-4CCC-8CCC-CCCCCCCCCCCC'),
            });
            query.setState({
                ...query.state,
                status: 'error',
                error: new ApiFailure(404, { code: 'not-found', message: 'No story.' }),
                fetchStatus: 'idle',
            });
        });
        expect(html).toContain(
            'No story, task or scenario has the id CCCCCCCC-CCCC-4CCC-8CCC-CCCCCCCCCCCC.',
        );
    });
});
