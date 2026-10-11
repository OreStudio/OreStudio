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
import type { ScenarioSummary } from '@ores/contracts';
import { TranslationProvider } from '../i18n/Provider.js';
import { MENU } from '../shell/areas.js';
import { DevelopmentArea } from './DevelopmentArea.js';
import { summary } from './testSummary.js';
import { QA_SCENARIOS_KEY } from './TestsQueue.js';

function screen(options: {
    readonly scenarios?: readonly ScenarioSummary[];
    readonly failure?: string;
}): string {
    const client = new QueryClient({
        defaultOptions: { queries: { retry: false, retryOnMount: false } },
    });
    if (options.failure !== undefined) {
        const query = client.getQueryCache().build(client, { queryKey: QA_SCENARIOS_KEY });
        query.setState({
            ...query.state,
            status: 'error',
            error: new Error(options.failure),
            fetchStatus: 'idle',
        });
    } else if (options.scenarios !== undefined) {
        client.setQueryData(QA_SCENARIOS_KEY, options.scenarios);
    }
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter
                    initialEntries={[
                        options.tab === undefined || options.tab === 'tests'
                            ? '/'
                            : `/?tab=${options.tab}`,
                    ]}
                >
                    <DevelopmentArea />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

const WAITING = summary({
    id: '22222222-2222-4222-8222-222222222222',
    title: 'Retake the currency screenshots',
    story: { id: 'S', title: 'Refresh the manual' },
    task: { id: 'T', title: 'Retake the shots' },
    clients: ['blue', 'red'],
    steps: { total: 4, pending: 2, pass: 2, fail: 0, dropped: 0 },
    sprint: 'sprint_27',
    created: '2026-10-11',
});
const IDLE = summary({
    id: '33333333-3333-4333-8333-333333333333',
    title: 'Open the books tree',
    sprint: 'sprint_27',
    created: '2026-10-05',
});
const OLDER = summary({
    id: '66666666-6666-4666-8666-666666666666',
    title: 'Check the party list',
    sprint: 'sprint_23',
    created: '2026-07-14',
});
const LOOSE = summary({
    id: '77777777-7777-4777-8777-777777777777',
    title: 'A scenario outside a sprint',
    sprint: null,
    created: '2026-01-01',
});
const PASSED = summary({
    id: '44444444-4444-4444-8444-444444444444',
    title: 'Create a counterparty',
    phase: 'done',
    state: 'PASSED',
    steps: { total: 3, pending: 0, pass: 3, fail: 0, dropped: 0 },
    completedAt: '2026-10-01T10:00:00Z',
    sprint: 'sprint_25',
    created: '2026-09-01',
});
const FAILED = summary({
    id: '55555555-5555-4555-8555-555555555555',
    title: 'Delete a counterparty',
    phase: 'done',
    state: 'FAILED',
    steps: { total: 3, pending: 0, pass: 2, fail: 1, dropped: 0 },
    completedAt: '2026-09-01T10:00:00Z',
    sprint: 'sprint_22',
    created: '2026-06-02',
});
const ANCIENT = summary({
    id: '88888888-8888-4888-8888-888888888888',
    title: 'An old finished scenario',
    phase: 'done',
    state: 'PASSED',
    steps: { total: 2, pending: 0, pass: 2, fail: 0, dropped: 0 },
    completedAt: '2026-05-01T10:00:00Z',
    sprint: 'sprint_22',
    created: '2026-05-01',
});
const GONE = summary({
    id: '99999999-9999-4999-8999-999999999999',
    title: 'A scenario for the Qt client',
    phase: 'abandoned',
    state: 'ABANDONED',
    steps: { total: 6, pending: 6, pass: 0, fail: 0, dropped: 0 },
    sprint: 'sprint_23',
    created: '2026-07-08',
});
const DONE27 = summary({
    id: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa',
    title: 'A scenario finished in sprint 27',
    phase: 'done',
    state: 'PASSED',
    steps: { total: 2, pending: 0, pass: 2, fail: 0, dropped: 0 },
    completedAt: '2026-10-10T10:00:00Z',
    sprint: 'sprint_27',
    created: '2026-10-01',
});
const ALL = [PASSED, IDLE, FAILED, WAITING, OLDER, LOOSE, ANCIENT, GONE, DONE27];

describe('the Development menu entry', () => {
    it('is in the menu for everybody, with no permission and no scope', () => {
        const entry = MENU.find((item) => item.to === '/development');
        expect(entry).toEqual({ nameKey: 'shell.menu.development', to: '/development' });
    });
});

describe('the Tests tab', () => {
    it('names the area and offers the Tests, Board and Backlog tabs, with none for the next tasks', () => {
        const html = screen({ scenarios: [] });
        expect(html).toContain('role="tablist"');
        expect(html).toContain('>Tests</button>');
        expect(html).toContain('>Board</button>');
        expect(html).toContain('>Backlog</button>');
        for (const other of ['Done', 'Abandoned', 'Charts', 'Stories', 'Tasks']) {
            expect(html).not.toContain(`>${other}</button>`);
        }
        expect(html).toContain('Development');
    });

    it('says there are no scenarios for an empty queue', () => {
        const html = screen({ scenarios: [] });
        expect(html).toContain('There are no test scenarios.');
        expect(html).not.toContain('<details');
    });

    it('groups by sprint, the newest sprint first, and what is outside a sprint last', () => {
        const html = screen({ scenarios: ALL });
        const at = (text: string) => html.indexOf(text);
        expect(at('Sprint 27')).toBeGreaterThan(-1);
        expect(at('Sprint 27')).toBeLessThan(at('Sprint 25'));
        expect(at('Sprint 25')).toBeLessThan(at('Sprint 23'));
        expect(at('Sprint 23')).toBeLessThan(at('Sprint 22'));
        expect(at('Sprint 22')).toBeLessThan(at('No sprint'));
    });

    it('counts what is active, done and abandoned in the heading of a sprint', () => {
        const html = screen({ scenarios: ALL });
        expect(html).toContain('2 active');
        expect(html).toContain('1 done');
        expect(html).toContain('1 abandoned');
        expect(html).toMatch(/Sprint 27[\s\S]*?2 active[\s\S]*?1 done/);
    });

    it('puts the active scenarios of a sprint at the top and the done ones below them', () => {
        const html = screen({ scenarios: ALL });
        const at = (text: string) => html.indexOf(text);
        expect(at('Retake the currency screenshots')).toBeGreaterThan(-1);
        expect(at('Retake the currency screenshots')).toBeLessThan(at('Open the books tree'));
        expect(at('Open the books tree')).toBeLessThan(at('A scenario finished in sprint 27'));
        expect(at('A scenario finished in sprint 27')).toBeLessThan(at('Sprint 25'));
    });

    it('labels the buckets, and shows the completion time only for the done one', () => {
        const html = screen({ scenarios: ALL });
        expect(html).toMatch(/>Active<span/);
        expect(html).toMatch(/>Done<span/);
        expect(html.match(/>Completed<\/th>/g)?.length).toBe(3);
        expect(html).toContain('2026-10-10T10:00:00Z');
        expect(html).toContain('2026-10-01T10:00:00Z');
    });

    it('opens a sprint that has something active and the newest sprint, and closes the rest', () => {
        const html = screen({ scenarios: ALL });
        const sprints = html.match(/<details [^>]*class="group[^>]*>/g) ?? [];
        expect(sprints).toHaveLength(5);
        const open = sprints.filter((tag) => tag.includes('open=""'));
        expect(open).toHaveLength(3);
    });

    it('opens the newest sprint even when nothing in it is active', () => {
        const html = screen({ scenarios: [PASSED, FAILED] });
        const sprints = html.match(/<details [^>]*class="group[^>]*>/g) ?? [];
        expect(sprints.map((tag) => tag.includes('open=""'))).toEqual([true, false]);
    });

    it('keeps the abandoned scenarios in a bucket of their own that is closed', () => {
        const html = screen({ scenarios: ALL });
        expect(html).toContain('A scenario for the Qt client');
        const nested = html.match(/<details [^>]*class="border-t[^>]*>/g) ?? [];
        expect(nested).toHaveLength(1);
        expect(nested[0]).not.toContain('open=""');
        expect(html.indexOf('A scenario for the Qt client')).toBeGreaterThan(
            html.indexOf('Sprint 23'),
        );
    });

    it('sorts a bucket by created date, newest first, and shows the date', () => {
        const html = screen({ scenarios: ALL });
        expect(html.indexOf('Retake the currency screenshots')).toBeLessThan(
            html.indexOf('Open the books tree'),
        );
        expect(html).toContain('>Created</th>');
        expect(html).toContain('2026-10-11');
        expect(html).toContain('2026-10-05');
    });

    it('shows the story, task, target, clients, progress and state of a row', () => {
        const html = screen({ scenarios: [WAITING] });
        expect(html).toContain('Refresh the manual · Retake the shots');
        expect(html).toContain('CurrencyDetailDialog');
        expect(html).toContain('>blue<');
        expect(html).toContain('>red<');
        expect(html).toContain('2 of 4 steps');
        expect(html).toContain('aria-valuenow="50"');
        expect(html).toContain('In progress');
    });

    it('shows the state of a done and an abandoned scenario', () => {
        const html = screen({ scenarios: ALL });
        expect(html).toContain('Passed');
        expect(html).toContain('Failed');
        expect(html).toContain('>Abandoned</span>');
    });

    it('opens the runner of the scenario from its row', () => {
        const html = screen({ scenarios: [WAITING] });
        expect(html).toContain('href="/development/tests/22222222-2222-4222-8222-222222222222"');
    });

    it('has no heading of its own', () => {
        expect(screen({ scenarios: [WAITING] })).not.toMatch(/<h1/);
    });

    it('shows the reason when the load fails', () => {
        const html = screen({ failure: 'The QA validation runner is off.' });
        expect(html).toContain('role="alert"');
        expect(html).toContain('The QA validation runner is off.');
        expect(html).not.toContain('There are no test scenarios.');
    });

    it('says it is loading before the first answer', () => {
        expect(screen({})).toContain('Loading...');
    });
});
