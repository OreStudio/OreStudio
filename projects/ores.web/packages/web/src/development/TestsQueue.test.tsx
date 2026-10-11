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
    readonly tab?: 'tests' | 'done' | 'abandoned';
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
const ALL = [PASSED, IDLE, FAILED, WAITING, OLDER, LOOSE, ANCIENT, GONE];

describe('the Development menu entry', () => {
    it('is in the menu for everybody, with no permission and no scope', () => {
        const entry = MENU.find((item) => item.to === '/development');
        expect(entry).toEqual({ nameKey: 'shell.menu.development', to: '/development' });
    });
});

describe('the Tests tab', () => {
    it('names the area, offers Tests, Done and Abandoned, and draws no tab for the next story', () => {
        const html = screen({ scenarios: [] });
        expect(html).toContain('role="tablist"');
        expect(html).toContain('>Tests</button>');
        expect(html).toContain('>Done</button>');
        expect(html).toContain('>Abandoned</button>');
        expect(html).toContain('Development');
        for (const later of ['Board', 'Stories', 'Tasks']) {
            expect(html).not.toContain(`>${later}</button>`);
        }
    });

    it('says nothing is waiting for an empty queue', () => {
        const html = screen({ scenarios: [] });
        expect(html).toContain('Nothing is waiting.');
        expect(html).not.toContain('<details');
    });

    it('shows only what waits, and none of what is done', () => {
        const html = screen({ scenarios: ALL });
        for (const title of [
            'Retake the currency screenshots',
            'Open the books tree',
            'Check the party list',
            'A scenario outside a sprint',
        ]) {
            expect(html).toContain(title);
        }
        for (const title of [
            'Create a counterparty',
            'Delete a counterparty',
            'An old finished scenario',
        ]) {
            expect(html).not.toContain(title);
        }
    });

    it('groups what waits by sprint, the newest sprint first, and what is outside a sprint last', () => {
        const html = screen({ scenarios: ALL });
        const at = (text: string) => html.indexOf(text);
        expect(at('Sprint 27')).toBeGreaterThan(-1);
        expect(at('Sprint 27')).toBeLessThan(at('Sprint 23'));
        expect(at('Sprint 23')).toBeLessThan(at('No sprint'));
        expect(at('Retake the currency screenshots')).toBeLessThan(at('Sprint 23'));
        expect(at('Check the party list')).toBeGreaterThan(at('Sprint 23'));
        expect(at('A scenario outside a sprint')).toBeGreaterThan(at('No sprint'));
    });

    it('counts the scenarios in the heading of a sprint', () => {
        const html = screen({ scenarios: ALL });
        expect(html).toMatch(/Sprint 27<span[^>]*>2<\/span>/);
        expect(html).toMatch(/Sprint 23<span[^>]*>1<\/span>/);
    });

    it('opens every sprint that has something waiting', () => {
        const html = screen({ scenarios: ALL });
        expect(html.match(/<details[^>]*open=""/g)?.length).toBe(3);
        expect(html.match(/<details/g)?.length).toBe(3);
    });

    it('sorts a sprint by created date, newest first, and shows the date', () => {
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

    it('draws no column for the completion time, which a waiting scenario has none of', () => {
        expect(screen({ scenarios: [WAITING] })).not.toContain('>Completed</th>');
    });

    it('opens the runner of the scenario from its row', () => {
        const html = screen({ scenarios: [WAITING] });
        expect(html).toContain('href="/development/tests/22222222-2222-4222-8222-222222222222"');
    });

    it('has no heading of its own', () => {
        const html = screen({ scenarios: [WAITING] });
        expect(html).not.toMatch(/<h1/);
    });

    it('shows the reason when the load fails', () => {
        const html = screen({ failure: 'The QA validation runner is off.' });
        expect(html).toContain('role="alert"');
        expect(html).toContain('The QA validation runner is off.');
        expect(html).not.toContain('Nothing is waiting.');
    });

    it('says it is loading before the first answer', () => {
        expect(screen({})).toContain('Loading...');
    });
});

describe('the Done tab', () => {
    it('shows only what is finished, and none of what waits', () => {
        const html = screen({ scenarios: ALL, tab: 'done' });
        for (const title of [
            'Create a counterparty',
            'Delete a counterparty',
            'An old finished scenario',
        ]) {
            expect(html).toContain(title);
        }
        expect(html).not.toContain('Retake the currency screenshots');
        expect(html).not.toContain('Check the party list');
    });

    it('groups by sprint, newest first, and opens only the newest sprint', () => {
        const html = screen({ scenarios: ALL, tab: 'done' });
        expect(html.indexOf('Sprint 25')).toBeLessThan(html.indexOf('Sprint 22'));
        expect(html.match(/<details/g)?.length).toBe(2);
        expect(html.match(/<details[^>]*open=""/g)?.length).toBe(1);
    });

    it('shows the completion time as well as the created date, and the state', () => {
        const html = screen({ scenarios: ALL, tab: 'done' });
        expect(html).toContain('>Completed</th>');
        expect(html).toContain('2026-10-01T10:00:00Z');
        expect(html).toContain('2026-09-01');
        expect(html).toContain('Passed');
        expect(html).toContain('Failed');
    });

    it('sorts a sprint by created date, newest first', () => {
        const html = screen({ scenarios: ALL, tab: 'done' });
        expect(html.indexOf('Delete a counterparty')).toBeLessThan(
            html.indexOf('An old finished scenario'),
        );
    });

    it('says nothing has been tested yet when nothing is done', () => {
        const html = screen({ scenarios: [WAITING], tab: 'done' });
        expect(html).toContain('Nothing has been tested yet.');
        expect(html).not.toContain('<details');
    });
});

describe('the Abandoned tab', () => {
    it('shows only what has been abandoned', () => {
        const html = screen({ scenarios: ALL, tab: 'abandoned' });
        expect(html).toContain('A scenario for the Qt client');
        for (const title of ['Retake the currency screenshots', 'Create a counterparty']) {
            expect(html).not.toContain(title);
        }
    });

    it('groups by sprint, marks the state, and draws no completion column', () => {
        const html = screen({ scenarios: ALL, tab: 'abandoned' });
        expect(html).toContain('Sprint 23');
        expect(html).toContain('>Abandoned</span>');
        expect(html).not.toContain('>Completed</th>');
    });

    it('says nothing has been abandoned when nothing has', () => {
        const html = screen({ scenarios: [WAITING], tab: 'abandoned' });
        expect(html).toContain('Nothing has been abandoned.');
        expect(html).not.toContain('<details');
    });
});
