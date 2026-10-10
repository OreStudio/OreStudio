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
                <MemoryRouter>
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
});
const IDLE = summary({ id: '33333333-3333-4333-8333-333333333333', title: 'Open the books tree' });
const PASSED = summary({
    id: '44444444-4444-4444-8444-444444444444',
    title: 'Create a counterparty',
    phase: 'done',
    state: 'PASSED',
    steps: { total: 3, pending: 0, pass: 3, fail: 0, dropped: 0 },
    completedAt: '2026-10-01T10:00:00Z',
});
const FAILED = summary({
    id: '55555555-5555-4555-8555-555555555555',
    title: 'Delete a counterparty',
    phase: 'done',
    state: 'FAILED',
    steps: { total: 3, pending: 0, pass: 2, fail: 1, dropped: 0 },
    completedAt: '2026-09-01T10:00:00Z',
});

describe('the Development menu entry', () => {
    it('is in the menu for everybody, with no permission and no scope', () => {
        const entry = MENU.find((item) => item.to === '/development');
        expect(entry).toEqual({ nameKey: 'shell.menu.development', to: '/development' });
    });
});

describe('the Tests tab', () => {
    it('names the area and the tab, and draws no tab for the next story', () => {
        const html = screen({ scenarios: [] });
        expect(html).toContain('role="tablist"');
        expect(html).toContain('>Tests</button>');
        expect(html).toContain('Development');
        for (const later of ['Board', 'Stories', 'Tasks']) {
            expect(html).not.toContain(`>${later}</button>`);
        }
    });

    it('says nothing is waiting and nothing is done for an empty queue', () => {
        const html = screen({ scenarios: [] });
        expect(html).toContain('Nothing is waiting.');
        expect(html).toContain('Nothing has been tested yet.');
        expect(html).not.toContain('<tr class="cursor-pointer');
    });

    it('lists waiting scenarios first and done ones second', () => {
        const html = screen({ scenarios: [PASSED, IDLE, FAILED, WAITING] });
        const at = (text: string) => html.indexOf(text);
        expect(at('Retake the currency screenshots')).toBeGreaterThan(-1);
        expect(at('Retake the currency screenshots')).toBeLessThan(at('Open the books tree'));
        expect(at('Open the books tree')).toBeLessThan(at('Done'));
        expect(at('Done')).toBeLessThan(at('Create a counterparty'));
        expect(at('Create a counterparty')).toBeLessThan(at('Delete a counterparty'));
    });

    it('shows the story, task, target, clients, progress and state of a row', () => {
        const html = screen({ scenarios: [WAITING, PASSED, FAILED] });
        expect(html).toContain('Refresh the manual · Retake the shots');
        expect(html).toContain('CurrencyDetailDialog');
        expect(html).toContain('>blue<');
        expect(html).toContain('>red<');
        expect(html).toContain('2 of 4 steps');
        expect(html).toContain('aria-valuenow="50"');
        expect(html).toContain('In progress');
        expect(html).toContain('Passed');
        expect(html).toContain('Failed');
        expect(html).toContain('2026-10-01T10:00:00Z');
    });

    it('opens the runner of the scenario from its row', () => {
        const html = screen({ scenarios: [WAITING] });
        expect(html).toContain('href="/development/tests/22222222-2222-4222-8222-222222222222"');
    });

    it('has no heading of its own', () => {
        const html = screen({ scenarios: [WAITING] });
        expect(html).not.toMatch(/<h1/);
        expect(html).not.toContain('Tests</h2>');
    });

    it('shows the reason when the load fails', () => {
        const html = screen({ failure: 'The QA validation runner is off.' });
        expect(html).toContain('role="alert"');
        expect(html).toContain('The QA validation runner is off.');
        expect(html).not.toContain('Nothing is waiting.');
    });

    it('says it is loading before the first answer', () => {
        const html = screen({});
        expect(html).toContain('Loading...');
    });
});
