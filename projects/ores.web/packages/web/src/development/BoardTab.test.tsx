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
import type { WorkTree } from '@ores/contracts';
import { TranslationProvider } from '../i18n/Provider.js';
import { board, story } from './boardFixture.js';
import { StoryDetail, TreeStrip } from './BoardTab.js';
import { DevelopmentArea } from './DevelopmentArea.js';
import { AGILE_SPRINTS_KEY, AGILE_TREES_KEY, boardKey } from './queries.js';

function screen(
    data: ReturnType<typeof board> | undefined,
    failure?: string,
    trees: readonly WorkTree[] = [],
): string {
    const client = new QueryClient({
        defaultOptions: { queries: { retry: false, retryOnMount: false } },
    });
    if (failure !== undefined) {
        const query = client.getQueryCache().build(client, { queryKey: boardKey(undefined) });
        query.setState({
            ...query.state,
            status: 'error',
            error: new Error(failure),
            fetchStatus: 'idle',
        });
    } else if (data !== undefined) {
        client.setQueryData(boardKey(undefined), data);
        client.setQueryData(AGILE_TREES_KEY, trees);
        client.setQueryData(AGILE_SPRINTS_KEY, [
            { version: 'v0', name: 'sprint_2', title: 'Sprint 2', path: 'p' },
            { version: 'v0', name: 'sprint_1', title: 'Sprint 1', path: 'p' },
        ]);
    }
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={['/?tab=board']}>
                    <DevelopmentArea />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the Board tab', () => {
    const started = story({
        id: 'S1',
        title: 'Draw the board',
        state: 'STARTED',
        epic: 'Web',
        environment: 'eager_maxwell',
        progress: { done: 1, total: 2, abandoned: 0 },
    });
    const done = story({ id: 'S2', title: 'Ship the store', state: 'DONE' });
    const markup = screen(board([started, done]));

    it('opens from its own tab', () => {
        expect(markup).toContain('role="tab" aria-selected="true"');
        expect(markup).toContain('Board');
    });

    it('draws the sprint line with the sprint list, the day and the count', () => {
        expect(markup).toContain('Sprint 1');
        expect(markup).toContain('<option value="sprint_2"');
        expect(markup).toContain('Day 3 of 14');
        expect(markup).toContain('2 stories');
        expect(screen(board([started]))).toContain('1 story');
    });

    it('draws a column for each state and a card in the column of its state', () => {
        for (const column of ['Backlog', 'Started', 'Blocked', 'Done', 'Abandoned']) {
            expect(markup).toContain(column);
        }
        expect(markup).toContain('Draw the board');
        expect(markup).toContain('Ship the store');
        expect(markup).toContain('2 tasks, 1 done');
        expect(markup).toContain('0 tasks, 0 done');
        expect(screen(board([story({ progress: { done: 1, total: 1, abandoned: 0 } })]))).toContain(
            '1 task, 1 done',
        );
    });

    it('draws the epic filters from the board', () => {
        expect(markup).toContain('>Billing<');
        expect(markup).toContain('>Web<');
    });

    it('draws the four tiles', () => {
        for (const label of ['Stories', 'In flight', 'Blocked', 'Tasks done']) {
            expect(markup).toContain(label);
        }
        expect(markup).toContain('1/2');
    });

    it('draws no detail until a card is opened', () => {
        expect(markup).not.toContain('Story detail');
    });

    it('says what failed', () => {
        expect(screen(undefined, 'No sprint has this name.')).toContain('No sprint has this name.');
    });

    it('keeps the sprint picker and names each column when the board fails', () => {
        const failed = screen(undefined, 'No sprint has this name.');
        expect(failed).toContain('aria-label="Sprint"');
        expect(markup).toContain('aria-label="Started"');
        expect(markup).not.toContain('aria-label="STARTED"');
    });

    it('shows loading before the board arrives', () => {
        expect(screen(undefined)).toContain('Loading');
    });
});

describe('the story detail', () => {
    const detail = renderToStaticMarkup(
        <TranslationProvider>
            <MemoryRouter>
                <StoryDetail
                    onClose={() => undefined}
                    story={story({
                        id: 'S1',
                        title: 'Draw the board',
                        description: 'Columns of stories.',
                        branches: ['feature/draw'],
                        prs: [3130],
                        tasks: [
                            { id: 'T1', title: 'Build the cards', state: 'DONE', pr: '3130' },
                            { id: 'T2', title: 'Build the detail', state: 'BLOCKED', pr: 'later' },
                            { id: 'T3', title: 'Odd pr', state: 'DONE', pr: '1e3' },
                        ],
                    })}
                />
            </MemoryRouter>
        </TranslationProvider>,
    );

    it('shows the fields, the description and a link to the story doc', () => {
        expect(detail).toContain('Story detail');
        expect(detail).toContain('feature/draw');
        expect(detail).toContain('Columns of stories.');
        expect(detail).toContain('href="/development/docs/S1"');
    });

    it('links a pull request by number and ignores a pr that is prose', () => {
        expect(detail).toContain('href="https://github.com/OreStudio/OreStudio/pull/3130"');
        expect(detail).not.toContain('pull/NaN');
        expect(detail).not.toContain('pull/1000');
    });

    it('lists each task with its state', () => {
        expect(detail).toContain('Build the cards');
        expect(detail).toContain('Build the detail');
        expect(detail).toContain('Blocked');
    });
});

describe('the work tree strip and the current story', () => {
    const entry = (taskTitle: string): WorkTree['current'] => ({
        storyId: 'S1',
        storyTitle: 'Draw the board',
        taskId: 'T1',
        taskTitle,
        state: 'STARTED',
        branch: 'feature/x',
        pr: 0,
    });
    const trees: WorkTree[] = [
        {
            label: 'eager_maxwell',
            branch: 'feature/x',
            isSession: true,
            current: entry('Show the work trees on the board'),
        },
        { label: 'solid_dirac', branch: 'feature/y', isSession: false, current: null },
    ];

    it('draws a chip for each tree, with its task or Idle, and marks this tree', () => {
        const html = renderToStaticMarkup(
            <TranslationProvider>
                <TreeStrip trees={trees} />
            </TranslationProvider>,
        );
        expect(html).toContain('eager_maxwell');
        expect(html).toContain('Show the work trees on the');
        expect(html).toContain('solid_dirac');
        expect(html).toContain('Idle');
        expect(html.match(/This tree/g)).toHaveLength(1);
    });

    it('draws the strip on the board and marks the card of the story this tree is on', () => {
        const html = screen(
            board([
                story({ id: 'S1', title: 'Draw the board' }),
                story({ id: 'S2', title: 'Other' }),
            ]),
            undefined,
            trees,
        );
        expect(html).toContain('aria-label="Work trees"');
        expect(html.match(/>Current</g)).toHaveLength(1);
    });

    it('draws no strip when there are no trees', () => {
        expect(screen(board([story({})]))).not.toContain('aria-label="Work trees"');
    });
});
