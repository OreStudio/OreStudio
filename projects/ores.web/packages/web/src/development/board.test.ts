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
import type { AgileStory, Board, WorkTree } from '@ores/contracts';
import {
    columnOf,
    columnsOf,
    currentStoryId,
    percentOfTasks,
    shortTitle,
    shownDay,
    visibleStories,
} from './board.js';
import { board as boardOf, story } from './boardFixture.js';

describe('columnOf', () => {
    it('puts a discovered story with the backlog and leaves the others', () => {
        expect(columnOf('DISCOVERED')).toBe('BACKLOG');
        expect(columnOf('STARTED')).toBe('STARTED');
        expect(columnOf('UNKNOWN')).toBe('UNKNOWN');
    });
});

describe('columnsOf', () => {
    const board = (stories: readonly AgileStory[]) => ({ stories }) as unknown as Board;

    it('always draws the five canonical columns', () => {
        expect(columnsOf(board([]))).toEqual([
            'BACKLOG',
            'STARTED',
            'BLOCKED',
            'DONE',
            'ABANDONED',
        ]);
    });

    it('adds Unknown only when a story names no known state', () => {
        expect(columnsOf(board([story({ state: 'UNKNOWN' })])).at(-1)).toBe('UNKNOWN');
    });
});

describe('visibleStories', () => {
    const stories = [
        story({ id: 'A', title: 'Pay the invoice', epic: 'Billing', environment: 'blue' }),
        story({
            id: 'B',
            title: 'Draw the board',
            epic: 'Web',
            tasks: [{ title: 'Fix the cache', branch: 'feature/cache' }],
        }),
    ];

    it('keeps everything for an empty filter', () => {
        expect(visibleStories(stories, { text: '  ', epic: null })).toHaveLength(2);
    });

    it('matches the text in the title, the environment and a task, ignoring case', () => {
        expect(visibleStories(stories, { text: 'INVOICE', epic: null }).map((s) => s.id)).toEqual([
            'A',
        ]);
        expect(visibleStories(stories, { text: 'blue', epic: null }).map((s) => s.id)).toEqual([
            'A',
        ]);
        expect(
            visibleStories(stories, { text: 'feature/cache', epic: null }).map((s) => s.id),
        ).toEqual(['B']);
    });

    it('narrows by epic, and with the text together', () => {
        expect(visibleStories(stories, { text: '', epic: 'Web' }).map((s) => s.id)).toEqual(['B']);
        expect(visibleStories(stories, { text: 'invoice', epic: 'Web' })).toEqual([]);
    });
});

describe('shownDay', () => {
    it('clamps the day to the sprint', () => {
        expect(shownDay(-3, 14)).toBe(1);
        expect(shownDay(0, 14)).toBe(1);
        expect(shownDay(7, 14)).toBe(7);
        expect(shownDay(20, 14)).toBe(14);
    });

    it('shows nothing when there is no day, and does not cap without a total', () => {
        expect(shownDay(null, 14)).toBeNull();
        expect(shownDay(20, null)).toBe(20);
    });
});

describe('percentOfTasks', () => {
    it('rounds the share done and is zero without tasks', () => {
        expect(percentOfTasks(story({ progress: { done: 1, total: 3, abandoned: 0 } }))).toBe(33);
        expect(percentOfTasks(story({ progress: { done: 0, total: 0, abandoned: 0 } }))).toBe(0);
    });
});

describe('currentStoryId', () => {
    const tree = (over: Partial<WorkTree> = {}): WorkTree => ({
        label: 'eager_maxwell',
        branch: 'feature/x',
        isSession: true,
        current: null,
        ...over,
    });
    const journal = (storyId: string): WorkTree['current'] => ({
        storyId,
        storyTitle: '',
        taskId: '',
        taskTitle: '',
        state: 'STARTED',
        branch: '',
        pr: 0,
    });
    const stories = [
        story({ id: 'A', updated: '2026-10-01', tasks: [{ branch: 'feature/x' }] }),
        story({ id: 'B', updated: '2026-10-05', tasks: [{ branch: 'feature/x' }] }),
        story({ id: 'C', tasks: [{ branch: 'feature/other' }] }),
    ];
    const board = boardOf(stories);

    it('takes the journal story when the board has it', () => {
        expect(currentStoryId(board, [tree({ current: journal('C') })])).toBe('C');
    });

    it('falls back to the story with a task on the branch, the latest update first', () => {
        expect(currentStoryId(board, [tree()])).toBe('B');
        expect(currentStoryId(board, [tree({ current: journal('NOT-HERE') })])).toBe('B');
    });

    it('is null for a tree with no branch, a branch no task has, or no session tree', () => {
        expect(currentStoryId(board, [tree({ branch: '' })])).toBeNull();
        expect(currentStoryId(board, [tree({ branch: 'feature/none' })])).toBeNull();
        expect(currentStoryId(board, [tree({ isSession: false })])).toBeNull();
        expect(currentStoryId(board, [])).toBeNull();
    });
});

describe('shortTitle', () => {
    it('keeps a short title and cuts a long one with an ellipsis inside the limit', () => {
        expect(shortTitle('Short')).toBe('Short');
        const cut = shortTitle('A very long task title that does not fit', 10);
        expect(cut).toHaveLength(10);
        expect(cut.endsWith('…')).toBe(true);
    });
});
