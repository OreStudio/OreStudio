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
import {
    buildBoard,
    normalizeState,
    parsePrNumber,
    parseSprint,
    parseStory,
    parseTask,
} from './agile-model.js';

function doc(
    id: string,
    keywords: Record<string, string>,
    state: string | null,
    extra = '',
): string {
    const lines = [':PROPERTIES:', `:ID: ${id}`, ':END:'];
    for (const [key, value] of Object.entries(keywords)) lines.push(`#+${key}: ${value}`);
    lines.push('', '* Status', '');
    if (state !== null)
        lines.push('| Field | Value |', '|-------+-------|', `| State | ${state} |`);
    lines.push('', extra);
    return lines.join('\n');
}

const SPRINT = `:PROPERTIES:
:ID: 11111111-1111-4111-8111-111111111111
:END:
#+title: Sprint 27
#+start_date: 2026-10-05
#+end_date: 2026-10-12

* Status

| Field | Value |
|-------+-------|
| State | STARTED |

* Stories

** Product

| Story | State |
|-------+-------|
| [[id:aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa][Alpha]] | STARTED |

** Hotfixes

| Story | State |
|-------+-------|
| [[id:bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb][Beta]] | DONE |
`;

describe('the state vocabulary', () => {
    it('reads a state in any case and folds anything else into UNKNOWN', () => {
        expect(normalizeState(' started ')).toBe('STARTED');
        expect(normalizeState('Discovered')).toBe('DISCOVERED');
        expect(normalizeState('WAITING')).toBe('UNKNOWN');
        expect(normalizeState(undefined)).toBe('UNKNOWN');
    });
});

describe('a pull request number', () => {
    it('is a number with an optional hash, and prose is no number', () => {
        expect(parsePrNumber('3097')).toBe(3097);
        expect(parsePrNumber('#3097')).toBe(3097);
        expect(parsePrNumber('see 3097')).toBeNull();
        expect(parsePrNumber('')).toBeNull();
    });
});

describe('a task doc', () => {
    it('reads the keywords, the state and the scaffold mark', () => {
        const task = parseTask(
            doc(
                'cccccccc-cccc-4ccc-8ccc-cccccccccccc',
                {
                    title: 'Task: Scaffold story: Alpha',
                    environment: 'brave_hopper brave_hopper',
                    branch: 'feature/alpha',
                    pr: '12',
                    blocked_on: '  the  trade set ',
                    created: '2026-10-09',
                },
                'blocked',
            ),
            'scaffold_story_alpha',
            'agile/versions/v0/sprint_27/alpha/task_scaffold_story_alpha.org',
        );
        expect(task).toMatchObject({
            id: 'CCCCCCCC-CCCC-4CCC-8CCC-CCCCCCCCCCCC',
            title: 'Scaffold story: Alpha',
            state: 'BLOCKED',
            environment: 'brave_hopper',
            branch: 'feature/alpha',
            pr: '12',
            blockedOn: 'the trade set',
            scaffold: true,
        });
    });

    it('has the state UNKNOWN when it has no Status table', () => {
        expect(parseTask(doc('d', { title: 'T' }, null), 't', 'p').state).toBe('UNKNOWN');
    });
});

describe('a story doc', () => {
    it('strips the Story prefix and reads the state from the Status table, not the todo line', () => {
        const story = parseStory(
            doc(
                'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa',
                { title: 'Story: Alpha', todo: 'BACKLOG | DONE' },
                'STARTED',
            ),
        );
        expect(story.title).toBe('Alpha');
        expect(story.state).toBe('STARTED');
    });
});

describe('a sprint doc', () => {
    it('reads the dates and takes the epic from the group heading that holds the row', () => {
        const sprint = parseSprint(SPRINT);
        expect(sprint.startDate).toBe('2026-10-05');
        expect(sprint.endDate).toBe('2026-10-12');
        expect(sprint.epics.get('AAAAAAAA-AAAA-4AAA-8AAA-AAAAAAAAAAAA')).toBe('Product');
        expect(sprint.epics.get('BBBBBBBB-BBBB-4BBB-8BBB-BBBBBBBBBBBB')).toBe('Hotfixes');
    });

    it('falls back to the Start and End rows of the Status table', () => {
        const sprint = parseSprint(
            [
                '#+title: S',
                '',
                '* Status',
                '',
                '| Field | Value |',
                '|-+-|',
                '| Start | 2026-01-01 |',
                '| End (expected) | 2026-01-08 |',
            ].join('\n'),
        );
        expect(sprint.startDate).toBe('2026-01-01');
        expect(sprint.endDate).toBe('2026-01-08');
    });
});

describe('a board', () => {
    const board = buildBoard({
        version: 'v0',
        name: 'sprint_27',
        path: 'agile/versions/v0/sprint_27/sprint.org',
        sprint: SPRINT,
        today: '2026-10-11',
        stories: [
            {
                slug: 'alpha',
                dirPath: 'agile/versions/v0/sprint_27/alpha/',
                story: doc(
                    'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa',
                    { title: 'Story: Alpha', environment: 'one' },
                    'STARTED',
                ),
                tasks: [
                    {
                        slug: 'b',
                        text: doc('c2', { title: 'Task: B', branch: 'feature/b', pr: '7' }, 'DONE'),
                    },
                    {
                        slug: 'a',
                        text: doc(
                            'c1',
                            {
                                title: 'Task: A',
                                branch: 'feature/a',
                                pr: 'prose',
                                environment: 'two',
                            },
                            'STARTED',
                        ),
                    },
                    { slug: 'x', text: doc('c3', { title: 'Task: X', pr: '3' }, 'ABANDONED') },
                ],
            },
            {
                slug: 'beta',
                dirPath: 'agile/versions/v0/sprint_27/beta/',
                story: doc(
                    'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb',
                    { title: 'Story: Beta' },
                    'DONE',
                ),
                tasks: [],
            },
            {
                slug: 'odd',
                dirPath: 'agile/versions/v0/sprint_27/odd/',
                story: doc(
                    'dddddddd-dddd-4ddd-8ddd-dddddddddddd',
                    { title: 'Story: Odd' },
                    'WAITING',
                ),
                tasks: [],
            },
        ],
    });

    it('counts the day of the sprint without an upper limit', () => {
        expect(board.sprint).toMatchObject({ dayOfSprint: 7, totalDays: 8, title: 'Sprint 27' });
        const late = buildBoard({
            version: 'v0',
            name: 'sprint_27',
            path: 'p',
            sprint: SPRINT,
            stories: [],
            today: '2026-10-20',
        });
        expect(late.sprint.dayOfSprint).toBe(16);
    });

    it('has the five columns in order, and UNKNOWN last only when a story is in it', () => {
        expect(board.columns.map((column) => column.id)).toEqual([
            'BACKLOG',
            'STARTED',
            'BLOCKED',
            'DONE',
            'ABANDONED',
            'UNKNOWN',
        ]);
        const empty = buildBoard({
            version: 'v0',
            name: 's',
            path: 'p',
            sprint: SPRINT,
            stories: [],
            today: '2026-10-11',
        });
        expect(empty.columns.map((column) => column.id)).toEqual([
            'BACKLOG',
            'STARTED',
            'BLOCKED',
            'DONE',
            'ABANDONED',
        ]);
        expect(empty.columns.every((column) => column.count === 0)).toBe(true);
    });

    it("orders stories by state, and a story's tasks by state, then title", () => {
        expect(board.stories.map((story) => story.title)).toEqual(['Alpha', 'Beta', 'Odd']);
        expect(board.stories[0]?.tasks.map((task) => task.title)).toEqual(['A', 'B', 'X']);
    });

    it('sums the progress, the branches and the pull requests of a story', () => {
        const alpha = board.stories[0];
        expect(alpha?.progress).toEqual({ done: 1, total: 3, abandoned: 1 });
        expect(alpha?.branches).toEqual(['feature/a', 'feature/b']);
        expect(alpha?.prs).toEqual([3, 7]);
        expect(alpha?.epic).toBe('Product');
        expect(alpha?.path).toBe('agile/versions/v0/sprint_27/alpha/story.org');
        expect(alpha?.tasks[0]?.path).toBe('agile/versions/v0/sprint_27/alpha/task_a.org');
    });

    it('counts the stories and tasks, and lists the filters', () => {
        expect(board.counts).toEqual({
            stories: 3,
            storiesDone: 1,
            storiesStarted: 1,
            storiesBlocked: 0,
            tasks: 3,
            tasksDone: 1,
        });
        expect(board.filters).toEqual({
            environments: ['one', 'two'],
            epics: ['Hotfixes', 'Product'],
        });
    });
});
