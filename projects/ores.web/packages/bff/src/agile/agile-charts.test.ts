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
import { buildCharts, doneDates } from './agile-charts.js';
import type { StoryFiles } from './agile-model.js';

function story(rows: string[], tasks: { slug: string; state: string }[] = []): StoryFiles {
    return {
        slug: 's',
        dirPath: 'agile/versions/v0/sprint_1/s/',
        story: `#+title: Story: S\n\n* Tasks\n\n| Task | State | Start | End | Description |\n|---+---+---+---+---|\n${rows.join('\n')}\n`,
        tasks: tasks.map((task) => ({
            slug: task.slug,
            text: `#+title: Task: ${task.slug}\n\n* Status\n\n| Field | Value |\n|---+---|\n| State | ${task.state} |\n`,
        })),
    };
}

const SPRINT = `#+title: Sprint 1\n#+start_date: 2026-10-01\n#+end_date: 2026-10-07\n`;

describe('doneDates', () => {
    it('keeps the end dates of the done tasks that have a date', () => {
        const text = story([
            '| A | DONE | 2026-10-01 | 2026-10-02 | x |',
            '| B | STARTED | 2026-10-01 | 2026-10-03 | x |',
            '| C | done | 2026-10-01 | 2026-10-04 | x |',
            '| D | DONE | 2026-10-01 |  | x |',
            '| E | DONE | 2026-10-01 | soon | x |',
        ]).story;
        expect(doneDates(text)).toEqual(['2026-10-02', '2026-10-04']);
    });

    it('returns nothing for a story with no Tasks table', () => {
        expect(doneDates('#+title: Story: S\n')).toEqual([]);
    });
});

describe('a bad date', () => {
    it('is skipped in the done dates', () => {
        const text = story(['| A | DONE | 2026-10-01 | 2026-13-45 | x |']).story;
        expect(doneDates(text)).toEqual([]);
    });

    it('does not break the series when the sprint dates are not days', () => {
        const charts = buildCharts({
            sprint: '#+title: S\n#+start_date: 2026-13-45\n#+end_date: 2026-10-07\n',
            stories: [story(['| A | DONE | 2026-10-01 | 2026-10-02 | x |'])],
            velocity: [],
            today: '2026-10-03',
        });
        expect(charts.perDay).toEqual([]);
        expect(charts.burnup).toEqual([{ date: '2026-10-02', done: 1 }]);
    });
});

describe('buildCharts', () => {
    const stories = [
        story(
            [
                '| A | DONE | 2026-10-01 | 2026-10-02 | x |',
                '| B | DONE | 2026-10-01 | 2026-10-02 | x |',
                '| C | DONE | 2026-10-01 | 2026-10-04 | x |',
            ],
            [
                { slug: 'a', state: 'DONE' },
                { slug: 'b', state: 'DONE' },
                { slug: 'c', state: 'DONE' },
                { slug: 'd', state: 'STARTED' },
            ],
        ),
    ];
    const charts = buildCharts({
        sprint: SPRINT,
        stories,
        velocity: [{ sprint: 'sprint_1', done: 1 }],
        today: '2026-10-03',
    });

    it('adds the done tasks up by the day they ended', () => {
        expect(charts.burnup).toEqual([
            { date: '2026-10-02', done: 2 },
            { date: '2026-10-04', done: 3 },
        ]);
    });

    it('counts every task of the sprint as the scope', () => {
        expect(charts.scope).toBe(4);
    });

    it('gives a day for each day of the sprint, with the days after today marked', () => {
        expect(charts.perDay).toHaveLength(7);
        expect(charts.perDay[0]).toEqual({ date: '2026-10-01', day: 1, done: 0, future: false });
        expect(charts.perDay[1]?.done).toBe(2);
        expect(charts.perDay[2]?.future).toBe(false);
        expect(charts.perDay[3]).toMatchObject({ date: '2026-10-04', done: 1, future: true });
    });

    it('counts the tasks by state in vocabulary order, leaving out empty states', () => {
        expect(charts.byState).toEqual([
            { state: 'STARTED', count: 1 },
            { state: 'DONE', count: 3 },
        ]);
    });

    it('leaves out a task finished before the sprint began', () => {
        const old = buildCharts({
            sprint: SPRINT,
            stories: [
                story([
                    '| A | DONE | 2026-07-01 | 2026-07-02 | x |',
                    '| B | DONE | 2026-10-01 | 2026-10-02 | x |',
                ]),
            ],
            velocity: [],
            today: '2026-10-03',
        });
        expect(old.burnup).toEqual([{ date: '2026-10-02', done: 1 }]);
    });

    it('passes the velocity through', () => {
        expect(charts.velocity).toEqual([{ sprint: 'sprint_1', done: 1 }]);
    });

    it('draws no days when the sprint has no start date, and caps a very long sprint', () => {
        const none = buildCharts({
            sprint: '#+title: S\n',
            stories,
            velocity: [],
            today: '2026-10-03',
        });
        expect(none.perDay).toEqual([]);
        const long = buildCharts({
            sprint: '#+title: S\n#+start_date: 2026-01-01\n#+end_date: 2026-12-31\n',
            stories,
            velocity: [],
            today: '2026-10-03',
        });
        expect(long.perDay).toHaveLength(31);
    });
});
