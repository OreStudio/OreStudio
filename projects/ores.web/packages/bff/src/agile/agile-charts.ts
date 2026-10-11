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

import { findSection, parseOrg } from '@ores/org';
import { parseSprint, parseTask, type AgileState, type StoryFiles } from './agile-model.js';

/**
 * The series of the sprint charts. They replace the panels of ores.org-js:
 * stories done in each sprint, tasks done by day with their running total
 * against the scope, and tasks by state. Nothing here touches the disk.
 *
 * The end dates of the tasks live only in the \`* Tasks\` table of a story, so
 * the burn-up and the day bars read that table. The scope and the states count
 * the task docs. The two can differ when a row has no doc or a doc has no row.
 */

export interface VelocityPoint {
    /** The sprint folder, such as `sprint_27`. */
    readonly sprint: string;
    readonly done: number;
}

export interface BurnupPoint {
    readonly date: string;
    /** Tasks done on or before the date. */
    readonly done: number;
}

export interface DayPoint {
    readonly date: string;
    /** The day of the sprint, counted from one. */
    readonly day: number;
    readonly done: number;
    /** The day is after today, so no task is done on it yet. */
    readonly future: boolean;
}

export interface StateCount {
    readonly state: AgileState;
    readonly count: number;
}

export interface Charts {
    /** Stories done in each sprint, oldest first. */
    readonly velocity: readonly VelocityPoint[];
    /** The running total of tasks done, by the date each was finished. */
    readonly burnup: readonly BurnupPoint[];
    /** Every task of the sprint, which the burn-up is drawn against. */
    readonly scope: number;
    /** Tasks done on each day of the sprint. Empty when the sprint has no start date. */
    readonly perDay: readonly DayPoint[];
    readonly byState: readonly StateCount[];
}

const ISO_DATE = /^\d{4}-\d{2}-\d{2}$/;

/** A date of the form 2026-10-04 that is also a day on the calendar, so 2026-13-45 is not. */
function isDay(value: string): boolean {
    return ISO_DATE.test(value) && Number.isFinite(Date.parse(`${value}T00:00:00Z`));
}
const DAY_MS = 86_400_000;
/** A sprint longer than this draws no day bars, so a bad date cannot make a huge series. */
const MAX_DAYS = 31;

const STATE_ORDER: readonly AgileState[] = [
    'DISCOVERED',
    'BACKLOG',
    'STARTED',
    'BLOCKED',
    'DONE',
    'ABANDONED',
    'UNKNOWN',
];

/**
 * The end dates of the done tasks in a story's `* Tasks` table, whose columns
 * the compass template fixes: task, state, start, end, description.
 */
export function doneDates(storyText: string): string[] {
    const doc = parseOrg(storyText);
    const section = findSection(doc, 'Tasks');
    if (section === undefined) return [];
    const out: string[] = [];
    for (const table of section.tables) {
        for (const row of table.rows) {
            const state = (row[1] ?? '').trim().toUpperCase();
            const end = (row[3] ?? '').trim();
            if (state === 'DONE' && isDay(end)) out.push(end);
        }
    }
    return out;
}

function addDays(date: string, days: number): string {
    return new Date(Date.parse(`${date}T00:00:00Z`) + days * DAY_MS).toISOString().slice(0, 10);
}

export interface ChartsInput {
    readonly sprint: string;
    readonly stories: readonly StoryFiles[];
    readonly velocity: readonly VelocityPoint[];
    readonly today: string;
}

export function buildCharts(input: ChartsInput): Charts {
    const sprint = parseSprint(input.sprint);
    // A task finished before the sprint began is an old date in a promoted story.
    // It would stretch the burn-up axis, so the burn-up starts at the sprint.
    const ends = input.stories
        .flatMap((files) => doneDates(files.story))
        .filter((date) => !isDay(sprint.startDate) || date >= sprint.startDate)
        .sort();
    const byDate = new Map<string, number>();
    for (const end of ends) byDate.set(end, (byDate.get(end) ?? 0) + 1);
    let running = 0;
    const burnup = [...byDate.entries()].map(([date, n]) => ({ date, done: (running += n) }));

    const tasks = input.stories.flatMap((files) =>
        files.tasks.map((task) => parseTask(task.text, task.slug, '')),
    );
    const byState = STATE_ORDER.map((state) => ({
        state,
        count: tasks.filter((task) => task.state === state).length,
    })).filter((entry) => entry.count > 0);

    const startOk = isDay(sprint.startDate);
    const endOk = isDay(sprint.endDate);
    const length = startOk
        ? endOk
            ? Math.round(
                  (Date.parse(`${sprint.endDate}T00:00:00Z`) -
                      Date.parse(`${sprint.startDate}T00:00:00Z`)) /
                      DAY_MS,
              ) + 1
            : 7
        : 0;
    const days = Math.min(Math.max(length, 0), MAX_DAYS);
    const perDay = Array.from({ length: days }, (_, index) => {
        const date = addDays(sprint.startDate, index);
        return { date, day: index + 1, done: byDate.get(date) ?? 0, future: date > input.today };
    });

    return { velocity: input.velocity, burnup, scope: tasks.length, perDay, byState };
}
