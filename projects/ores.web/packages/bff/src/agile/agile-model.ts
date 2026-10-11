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

import { fieldTable, findSection, idLink, parseOrg, sectionChildren, type OrgDoc } from '@ores/org';

/**
 * The agile tree as a model. It reads the org docs compass creates for a
 * sprint, its stories and their tasks. Nothing here touches the disk, so a
 * test hands in text. The state vocabulary is closed, and a doc that names
 * no known state is UNKNOWN.
 */

/** The closed state vocabulary, in board order. */
const STATES = [
    { id: 'DISCOVERED', title: 'Backlog', column: 'BACKLOG' },
    { id: 'BACKLOG', title: 'Backlog', column: 'BACKLOG' },
    { id: 'STARTED', title: 'Started', column: 'STARTED' },
    { id: 'BLOCKED', title: 'Blocked', column: 'BLOCKED' },
    { id: 'DONE', title: 'Done', column: 'DONE' },
    { id: 'ABANDONED', title: 'Abandoned', column: 'ABANDONED' },
] as const;

export type AgileState = (typeof STATES)[number]['id'] | 'UNKNOWN';

const STATE_ORDER = new Map<string, number>(STATES.map((state, index) => [state.id, index]));
const STATE_COLUMN = new Map<string, string>(STATES.map((state) => [state.id, state.column]));

export function normalizeState(raw: string | undefined): AgileState {
    const state = (raw ?? '').trim().toUpperCase();
    return STATE_ORDER.has(state) ? (state as AgileState) : 'UNKNOWN';
}

function stateRank(state: AgileState): number {
    return STATE_ORDER.get(state) ?? STATES.length;
}

export interface AgileTask {
    readonly id: string;
    readonly slug: string;
    readonly title: string;
    readonly state: AgileState;
    readonly description: string;
    readonly environment: string;
    readonly branch: string;
    /** The text of the `#+pr:` line, which may be empty or prose. */
    readonly pr: string;
    readonly blockedOn: string;
    readonly blockedSince: string;
    readonly created: string;
    readonly updated: string;
    readonly scaffold: boolean;
    readonly path: string;
}

export interface AgileStory {
    readonly id: string;
    readonly slug: string;
    readonly title: string;
    readonly state: AgileState;
    /** The `**` group of the sprint's `* Stories` that holds the story's row, or empty. */
    readonly epic: string;
    readonly description: string;
    readonly environment: string;
    readonly created: string;
    readonly updated: string;
    readonly path: string;
    readonly progress: {
        readonly done: number;
        readonly total: number;
        readonly abandoned: number;
    };
    readonly branches: readonly string[];
    readonly prs: readonly number[];
    readonly tasks: readonly AgileTask[];
}

export interface BoardColumn {
    readonly id: string;
    readonly title: string;
    readonly count: number;
}

export interface SprintInfo {
    readonly version: string;
    readonly name: string;
    readonly title: string;
    readonly startDate: string;
    readonly endDate: string;
    /** Today minus the start date, plus one, or null. It has no upper limit and is zero or negative before the sprint starts; a client clamps it. */
    readonly dayOfSprint: number | null;
    readonly totalDays: number | null;
    readonly path: string;
}

export interface Board {
    readonly sprint: SprintInfo;
    readonly columns: readonly BoardColumn[];
    readonly stories: readonly AgileStory[];
    readonly counts: {
        readonly stories: number;
        readonly storiesDone: number;
        readonly storiesStarted: number;
        readonly storiesBlocked: number;
        readonly tasks: number;
        readonly tasksDone: number;
    };
    readonly filters: {
        readonly environments: readonly string[];
        readonly epics: readonly string[];
    };
}

function collapse(value: string | undefined): string {
    return (value ?? '').trim().replace(/\s+/g, ' ');
}

/** A keyword the repository documents as one token is cut at its first space. */
function firstToken(value: string | undefined): string {
    return collapse(value).split(' ')[0] ?? '';
}

function keyword(doc: OrgDoc, name: string): string {
    return collapse(doc.keywords.get(name));
}

function stripTitlePrefix(title: string): string {
    return title.replace(/^(Story|Task):\s*/i, '').trim();
}

function docId(doc: OrgDoc): string {
    return (doc.id ?? '').toUpperCase();
}

function stateOf(doc: OrgDoc): AgileState {
    const status = findSection(doc, 'Status');
    return normalizeState(status === undefined ? undefined : fieldTable(status).get('state'));
}

/**
 * A pull request number. The text must be a number, with an optional `#`,
 * because a number becomes a link to a real pull request, and prose must not
 * invent one.
 */
export function parsePrNumber(raw: string): number | null {
    const match = /^#?(\d+)$/.exec(raw.trim());
    return match?.[1] === undefined ? null : Number(match[1]);
}

export function parseTask(text: string, slug: string, path: string): AgileTask {
    const doc = parseOrg(text);
    const title = stripTitlePrefix(keyword(doc, 'title'));
    return {
        id: docId(doc),
        slug,
        title,
        state: stateOf(doc),
        description: keyword(doc, 'description'),
        environment: firstToken(doc.keywords.get('environment')),
        branch: keyword(doc, 'branch'),
        pr: keyword(doc, 'pr'),
        blockedOn: keyword(doc, 'blocked_on'),
        blockedSince: keyword(doc, 'blocked_since'),
        created: keyword(doc, 'created'),
        updated: keyword(doc, 'updated'),
        scaffold: /^Scaffold story:/i.test(title),
        path,
    };
}

export interface StoryDoc {
    readonly id: string;
    readonly title: string;
    readonly state: AgileState;
    readonly description: string;
    readonly environment: string;
    readonly created: string;
    readonly updated: string;
}

export function parseStory(text: string): StoryDoc {
    const doc = parseOrg(text);
    return {
        id: docId(doc),
        title: stripTitlePrefix(keyword(doc, 'title')),
        state: stateOf(doc),
        description: keyword(doc, 'description'),
        environment: firstToken(doc.keywords.get('environment')),
        created: keyword(doc, 'created'),
        updated: keyword(doc, 'updated'),
    };
}

export interface SprintDoc {
    readonly title: string;
    readonly startDate: string;
    readonly endDate: string;
    /** The epic of each story id, from the group heading that holds the story's row. */
    readonly epics: ReadonlyMap<string, string>;
}

export function parseSprint(text: string): SprintDoc {
    const doc = parseOrg(text);
    const status = findSection(doc, 'Status');
    const fields = status === undefined ? new Map<string, string>() : fieldTable(status);
    const epics = new Map<string, string>();
    const stories = findSection(doc, 'Stories');
    if (stories !== undefined) {
        for (const group of sectionChildren(doc, stories)) {
            for (const table of group.tables) {
                for (const row of table.rows) {
                    const id = idLink(row[0] ?? '');
                    if (id !== null && !epics.has(id)) epics.set(id, group.title);
                }
            }
        }
    }
    return {
        title: keyword(doc, 'title'),
        startDate: keyword(doc, 'start_date') || collapse(fields.get('start')),
        endDate:
            keyword(doc, 'end_date') ||
            collapse(fields.get('end (expected)')) ||
            collapse(fields.get('end')),
        epics,
    };
}

function utcDay(date: string): number {
    return Date.parse(`${date}T00:00:00Z`);
}

function sprintDay(start: string, today: string): number | null {
    const from = utcDay(start);
    const now = utcDay(today);
    if (!Number.isFinite(from) || !Number.isFinite(now)) return null;
    return Math.floor((now - from) / 86_400_000) + 1;
}

function totalDays(start: string, end: string): number | null {
    const from = utcDay(start);
    const to = utcDay(end);
    if (!Number.isFinite(from) || !Number.isFinite(to)) return null;
    return Math.max(Math.round((to - from) / 86_400_000) + 1, 0);
}

function byStateThenTitle(
    a: { readonly state: AgileState; readonly title: string },
    b: { readonly state: AgileState; readonly title: string },
): number {
    const order = stateRank(a.state) - stateRank(b.state);
    if (order !== 0) return order;
    return a.title < b.title ? -1 : a.title > b.title ? 1 : 0;
}

function unique(values: readonly string[]): string[] {
    return [...new Set(values.filter((value) => value !== ''))].sort();
}

/** The canonical columns always exist (BACKLOG with DISCOVERED, STARTED, BLOCKED, DONE and ABANDONED), so the board does not rearrange as cards move. */
function buildColumns(stories: readonly AgileStory[]): BoardColumn[] {
    const counts = new Map<string, number>();
    for (const story of stories) {
        const column = STATE_COLUMN.get(story.state) ?? 'UNKNOWN';
        counts.set(column, (counts.get(column) ?? 0) + 1);
    }
    const columns: BoardColumn[] = [];
    const seen = new Set<string>();
    for (const state of STATES) {
        if (seen.has(state.column)) continue;
        seen.add(state.column);
        columns.push({
            id: state.column,
            title: state.title,
            count: counts.get(state.column) ?? 0,
        });
    }
    const unknown = counts.get('UNKNOWN');
    if (unknown !== undefined) columns.push({ id: 'UNKNOWN', title: 'Unknown', count: unknown });
    return columns;
}

/** A story directory as the store read it. */
export interface StoryFiles {
    readonly slug: string;
    /** The sprint folder path of the directory, with a trailing slash. */
    readonly dirPath: string;
    readonly story: string;
    readonly tasks: readonly { readonly slug: string; readonly text: string }[];
}

export interface BoardInput {
    readonly version: string;
    readonly name: string;
    readonly path: string;
    readonly sprint: string;
    readonly stories: readonly StoryFiles[];
    readonly today: string;
}

export function buildBoard(input: BoardInput): Board {
    const sprint = parseSprint(input.sprint);
    const stories: AgileStory[] = [];
    for (const files of input.stories) {
        const story = parseStory(files.story);
        const tasks = files.tasks
            .map((task) => parseTask(task.text, task.slug, `${files.dirPath}task_${task.slug}.org`))
            .sort(byStateThenTitle);
        const prs = new Set<number>();
        for (const task of tasks) {
            const number = parsePrNumber(task.pr);
            if (number !== null) prs.add(number);
        }
        stories.push({
            ...story,
            slug: files.slug,
            epic: sprint.epics.get(story.id) ?? '',
            path: `${files.dirPath}story.org`,
            progress: {
                done: tasks.filter((task) => task.state === 'DONE').length,
                total: tasks.length,
                abandoned: tasks.filter((task) => task.state === 'ABANDONED').length,
            },
            branches: unique(tasks.map((task) => task.branch)),
            prs: [...prs].sort((a, b) => a - b),
            tasks,
        });
    }
    stories.sort(byStateThenTitle);
    const tasks = stories.flatMap((story) => story.tasks);
    const count = (list: readonly { readonly state: AgileState }[], state: AgileState) =>
        list.filter((item) => item.state === state).length;
    return {
        sprint: {
            version: input.version,
            name: input.name,
            title: sprint.title,
            startDate: sprint.startDate,
            endDate: sprint.endDate,
            dayOfSprint: sprintDay(sprint.startDate, input.today),
            totalDays: totalDays(sprint.startDate, sprint.endDate),
            path: input.path,
        },
        columns: buildColumns(stories),
        stories,
        counts: {
            stories: stories.length,
            storiesDone: count(stories, 'DONE'),
            storiesStarted: count(stories, 'STARTED'),
            storiesBlocked: count(stories, 'BLOCKED'),
            tasks: tasks.length,
            tasksDone: count(tasks, 'DONE'),
        },
        filters: {
            environments: unique([
                ...stories.map((story) => story.environment),
                ...tasks.map((task) => task.environment),
            ]),
            epics: unique(stories.map((story) => story.epic)),
        },
    };
}
