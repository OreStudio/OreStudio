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

import type { AgileState, AgileStory, Board } from '@ores/contracts';

/** The board column a state sits in. A discovered story waits with the backlog. */
export function columnOf(state: AgileState): AgileState {
    return state === 'DISCOVERED' ? 'BACKLOG' : state;
}

/**
 * The columns to draw: the five canonical ones always, so the board does not
 * rearrange as cards move, and Unknown only when a doc names no known state.
 */
export function columnsOf(board: Board): readonly AgileState[] {
    const canonical = ['BACKLOG', 'STARTED', 'BLOCKED', 'DONE', 'ABANDONED'] as const;
    const unknown = board.stories.some((story) => columnOf(story.state) === 'UNKNOWN');
    return unknown ? [...canonical, 'UNKNOWN'] : canonical;
}

/** The text a search looks in: the story and its tasks, never the long description. */
function haystack(story: AgileStory): string {
    return [
        story.title,
        story.id,
        story.epic,
        story.environment,
        ...story.tasks.flatMap((task) => [task.title, task.branch, task.environment]),
    ]
        .join(' ')
        .toLowerCase();
}

/** The stories that match the search text and the epic. An empty filter matches all. */
export function visibleStories(
    stories: readonly AgileStory[],
    filter: { readonly text: string; readonly epic: string | null },
): readonly AgileStory[] {
    const text = filter.text.trim().toLowerCase();
    return stories.filter(
        (story) =>
            (filter.epic === null || story.epic === filter.epic) &&
            (text === '' || haystack(story).includes(text)),
    );
}

/**
 * The day of the sprint to show. The server sends it raw: zero or negative
 * before the sprint starts and past the total after it ends. The board shows
 * the day inside the sprint.
 */
export function shownDay(day: number | null, total: number | null): number | null {
    if (day === null) return null;
    const lowest = Math.max(day, 1);
    return total === null ? lowest : Math.min(lowest, Math.max(total, 1));
}

/** The share of a story's tasks that are done, as a whole percent. */
export function percentOfTasks(story: AgileStory): number {
    return story.progress.total === 0
        ? 0
        : Math.round((100 * story.progress.done) / story.progress.total);
}
