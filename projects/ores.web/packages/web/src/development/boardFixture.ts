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

import type { AgileStory, AgileTask, Board } from '@ores/contracts';

export function task(over: Partial<AgileTask> = {}): AgileTask {
    return {
        id: 'T1',
        slug: 'task',
        title: 'A task',
        state: 'DONE',
        description: '',
        environment: '',
        branch: '',
        pr: '',
        blockedOn: '',
        blockedSince: '',
        created: '2026-10-01',
        updated: '2026-10-02',
        scaffold: false,
        path: 'agile/versions/v0/sprint_1/s/task.org',
        ...over,
    };
}

export function story(
    over: Partial<Omit<AgileStory, 'tasks'>> & { tasks?: Partial<AgileTask>[] } = {},
): AgileStory {
    const { tasks, ...rest } = over;
    return {
        id: 'S1',
        slug: 'story',
        title: 'A story',
        state: 'STARTED',
        epic: '',
        description: '',
        environment: '',
        created: '2026-10-01',
        updated: '2026-10-02',
        path: 'agile/versions/v0/sprint_1/s/story.org',
        progress: { done: 0, total: 0, abandoned: 0 },
        branches: [],
        prs: [],
        ...rest,
        tasks: (tasks ?? []).map((over, index) => task({ id: `T${index}`, ...over })),
    };
}

export function board(stories: readonly AgileStory[], over: Partial<Board> = {}): Board {
    return {
        sprint: {
            version: 'v0',
            name: 'sprint_1',
            title: 'Sprint 1',
            startDate: '2026-10-01',
            endDate: '2026-10-14',
            dayOfSprint: 3,
            totalDays: 14,
            path: 'agile/versions/v0/sprint_1/sprint.org',
        },
        columns: [],
        stories: [...stories],
        counts: {
            stories: stories.length,
            storiesDone: 0,
            storiesStarted: 1,
            storiesBlocked: 0,
            tasks: 2,
            tasksDone: 1,
        },
        filters: { environments: [], epics: ['Billing', 'Web'] },
        ...over,
    };
}
