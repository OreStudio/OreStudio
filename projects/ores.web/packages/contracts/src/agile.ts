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

import { z } from 'zod';
import { qaDocSchema } from './qa.js';

/**
 * The agile tree as the browser reads it: the sprints, one sprint's board, the
 * backlog buckets and any agile doc by id. The BFF reads the org docs that
 * compass creates and answers with these shapes, parsed with the same schemas
 * on both sides. Nothing here writes: compass stays the only way an agile doc
 * changes.
 */

/** The closed state vocabulary, plus UNKNOWN for a doc that names none. */
export const agileStateSchema = z.enum([
    'DISCOVERED',
    'BACKLOG',
    'STARTED',
    'BLOCKED',
    'DONE',
    'ABANDONED',
    'UNKNOWN',
]);
export type AgileState = z.infer<typeof agileStateSchema>;

export const agileTaskSchema = z.object({
    id: z.string(),
    slug: z.string(),
    title: z.string(),
    state: agileStateSchema,
    description: z.string(),
    environment: z.string(),
    branch: z.string(),
    pr: z.string(),
    blockedOn: z.string(),
    blockedSince: z.string(),
    created: z.string(),
    updated: z.string(),
    scaffold: z.boolean(),
    path: z.string(),
});
export type AgileTask = z.infer<typeof agileTaskSchema>;

export const agileStorySchema = z.object({
    id: z.string(),
    slug: z.string(),
    title: z.string(),
    state: agileStateSchema,
    epic: z.string(),
    description: z.string(),
    environment: z.string(),
    created: z.string(),
    updated: z.string(),
    path: z.string(),
    progress: z.object({ done: z.number(), total: z.number(), abandoned: z.number() }),
    branches: z.array(z.string()),
    prs: z.array(z.number()),
    tasks: z.array(agileTaskSchema),
});
export type AgileStory = z.infer<typeof agileStorySchema>;

export const boardColumnSchema = z.object({
    id: z.string(),
    title: z.string(),
    count: z.number(),
});

export const sprintInfoSchema = z.object({
    version: z.string(),
    name: z.string(),
    title: z.string(),
    startDate: z.string(),
    endDate: z.string(),
    /**
     * Zero or negative before the sprint starts, and past the total after it
     * ends. Null when the sprint doc has no valid start date.
     */
    dayOfSprint: z.number().nullable(),
    totalDays: z.number().nullable(),
    path: z.string(),
});

export const boardSchema = z.object({
    sprint: sprintInfoSchema,
    columns: z.array(boardColumnSchema),
    stories: z.array(agileStorySchema),
    counts: z.object({
        stories: z.number(),
        storiesDone: z.number(),
        storiesStarted: z.number(),
        storiesBlocked: z.number(),
        tasks: z.number(),
        tasksDone: z.number(),
    }),
    filters: z.object({ environments: z.array(z.string()), epics: z.array(z.string()) }),
});
export type Board = z.infer<typeof boardSchema>;
export const boardResponseSchema = z.object({ board: boardSchema });

export const sprintRefSchema = z.object({
    version: z.string(),
    name: z.string(),
    title: z.string(),
    path: z.string(),
});
export const sprintListSchema = z.object({ sprints: z.array(sprintRefSchema) });

export const chartsSchema = z.object({
    /** Stories done in each sprint, oldest first. */
    velocity: z.array(z.object({ sprint: z.string(), done: z.number() })),
    /** The running total of tasks done, by the date each was finished. */
    burnup: z.array(z.object({ date: z.string(), done: z.number() })),
    /** Every task of the sprint, which the burn-up is drawn against. */
    scope: z.number(),
    /** Tasks done on each day of the sprint. Empty when the sprint has no start date. */
    perDay: z.array(
        z.object({ date: z.string(), day: z.number(), done: z.number(), future: z.boolean() }),
    ),
    byState: z.array(z.object({ state: agileStateSchema, count: z.number() })),
});
export type Charts = z.infer<typeof chartsSchema>;
export const chartsResponseSchema = z.object({ charts: chartsSchema });

export const journalEntrySchema = z.object({
    storyId: z.string(),
    storyTitle: z.string(),
    taskId: z.string(),
    taskTitle: z.string(),
    state: agileStateSchema,
    branch: z.string(),
    /** The pull request number, or zero when the entry has none. */
    pr: z.number(),
});

export const workTreeSchema = z.object({
    /** The directory name without the ores_dev_ prefix, as an environment is written. */
    label: z.string(),
    branch: z.string(),
    /** The work tree the BFF reads the agile tree of. */
    isSession: z.boolean(),
    /** The work item of the tree, from its journal, or null when it has none. */
    current: journalEntrySchema.nullable(),
});
export type WorkTree = z.infer<typeof workTreeSchema>;
export const workTreesResponseSchema = z.object({ trees: z.array(workTreeSchema) });

export const BACKLOG_BUCKET_IDS = ['inbox', 'next', 'deferred', 'discarded'] as const;

export const backlogItemSchema = z.object({
    id: z.string(),
    title: z.string(),
    description: z.string(),
    created: z.string(),
    kind: z.enum(['capture', 'story']),
    path: z.string(),
    tasks: z.number(),
});
export const backlogSchema = z.object({
    buckets: z.array(
        z.object({ id: z.enum(BACKLOG_BUCKET_IDS), items: z.array(backlogItemSchema) }),
    ),
});
export type Backlog = z.infer<typeof backlogSchema>;

/** The doc types the agile routes serve. The rest of the doc root stays unread. */
export const AGILE_DOC_TYPES = ['sprint', 'story', 'task', 'capture', 'test_scenario'] as const;

export const agileDocResponseSchema = z.object({ doc: qaDocSchema });
