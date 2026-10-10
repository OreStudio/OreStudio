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

/**
 * The HTTP contract of the QA validation runner.
 *
 * A scenario is an org doc that the compass test_scenario template creates.
 * The BFF reads the doc and writes a tester's run back into it, so these
 * shapes describe what a tester sees and what a save carries. A save carries
 * no overall state and no time: the server works out whether the scenario is
 * closed from its steps, and it stamps the time.
 */

export const stepStatusSchema = z.enum(['PENDING', 'PASS', 'FAIL', 'DROPPED']);
export type StepStatus = z.infer<typeof stepStatusSchema>;

export const scenarioStateSchema = z.enum(['PENDING', 'PASSED', 'FAILED']);
export type ScenarioState = z.infer<typeof scenarioStateSchema>;

/** A link to another doc: its :ID: and the text of the link. */
export const docLinkSchema = z.object({ id: z.string(), title: z.string() });
export type DocLink = z.infer<typeof docLinkSchema>;

export const stepCountsSchema = z.object({
    total: z.int(),
    pending: z.int(),
    pass: z.int(),
    fail: z.int(),
    dropped: z.int(),
});

export const scenarioSummarySchema = z.object({
    id: z.string(),
    title: z.string(),
    description: z.string(),
    path: z.string(),
    state: scenarioStateSchema,
    /** A scenario waits for a tester until it is PASSED or FAILED. */
    phase: z.enum(['waiting', 'done']),
    target: z.string(),
    story: docLinkSchema.nullable(),
    task: docLinkSchema.nullable(),
    clients: z.array(z.string()),
    steps: stepCountsSchema,
    completedAt: z.string(),
});
export type ScenarioSummary = z.infer<typeof scenarioSummarySchema>;

export const scenarioListSchema = z.object({ scenarios: z.array(scenarioSummarySchema) });

export const scenarioStepSchema = z.object({
    /** The client the step runs on, or null in a single-client scenario. */
    client: z.string().nullable(),
    title: z.string(),
    instructions: z.array(z.string()),
    status: stepStatusSchema,
    notes: z.string(),
});

export const scenarioSchema = z.object({
    id: z.string(),
    title: z.string(),
    description: z.string(),
    state: scenarioStateSchema,
    target: z.string(),
    story: docLinkSchema.nullable(),
    task: docLinkSchema.nullable(),
    clients: z.array(z.string()),
    beforeYouStart: z.array(z.string()),
    steps: z.array(scenarioStepSchema),
    run: z.object({
        status: z.string(),
        completedAt: z.string(),
        branch: z.string(),
        commit: z.string(),
        worktree: z.string(),
    }),
});
export type Scenario = z.infer<typeof scenarioSchema>;

export const scenarioResponseSchema = z.object({ scenario: scenarioSchema });

/** A doc the runner shows beside the steps: a story, a task or a scenario. */
export const qaDocSchema = z.object({
    id: z.string(),
    title: z.string(),
    type: z.string().nullable(),
    path: z.string(),
    /** The org text of the doc. The browser renders it. */
    text: z.string(),
});
export type QaDoc = z.infer<typeof qaDocSchema>;

export const qaDocResponseSchema = z.object({ doc: qaDocSchema });

/** The outcome of one step in a save. A save never marks a step DROPPED. */
export const stepOutcomeSchema = z.strictObject({
    client: z.string().min(1).max(200).nullable(),
    title: z.string().min(1).max(500),
    status: z.enum(['PENDING', 'PASS', 'FAIL']),
    notes: z.string().max(10_000),
});
export type StepOutcome = z.infer<typeof stepOutcomeSchema>;

/** A tester's save. It names every step it writes, and the build it ran against. */
export const runRequestSchema = z.strictObject({
    steps: z.array(stepOutcomeSchema).min(1).max(500),
    branch: z.string().max(200),
    commit: z.string().max(200),
    worktree: z.string().max(200),
});
export type RunRequest = z.infer<typeof runRequestSchema>;

export const runResponseSchema = z.object({
    scenario: scenarioSchema,
    state: scenarioStateSchema,
    /** The steps the save wrote, in the order the scenario has them. */
    changed: z.array(z.object({ client: z.string().nullable(), title: z.string() })),
});
export type RunResponse = z.infer<typeof runResponseSchema>;
