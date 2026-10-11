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

/**
 * The state a scenario has once its steps have these statuses. It closes when
 * no step is pending and some step passed or failed. It is then FAILED if a
 * step failed, else PASSED. A scenario in which nothing passed or failed, such
 * as one whose steps are all dropped, has not been run, so it stays PENDING.
 *
 * The server decides with this when it saves a run, and the browser shows the
 * same answer while a tester marks steps, so the two cannot differ.
 */
export function scenarioStateOf(statuses: readonly StepStatus[]): ScenarioState {
    const ran = statuses.some((status) => status === 'PASS' || status === 'FAIL');
    if (statuses.includes('PENDING') || !ran) return 'PENDING';
    return statuses.includes('FAIL') ? 'FAILED' : 'PASSED';
}

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
    /**
     * The sprint folder the scenario sits in, such as `sprint_27`, or null for
     * a scenario outside any sprint folder.
     */
    sprint: z.string().nullable(),
    /** The date from the doc's `#+created:` line, such as `2026-10-11`, or empty. */
    created: z.string(),
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
    created: z.string(),
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

/**
 * A tester's save. It names every step it writes. It may name the build it ran
 * against. A field it leaves out keeps the value the doc already holds.
 */
export const runRequestSchema = z.strictObject({
    steps: z.array(stepOutcomeSchema).min(1).max(500),
    branch: z.string().max(200).optional(),
    commit: z.string().max(200).optional(),
    worktree: z.string().max(200).optional(),
});
export type RunRequest = z.infer<typeof runRequestSchema>;

export const runResponseSchema = z.object({
    scenario: scenarioSchema,
    state: scenarioStateSchema,
    /** The steps the save wrote, in the order the scenario has them. */
    changed: z.array(z.object({ client: z.string().nullable(), title: z.string() })),
});
export type RunResponse = z.infer<typeof runResponseSchema>;

/** The media types a step screenshot may have. */
export const IMAGE_MEDIA_TYPES = ['image/png', 'image/jpeg', 'image/gif', 'image/webp'] as const;

/** The most bytes of image the server keeps for one screenshot. */
export const MAX_IMAGE_BYTES = 8 * 1024 * 1024;

/**
 * A screenshot for one step: its media type and its bytes in base64. The server
 * checks the bytes, not the media type, and answers 415 for a file that is not
 * a PNG, JPEG, GIF or WebP image, and 413 for one over the limit.
 */
export const imageUploadRequestSchema = z.strictObject({
    mediaType: z.string().max(100),
    data: z.string().min(1),
});
export type ImageUploadRequest = z.infer<typeof imageUploadRequestSchema>;

/** The name the server gave the image. It is saved beside the scenario doc. */
export const imageUploadResponseSchema = z.object({ name: z.string() });
export type ImageUploadResponse = z.infer<typeof imageUploadResponseSchema>;
