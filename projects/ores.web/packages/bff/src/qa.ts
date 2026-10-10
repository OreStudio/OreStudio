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

import type { FastifyInstance, FastifyRequest } from 'fastify';
import { runRequestSchema } from '@ores/contracts';
import { HttpFailure, invalidRequest, notFound } from './errors.js';
import {
    AmbiguousStepError,
    DuplicateOutcomeError,
    ScenarioFormatError,
    UnknownStepError,
} from './scenarios/scenario-doc.js';
import {
    InvalidDocIdError,
    ScenarioNotFoundError,
    type ScenarioStore,
} from './scenarios/scenario-store.js';
import type { LiveSession } from './sessions.js';

/**
 * The QA validation runner routes: the scenarios a tester can run, one
 * scenario with its steps, the docs it points at, and a save.
 *
 * A route never touches a file. It asks the scenario store, so a database
 * store can replace the files. With no store the runner is off and every route
 * answers not found.
 *
 * Every route needs a signed-in session. It needs no more, because a trader
 * may run scenarios. The doc route serves stories, tasks and scenarios only, so
 * the rest of the doc root stays unread.
 */

const SERVED_TYPES: ReadonlySet<string> = new Set(['story', 'task', 'test_scenario']);

const RUNNER_OFF =
    'The QA validation runner is off. Set ORES_WEB_DOC_ROOT to the folder that holds the scenarios.';

/** An error code that Node gives a failed file call, such as EACCES. */
function isFileError(error: unknown): boolean {
    const code = (error as { code?: unknown } | null)?.code;
    return typeof code === 'string' && /^E[A-Z0-9]+$/.test(code);
}

/** Say what a store refused in the words of the browser-facing contract. */
function failure(error: unknown): unknown {
    if (error instanceof InvalidDocIdError) return invalidRequest(error.message);
    if (error instanceof ScenarioNotFoundError) return notFound(error.message);
    if (
        error instanceof UnknownStepError ||
        error instanceof AmbiguousStepError ||
        error instanceof DuplicateOutcomeError
    ) {
        return invalidRequest(error.message);
    }
    if (error instanceof ScenarioFormatError) {
        return new HttpFailure(422, { code: 'invalid-request', message: error.message });
    }
    if (isFileError(error)) {
        return new HttpFailure(503, {
            code: 'upstream-unavailable',
            message: 'The scenario files cannot be read now. Try again.',
        });
    }
    return error;
}

async function guarded<T>(work: () => Promise<T>): Promise<T> {
    try {
        return await work();
    } catch (error) {
        throw failure(error);
    }
}

/** The time of a save, to the second, as the Results table shows it. */
function stamp(now: Date): string {
    return now.toISOString().replace(/\.\d{3}Z$/, 'Z');
}

export function registerQaRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
    store: ScenarioStore | undefined,
    now: () => Date = () => new Date(),
): void {
    const open = (request: FastifyRequest): ScenarioStore => {
        requireSession(request);
        if (store === undefined) throw notFound(RUNNER_OFF);
        return store;
    };
    const idOf = (request: FastifyRequest): string => (request.params as { id: string }).id;

    server.get('/api/qa/scenarios', async (request) => {
        const scenarios = open(request);
        return { scenarios: await guarded(() => scenarios.list()) };
    });

    server.get('/api/qa/scenarios/:id', async (request) => {
        const scenarios = open(request);
        const scenario = await guarded(() => scenarios.read(idOf(request)));
        if (scenario === null) throw notFound('No scenario has this id.');
        return { scenario };
    });

    server.get('/api/qa/docs/:id', async (request) => {
        const scenarios = open(request);
        const doc = await guarded(() => scenarios.readDoc(idOf(request)));
        if (doc === null || doc.type === null || !SERVED_TYPES.has(doc.type)) {
            throw notFound('No story, task or scenario has this id.');
        }
        return { doc };
    });

    server.put('/api/qa/scenarios/:id/run', async (request) => {
        const scenarios = open(request);
        const parsed = runRequestSchema.safeParse(request.body);
        if (!parsed.success) throw invalidRequest('The save is not a valid run.');
        const run = parsed.data;
        const recorded = await guarded(() =>
            scenarios.record(idOf(request), {
                steps: run.steps,
                branch: run.branch,
                commit: run.commit,
                worktree: run.worktree,
                completedAt: stamp(now()),
            }),
        );
        return {
            scenario: recorded.scenario,
            state: recorded.scenario.state,
            changed: recorded.changed,
        };
    });
}
