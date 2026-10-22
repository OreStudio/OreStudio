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

import {
    scenarioStateOf,
    type RunRequest,
    type RunResponse,
    type Scenario,
    type ScenarioState,
    type StepStatus,
} from '@ores/contracts';

/**
 * The runner's working copy of a scenario, and the rules for changing it.
 *
 * A tester marks steps one at a time. A Pass moves to the next step and a Fail
 * stays on the step, so the tester can write what went wrong. Marking never
 * closes the scenario: only a save does, and the server closes it when no step
 * is pending. A closed scenario is read-only until a retest clears its steps.
 */

export interface DraftStep {
    readonly client: string | null;
    readonly title: string;
    readonly instructions: readonly string[];
    readonly status: StepStatus;
    readonly notes: string;
}

export interface Runner {
    readonly steps: readonly DraftStep[];
    /** The step on show. */
    readonly index: number;
    /** The scenario is closed on the server and no retest has begun. */
    readonly closed: boolean;
}

export type Mark = 'PASS' | 'FAIL' | 'PENDING';

export type Action =
    | { readonly type: 'mark'; readonly mark: Mark }
    | { readonly type: 'note'; readonly notes: string }
    | { readonly type: 'previous' }
    | { readonly type: 'next' }
    | { readonly type: 'attach'; readonly index: number; readonly name: string }
    | { readonly type: 'retest' }
    | { readonly type: 'saved'; readonly scenario: Scenario };

function draftsOf(scenario: Scenario): DraftStep[] {
    return scenario.steps.map((step) => ({
        client: step.client,
        title: step.title,
        instructions: step.instructions,
        status: step.status,
        notes: step.notes,
    }));
}

/** Open a scenario on its first pending step, or on the first step when none is. */
export function openRunner(scenario: Scenario): Runner {
    const steps = draftsOf(scenario);
    const first = steps.findIndex((step) => step.status === 'PENDING');
    return { steps, index: first < 0 ? 0 : first, closed: scenario.state !== 'PENDING' };
}

function replaceStep(
    steps: readonly DraftStep[],
    index: number,
    change: Partial<DraftStep>,
): DraftStep[] {
    return steps.map((step, at) => (at === index ? { ...step, ...change } : step));
}

export function reduce(runner: Runner, action: Action): Runner {
    const last = runner.steps.length - 1;
    switch (action.type) {
        case 'mark':
            if (runner.closed) return runner;
            return {
                ...runner,
                steps: replaceStep(runner.steps, runner.index, { status: action.mark }),
                index:
                    (action.mark === 'PASS' || action.mark === 'FAIL') && runner.index < last
                        ? runner.index + 1
                        : runner.index,
            };
        case 'note':
            if (runner.closed) return runner;
            return {
                ...runner,
                steps: replaceStep(runner.steps, runner.index, { notes: action.notes }),
            };
        case 'attach': {
            // The link goes to the step the image was dropped on, which may no
            // longer be the step on show when the upload ends.
            const step = runner.steps[action.index];
            if (runner.closed || step === undefined) return runner;
            const link = `[[file:${action.name}]]`;
            return {
                ...runner,
                steps: replaceStep(runner.steps, action.index, {
                    notes: step.notes === '' ? link : `${step.notes}\n${link}`,
                }),
            };
        }
        case 'previous':
            return { ...runner, index: Math.max(0, runner.index - 1) };
        case 'next':
            return { ...runner, index: Math.min(last, runner.index + 1) };
        case 'retest':
            return {
                steps: runner.steps.map((step) =>
                    step.status === 'DROPPED' ? step : { ...step, status: 'PENDING', notes: '' },
                ),
                index: 0,
                closed: false,
            };
        case 'saved': {
            const reopened = openRunner(action.scenario);
            return {
                ...reopened,
                index: Math.min(runner.index, Math.max(0, reopened.steps.length - 1)),
            };
        }
    }
}

/** The state the scenario would have if it were saved now. */
export function stateNow(runner: Runner): ScenarioState {
    return scenarioStateOf(runner.steps.map((step) => step.status));
}

/** The steps with an outcome. */
export function markedCount(runner: Runner): number {
    return runner.steps.filter((step) => step.status !== 'PENDING').length;
}

export function percentMarked(runner: Runner): number {
    return runner.steps.length === 0
        ? 0
        : Math.round((markedCount(runner) / runner.steps.length) * 100);
}

/**
 * Whether the working copy differs from the scenario as the server last
 * answered it, in the status or the note of any step. A tester who has marked
 * or written something that is not saved leaves it behind by leaving the page.
 */
export function isDirty(runner: Runner, scenario: Scenario): boolean {
    return (
        runner.steps.length !== scenario.steps.length ||
        runner.steps.some((step, at) => {
            const saved = scenario.steps[at];
            return (
                saved === undefined || step.status !== saved.status || step.notes !== saved.notes
            );
        })
    );
}

/**
 * What a save sends: every step the tester can mark. A dropped step is left
 * out, because a save never writes one. It names no build, so the doc keeps the
 * build of the last run.
 */
export function saveRequest(runner: Runner): RunRequest {
    return {
        steps: runner.steps.flatMap((step) =>
            step.status === 'DROPPED'
                ? []
                : [
                      {
                          client: step.client,
                          title: step.title,
                          status: step.status,
                          notes: step.notes,
                      },
                  ],
        ),
    };
}

/** What the page tells the tester after a save, or after a failed one. */
export type Outcome =
    | { readonly kind: 'closed'; readonly state: ScenarioState }
    | { readonly kind: 'open'; readonly pending: number }
    | { readonly kind: 'failed'; readonly message: string };

/** The message for a save the server accepted: closed with its state, or still open. */
export function outcomeOf(saved: RunResponse): Outcome {
    return saved.state === 'PENDING'
        ? {
              kind: 'open',
              pending: saved.scenario.steps.filter((step) => step.status === 'PENDING').length,
          }
        : { kind: 'closed', state: saved.state };
}
