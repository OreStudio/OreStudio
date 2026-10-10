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

import type { ScenarioSummary } from '@ores/contracts';

/**
 * What the Tests queue says about a scenario, worked out from its summary.
 *
 * A scenario waits until it is PASSED or FAILED. A waiting scenario with some
 * steps done is in progress, so a tester sees which ones were started.
 */
export type QueueState = 'pending' | 'inProgress' | 'passed' | 'failed';

/** The steps with an outcome: passed, failed or dropped. */
export function stepsDone(scenario: ScenarioSummary): number {
    return scenario.steps.total - scenario.steps.pending;
}

export function stateOf(scenario: ScenarioSummary): QueueState {
    if (scenario.state === 'PASSED') return 'passed';
    if (scenario.state === 'FAILED') return 'failed';
    return stepsDone(scenario) > 0 ? 'inProgress' : 'pending';
}

/** How much of the scenario is done, as a whole percent. */
export function percentDone(scenario: ScenarioSummary): number {
    return scenario.steps.total === 0
        ? 0
        : Math.round((stepsDone(scenario) / scenario.steps.total) * 100);
}

/**
 * Open a scenario from a click on its row. A click on the title link reaches
 * the row after the link has handled it and marked the event as handled, so the
 * row opens the scenario only for a click the link did not take.
 */
export function openFromRow(event: { readonly defaultPrevented: boolean }, open: () => void): void {
    if (!event.defaultPrevented) open();
}

export interface Queue {
    readonly waiting: readonly ScenarioSummary[];
    readonly done: readonly ScenarioSummary[];
}

/**
 * Waiting scenarios first, with the ones in progress ahead of the ones not
 * started, and done scenarios second, newest completion first. Within a group
 * the server's order stands. The completion times are ISO-8601 strings in one
 * format, so a text comparison orders them by time.
 */
export function queueOf(scenarios: readonly ScenarioSummary[]): Queue {
    const waiting = scenarios.filter((s) => s.phase === 'waiting');
    const started = waiting.filter((s) => stepsDone(s) > 0);
    const notStarted = waiting.filter((s) => stepsDone(s) === 0);
    const done = scenarios
        .filter((s) => s.phase === 'done')
        .map((scenario, index) => ({ scenario, index }))
        .sort(
            (a, b) =>
                b.scenario.completedAt.localeCompare(a.scenario.completedAt) || a.index - b.index,
        )
        .map(({ scenario }) => scenario);
    return { waiting: [...started, ...notStarted], done };
}
