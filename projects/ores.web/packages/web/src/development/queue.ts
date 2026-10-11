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
export type QueueState = 'pending' | 'inProgress' | 'passed' | 'failed' | 'abandoned';

/** The steps with an outcome: passed, failed or dropped. */
export function stepsDone(scenario: ScenarioSummary): number {
    return scenario.steps.total - scenario.steps.pending;
}

export function stateOf(scenario: ScenarioSummary): QueueState {
    if (scenario.state === 'PASSED') return 'passed';
    if (scenario.state === 'FAILED') return 'failed';
    if (scenario.state === 'ABANDONED') return 'abandoned';
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

/** Whether a scenario waits for a tester, is finished, or has been abandoned. */
export type Phase = ScenarioSummary['phase'];

/** The number in a sprint folder name such as `sprint_27`, or null. */
export function sprintNumber(sprint: string | null): number | null {
    const match = sprint === null ? null : /^sprint_(\d+)$/.exec(sprint);
    return match?.[1] === undefined ? null : Number(match[1]);
}

/** The scenarios of one sprint, or of no sprint when `sprint` is null. */
export interface SprintGroup {
    readonly sprint: string | null;
    readonly number: number | null;
    readonly scenarios: readonly ScenarioSummary[];
}

/**
 * The scenarios of one phase, grouped by sprint, newest sprint first, with the
 * scenarios outside any sprint last. Inside a sprint they run from the newest
 * created date to the oldest, and a scenario with no date comes last, then by
 * title. The created dates are ISO dates, so a text comparison orders them.
 */
export function sprintGroups(
    scenarios: readonly ScenarioSummary[],
    phase: Phase,
): readonly SprintGroup[] {
    const bySprint = new Map<string | null, ScenarioSummary[]>();
    for (const scenario of scenarios) {
        if (scenario.phase !== phase) continue;
        const group = bySprint.get(scenario.sprint) ?? [];
        group.push(scenario);
        bySprint.set(scenario.sprint, group);
    }
    const newestFirst = (a: ScenarioSummary, b: ScenarioSummary): number => {
        if (a.created === '' || b.created === '') {
            return a.created === b.created
                ? a.title.localeCompare(b.title)
                : a.created === ''
                  ? 1
                  : -1;
        }
        return b.created.localeCompare(a.created) || a.title.localeCompare(b.title);
    };
    return [...bySprint]
        .map(([sprint, group]) => ({
            sprint,
            number: sprintNumber(sprint),
            scenarios: [...group].sort(newestFirst),
        }))
        .sort((a, b) => {
            if (a.number === null || b.number === null) {
                return a.number === b.number ? 0 : a.number === null ? 1 : -1;
            }
            return b.number - a.number;
        });
}
