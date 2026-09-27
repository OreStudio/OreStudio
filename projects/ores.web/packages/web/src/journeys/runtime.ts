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

/**
 * The journey runtime.
 *
 * A journey is an ordered list of step definitions. The rail is derived from
 * that same list, so the rail and the steps cannot disagree: there is no
 * second place a step is declared.
 *
 * The step type is generic over its body, which keeps this module free of a
 * rendering library. The browser passes a node; a test passes a string. The
 * ordering rules are the same either way, and they are the part that has to be
 * right.
 *
 * See `doc/knowledge/architecture/journey_runtime.org` for the contract, and
 * `doc/knowledge/architecture/journey_execution.org` for the server half.
 */

/** A step's identity within one journey. Unique, and stable across releases. */
export type StepId = string;

/**
 * The footer's primary action.
 *
 * `run` performs the step's server work. It is absent on a step that only
 * moves on, and it is the only place a step reaches the server.
 */
export interface StepAction {
    readonly label: string;
    readonly enabled: boolean;
    readonly run?: () => void | Promise<void>;
}

export interface JourneyStep<Body> {
    readonly id: StepId;
    readonly title: string;
    readonly lead: string;
    readonly body: Body;
    /** Absent when the step advances by itself, as a step awaiting the server does. */
    readonly next?: StepAction;
    /**
     * Once passed, the person cannot come back here.
     *
     * Set it on a step that changed server state. Walking backwards into a
     * half-finished write is the failure this prevents, and it is why the flag
     * belongs to the definition rather than to the page.
     */
    readonly final?: boolean;
    /** Shown above the body from this step onward. */
    readonly header?: Body;
}

export type RailState = 'done' | 'current' | 'ahead';

export interface RailEntry {
    readonly id: StepId;
    readonly title: string;
    readonly state: RailState;
}

/**
 * Validates a journey definition once, at the point it is written.
 *
 * Two steps with the same id would make the named lookup below lie, and it
 * would lie quietly. A journey that inlines another journey's steps is exactly
 * where that happens, so the check runs when the two lists are joined rather
 * than when somebody notices.
 */
export function defineJourney<Body>(steps: readonly JourneyStep<Body>[]): readonly JourneyStep<Body>[] {
    if (steps.length === 0) {
        throw new Error('a journey needs at least one step');
    }
    const seen = new Set<StepId>();
    for (const step of steps) {
        if (seen.has(step.id)) {
            throw new Error(`duplicate step id "${step.id}"`);
        }
        seen.add(step.id);
    }
    return steps;
}

/**
 * The rail, derived from the step list.
 *
 * One entry per step, in the order the steps declare. The caller does not pass
 * titles or order separately, because a caller that could would eventually pass
 * them differently.
 */
export function rail<Body>(steps: readonly JourneyStep<Body>[], at: number): readonly RailEntry[] {
    assertPosition(steps, at);
    return steps.map((step, index) => ({
        id: step.id,
        title: step.title,
        state: stateOf(index, at),
    }));
}

/**
 * Where a step sits, found by name.
 *
 * Named lookup rather than arithmetic is what lets one journey inline another's
 * steps: adding a step to the inner journey cannot silently change what the
 * outer journey means by "the third step".
 */
export function indexOfStep<Body>(steps: readonly JourneyStep<Body>[], id: StepId): number {
    const index = steps.findIndex((step) => step.id === id);
    if (index < 0) {
        throw new Error(`journey has no step "${id}"`);
    }
    return index;
}

/** False at the first step, and false when the step before this one is final. */
export function canGoBack<Body>(steps: readonly JourneyStep<Body>[], at: number): boolean {
    assertPosition(steps, at);
    return at > 0 && steps[at - 1]?.final !== true;
}

/** The next position, or undefined at the last step. */
export function nextPosition<Body>(steps: readonly JourneyStep<Body>[], at: number): number | undefined {
    assertPosition(steps, at);
    return at + 1 < steps.length ? at + 1 : undefined;
}

function stateOf(index: number, at: number): RailState {
    if (index < at) {
        return 'done';
    }
    return index === at ? 'current' : 'ahead';
}

/**
 * A position outside the journey is a defect in the caller, not a state to
 * render. Returning an empty rail or clamping would hide it.
 */
function assertPosition<Body>(steps: readonly JourneyStep<Body>[], at: number): void {
    if (!Number.isInteger(at) || at < 0 || at >= steps.length) {
        throw new RangeError(`journey position ${at} is outside 0..${steps.length - 1}`);
    }
}
