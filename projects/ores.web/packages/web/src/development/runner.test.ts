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

import { describe, expect, it } from 'vitest';
import type { Scenario, StepStatus } from '@ores/contracts';
import {
    markedCount,
    openRunner,
    outcomeOf,
    percentMarked,
    reduce,
    saveRequest,
    stateNow,
    type Runner,
} from './runner.js';

function scenario(statuses: StepStatus[], overrides: Partial<Scenario> = {}): Scenario {
    return {
        id: '11111111-1111-4111-8111-111111111111',
        title: 'A scenario',
        description: '',
        state: 'PENDING',
        target: 'Dialog',
        story: null,
        task: null,
        clients: [],
        beforeYouStart: [],
        steps: statuses.map((status, i) => ({
            client: null,
            title: `Step ${i + 1}`,
            instructions: [`Do ${i + 1}.`],
            status,
            notes: '',
        })),
        run: { status: '', completedAt: '', branch: '', commit: '', worktree: '' },
        ...overrides,
    };
}

const fresh = (n = 3) => openRunner(scenario(Array<StepStatus>(n).fill('PENDING')));

describe('opening a scenario', () => {
    it('starts on the first pending step', () => {
        expect(openRunner(scenario(['PASS', 'PASS', 'PENDING', 'PENDING'])).index).toBe(2);
    });

    it('starts on the first step when none is pending', () => {
        expect(openRunner(scenario(['PASS', 'FAIL'], { state: 'FAILED' })).index).toBe(0);
    });

    it('is closed exactly when the server closed the scenario', () => {
        expect(openRunner(scenario(['PENDING'])).closed).toBe(false);
        expect(openRunner(scenario(['PASS'], { state: 'PASSED' })).closed).toBe(true);
        expect(openRunner(scenario(['FAIL'], { state: 'FAILED' })).closed).toBe(true);
    });
});

describe('marking steps', () => {
    it('moves to the next step after a Pass', () => {
        const next = reduce(fresh(), { type: 'mark', mark: 'PASS' });
        expect(next.steps[0]?.status).toBe('PASS');
        expect(next.index).toBe(1);
    });

    it('stays on the step after a Fail, so the tester can write a note', () => {
        const failed = reduce(fresh(), { type: 'mark', mark: 'FAIL' });
        expect(failed.steps[0]?.status).toBe('FAIL');
        expect(failed.index).toBe(0);
        const noted = reduce(failed, { type: 'note', notes: 'The dialog stayed blank.' });
        expect(noted.steps[0]).toMatchObject({ status: 'FAIL', notes: 'The dialog stayed blank.' });
        expect(noted.index).toBe(0);
    });

    it('stays on the last step after the last Pass, and does not close the scenario', () => {
        let runner: Runner = fresh(2);
        runner = reduce(runner, { type: 'mark', mark: 'PASS' });
        runner = reduce(runner, { type: 'mark', mark: 'PASS' });
        expect(runner.index).toBe(1);
        expect(runner.closed).toBe(false);
        expect(stateNow(runner)).toBe('PASSED');
    });

    it('clears a mark and stays on the step', () => {
        let runner = reduce(fresh(), { type: 'mark', mark: 'FAIL' });
        runner = reduce(runner, { type: 'mark', mark: 'PENDING' });
        expect(runner.steps[0]?.status).toBe('PENDING');
        expect(runner.index).toBe(0);
    });

    it('moves back and forward without marking, and stops at the ends', () => {
        let runner = fresh();
        runner = reduce(runner, { type: 'previous' });
        expect(runner.index).toBe(0);
        runner = reduce(reduce(reduce(runner, { type: 'next' }), { type: 'next' }), {
            type: 'next',
        });
        expect(runner.index).toBe(2);
        expect(markedCount(runner)).toBe(0);
    });

    it('works out the state live from the steps, and never closes by marking', () => {
        let runner = fresh(2);
        expect(stateNow(runner)).toBe('PENDING');
        runner = reduce(runner, { type: 'mark', mark: 'FAIL' });
        expect(stateNow(runner)).toBe('PENDING');
        runner = reduce(reduce(runner, { type: 'next' }), { type: 'mark', mark: 'PASS' });
        expect(stateNow(runner)).toBe('FAILED');
        expect(runner.closed).toBe(false);
        expect(percentMarked(runner)).toBe(100);
    });
});

describe('a closed scenario', () => {
    const closed = openRunner(scenario(['PASS', 'FAIL'], { state: 'FAILED' }));

    it('ignores a mark and a note', () => {
        expect(reduce(closed, { type: 'mark', mark: 'PASS' })).toBe(closed);
        expect(reduce(closed, { type: 'note', notes: 'x' })).toBe(closed);
    });

    it('can still be read step by step', () => {
        expect(reduce(closed, { type: 'next' }).index).toBe(1);
    });
});

describe('a retest', () => {
    it('clears every step and its note, opens the scenario and starts at the first step', () => {
        const base = openRunner(scenario(['PASS', 'FAIL'], { state: 'FAILED' }));
        const closed: Runner = {
            ...base,
            steps: [
                { ...base.steps[0]!, notes: 'fine' },
                { ...base.steps[1]!, notes: 'broke' },
            ],
            index: 1,
        };
        const again = reduce(closed, { type: 'retest' });
        expect(again.closed).toBe(false);
        expect(again.index).toBe(0);
        expect(again.steps.map((s) => [s.status, s.notes])).toEqual([
            ['PENDING', ''],
            ['PENDING', ''],
        ]);
        expect(reduce(again, { type: 'mark', mark: 'PASS' }).steps[0]?.status).toBe('PASS');
    });

    it('leaves a dropped step as it was', () => {
        const dropped = openRunner(scenario(['PASS', 'DROPPED'], { state: 'PASSED' }));
        expect(reduce(dropped, { type: 'retest' }).steps[1]?.status).toBe('DROPPED');
    });
});

describe('saving', () => {
    it('sends every step the tester can mark, with its status and note', () => {
        let runner = fresh(2);
        runner = reduce(runner, { type: 'mark', mark: 'FAIL' });
        runner = reduce(runner, { type: 'note', notes: 'broke' });
        expect(saveRequest(runner).steps).toEqual([
            { client: null, title: 'Step 1', status: 'FAIL', notes: 'broke' },
            { client: null, title: 'Step 2', status: 'PENDING', notes: '' },
        ]);
    });

    it('leaves a dropped step out', () => {
        const runner = openRunner(scenario(['PENDING', 'DROPPED']));
        expect(saveRequest(runner).steps.map((s) => s.title)).toEqual(['Step 1']);
    });

    it('keeps the step on show after a partial save, and closes only as the server says', () => {
        let runner = fresh(3);
        runner = reduce(runner, { type: 'mark', mark: 'PASS' });
        const partial = reduce(runner, {
            type: 'saved',
            scenario: scenario(['PASS', 'PENDING', 'PENDING']),
        });
        expect(partial.closed).toBe(false);
        expect(partial.index).toBe(1);
        const full = reduce(partial, {
            type: 'saved',
            scenario: scenario(['PASS', 'PASS', 'PASS'], { state: 'PASSED' }),
        });
        expect(full.closed).toBe(true);
    });
});

describe('the message after a save', () => {
    it('says how many steps are left when the server left the scenario open', () => {
        const saved = scenario(['PASS', 'PENDING', 'PENDING']);
        expect(outcomeOf({ scenario: saved, state: 'PENDING', changed: [] })).toEqual({
            kind: 'open',
            pending: 2,
        });
    });

    it('names the state when the server closed the scenario', () => {
        const saved = scenario(['PASS', 'FAIL'], { state: 'FAILED' });
        expect(outcomeOf({ scenario: saved, state: 'FAILED', changed: [] })).toEqual({
            kind: 'closed',
            state: 'FAILED',
        });
    });
});
