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
    isDirty,
    markedCount,
    movesOn,
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
        created: '',
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

    it('moves to the next step after a Fail, and a note goes to the step on show', () => {
        const failed = reduce(fresh(), { type: 'mark', mark: 'FAIL' });
        expect(failed.steps[0]?.status).toBe('FAIL');
        expect(failed.index).toBe(1);
        const back = reduce(failed, { type: 'previous' });
        const noted = reduce(back, { type: 'note', notes: 'The dialog stayed blank.' });
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
        runner = reduce(runner, { type: 'previous' });
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
        runner = reduce(runner, { type: 'previous' });
        runner = reduce(runner, { type: 'note', notes: 'broke' });
        expect(saveRequest(runner).steps).toEqual([
            { client: null, title: 'Step 1', status: 'FAIL', notes: 'broke' },
            { client: null, title: 'Step 2', status: 'PENDING', notes: '' },
        ]);
    });

    it('names no build, so the doc keeps the build of the last run', () => {
        const request = saveRequest(fresh(2));
        expect(Object.keys(request)).toEqual(['steps']);
    });

    it('leaves a dropped step out', () => {
        const runner = openRunner(scenario(['PENDING', 'DROPPED']));
        expect(saveRequest(runner).steps.map((s) => s.title)).toEqual(['Step 1']);
    });

    it('keeps the index inside the steps when the server answers with fewer', () => {
        let runner = fresh(4);
        runner = reduce(reduce(reduce(runner, { type: 'next' }), { type: 'next' }), {
            type: 'next',
        });
        expect(runner.index).toBe(3);
        const saved = reduce(runner, { type: 'saved', scenario: scenario(['PASS', 'PENDING']) });
        expect(saved.index).toBe(1);
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

describe('attaching an image to a step', () => {
    it('puts the link in the note of the step, in the form the Qt tool used', () => {
        const next = reduce(fresh(), {
            type: 'attach',
            index: 0,
            name: 'scenario_step1_20261011_093015.png',
        });
        expect(next.steps[0]?.notes).toBe('[[file:scenario_step1_20261011_093015.png]]');
    });

    it('goes on a new line after the note the tester wrote', () => {
        let runner = reduce(fresh(), { type: 'note', notes: 'The tab stayed blank.' });
        runner = reduce(runner, { type: 'attach', index: 0, name: 'a.png' });
        runner = reduce(runner, { type: 'attach', index: 0, name: 'b.png' });
        expect(runner.steps[0]?.notes).toBe(
            'The tab stayed blank.\n[[file:a.png]]\n[[file:b.png]]',
        );
    });

    it('goes to the step the image was dropped on, not the step on show now', () => {
        let runner = reduce(fresh(), { type: 'note', notes: 'first note' });
        runner = reduce(reduce(runner, { type: 'next' }), { type: 'note', notes: 'second note' });
        const next = reduce(runner, { type: 'attach', index: 0, name: 'a.png' });
        expect(next.index).toBe(1);
        expect(next.steps[0]?.notes).toBe('first note\n[[file:a.png]]');
        expect(next.steps[1]?.notes).toBe('second note');
    });

    it('is ignored for a closed scenario and for a step that is not there', () => {
        const closed = openRunner(scenario(['PASS'], { state: 'PASSED' }));
        expect(reduce(closed, { type: 'attach', index: 0, name: 'a.png' })).toBe(closed);
        const open = fresh();
        expect(reduce(open, { type: 'attach', index: 9, name: 'a.png' })).toBe(open);
    });

    it('reaches the save in the note, so the doc records the name', () => {
        const runner = reduce(fresh(2), { type: 'attach', index: 1, name: 'a.png' });
        expect(saveRequest(runner).steps[1]?.notes).toBe('[[file:a.png]]');
    });
});

describe('what has not been saved', () => {
    const saved = scenario(['PASS', 'PENDING', 'PENDING']);

    it('is nothing when the working copy is the scenario as the server has it', () => {
        expect(isDirty(openRunner(saved), saved)).toBe(false);
    });

    it('is a mark', () => {
        expect(isDirty(reduce(openRunner(saved), { type: 'mark', mark: 'PASS' }), saved)).toBe(
            true,
        );
    });

    it('is a note, and an attached image link', () => {
        const base = openRunner(saved);
        expect(isDirty(reduce(base, { type: 'note', notes: 'x' }), saved)).toBe(true);
        expect(isDirty(reduce(base, { type: 'attach', index: 1, name: 'a.png' }), saved)).toBe(
            true,
        );
    });

    it('is nothing again once a mark is cleared or a note is taken back', () => {
        let runner = reduce(openRunner(saved), { type: 'mark', mark: 'FAIL' });
        runner = reduce(runner, { type: 'previous' });
        runner = reduce(runner, { type: 'mark', mark: 'PENDING' });
        expect(isDirty(runner, saved)).toBe(false);
        const noted = reduce(openRunner(saved), { type: 'note', notes: 'x' });
        expect(isDirty(reduce(noted, { type: 'note', notes: '' }), saved)).toBe(false);
    });

    it('is nothing after a save, because the server answer is the new baseline', () => {
        const marked = reduce(openRunner(saved), { type: 'mark', mark: 'PASS' });
        const answer = scenario(['PASS', 'PASS', 'PENDING']);
        expect(isDirty(reduce(marked, { type: 'saved', scenario: answer }), answer)).toBe(false);
    });

    it('is nothing for moving between steps, which marks and writes nothing', () => {
        const moved = reduce(reduce(openRunner(saved), { type: 'next' }), { type: 'previous' });
        expect(isDirty(moved, saved)).toBe(false);
    });

    it('is something when the scenario has a different number of steps', () => {
        expect(isDirty(openRunner(saved), scenario(['PASS', 'PENDING']))).toBe(true);
    });
});

describe('what saves a run as it goes', () => {
    it('saves when a mark moves the tester on', () => {
        expect(movesOn({ type: 'mark', mark: 'PASS' })).toBe(true);
        expect(movesOn({ type: 'mark', mark: 'FAIL' })).toBe(true);
    });

    it('saves when the tester skips a step', () => {
        expect(movesOn({ type: 'next' })).toBe(true);
    });

    it('leaves the save to the button for a note, an image or going back', () => {
        expect(movesOn({ type: 'note', notes: 'The list is empty.' })).toBe(false);
        expect(movesOn({ type: 'attach', index: 0, name: 'shot.png' })).toBe(false);
        expect(movesOn({ type: 'previous' })).toBe(false);
    });
});
