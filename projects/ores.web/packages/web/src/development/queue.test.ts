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
import {
    openFromRow,
    percentDone,
    sprintBuckets,
    sprintNumber,
    stateOf,
    stepsDone,
} from './queue.js';
import { summary } from './testSummary.js';

describe('the state of a scenario in the queue', () => {
    it('is pending when nothing is done, and in progress once a step is', () => {
        expect(stateOf(summary())).toBe('pending');
        expect(
            stateOf(summary({ steps: { total: 4, pending: 3, pass: 1, fail: 0, dropped: 0 } })),
        ).toBe('inProgress');
    });

    it('is abandoned for a scenario a person has abandoned, whatever its steps say', () => {
        expect(stateOf(summary({ state: 'ABANDONED', phase: 'abandoned' }))).toBe('abandoned');
        expect(
            stateOf(
                summary({
                    state: 'ABANDONED',
                    phase: 'abandoned',
                    steps: { total: 4, pending: 1, pass: 3, fail: 0, dropped: 0 },
                }),
            ),
        ).toBe('abandoned');
    });

    it('is passed or failed once the scenario is closed', () => {
        expect(stateOf(summary({ state: 'PASSED', phase: 'done' }))).toBe('passed');
        expect(stateOf(summary({ state: 'FAILED', phase: 'done' }))).toBe('failed');
    });
});

describe('progress', () => {
    it('counts a pass, a fail and a drop as done', () => {
        const s = summary({ steps: { total: 5, pending: 2, pass: 1, fail: 1, dropped: 1 } });
        expect(stepsDone(s)).toBe(3);
        expect(percentDone(s)).toBe(60);
    });

    it('is zero for a scenario with no steps', () => {
        expect(
            percentDone(summary({ steps: { total: 0, pending: 0, pass: 0, fail: 0, dropped: 0 } })),
        ).toBe(0);
    });
});

describe('the number of a sprint', () => {
    it('is read from the folder name', () => {
        expect(sprintNumber('sprint_27')).toBe(27);
        expect(sprintNumber('sprint_3')).toBe(3);
    });

    it('is null for no sprint and for a name that is not one', () => {
        expect(sprintNumber(null)).toBeNull();
        expect(sprintNumber('release_2')).toBeNull();
        expect(sprintNumber('sprint_')).toBeNull();
    });
});

describe('grouping the queue by sprint', () => {
    const at = (id: string, sprint: string | null, created: string, extra = {}) =>
        summary({ id, title: id, sprint, created, ...extra });
    const done = { phase: 'done', state: 'PASSED' } as const;
    const gone = { phase: 'abandoned', state: 'ABANDONED' } as const;
    const ids = (rows: readonly { id: string }[]) => rows.map((r) => r.id);

    it('puts the newest sprint first and the scenarios outside any sprint last', () => {
        const groups = sprintBuckets([
            at('a', 'sprint_9', '2026-01-01'),
            at('b', null, '2026-05-01'),
            at('c', 'sprint_27', '2026-10-01'),
            at('d', 'sprint_23', '2026-07-01'),
        ]);
        expect(groups.map((g) => g.sprint)).toEqual(['sprint_27', 'sprint_23', 'sprint_9', null]);
    });

    it('orders sprints by number, not by name', () => {
        const groups = sprintBuckets([
            at('a', 'sprint_9', ''),
            at('b', 'sprint_10', ''),
            at('c', 'sprint_2', ''),
        ]);
        expect(groups.map((g) => g.number)).toEqual([10, 9, 2]);
    });

    it('splits a sprint into the active, the done and the abandoned scenarios', () => {
        const [sprint] = sprintBuckets([
            at('w1', 'sprint_27', '2026-10-01'),
            at('d1', 'sprint_27', '2026-10-02', done),
            at('a1', 'sprint_27', '2026-10-03', gone),
            at('w2', 'sprint_27', '2026-10-04'),
        ]);
        expect(ids(sprint?.active ?? [])).toEqual(['w2', 'w1']);
        expect(ids(sprint?.done ?? [])).toEqual(['d1']);
        expect(ids(sprint?.abandoned ?? [])).toEqual(['a1']);
    });

    it('sorts each bucket by created date, newest first, with undated scenarios last, then by title', () => {
        const [sprint] = sprintBuckets([
            at('old', 'sprint_1', '2026-01-01'),
            at('undated-b', 'sprint_1', ''),
            at('new', 'sprint_1', '2026-09-30'),
            at('undated-a', 'sprint_1', ''),
            at('same-b', 'sprint_1', '2026-05-05'),
            at('same-a', 'sprint_1', '2026-05-05'),
        ]);
        expect(ids(sprint?.active ?? [])).toEqual([
            'new',
            'same-a',
            'same-b',
            'old',
            'undated-a',
            'undated-b',
        ]);
    });

    it('keeps a sprint that has only done scenarios, with an empty active bucket', () => {
        const groups = sprintBuckets([at('d', 'sprint_22', '2026-06-01', done)]);
        expect(groups).toHaveLength(1);
        expect(groups[0]?.active).toEqual([]);
        expect(ids(groups[0]?.done ?? [])).toEqual(['d']);
    });

    it('is empty for no scenarios', () => {
        expect(sprintBuckets([])).toEqual([]);
    });
});

describe('opening a scenario from its row', () => {
    it('opens it for a click on the row itself', () => {
        let opened = 0;
        openFromRow({ defaultPrevented: false }, () => (opened += 1));
        expect(opened).toBe(1);
    });

    it('does not open it again for a click the title link already handled', () => {
        let opened = 0;
        openFromRow({ defaultPrevented: true }, () => (opened += 1));
        expect(opened).toBe(0);
    });
});
