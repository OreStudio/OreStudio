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
import { percentDone, queueOf, stateOf, stepsDone } from './queue.js';
import { summary } from './testSummary.js';

describe('the state of a scenario in the queue', () => {
    it('is pending when nothing is done, and in progress once a step is', () => {
        expect(stateOf(summary())).toBe('pending');
        expect(
            stateOf(summary({ steps: { total: 4, pending: 3, pass: 1, fail: 0, dropped: 0 } })),
        ).toBe('inProgress');
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

describe('ordering the queue', () => {
    const idle = summary({ id: 'idle', title: 'Idle' });
    const started = summary({
        id: 'started',
        title: 'Started',
        steps: { total: 4, pending: 2, pass: 2, fail: 0, dropped: 0 },
    });
    const old = summary({
        id: 'old',
        phase: 'done',
        state: 'PASSED',
        completedAt: '2026-07-01T10:00:00Z',
    });
    const recent = summary({
        id: 'recent',
        phase: 'done',
        state: 'FAILED',
        completedAt: '2026-10-01T10:00:00Z',
    });
    const undated = summary({ id: 'undated', phase: 'done', state: 'PASSED' });

    it('puts waiting scenarios apart from done ones', () => {
        const queue = queueOf([old, idle, recent]);
        expect(queue.waiting.map((s) => s.id)).toEqual(['idle']);
        expect(queue.done.map((s) => s.id)).toEqual(['recent', 'old']);
    });

    it('lists scenarios in progress before scenarios not started, keeping the server order within each', () => {
        const other = summary({ id: 'other' });
        expect(queueOf([idle, other, started]).waiting.map((s) => s.id)).toEqual([
            'started',
            'idle',
            'other',
        ]);
    });

    it('lists the done scenario with the newest completion first and undated ones last', () => {
        expect(queueOf([undated, old, recent]).done.map((s) => s.id)).toEqual([
            'recent',
            'old',
            'undated',
        ]);
    });

    it('is empty for no scenarios', () => {
        expect(queueOf([])).toEqual({ waiting: [], done: [] });
    });
});
