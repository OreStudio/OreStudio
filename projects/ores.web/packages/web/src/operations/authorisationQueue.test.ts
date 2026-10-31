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
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

import { describe, expect, it } from 'vitest';
import { countsOf, groupOf, rowsOf, type QueueRow } from '../api/queue.js';

/**
 * The queue's two divisions.
 *
 * The screen draws the same rows twice: once by how much work a deal is, once by
 * the state its authorisation is in. Neither is a filter the read applies, so
 * both are stated here, on rows and nothing else.
 */

function row(overrides: Partial<QueueRow> = {}): QueueRow {
    return {
        requestId: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa',
        requestVersion: 1,
        stateCode: 'waiting',
        requestedBy: 'diane.kay',
        requestedAt: '2026-10-31 09:00:00Z',
        reason: 'A live trade awaits operational authorisation.',
        tradeId: 'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb',
        reference: 'POP-1A2B-000001',
        classCode: 'flow',
        bookId: 'cccccccc-cccc-4ccc-8ccc-cccccccccccc',
        counterpartyId: 'dddddddd-dddd-4ddd-8ddd-dddddddddddd',
        tradeType: 'Swap',
        tradeTypeName: 'Interest Rate Swap',
        book: 'GBP Vanilla Swaps',
        counterparty: 'Deutsche Bank AG',
        tradeDate: '2026-10-31',
        status: 'live',
        requestTradeVersion: 1,
        tradeVersion: 1,
        changed: false,
        ...overrides,
    };
}

describe('the authorisation queue', () => {
    it('groups a waiting deal as awaiting and an approved one as authorised', () => {
        expect(groupOf(row())).toBe('awaiting');
        expect(groupOf(row({ stateCode: 'approved' }))).toBe('authorised');
        expect(groupOf(row({ stateCode: 'refused' }))).toBe('denied');
        expect(groupOf(row({ stateCode: 'held' }))).toBe('held');
    });

    it('files the endings that are neither a yes nor a no as closed', () => {
        // The asker took it back, nobody decided before it expired, or it was
        // approved and the owning component could not apply it. Drawing any of
        // them as a denial would record a refusal nobody gave.
        for (const stateCode of ['withdrawn', 'expired', 'apply_failed']) {
            expect(groupOf(row({ stateCode }))).toBe('closed');
        }
    });

    it('reads a state the inbox does not hold as awaiting, not as closed', () => {
        // A state nobody has named keeps the deal in front of a person.
        expect(groupOf(row({ stateCode: 'escalated' }))).toBe('awaiting');
    });

    it('draws a deal that moved after its request as a change, whatever its state', () => {
        // The state is still waiting, but the work now is to re-read the deal.
        expect(groupOf(row({ changed: true }))).toBe('changed');
        expect(groupOf(row({ changed: true, stateCode: 'held' }))).toBe('changed');
    });

    it('counts each class in each state, and all of them together', () => {
        const rows = [
            row(),
            row({ requestId: 'c1', classCode: 'non-flow' }),
            row({ requestId: 'c2', stateCode: 'held' }),
            row({ requestId: 'c3', classCode: 'non-flow', changed: true }),
        ];
        expect(countsOf(rows, 'all')).toMatchObject({ all: 4, awaiting: 2, held: 1, changed: 1 });
        expect(countsOf(rows, 'flow')).toMatchObject({ all: 2, awaiting: 1, held: 1, changed: 0 });
        expect(countsOf(rows, 'non-flow')).toMatchObject({ all: 2, awaiting: 1, changed: 1 });
    });

    it('narrows to one class and the states a person chose, newest request first', () => {
        const rows = [
            row({ requestId: 'old', requestedAt: '2026-10-31 08:00:00Z' }),
            row({ requestId: 'new', requestedAt: '2026-10-31 11:00:00Z' }),
            row({ requestId: 'other', classCode: 'non-flow' }),
            row({ requestId: 'held', stateCode: 'held' }),
        ];
        expect(rowsOf(rows, 'flow', ['awaiting']).map((one) => one.requestId)).toEqual([
            'new',
            'old',
        ]);
        // More than one state at once: the waiting and the held, across classes.
        expect(rowsOf(rows, 'all', ['awaiting', 'held']).map((one) => one.requestId)).toEqual([
            'new',
            'other',
            'held',
            'old',
        ]);
        expect(rowsOf(rows, 'all', ['awaiting']).map((one) => one.requestId)).toEqual([
            'new',
            'other',
            'old',
        ]);
        // Nothing chosen draws nothing: the person unticked every box.
        expect(rowsOf(rows, 'all', [])).toEqual([]);
    });
});
