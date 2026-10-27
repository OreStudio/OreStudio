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
    aggregateRows,
    comparedWith,
    withGroups,
    withPortfolioTree,
} from './grid-report-results.js';
import type { AggregateRow, RunResultRow } from './grid-report-results.js';

/*
 * The rows a run's post-processor loaded, read from the result table.
 *
 * The CSV the engine wrote is no longer read here: these tests state the rows the
 * table holds, as the screen receives them.
 */

describe('aggregateRows', () => {
    const row = (over: Partial<RunResultRow>): RunResultRow => ({
        trade_id: 'a',
        trade_type: 'Bond',
        npv: '100',
        npv_currency: 'GBP',
        npv_base: '100',
        notional: '1000',
        notional_currency: 'GBP',
        counterparty: 'c1',
        counterparty_name: 'First',
        book_id: 'b1',
        book_name: 'Book One',
        ...over,
    });

    it('sums each book and orders the largest figure first', () => {
        const rows = aggregateRows(
            [
                row({ trade_id: 'a', npv: '100', notional: '1000' }),
                row({ trade_id: 'b', npv: '250', notional: '2000' }),
                row({
                    trade_id: 'c',
                    book_id: 'b2',
                    book_name: 'Book Two',
                    npv: '-500',
                    notional: '500',
                }),
            ],
            (entry) => entry.book_id,
            new Map([
                ['b1', 'Book One'],
                ['b2', 'Book Two'],
            ]),
        );

        expect(rows.map((entry) => entry.name)).toEqual(['Book One', 'Book Two']);
        expect(rows[0]?.trades).toBe(2);
        expect(rows[0]?.totals).toEqual([{ currency: 'GBP', total: '350.00' }]);
        expect(rows[0]?.notional).toEqual([{ currency: 'GBP', total: '3000.00' }]);
        expect(rows[1]?.totals).toEqual([{ currency: 'GBP', total: '-500.00' }]);
    });

    it('keeps a trade with no book as its own row rather than dropping it', () => {
        const rows = aggregateRows(
            [row({ trade_id: 'a' }), row({ trade_id: 'b', book_id: '', book_name: '' })],
            (entry) => entry.book_id,
            new Map([['b1', 'Book One']]),
        );

        expect(rows.map((entry) => entry.key).sort()).toEqual(['', 'b1']);
        expect(rows.find((entry) => entry.key === '')?.trades).toBe(1);
    });

    it('totals each currency apart', () => {
        const rows = aggregateRows(
            [
                row({ trade_id: 'a', npv: '100' }),
                row({ trade_id: 'b', npv: '200', npv_currency: 'USD' }),
            ],
            (entry) => entry.book_id,
            new Map([['b1', 'Book One']]),
        );

        expect(rows[0]?.totals).toEqual([
            { currency: 'GBP', total: '100.00' },
            { currency: 'USD', total: '200.00' },
        ]);
    });
});

describe('withGroups', () => {
    const aggregate = (over: Partial<AggregateRow>): AggregateRow => ({
        key: 'c1',
        name: 'First',
        trades: 1,
        totals: [{ currency: 'GBP', total: '100.00' }],
        notional: [{ currency: 'GBP', total: '1000.00' }],
        group: '',
        is_group: false,
        ...over,
    });

    it('puts a group row above the members that share a parent', () => {
        const rows = withGroups(
            [
                aggregate({
                    key: 'c1',
                    name: 'First',
                    totals: [{ currency: 'GBP', total: '100.00' }],
                }),
                aggregate({
                    key: 'c2',
                    name: 'Second',
                    totals: [{ currency: 'GBP', total: '50.00' }],
                }),
                aggregate({
                    key: 'c3',
                    name: 'Third',
                    totals: [{ currency: 'GBP', total: '200.00' }],
                }),
            ],
            new Map([
                ['c1', 'p1'],
                ['c2', 'p1'],
            ]),
            new Map([['p1', 'The Group']]),
        );

        expect(rows.map((row) => `${row.name}${row.is_group ? ' (group)' : ''}`)).toEqual([
            'The Group (group)',
            'First',
            'Second',
            'Third',
        ]);
        expect(rows[0]?.trades).toBe(2);
        expect(rows[0]?.totals).toEqual([{ currency: 'GBP', total: '150.00' }]);
        expect(rows[1]?.group).toBe('The Group');
        expect(rows[3]?.group).toBe('');
    });

    it('leaves a counterparty with no parent standing on its own', () => {
        const rows = withGroups([aggregate({ key: 'c3', name: 'Third' })], new Map(), new Map());

        expect(rows).toHaveLength(1);
        expect(rows[0]?.is_group).toBe(false);
    });
});

describe('withGroups, the cases a group hides', () => {
    const aggregate = (over: Partial<AggregateRow>): AggregateRow => ({
        key: 'c1',
        name: 'First',
        trades: 1,
        totals: [{ currency: 'GBP', total: '100.00' }],
        notional: [{ currency: 'GBP', total: '1000.00' }],
        group: '',
        is_group: false,
        ...over,
    });

    it('counts a parent that trades in the run as one of its own members', () => {
        const rows = withGroups(
            [
                aggregate({
                    key: 'p1',
                    name: 'The Group',
                    totals: [{ currency: 'GBP', total: '70.00' }],
                }),
                aggregate({
                    key: 'c1',
                    name: 'First',
                    totals: [{ currency: 'GBP', total: '100.00' }],
                }),
                aggregate({
                    key: 'c2',
                    name: 'Second',
                    totals: [{ currency: 'GBP', total: '50.00' }],
                }),
            ],
            new Map([
                ['c1', 'p1'],
                ['c2', 'p1'],
            ]),
            new Map([['p1', 'The Group']]),
        );

        const group = rows.find((row) => row.is_group);
        expect(group?.trades).toBe(3);
        expect(group?.totals).toEqual([{ currency: 'GBP', total: '220.00' }]);
        expect(rows.filter((row) => row.key === 'p1')).toHaveLength(2);
        expect(rows.filter((row) => row.key === 'p1' && row.is_group)).toHaveLength(1);
    });

    it('leaves members standing when the parent name was not read', () => {
        const rows = withGroups(
            [aggregate({ key: 'c1', name: 'First' }), aggregate({ key: 'c2', name: 'Second' })],
            new Map([
                ['c1', 'p1'],
                ['c2', 'p1'],
            ]),
            new Map(),
        );

        expect(rows.every((row) => !row.is_group)).toBe(true);
        expect(rows.every((row) => row.group === '')).toBe(true);
    });

    it('keeps each currency apart in a group total', () => {
        const rows = withGroups(
            [
                aggregate({ key: 'c1', totals: [{ currency: 'GBP', total: '100.00' }] }),
                aggregate({
                    key: 'c2',
                    totals: [
                        { currency: 'GBP', total: '50.00' },
                        { currency: 'USD', total: '25.00' },
                    ],
                }),
            ],
            new Map([
                ['c1', 'p1'],
                ['c2', 'p1'],
            ]),
            new Map([['p1', 'The Group']]),
        );

        expect(rows[0]?.totals).toEqual([
            { currency: 'GBP', total: '150.00' },
            { currency: 'USD', total: '25.00' },
        ]);
    });
});

describe('withGroups, the group ordering', () => {
    const aggregate = (over: Partial<AggregateRow>): AggregateRow => ({
        key: 'c1',
        name: 'First',
        trades: 1,
        totals: [{ currency: 'GBP', total: '100.00' }],
        notional: [{ currency: 'GBP', total: '1000.00' }],
        group: '',
        is_group: false,
        ...over,
    });

    it('orders the groups by their own figure, not by their members', () => {
        const rows = withGroups(
            [
                aggregate({ key: 'a1', totals: [{ currency: 'GBP', total: '10.00' }] }),
                aggregate({ key: 'a2', totals: [{ currency: 'GBP', total: '20.00' }] }),
                aggregate({ key: 'b1', totals: [{ currency: 'GBP', total: '500.00' }] }),
            ],
            new Map([
                ['a1', 'pa'],
                ['a2', 'pa'],
                ['b1', 'pb'],
            ]),
            new Map([
                ['pa', 'Group A'],
                ['pb', 'Group B'],
            ]),
        );

        expect(rows.filter((row) => row.is_group).map((row) => row.name)).toEqual([
            'Group B',
            'Group A',
        ]);
        expect(rows.map((row) => row.name)).toEqual([
            'Group B',
            'First',
            'Group A',
            'First',
            'First',
        ]);
    });

    it('leaves a parent alone when none of its children traded', () => {
        const rows = withGroups(
            [aggregate({ key: 'p1', name: 'The Group' })],
            new Map([['p1', 'p0']]),
            new Map([['p1', 'The Group']]),
        );

        expect(rows.every((row) => !row.is_group)).toBe(true);
    });
});

describe('comparedWith', () => {
    const aggregate = (over: Partial<AggregateRow>): AggregateRow => ({
        key: 'b1',
        name: 'Book One',
        trades: 2,
        totals: [{ currency: 'GBP', total: '150.00' }],
        notional: [{ currency: 'GBP', total: '3000.00' }],
        group: '',
        is_group: false,
        previous: [],
        change: [],
        change_pct: [],
        is_removed: false,
        ...over,
    });

    it('states each row against the earlier run, and a new row against nothing', () => {
        const rows = comparedWith(
            [
                aggregate({ key: 'b1', totals: [{ currency: 'GBP', total: '150.00' }] }),
                aggregate({ key: 'b2', name: 'New Book' }),
            ],
            [aggregate({ key: 'b1', totals: [{ currency: 'GBP', total: '100.00' }] })],
        );

        const book = rows.find((row) => row.key === 'b1');
        expect(book?.previous).toEqual([{ currency: 'GBP', total: '100.00' }]);
        expect(book?.change).toEqual([{ currency: 'GBP', total: '50.00' }]);
        expect(book?.change_pct).toEqual([{ currency: 'GBP', total: '50.00' }]);

        const fresh = rows.find((row) => row.key === 'b2');
        expect(fresh?.previous).toEqual([]);
        expect(fresh?.change_pct).toEqual([{ currency: 'GBP', total: '' }]);
    });

    it('keeps a row only the earlier run had, marked as gone', () => {
        const rows = comparedWith(
            [aggregate({ key: 'b1' })],
            [
                aggregate({ key: 'b1' }),
                aggregate({
                    key: 'b9',
                    name: 'Closed Book',
                    totals: [{ currency: 'GBP', total: '40.00' }],
                }),
            ],
        );

        const gone = rows.find((row) => row.key === 'b9');
        expect(gone?.is_removed).toBe(true);
        expect(gone?.trades).toBe(0);
        expect(gone?.totals).toEqual([]);
        expect(gone?.notional).toEqual([]);
        expect(gone?.previous).toEqual([{ currency: 'GBP', total: '40.00' }]);
        expect(gone?.change).toEqual([{ currency: 'GBP', total: '-40.00' }]);
    });
});

describe('comparedWith, the rows only the earlier run had', () => {
    const aggregate = (over: Partial<AggregateRow>): AggregateRow => ({
        key: 'c1',
        name: 'First',
        trades: 1,
        totals: [{ currency: 'GBP', total: '100.00' }],
        notional: [{ currency: 'GBP', total: '1000.00' }],
        group: '',
        is_group: false,
        previous: [],
        change: [],
        change_pct: [],
        is_removed: false,
        ...over,
    });

    it('keeps a removed member beside its group rather than at the end', () => {
        const rows = comparedWith(
            [
                aggregate({ key: 'p1', name: 'The Group', is_group: true }),
                aggregate({ key: 'c1', name: 'First', group: 'The Group' }),
                aggregate({ key: 'p2', name: 'Other Group', is_group: true }),
                aggregate({ key: 'c2', name: 'Second', group: 'Other Group' }),
            ],
            [
                aggregate({ key: 'p1', name: 'The Group', is_group: true }),
                aggregate({ key: 'c1', name: 'First', group: 'The Group' }),
                aggregate({ key: 'c3', name: 'Gone Member', group: 'The Group' }),
                aggregate({ key: 'p2', name: 'Other Group', is_group: true }),
                aggregate({ key: 'c2', name: 'Second', group: 'Other Group' }),
            ],
        );

        const index = rows.findIndex((row) => row.key === 'c3');
        expect(rows[index]?.is_removed).toBe(true);
        expect(rows[index - 1]?.key).toBe('c1');
        expect(rows[index + 1]?.key).toBe('p2');
    });
});

describe('comparedWith, currencies in a different order', () => {
    const aggregate = (over: Partial<AggregateRow>): AggregateRow => ({
        key: 'b1',
        name: 'Book One',
        trades: 1,
        totals: [],
        notional: [],
        group: '',
        is_group: false,
        previous: [],
        change: [],
        change_pct: [],
        is_removed: false,
        ...over,
    });

    it('states each currency against its own earlier figure', () => {
        const rows = comparedWith(
            [
                aggregate({
                    totals: [
                        { currency: 'USD', total: '200.00' },
                        { currency: 'GBP', total: '150.00' },
                    ],
                }),
            ],
            [aggregate({ totals: [{ currency: 'GBP', total: '100.00' }] })],
        );

        const row = rows[0];
        const changeFor = (currency: string): string | undefined =>
            row?.change.find((money) => money.currency === currency)?.total;
        expect(changeFor('GBP')).toBe('50.00');
        expect(changeFor('USD')).toBe('200.00');
        expect(row?.change_pct.find((money) => money.currency === 'GBP')?.total).toBe('50.00');
    });
});

describe('comparedWith, a group that disappeared', () => {
    const aggregate = (over: Partial<AggregateRow>): AggregateRow => ({
        key: 'c1',
        name: 'First',
        trades: 1,
        totals: [{ currency: 'GBP', total: '100.00' }],
        notional: [{ currency: 'GBP', total: '1000.00' }],
        group: '',
        is_group: false,
        previous: [],
        change: [],
        change_pct: [],
        is_removed: false,
        ...over,
    });

    it('keeps a removed roll-up above the members it lost', () => {
        const rows = comparedWith(
            [
                aggregate({ key: 'p2', name: 'Other Group', is_group: true }),
                aggregate({ key: 'c2', name: 'Second', group: 'Other Group' }),
            ],
            [
                aggregate({ key: 'p1', name: 'Gone Group', is_group: true }),
                aggregate({ key: 'c1', name: 'First', group: 'Gone Group' }),
                aggregate({ key: 'p2', name: 'Other Group', is_group: true }),
                aggregate({ key: 'c2', name: 'Second', group: 'Other Group' }),
            ],
        );

        const rollUp = rows.findIndex((row) => row.key === 'p1');
        expect(rows[rollUp]?.is_removed).toBe(true);
        expect(rows[rollUp + 1]?.key).toBe('c1');
        expect(rows[rollUp + 1]?.is_removed).toBe(true);
    });
});

describe('withGroups, the book level', () => {
    const book = (over: Partial<AggregateRow>): AggregateRow => ({
        key: 'b1',
        name: 'Book One',
        trades: 1,
        totals: [{ currency: 'GBP', total: '100.00' }],
        notional: [{ currency: 'GBP', total: '1000.00' }],
        group: '',
        is_group: false,
        previous: [],
        change: [],
        change_pct: [],
        is_removed: false,
        ...over,
    });

    it('groups books under a portfolio id, which is a different id space', () => {
        const rows = withGroups(
            [book({ key: 'b1', name: 'Book One' }), book({ key: 'b2', name: 'Book Two' })],
            new Map([
                ['b1', 'pf1'],
                ['b2', 'pf1'],
            ]),
            new Map([['pf1', 'The Portfolio']]),
        );

        expect(rows[0]?.name).toBe('The Portfolio');
        expect(rows[0]?.is_group).toBe(true);
        expect(rows[0]?.trades).toBe(2);
        expect(rows.slice(1).every((row) => row.group === 'The Portfolio')).toBe(true);
        expect(rows.every((row) => row.is_group || row.group !== '')).toBe(true);
    });

    it('leaves a book in no portfolio standing', () => {
        const rows = withGroups([book({ key: 'b1' })], new Map(), new Map());

        expect(rows).toHaveLength(1);
        expect(rows[0]?.is_group).toBe(false);
    });
});

describe('withPortfolioTree', () => {
    const book = (over: Partial<AggregateRow>): AggregateRow => ({
        key: 'b1',
        name: 'Book One',
        trades: 1,
        totals: [{ currency: 'GBP', total: '100.00' }],
        notional: [{ currency: 'GBP', total: '1000.00' }],
        group: '',
        is_group: false,
        previous: [],
        change: [],
        change_pct: [],
        is_removed: false,
        depth: 0,
        ...over,
    });

    it('nests a portfolio inside its parent and rolls the figures up', () => {
        const rows = withPortfolioTree(
            [
                book({ key: 'b1', name: 'Book One' }),
                book({
                    key: 'b2',
                    name: 'Book Two',
                    totals: [{ currency: 'GBP', total: '50.00' }],
                }),
                book({ key: 'b3', name: 'Book Three' }),
            ],
            new Map([
                ['b1', 'pf1'],
                ['b2', 'pf2'],
                ['b3', 'pf2'],
            ]),
            new Map([
                ['pf1', 'Portfolio One'],
                ['pf2', 'Portfolio Two'],
            ]),
            new Map([['pf2', 'pf1']]),
        );

        expect(rows.map((row) => `${row.name}:${row.depth}`)).toEqual([
            'Portfolio One:0',
            'Book One:1',
            'Portfolio Two:1',
            'Book Three:2',
            'Book Two:2',
        ]);
        expect(rows[0]?.totals).toEqual([{ currency: 'GBP', total: '250.00' }]);
        expect(rows[0]?.trades).toBe(3);
        expect(rows[2]?.is_group).toBe(true);
        // The parent holds one book of its own besides its child's two.
        expect(rows[0]?.trades).toBe(3);
        expect(rows[2]?.trades).toBe(2);
    });

    it('leaves a book in no portfolio at the top', () => {
        const rows = withPortfolioTree(
            [book({ key: 'b9', name: 'Loose Book' })],
            new Map(),
            new Map(),
            new Map(),
        );

        expect(rows).toHaveLength(1);
        expect(rows[0]?.depth).toBe(0);
        expect(rows[0]?.is_group).toBe(false);
    });
});

describe('withGroups and withPortfolioTree, the depths and the cycles', () => {
    const row = (over: Partial<AggregateRow>): AggregateRow => ({
        key: 'c1',
        name: 'First',
        trades: 1,
        totals: [{ currency: 'GBP', total: '100.00' }],
        notional: [{ currency: 'GBP', total: '1000.00' }],
        group: '',
        is_group: false,
        previous: [],
        change: [],
        change_pct: [],
        is_removed: false,
        depth: 0,
        ...over,
    });

    it('states a member at depth one under its group', () => {
        const rows = withGroups(
            [row({ key: 'c1' })],
            new Map([['c1', 'p1']]),
            new Map([['p1', 'The Group']]),
        );

        expect(rows[0]?.depth).toBe(0);
        expect(rows[0]?.is_group).toBe(true);
        expect(rows[1]?.depth).toBe(1);
    });

    it('keeps a portfolio that sits in a cycle rather than dropping its books', () => {
        const rows = withPortfolioTree(
            [row({ key: 'b1', name: 'Book One' })],
            new Map([['b1', 'pa']]),
            new Map([
                ['pa', 'Portfolio A'],
                ['pb', 'Portfolio B'],
            ]),
            new Map([
                ['pa', 'pb'],
                ['pb', 'pa'],
            ]),
        );

        const names = rows.map((entry) => entry.name);
        expect(names).toContain('Book One');
        expect(names).toContain('Portfolio A');
        expect(names).toContain('Portfolio B');
    });
});

describe('withPortfolioTree, a parent holding no book of its own', () => {
    const book = (over: Partial<AggregateRow>): AggregateRow => ({
        key: 'b1',
        name: 'Book One',
        trades: 1,
        totals: [{ currency: 'GBP', total: '100.00' }],
        notional: [{ currency: 'GBP', total: '1000.00' }],
        group: '',
        is_group: false,
        previous: [],
        change: [],
        change_pct: [],
        is_removed: false,
        depth: 0,
        ...over,
    });

    it('rolls a child portfolio up into a parent that trades nothing itself', () => {
        const rows = withPortfolioTree(
            [
                book({ key: 'b1', name: 'Book One' }),
                book({
                    key: 'b2',
                    name: 'Book Two',
                    totals: [{ currency: 'GBP', total: '50.00' }],
                }),
            ],
            new Map([
                ['b1', 'pf2'],
                ['b2', 'pf2'],
            ]),
            new Map([
                ['pf1', 'The Parent'],
                ['pf2', 'The Child'],
            ]),
            new Map([['pf2', 'pf1']]),
        );

        expect(rows.map((row) => `${row.name}:${row.depth}`)).toEqual([
            'The Parent:0',
            'The Child:1',
            'Book One:2',
            'Book Two:2',
        ]);
        expect(rows[0]?.trades).toBe(2);
        expect(rows[0]?.totals).toEqual([{ currency: 'GBP', total: '150.00' }]);
    });
});
