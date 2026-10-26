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
    entriesOf,
    rowsOf,
    withGroups,
} from './grid-report-results.js';
import type { AggregateRow, RunResultRow } from './grid-report-results.js';

/*
 * The run result file: the engine's npv.csv, read by column name.
 *
 * The header the engine writes starts with a hash, and a counterparty name can
 * hold a comma inside quotes, so both are checked against a file shaped the way
 * the engine writes it.
 */

const CSV = [
    '#TradeId,TradeType,Maturity,MaturityTime,NPV,NpvCurrency,NPV(Base),BaseCurrency,Notional,NotionalCurrency,Notional(Base),NettingSet,CounterParty',
    '247917c2-b008-5482-930e-8e776fb73aa8,Bond,2030-05-05,4.0,7076092.454615,GBP,7076092.454615,GBP,7087413.92,GBP,7087413.92,NS-1,"Northwind Capital, Ltd"',
    '33bbf3d1-a35a-563a-b951-85f240c55db5,Bond,2031-05-05,5.0,1142973.707486,GBP,1142973.707486,GBP,1149669.91,GBP,1149669.91,NS-1,Acme Corporation Plc',
].join('\n');

/** A tar holding one file, in the plain ustar form the wrapper writes. */
function tarOf(name: string, content: string): Buffer {
    const body = Buffer.from(content, 'utf8');
    const header = Buffer.alloc(512);
    header.write(name, 0, 100, 'utf8');
    header.write('0000644', 100, 8, 'utf8');
    header.write(`${body.length.toString(8).padStart(11, '0')} `, 124, 12, 'utf8');
    header.write('00000000000 ', 136, 12, 'utf8');
    header.write('        ', 148, 8, 'utf8');
    header.write('0', 156, 1, 'utf8');
    header.write('ustar\0', 257, 6, 'utf8');
    const padding = Buffer.alloc((512 - (body.length % 512)) % 512);
    return Buffer.concat([header, body, padding, Buffer.alloc(1024)]);
}

describe('rowsOf', () => {
    it('reads the figures by name, past the hash the engine writes first', () => {
        const rows = rowsOf(CSV);

        expect(rows).toHaveLength(2);
        expect(rows?.[0]).toEqual({
            trade_id: '247917c2-b008-5482-930e-8e776fb73aa8',
            trade_type: 'Bond',
            npv: '7076092.454615',
            npv_currency: 'GBP',
            npv_base: '7076092.454615',
            notional: '7087413.92',
            counterparty: 'Northwind Capital, Ltd',
            counterparty_name: '',
            notional_currency: 'GBP',
            book_id: '',
            book_name: '',
        });
        expect(rows?.[1]?.npv).toBe('1142973.707486');
    });

    it('reads a file that starts with a byte-order mark', () => {
        expect(rowsOf(`\uFEFF${CSV}`)).toHaveLength(2);
    });

    it('reports nothing when the figures are not named in the header', () => {
        expect(rowsOf('TradeType,Notional\nBond,1000')).toBeNull();
    });

    it('reads a file with a header and no rows as no result, not as a failure', () => {
        expect(rowsOf('#TradeId,NPV\n')).toEqual([]);
    });
});

describe('entriesOf', () => {
    it('finds the file the engine writes inside the archive', () => {
        const entries = entriesOf(tarOf('Output/npv.csv', CSV));

        expect([...entries.keys()]).toEqual(['Output/npv.csv']);
        expect(entries.get('Output/npv.csv')?.toString('utf8')).toBe(CSV);
    });
});

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
