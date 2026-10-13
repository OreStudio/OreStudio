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
import type { CurveSnapshot } from '@ores/contracts';
import { slicesOf, surfaceOf } from './surfaceView.js';

/**
 * The plot of a surface, as the screen draws it.
 *
 * What is checked is what a reader would notice if it were wrong: the rows and columns
 * follow the declared order, a hole is empty and never zero, the scale decides the
 * colours, and a line breaks at a hole.
 */

function snapshotOf(
    axes: readonly string[],
    nodes: readonly (readonly string[])[],
    values: readonly string[],
): CurveSnapshot {
    return {
        as_of: '2026-10-10 12:00:00Z',
        coordinate_fields: [...axes],
        nodes: nodes.map((coordinates) => ({ coordinates: [...coordinates], status: '' })),
        values: [...values],
        recorded_at: values.map(() => '2026-10-10 09:00:00Z'),
        provenance: [],
        oldest_age_seconds: 0,
        spread_seconds: 0,
        warning: false,
    };
}

/** A two-by-three surface: expiries 1M and 1Y against ATM, 25RR and 25BF. */
const GRID = snapshotOf(
    ['expiry', 'strike_label'],
    [
        ['1M', 'ATM'],
        ['1M', '25RR'],
        ['1M', '25BF'],
        ['1Y', 'ATM'],
        ['1Y', '25RR'],
        ['1Y', '25BF'],
    ],
    ['0.080', '0.010', '0.004', '0.100', '0.012', ''],
);

function drawn(snapshot: CurveSnapshot, scale: 'surface' | 'column') {
    const result = surfaceOf(snapshot, scale);
    if (!result.ok) {
        throw new Error(`not drawn: ${result.reason}`);
    }
    return result.surface;
}

describe('surfaceOf', () => {
    it('takes the rows and columns in the order the shape declares them', () => {
        const surface = drawn(GRID, 'surface');
        expect(surface.rowAxis).toBe('expiry');
        expect(surface.columnAxis).toBe('strike_label');
        expect(surface.rows).toEqual(['1M', '1Y']);
        expect(surface.columns).toEqual(['ATM', '25RR', '25BF']);
        expect(surface.cells[1]![0]!.text).toBe('0.100');
    });

    it('draws a node no value stands at as a hole, with no number and no shade', () => {
        const hole = drawn(GRID, 'surface').cells[1]![2]!;
        expect(hole.text).toBe('');
        expect(hole.number).toBeUndefined();
        expect(hole.shade).toBeUndefined();
    });

    it('treats a value that is no number as a hole and not as zero', () => {
        const odd = snapshotOf(
            ['a', 'b'],
            [
                ['x', 'p'],
                ['x', 'q'],
            ],
            ['0.5', 'n/a'],
        );
        const surface = drawn(odd, 'surface');
        expect(surface.cells[0]![1]!.number).toBeUndefined();
        expect(surface.cells[0]![1]!.shade).toBeUndefined();
    });

    it('shades every cell against the whole surface', () => {
        const surface = drawn(GRID, 'surface');
        // The lowest value is 0.004 and the highest 0.100.
        expect(surface.cells[1]![0]!.shade).toBe(1);
        expect(surface.cells[0]![2]!.shade).toBe(0);
        expect(surface.cells[0]![1]!.shade).toBeCloseTo((0.01 - 0.004) / (0.1 - 0.004), 10);
    });

    it('shades each column against its own range when asked', () => {
        const surface = drawn(GRID, 'column');
        // Each column here holds a low and a high, so they take the ends of the scale.
        expect(surface.cells[0]![1]!.shade).toBe(0);
        expect(surface.cells[1]![1]!.shade).toBe(1);
        expect(surface.cells[0]![0]!.shade).toBe(0);
        expect(surface.cells[1]![0]!.shade).toBe(1);
    });

    it('puts a column with one value, or equal values, in the middle of its scale', () => {
        const surface = drawn(GRID, 'column');
        // The 25BF column holds one value and one hole.
        expect(surface.cells[0]![2]!.shade).toBe(0.5);
        const flat = snapshotOf(
            ['a', 'b'],
            [
                ['x', 'p'],
                ['y', 'p'],
            ],
            ['0.2', '0.2'],
        );
        expect(drawn(flat, 'surface').cells[0]![0]!.shade).toBe(0.5);
    });

    it('leaves a column of only holes with no shade', () => {
        const empty = snapshotOf(
            ['a', 'b'],
            [
                ['x', 'p'],
                ['x', 'q'],
            ],
            ['0.2', ''],
        );
        const surface = drawn(empty, 'column');
        expect(surface.cells[0]![1]!.shade).toBeUndefined();
    });

    it('refuses a shape with one axis, or with more than two', () => {
        const one = snapshotOf(['tenor'], [['1Y'], ['2Y']], ['0.1', '0.2']);
        const three = snapshotOf(['a', 'b', 'c'], [['x', 'y', 'z']], ['0.1']);
        expect(surfaceOf(one, 'surface')).toEqual({ ok: false, reason: 'axes' });
        expect(surfaceOf(three, 'surface')).toEqual({ ok: false, reason: 'axes' });
    });

    it('refuses a surface with no node', () => {
        const none = snapshotOf(['a', 'b'], [], []);
        expect(surfaceOf(none, 'surface')).toEqual({ ok: false, reason: 'empty' });
    });
});

describe('slicesOf', () => {
    it('draws one slice for each row, across the columns in order', () => {
        const slices = slicesOf(drawn(GRID, 'surface'));
        expect(slices.map((slice) => slice.label)).toEqual(['1M', '1Y']);
        expect(slices[0]!.runs).toEqual([
            [
                { column: 0, value: 0.08 },
                { column: 1, value: 0.01 },
                { column: 2, value: 0.004 },
            ],
        ]);
    });

    it('ends a run at a hole and starts another after it', () => {
        const gap = snapshotOf(
            ['a', 'b'],
            [
                ['x', 'p'],
                ['x', 'q'],
                ['x', 'r'],
                ['x', 's'],
            ],
            ['0.1', '', '0.3', '0.4'],
        );
        const [slice] = slicesOf(drawn(gap, 'surface'));
        expect(slice!.runs).toEqual([
            [{ column: 0, value: 0.1 }],
            [
                { column: 2, value: 0.3 },
                { column: 3, value: 0.4 },
            ],
        ]);
    });

    it('draws a row with no value as a slice with no run', () => {
        const blank = snapshotOf(
            ['a', 'b'],
            [
                ['x', 'p'],
                ['x', 'q'],
            ],
            ['', ''],
        );
        expect(slicesOf(drawn(blank, 'surface'))[0]!.runs).toEqual([]);
    });
});
