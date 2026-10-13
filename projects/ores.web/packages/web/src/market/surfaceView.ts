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

import type { CurveSnapshot } from '@ores/contracts';

/**
 * The plot of a surface: the grid of a snapshot with two axes, as rows, columns and
 * cells, and the lines of its slices.
 *
 * The rows are the values of the first axis and the columns the values of the second,
 * each in the order the shape declares them, which is the order the snapshot returns the
 * nodes in. A node no value stands at is a hole, and a hole is an empty cell and never a
 * value of zero.
 */

/** What a colour is scaled against: the whole surface, or each column on its own. */
export type SurfaceScale = 'surface' | 'column';

export interface SurfaceCell {
    /** The value as the snapshot states it; empty for a hole. */
    readonly text: string;
    /** The value as a number, or none for a hole. */
    readonly number: number | undefined;
    /** Where the value falls in its scale, from 0 to 1, or none for a hole. */
    readonly shade: number | undefined;
}

export interface Surface {
    readonly rowAxis: string;
    readonly columnAxis: string;
    readonly rows: readonly string[];
    readonly columns: readonly string[];
    /** `cells[row][column]`. */
    readonly cells: readonly (readonly SurfaceCell[])[];
}

/** Why a snapshot is not drawn as a surface. */
export type SurfaceVerdict = 'axes' | 'empty';

export type SurfaceResult =
    | { readonly ok: true; readonly surface: Surface }
    | { readonly ok: false; readonly reason: SurfaceVerdict };

/** A decimal number as the server writes one: a sign, digits, a point and an exponent. */
const DECIMAL = /^[+-]?(\d+\.?\d*|\.\d+)([eE][+-]?\d+)?$/;

/** A value as a number, or none when it is empty or not a decimal number. */
function numberOf(text: string): number | undefined {
    const trimmed = text.trim();
    // Number() also reads 0x10 and Infinity, which no snapshot value is.
    if (!DECIMAL.test(trimmed)) {
        return undefined;
    }
    const value = Number(trimmed);
    return Number.isFinite(value) ? value : undefined;
}

/** The distinct values in the order they first appear. */
function inOrder(values: readonly string[]): string[] {
    return [...new Set(values)];
}

/**
 * Where each number falls between the lowest and the highest of its group, from 0 to 1.
 * A group of equal numbers sits in the middle, so a flat surface does not read as empty.
 */
function shadesOf(numbers: readonly (number | undefined)[]): (number | undefined)[] {
    const present = numbers.filter((value): value is number => value !== undefined);
    const low = Math.min(...present);
    const high = Math.max(...present);
    return numbers.map((value) =>
        value === undefined ? undefined : high === low ? 0.5 : (value - low) / (high - low),
    );
}

/**
 * The surface a snapshot draws, or the reason it draws none.
 *
 * Only a shape with exactly two axes is drawn as a grid. A shape with one axis has a
 * curve and its own chart, and a shape with more than two has no flat picture.
 */
export function surfaceOf(snapshot: CurveSnapshot, scale: SurfaceScale): SurfaceResult {
    if (snapshot.coordinate_fields.length !== 2) {
        return { ok: false, reason: 'axes' };
    }
    if (snapshot.nodes.length === 0) {
        return { ok: false, reason: 'empty' };
    }
    const rows = inOrder(snapshot.nodes.map((node) => node.coordinates[0] ?? ''));
    const columns = inOrder(snapshot.nodes.map((node) => node.coordinates[1] ?? ''));
    const rowAt = new Map(rows.map((label, index) => [label, index]));
    const columnAt = new Map(columns.map((label, index) => [label, index]));

    // The snapshot returns one value for each node, index for index. A node that
    // repeats the coordinates of an earlier one replaces it, so the last value stands.
    const texts = rows.map(() => columns.map(() => ''));
    snapshot.nodes.forEach((node, index) => {
        const row = rowAt.get(node.coordinates[0] ?? '');
        const column = columnAt.get(node.coordinates[1] ?? '');
        if (row !== undefined && column !== undefined) {
            texts[row]![column] = snapshot.values[index] ?? '';
        }
    });
    const numbers = texts.map((row) => row.map(numberOf));

    // A scale of the whole surface shades every cell against one range; a scale of each
    // column shades each column against its own.
    const shades =
        scale === 'surface'
            ? (() => {
                  const flat = shadesOf(numbers.flat());
                  return rows.map((_, r) => columns.map((__, c) => flat[r * columns.length + c]));
              })()
            : (() => {
                  const perColumn = columns.map((_, c) => shadesOf(numbers.map((row) => row[c])));
                  return rows.map((_, r) => columns.map((__, c) => perColumn[c]![r]));
              })();

    return {
        ok: true,
        surface: {
            rowAxis: snapshot.coordinate_fields[0] ?? '',
            columnAxis: snapshot.coordinate_fields[1] ?? '',
            rows,
            columns,
            cells: rows.map((_, r) =>
                columns.map((__, c) => ({
                    text: texts[r]![c]!,
                    number: numbers[r]![c],
                    shade: shades[r]![c],
                })),
            ),
        },
    };
}

/** One slice: the values of one row across the columns, as runs a hole separates. */
export interface Slice {
    readonly label: string;
    /** Each run is the columns that hold a value one after another; a hole ends a run. */
    readonly runs: readonly (readonly { readonly column: number; readonly value: number }[])[];
}

/**
 * The lines of a surface, one for each row.
 *
 * A hole ends a run, so a line is never drawn across a column that holds no value.
 */
export function slicesOf(surface: Surface): readonly Slice[] {
    return surface.rows.map((label, r) => {
        const runs: { column: number; value: number }[][] = [];
        let current: { column: number; value: number }[] = [];
        surface.cells[r]!.forEach((cell, column) => {
            if (cell.number === undefined) {
                if (current.length > 0) {
                    runs.push(current);
                    current = [];
                }
                return;
            }
            current.push({ column, value: cell.number });
        });
        if (current.length > 0) {
            runs.push(current);
        }
        return { label, runs };
    });
}
