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

import type { ClassificationRule, Requirement, Shape, ShapeKey } from '../api/identityShapes.js';
import type { EditorRow, EditorState } from './editor.js';

/**
 * The family of the URI on screen and what its shape says of each field.
 *
 * A family is keyed as the writer keys it: the asset class the schema row
 * names, the series subclass the classification rules give, and the index
 * family of a fixing. The shapes are stated per identity column, so each field
 * the editor offers is named by the column the writer projects it into.
 */
export type FamilyKey = Omit<ShapeKey, 'field'>;

/** The subclass every fixing has, which the identity kind rule requires. */
const FIXING_SUBCLASS = 'index_fixing';

/** The index fields whose identity column has another name; the rest keep theirs. */
const INDEX_COLUMNS: Readonly<Record<string, string>> = {
    name: 'index_name',
    tag: 'quote_tag',
    unit: 'unit_ccy',
    security: 'security_id',
    start: 'delivery_start',
    end: 'delivery_end',
};

/** The identity column a field of the editor is projected into. */
export function columnOf(state: EditorState, field: string): string {
    return state.kind === 'fixing' ? (INDEX_COLUMNS[field] ?? field) : field;
}

/**
 * The family of the URI on screen, or null while the editor cannot name it: no
 * instrument or quote is chosen yet, or no rule gives the series a subclass.
 */
export function familyOfEditor(
    row: EditorRow | null,
    state: EditorState,
    rules: readonly ClassificationRule[],
): FamilyKey | null {
    if (row === null) return null;
    if (state.kind === 'fixing') {
        const rule = rules.find((candidate) => candidate.key_kind === 'index');
        return {
            asset_class: row.asset,
            series_subclass: rule?.series_subclass_code ?? FIXING_SUBCLASS,
            index_family: state.instrument,
        };
    }
    if (state.quote === '') return null;
    const datum = rules.filter(
        (candidate) => candidate.key_kind === 'datum' && candidate.instrument === state.instrument,
    );
    const rule =
        datum.find((candidate) => candidate.quote === state.quote) ??
        datum.find((candidate) => candidate.quote === '');
    if (rule === undefined) return null;
    // The writer keys a series by the index its identity holds, which a URI writes
    // as its path or as a field.
    const index = row.subject === 'index' ? state.subject : (state.values['index'] ?? '');
    return {
        asset_class: row.asset,
        series_subclass: rule.series_subclass_code,
        index_family: index,
    };
}

/** What the shape of a family says, by identity column. Empty when no row states the family. */
export function marksOf(
    family: FamilyKey | null,
    shapes: readonly Shape[],
): ReadonlyMap<string, Requirement> {
    if (family === null) return new Map();
    return new Map(
        shapes
            .filter(
                (shape) =>
                    shape.asset_class === family.asset_class &&
                    shape.series_subclass === family.series_subclass &&
                    shape.index_family === family.index_family,
            )
            .map((shape) => [shape.field, shape.requirement]),
    );
}

/**
 * The columns the family requires that no input on screen writes, other than
 * the ones the kind and instrument choices state. A family that requires one of
 * these cannot be written by this kind of URI.
 */
export function unreachableOf(
    marks: ReadonlyMap<string, Requirement>,
    row: EditorRow | null,
    state: EditorState,
): readonly string[] {
    if (row === null) return [];
    const offered = new Set([
        columnOf(state, row.subject),
        ...row.fields.map((field) => columnOf(state, field.name)),
        state.kind === 'fixing' ? 'index' : 'instrument_type',
        'quote_type',
    ]);
    return [...marks]
        .filter(([column, requirement]) => requirement === 'required' && !offered.has(column))
        .map(([column]) => column)
        .sort();
}
