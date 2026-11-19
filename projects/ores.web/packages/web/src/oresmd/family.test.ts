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
import type { ClassificationRule, Shape } from '../api/identityShapes.js';
import { EMPTY, rowOf, type EditorState } from './editor.js';
import { columnOf, familyOfEditor, marksOf, unreachableOf } from './family.js';
import { SCHEMA } from './testSchema.js';

/**
 * The family the editor names, and what its shape says of each field.
 *
 * The family must be keyed as the writer keys it, or the editor marks the shape
 * of another family than the one the writer holds the URI to.
 */

const RULES: readonly ClassificationRule[] = [
    { instrument: '', quote: '', key_kind: 'index', series_subclass_code: 'index_fixing' },
    { instrument: 'capfloor', quote: '', key_kind: 'datum', series_subclass_code: 'capfloor' },
    { instrument: 'capfloor', quote: 'price', key_kind: 'datum', series_subclass_code: 'premium' },
    {
        instrument: 'seasonality',
        quote: '',
        key_kind: 'datum',
        series_subclass_code: 'seasonality',
    },
];

function shape(
    family: Pick<Shape, 'asset_class' | 'series_subclass' | 'index_family'>,
    field: string,
    requirement: Shape['requirement'],
): Shape {
    return {
        ...family,
        field,
        requirement,
        description: '',
        version: 1,
        system: true,
        modified_by: '',
        recorded_at: '',
        change_reason_code: '',
        change_commentary: '',
    };
}

const IBOR = { asset_class: 'ir', series_subclass: 'index_fixing', index_family: 'ibor' };

const FIXING: EditorState = {
    ...EMPTY,
    kind: 'fixing',
    instrument: 'ibor',
    subject: 'EUR',
    values: { name: 'EURIBOR' },
};

describe('the family of the URI on screen', () => {
    it('keys a fixing by its index family, under the subclass the index rule gives', () => {
        expect(familyOfEditor(rowOf(SCHEMA, FIXING), FIXING, RULES)).toEqual(IBOR);
    });

    it('keys a series by the rule of its instrument and quote, before the rule of every quote', () => {
        const state: EditorState = {
            ...EMPTY,
            kind: 'series',
            instrument: 'capfloor',
            quote: 'price',
        };
        expect(familyOfEditor(rowOf(SCHEMA, state), state, RULES)?.series_subclass).toBe('premium');
        const rate = { ...state, quote: 'rate' };
        expect(familyOfEditor(rowOf(SCHEMA, rate), rate, RULES)).toEqual({
            asset_class: 'ir',
            series_subclass: 'capfloor',
            index_family: '',
        });
    });

    it('keys a series whose path is its index by that path', () => {
        const state: EditorState = {
            ...EMPTY,
            kind: 'series',
            instrument: 'seasonality',
            quote: 'rate',
            subject: 'EUHICPXT',
        };
        expect(familyOfEditor(rowOf(SCHEMA, state), state, RULES)?.index_family).toBe('EUHICPXT');
    });

    it('names no family before a quote is chosen, or when no rule gives a subclass', () => {
        const state: EditorState = { ...EMPTY, kind: 'series', instrument: 'capfloor' };
        expect(familyOfEditor(rowOf(SCHEMA, state), state, RULES)).toBeNull();
        const ruled = { ...state, quote: 'rate' };
        expect(familyOfEditor(rowOf(SCHEMA, ruled), ruled, [])).toBeNull();
    });
});

describe('what the shape says of each field', () => {
    const shapes = [
        shape(IBOR, 'ccy', 'required'),
        shape(IBOR, 'index_name', 'required'),
        shape(IBOR, 'unit_ccy', 'forbidden'),
        shape({ ...IBOR, index_family: 'swap' }, 'tenor', 'required'),
    ];

    it("marks the family's own rows and no other family's", () => {
        expect([...marksOf(IBOR, shapes)]).toEqual([
            ['ccy', 'required'],
            ['index_name', 'required'],
            ['unit_ccy', 'forbidden'],
        ]);
        expect(marksOf(null, shapes).size).toBe(0);
    });

    it('names a fixing field by the column the writer projects it into', () => {
        expect(columnOf(FIXING, 'name')).toBe('index_name');
        expect(columnOf(FIXING, 'tenor')).toBe('tenor');
        expect(columnOf({ ...FIXING, kind: 'series' }, 'name')).toBe('name');
    });

    it('names a required column that no input on screen writes', () => {
        const marks = marksOf(IBOR, [...shapes, shape(IBOR, 'source', 'required')]);
        expect(unreachableOf(marks, rowOf(SCHEMA, FIXING), FIXING)).toEqual(['source']);
    });
});
