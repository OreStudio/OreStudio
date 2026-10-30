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
import { controlOfValueSet, type ValueSetFields } from './control.js';

/** A value set that states nothing beyond what a case gives it. */
function fields(overrides: Partial<ValueSetFields>): ValueSetFields {
    return {
        valueSet: '',
        sourceKind: '',
        choices: [],
        minimum: null,
        maximum: null,
        pattern: '',
        ...overrides,
    };
}

const CALIBRATION = [
    { value: 'Bootstrap', label: 'Bootstrap calibration' },
    { value: 'BestFit', label: 'Best fit' },
];

describe('the control a pricing value set calls for', () => {
    it('draws the choices an enumerated set states, as their labels', () => {
        expect(
            controlOfValueSet(fields({ sourceKind: 'enumerated', choices: CALIBRATION })),
        ).toEqual({ kind: 'choice', choices: CALIBRATION });
    });

    it('draws the choices a catalogue states', () => {
        expect(
            controlOfValueSet(
                fields({
                    sourceKind: 'catalogue',
                    choices: [{ value: '3M', label: '3 months' }],
                }),
            ),
        ).toEqual({ kind: 'choice', choices: [{ value: '3M', label: '3 months' }] });
    });

    it('lets the choices decide when the set names no kind this build knows', () => {
        expect(controlOfValueSet(fields({ sourceKind: '', choices: CALIBRATION }))).toEqual({
            kind: 'choice',
            choices: CALIBRATION,
        });
    });

    it('draws a boolean as true and false', () => {
        expect(
            controlOfValueSet(
                fields({
                    sourceKind: 'boolean',
                    choices: [
                        { value: 'true', label: 'true' },
                        { value: 'false', label: 'false' },
                    ],
                }),
            ),
        ).toEqual({ kind: 'boolean' });
    });

    it('bounds a range by the numbers it states, as a number that need not be whole', () => {
        expect(
            controlOfValueSet(fields({ sourceKind: 'range', minimum: 0.5, maximum: 2.5 })),
        ).toEqual({ kind: 'number', minimum: 0.5, maximum: 2.5, whole: false });
    });

    it('draws an integer as a whole number', () => {
        expect(
            controlOfValueSet(fields({ sourceKind: 'integer', minimum: 1, maximum: 500 })),
        ).toEqual({ kind: 'number', minimum: 1, maximum: 500, whole: true });
    });

    it('draws a number by its bounds, whole only when the set says an integer', () => {
        expect(controlOfValueSet(fields({ sourceKind: 'number', minimum: 0 }))).toEqual({
            kind: 'number',
            minimum: 0,
            maximum: null,
            whole: false,
        });
    });

    it('draws text with the pattern the set states', () => {
        expect(controlOfValueSet(fields({ sourceKind: 'text', pattern: '^[A-Z]+$' }))).toEqual({
            kind: 'text',
            pattern: '^[A-Z]+$',
        });
    });

    it('draws a regular expression as a text that must match its pattern', () => {
        expect(controlOfValueSet(fields({ sourceKind: 'regex', pattern: '\\d{1,9}' }))).toEqual({
            kind: 'text',
            pattern: '\\d{1,9}',
        });
    });

    it('draws a set of no stated kind, and a setting with no set at all, as free text', () => {
        expect(controlOfValueSet(fields({ sourceKind: '' }))).toEqual({
            kind: 'text',
            pattern: '',
        });
        expect(controlOfValueSet(undefined)).toEqual({ kind: 'text', pattern: '' });
    });
});
