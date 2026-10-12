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
    leanOf,
    parameterValues,
    percentText,
    plain,
    rateOf,
    readoutOf,
    typedFrom,
    viewOf,
} from './irSimple.js';
import type { ParameterLine } from './irCurve.js';

const line = (id: string, name: string, value: string): ParameterLine => ({
    definition: { id, parameter_name: name },
    saved: undefined,
    value,
});

const view = {
    start_rate: 0.03,
    long_run_rate: 0.045,
    half_life_years: 1.386,
    yearly_move_bp: 100,
};

describe('irSimple', () => {
    it('writes a number the way a person types it', () => {
        expect(plain(0.1 + 0.2)).toBe('0.3');
        expect(percentText(0.045)).toBe('4.5');
        expect(rateOf(4.5)).toBe(0.045);
    });

    it('reads the values on the screen by parameter name', () => {
        const lines = [line('a', 'kappa', '0.5'), line('b', 'theta', '0.03')];
        expect(parameterValues(lines, { b: '0.04' })).toEqual({ kappa: 0.5, theta: 0.04 });
    });

    it('has no values while one is empty or not a number', () => {
        const lines = [line('a', 'kappa', '0.5'), line('b', 'theta', '0.03')];
        expect(parameterValues(lines, { b: '' })).toBeUndefined();
        expect(parameterValues(lines, { b: 'x' })).toBeUndefined();
        expect(parameterValues([], {})).toBeUndefined();
    });

    it('puts what the service returned back into the draft by definition id', () => {
        const lines = [line('a', 'kappa', '0.5'), line('b', 'theta', '0.03')];
        expect(typedFrom(lines, { kappa: 0.34657359, other: 1 })).toEqual({ a: '0.34657359' });
    });

    it('says which way the curve leans', () => {
        expect(leanOf(view)).toBe('up');
        expect(leanOf({ ...view, start_rate: 0.06 })).toBe('down');
        expect(leanOf({ ...view, start_rate: 0.0452 })).toBe('flat');
    });

    it('takes the view only when the service returned all four answers', () => {
        expect(
            viewOf({
                start_rate: 0.03,
                long_run_rate: 0.045,
                half_life_years: 2,
                yearly_move_bp: 90,
            }),
        ).toBeDefined();
        expect(viewOf({ start_rate: 0.03 })).toBeUndefined();
    });

    it('says what each parameter means in the words of the view', () => {
        expect(readoutOf('kappa', view)).toEqual({ key: 'halfLife', value: '1.39' });
        expect(readoutOf('theta', view)).toEqual({ key: 'rate', value: '4.5' });
        expect(readoutOf('initial_rate', view)).toEqual({ key: 'rate', value: '3' });
        expect(readoutOf('sigma', view)).toEqual({ key: 'move', value: '100' });
        expect(readoutOf('rho', view)).toBeUndefined();
    });
});
