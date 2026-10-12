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
    completeIrFeed,
    curveOf,
    indexTenorOf,
    irSeriesUri,
    parameterError,
    parameterLimits,
    parameterLines,
    parameterPlan,
} from './irCurve.js';

const SOFR = { currency_code: 'USD', index_family: 'sofr', tenor: '' };
const EURIBOR_6M = { currency_code: 'EUR', index_family: 'euribor', tenor: '6M' };

describe('the series a curve publishes', () => {
    it('has the tenor 1D for an overnight index and for the FOMC variant', () => {
        expect(indexTenorOf(SOFR)).toBe('1D');
        expect(indexTenorOf({ ...SOFR, tenor: 'FOMC' })).toBe('1D');
    });

    it('has the index tenor of a term index, so two tenors are two series', () => {
        expect(indexTenorOf(EURIBOR_6M)).toBe('6M');
        expect(irSeriesUri(EURIBOR_6M)).toBe(
            'oresmd://ir/EUR?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=6M',
        );
        expect(irSeriesUri(EURIBOR_6M)).not.toBe(irSeriesUri({ ...EURIBOR_6M, tenor: '3M' }));
        expect(irSeriesUri(SOFR)).toBe(
            'oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D',
        );
    });
});

describe('the name of a curve', () => {
    it('says the currency, the index and the tenor when there is one', () => {
        expect(curveOf(SOFR)).toBe('USD SOFR');
        expect(curveOf(EURIBOR_6M)).toBe('EUR EURIBOR 6M');
    });
});

describe('the fields a curve derives', () => {
    const base = {
        ...SOFR,
        source_name: '',
        price_source: 'fixed',
        vintage_source: 'x',
        vintage_date: 'y',
        vintage_series_uri: 'z',
    };

    it('derives a source name and empties the vintage of a fixed price', () => {
        const done = completeIrFeed(base);
        expect(done['source_name']).toBe('synthetic.usdsofr');
        expect(done['vintage_source']).toBe('');
        expect(done['vintage_date']).toBe('');
        expect(done['vintage_series_uri']).toBe('');
    });

    it('keeps a source name that is given and the vintage of a vintage price', () => {
        const done = completeIrFeed({
            ...base,
            source_name: 'mine',
            price_source: 'vintage',
        });
        expect(done['source_name']).toBe('mine');
        expect(done['vintage_source']).toBe('x');
    });

    it('keeps the tenor of a term index and of the FOMC variant, and drops any other', () => {
        expect(completeIrFeed({ ...EURIBOR_6M, price_source: 'fixed' })['tenor']).toBe('6M');
        expect(completeIrFeed({ ...base, tenor: 'FOMC' })['tenor']).toBe('FOMC');
        expect(completeIrFeed({ ...base, index_family: 'estr', tenor: '3M' })['tenor']).toBe('');
    });
});

const DEFINITIONS = [
    { id: 'd2', process_type_code: 'VASICEK', display_order: 2, default_value: 0.05 },
    { id: 'd1', process_type_code: 'VASICEK', display_order: 1, default_value: 0.5 },
    { id: 'd9', process_type_code: 'HULL_WHITE', display_order: 1, default_value: 0.5 },
];

describe('the parameters of an engine', () => {
    it('lists the engine parameters in screen order, with the default for an unsaved one', () => {
        const lines = parameterLines(DEFINITIONS, [], 'VASICEK');
        expect(lines.map((line) => line.definition['id'])).toEqual(['d1', 'd2']);
        expect(lines.map((line) => line.value)).toEqual(['0.5', '0.05']);
    });

    it('shows the saved value of a parameter the feed holds', () => {
        const saved = [
            { id: 'v1', parameter_definition_id: 'd1', parameter_value: 0.7, version: 3 },
        ];
        const lines = parameterLines(DEFINITIONS, saved, 'VASICEK');
        expect(lines[0]?.value).toBe('0.7');
        expect(lines[0]?.saved).toBe(saved[0]);
        expect(lines[1]?.saved).toBeUndefined();
    });

    it('refuses a value that is not a number or lies outside the bounds', () => {
        const definition = { min_value: 0, max_value: 1 };
        expect(parameterError(definition, '')).toBe('number');
        expect(parameterError(definition, 'abc')).toBe('number');
        expect(parameterError(definition, '-0.1')).toBe('min');
        expect(parameterError(definition, '1.5')).toBe('max');
        expect(parameterError(definition, '0.5')).toBeUndefined();
        expect(parameterError({ min_value: null, max_value: null }, '-3')).toBeUndefined();
    });
});

describe('the writes that save the parameters', () => {
    const saved = [
        { id: 'v1', parameter_definition_id: 'd1', parameter_value: 0.5, version: 3 },
        { id: 'old', parameter_definition_id: 'd9', parameter_value: 0.1, version: 2 },
    ];
    const lines = parameterLines(DEFINITIONS, saved, 'VASICEK');
    let counter = 0;
    const newId = (): string => `new${String(++counter)}`;

    it("writes a new parameter and a changed one, leaves an unchanged one, and removes another engine's", () => {
        const plan = parameterPlan('feed', lines, { d1: '0.5', d2: '0.06' }, saved, newId);
        expect(plan.changes).toEqual([
            {
                write: {
                    id: 'new1',
                    config_id: 'feed',
                    parameter_definition_id: 'd2',
                    parameter_value: 0.06,
                },
                version: null,
            },
        ]);
        expect(plan.removals).toEqual([{ key: 'old', version: 2 }]);
    });

    it('writes a changed saved parameter against the version read', () => {
        const plan = parameterPlan('feed', lines, { d1: '0.9' }, saved, newId);
        expect(plan.changes[0]).toEqual({
            write: {
                id: 'v1',
                config_id: 'feed',
                parameter_definition_id: 'd1',
                parameter_value: 0.9,
            },
            version: 3,
        });
    });
});

describe('what a parameter accepts', () => {
    it('is named by which limits it has', () => {
        expect(parameterLimits(undefined, undefined).kind).toBe('none');
        expect(parameterLimits(0, undefined).kind).toBe('min');
        expect(parameterLimits(undefined, 1).kind).toBe('max');
        expect(parameterLimits(0, 1).kind).toBe('both');
    });
});
