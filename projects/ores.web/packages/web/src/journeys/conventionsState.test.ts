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
import type { ConventionRow } from './conventionsServer.js';
import {
    blankDraft,
    changesOf,
    reduceConvention,
    termProblems,
    termText,
    termValue,
    writePlan,
    type ConventionAction,
    type ConventionDraft,
} from './conventionsState.js';

const swap: ConventionRow = {
    version: 3,
    id: 'swap-1',
    party_id: 'party-1',
    fixed_calendar: 'TARGET',
    fixed_frequency: 'Annual',
    fixed_convention: null,
    fixed_day_count_fraction: '30/360',
    index: 'EUR-EURIBOR-6M',
    float_frequency: null,
    sub_periods_coupon_type: null,
    oresmd_uri: null,
};

function walk(
    family: string,
    row: ConventionRow | undefined,
    ...actions: readonly ConventionAction[]
): ConventionDraft {
    const start: ConventionAction[] = [
        { kind: 'choose-family', family },
        row === undefined ? { kind: 'start-convention' } : { kind: 'open-convention', row },
    ];
    return [...start, ...actions].reduce(reduceConvention, blankDraft());
}

describe('convention terms', () => {
    it('turns a column value into form text and back', () => {
        expect(termText('pick', null)).toBe('');
        expect(termText('flag', null)).toBe('false');
        expect(termText('integer', 2)).toBe('2');
        expect(termValue('pick', ' ')).toBeNull();
        expect(termValue('flag', 'true')).toBe(true);
        expect(termValue('tristate', '')).toBeNull();
        expect(termValue('tristate', 'false')).toBe(false);
        expect(termValue('integer', '2')).toBe(2);
        expect(termValue('integer', '')).toBeNull();
    });

    it('has no change until the person changes a term', () => {
        expect(changesOf(walk('swap', swap))).toEqual([]);
    });

    it('lists a changed term with its before and after', () => {
        const changes = changesOf(
            walk('swap', swap, {
                kind: 'set-term',
                column: 'fixed_frequency',
                value: 'Semiannual',
            }),
        );
        expect(changes).toEqual([
            expect.objectContaining({
                column: 'fixed_frequency',
                before: 'Annual',
                after: 'Semiannual',
            }),
        ]);
    });

    it('writes a changed convention against the version read, keeping the row it read', () => {
        const plan = writePlan(
            walk('swap', swap, { kind: 'set-term', column: 'index', value: 'EUR-EURIBOR-3M' }),
            'party-1',
        );
        expect(plan?.version).toBe(3);
        expect(plan?.write).toMatchObject({
            id: 'swap-1',
            index: 'EUR-EURIBOR-3M',
            fixed_convention: null,
        });
        expect(plan?.intent.reason_code).toBe('common.non_material_update');
    });

    it('writes a new convention as new, under the new-record reason', () => {
        const draft = walk('swap', undefined, {
            kind: 'set-term',
            column: 'fixed_frequency',
            value: 'Annual',
        });
        const plan = writePlan(draft, 'party-1');
        expect(plan?.version).toBeNull();
        expect(plan?.intent.reason_code).toBe('system.new_record');
        expect(plan?.write['party_id']).toBe('party-1');
    });

    it('has no plan before a convention is being authored', () => {
        expect(
            writePlan(
                reduceConvention(blankDraft(), { kind: 'choose-family', family: 'swap' }),
                'p',
            ),
        ).toBeUndefined();
    });

    it('names the required swap terms a new convention lacks', () => {
        expect(termProblems(walk('swap', undefined)).sort()).toEqual([
            'fixed_day_count_fraction',
            'fixed_frequency',
            'index',
        ]);
        expect(termProblems(walk('swap', swap))).toEqual([]);
    });

    it('asks a deposit for its index when it is index based, and for its own terms when not', () => {
        const based = walk('deposit', undefined, {
            kind: 'set-term',
            column: 'index_based',
            value: 'true',
        });
        expect(termProblems(based)).toEqual(['index']);
        const own = walk('deposit', undefined);
        expect(termProblems(own).sort()).toEqual(['day_count_fraction', 'settlement_days']);
    });

    it('refuses settlement days that are not a whole number', () => {
        const draft = walk('deposit', undefined, {
            kind: 'set-term',
            column: 'settlement_days',
            value: '2.5',
        });
        expect(termProblems(draft)).toContain('settlement_days');
    });

    it('draws no terms for a family whose terms are not drawn', () => {
        const draft = walk('fra', undefined);
        expect(changesOf(draft)).toEqual([]);
        expect(termProblems(draft)).toEqual([]);
    });
});
