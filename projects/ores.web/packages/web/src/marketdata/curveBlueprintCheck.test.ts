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
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

import { describe, expect, it } from 'vitest';
import type { RecordRow } from '../api/client.js';
import {
    buildPoints,
    checkRecipe,
    compareTenors,
    findingsOf,
    inMaturityOrder,
    splitTenorPasses,
    tenorRank,
    unfilledPillars,
} from './curveBlueprintCheck.js';

function row(fields: Readonly<Record<string, unknown>>): RecordRow {
    return { version: 1, ...fields };
}

const pillar = (start: string, end: string, role = 'SWAP'): RecordRow =>
    row({
        id: `${start}-${end}`,
        start_tenor_code: start,
        end_tenor_code: end,
        curve_role_code: role,
    });

const config = (fields: Readonly<Record<string, unknown>> = {}): RecordRow =>
    row({
        id: 'config-1',
        currency_code: 'USD',
        index_tenor: '1D',
        curve_family_role: 'FUNDING',
        discount_curve_config_id: null,
        interpolation_method: 'LOG_LINEAR_DISCOUNT',
        split_tenor_code: '1Y',
        ...fields,
    });

describe('the maturity order of a ladder', () => {
    it('puts spot first, then the periods by duration', () => {
        expect(['1Y', 'SPOT', '3M', '1D'].sort(compareTenors)).toEqual(['SPOT', '1D', '3M', '1Y']);
    });

    it('puts a schedule-resolved tenor after spot and before the swap grid', () => {
        expect(['1Y', '2F', 'SPOT', '1F'].sort(compareTenors)).toEqual(['SPOT', '1F', '2F', '1Y']);
    });

    it('orders a ladder of rows by its end tenor', () => {
        const ordered = inMaturityOrder([
            pillar('SPOT', '10Y'),
            pillar('SPOT', '1Y'),
            pillar('SPOT', '1M'),
        ]);
        expect(ordered.map((one) => one['end_tenor_code'])).toEqual(['1M', '1Y', '10Y']);
    });

    it('reads a tenor the ladder cannot place after every other', () => {
        expect(tenorRank('NONSENSE')).toBe(999);
        expect(tenorRank('6M')).toBe(1006);
    });
});

describe('the split tenor', () => {
    const pillars = [pillar('SPOT', '1M'), pillar('SPOT', '1Y')];

    it('is the last pillar for a single-segment method', () => {
        expect(splitTenorPasses('LOG_LINEAR_DISCOUNT', '1Y', pillars)).toBe(true);
        expect(splitTenorPasses('LOG_LINEAR_DISCOUNT', '6M', pillars)).toBe(false);
    });

    it('is a tenor inside the ladder for the two-segment method', () => {
        expect(splitTenorPasses('FLAT_FORWARD_THEN_LOG_LINEAR', '6M', pillars)).toBe(true);
        expect(splitTenorPasses('FLAT_FORWARD_THEN_LOG_LINEAR', '1M', pillars)).toBe(true);
        expect(splitTenorPasses('FLAT_FORWARD_THEN_LOG_LINEAR', '2Y', pillars)).toBe(false);
        expect(splitTenorPasses('FLAT_FORWARD_THEN_LOG_LINEAR', 'SPOT', pillars)).toBe(false);
    });
});

describe('the recipe checks', () => {
    const sound = [pillar('SPOT', '1M', 'DEPOSIT'), pillar('SPOT', '1Y', 'SWAP')];

    it('needs no discount curve on a funding recipe, which has none', () => {
        const checks = checkRecipe({ config: config(), pillars: sound, discount: undefined });
        expect(checks.find((check) => check.id === 'projection_funding')?.passed).toBe(true);
        expect(checks.find((check) => check.id === 'funding_not_projection')?.passed).toBe(true);
    });

    it('fails a projection that names no discount curve', () => {
        const checks = checkRecipe({
            config: config({ curve_family_role: 'PROJECTION', discount_curve_config_id: null }),
            pillars: sound,
            discount: undefined,
        });
        expect(checks.find((check) => check.id === 'projection_funding')?.passed).toBe(false);
        expect(checks.find((check) => check.id === 'funding_not_projection')?.passed).toBe(false);
    });

    it('passes every check on a sound funding recipe', () => {
        const checks = checkRecipe({ config: config(), pillars: sound, discount: undefined });
        expect(checks.map((check) => [check.id, check.passed])).toEqual([
            ['one_pillar', true],
            ['maturity_order', true],
            ['deposit_from_spot', true],
            ['split_tenor', true],
            ['projection_funding', true],
            ['funding_not_projection', true],
        ]);
    });

    it('fails the first check and the split tenor when the recipe holds no pillar', () => {
        const checks = checkRecipe({ config: config(), pillars: [], discount: undefined });
        expect(checks.filter((check) => !check.passed).map((check) => check.id)).toEqual([
            'one_pillar',
            'split_tenor',
        ]);
    });

    it('fails a ladder whose pillars do not each mature after the one before', () => {
        const checks = checkRecipe({
            config: config(),
            pillars: [pillar('SPOT', '10Y'), pillar('SPOT', '10Y')],
            discount: undefined,
        });
        expect(checks.filter((check) => !check.passed).map((check) => check.id)).toEqual([
            'maturity_order',
            'split_tenor',
        ]);
    });

    it('fails a deposit that does not start at spot', () => {
        const checks = checkRecipe({
            config: config(),
            pillars: [pillar('3M', '1Y', 'DEPOSIT')],
            discount: undefined,
        });
        expect(checks.find((check) => check.id === 'deposit_from_spot')?.passed).toBe(false);
    });

    it('fails a projection that names a blueprint the reader cannot see', () => {
        const checks = checkRecipe({
            config: config({
                curve_family_role: 'PROJECTION',
                discount_curve_config_id: 'config-2',
            }),
            pillars: sound,
            discount: undefined,
        });
        expect(checks.filter((check) => !check.passed).map((check) => check.id)).toEqual([
            'projection_funding',
            'funding_not_projection',
        ]);
    });

    it('fails a projection that names itself', () => {
        const projection = config({
            curve_family_role: 'PROJECTION',
            discount_curve_config_id: 'config-1',
        });
        const checks = checkRecipe({ config: projection, pillars: sound, discount: projection });
        expect(checks.find((check) => check.id === 'projection_funding')?.passed).toBe(false);
    });

    it('fails a projection that names a projection', () => {
        const other = config({ id: 'config-2', curve_family_role: 'PROJECTION' });
        const checks = checkRecipe({
            config: config({
                curve_family_role: 'PROJECTION',
                discount_curve_config_id: 'config-2',
            }),
            pillars: sound,
            discount: other,
        });
        expect(checks.find((check) => check.id === 'projection_funding')?.passed).toBe(false);
        expect(checks.find((check) => check.id === 'funding_not_projection')?.passed).toBe(false);
    });

    it('passes a projection of another currency nowhere', () => {
        const other = config({ id: 'config-2', currency_code: 'EUR' });
        const checks = checkRecipe({
            config: config({
                curve_family_role: 'PROJECTION',
                discount_curve_config_id: 'config-2',
            }),
            pillars: sound,
            discount: other,
        });
        expect(checks.find((check) => check.id === 'projection_funding')?.passed).toBe(false);
    });
});

describe('the points a build returns', () => {
    const pillars = [pillar('SPOT', '1Y'), pillar('SPOT', '2Y')];
    const computed = [
        { pointId: '1Y', date: '2027-10-07', discountFactor: 0.97 },
        { pointId: '2Y', date: '2028-10-07', discountFactor: 0.94 },
    ];

    it('quotes each forward over the days to the next point', () => {
        const points = buildPoints(pillars, computed);
        expect(points).toHaveLength(2);
        expect(points[0]?.pillar).toBe('1Y');
        expect(points[0]?.forward).toBeCloseTo((0.97 / 0.94 - 1) / (366 / 365), 10);
        expect(points[1]?.forward).toBeNull();
    });

    it('keeps a pillar the market could not fill out of the points and names it', () => {
        const points = buildPoints(pillars, [computed[0]!]);
        expect(points.map((point) => point.pillar)).toEqual(['1Y']);
        expect(unfilledPillars(pillars, [computed[0]!])).toEqual(['2Y']);
    });

    it('finds a forward below zero and a jump between neighbours', () => {
        const points = [
            {
                pillar: '1Y',
                date: '2027-01-01',
                discountFactor: 0.97,
                forward: 0.05,
                daysToNext: 365,
            },
            {
                pillar: '2Y',
                date: '2028-01-01',
                discountFactor: 0.94,
                forward: -0.01,
                daysToNext: 365,
            },
            {
                pillar: '3Y',
                date: '2029-01-01',
                discountFactor: 0.9,
                forward: 0.04,
                daysToNext: null,
            },
        ];
        const found = findingsOf(points, []);
        expect(found.map((finding) => [finding.kind, finding.pillar])).toEqual([
            ['negative_forward', '2Y'],
            ['jump', '2Y'],
            ['jump', '3Y'],
        ]);
    });

    it('names a gap the market left, and no other finding', () => {
        const points = [
            {
                pillar: '1Y',
                date: '2027-01-01',
                discountFactor: 0.97,
                forward: 0.05,
                daysToNext: 365,
            },
            {
                pillar: '2Y',
                date: '2028-01-01',
                discountFactor: 0.94,
                forward: 0.05,
                daysToNext: null,
            },
        ];
        expect(findingsOf(points, ['3Y'])).toEqual([{ kind: 'gap', pillar: '3Y', value: null }]);
    });
});
