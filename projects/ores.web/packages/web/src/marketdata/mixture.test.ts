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
    densityOf,
    draftOf,
    gaussiansOf,
    newComponent,
    problemsOf,
    changesOf,
    removedFrom,
    savePlan,
    sharesOf,
    weightSum,
    type DraftComponent,
} from './mixture.js';

const FEED = 'f0000000-0000-4000-8000-000000000000';

function saved(id: string, index: number, weight: number, version = 3): DraftComponent {
    return draftOf([
        {
            id,
            version,
            component_index: index,
            description: `c${String(index)}`,
            mean: 0,
            stdev: 0.01,
            weight,
        },
    ])[0] as DraftComponent;
}

describe('the mixture draft', () => {
    it('reads the saved components in the order of their index', () => {
        const draft = draftOf([
            {
                id: 'b',
                version: 1,
                component_index: 1,
                description: '',
                mean: 0,
                stdev: 1,
                weight: 1,
            },
            {
                id: 'a',
                version: 2,
                component_index: 0,
                description: '',
                mean: 0,
                stdev: 1,
                weight: 1,
            },
        ]);
        expect(draft.map((component) => component.id)).toEqual(['a', 'b']);
        expect(draft[0]?.version).toBe(2);
    });

    it('starts a new component as a plain Gaussian that is not saved yet', () => {
        expect(newComponent('n')).toEqual({
            id: 'n',
            version: null,
            description: '',
            mean: '0',
            stdev: '0.01',
            weight: '1',
        });
    });

    it('reads no numbers while one of them is half typed', () => {
        expect(gaussiansOf([{ ...newComponent('n'), mean: '-' }])).toBeUndefined();
        expect(gaussiansOf([{ ...newComponent('n'), weight: '' }])).toBeUndefined();
        expect(gaussiansOf([newComponent('n')])).toEqual([{ mean: 0, stdev: 0.01, weight: 1 }]);
    });

    it('adds the weights as typed', () => {
        expect(
            weightSum([
                { mean: 0, stdev: 1, weight: 0.5 },
                { mean: 0, stdev: 1, weight: 0.4 },
            ]),
        ).toBeCloseTo(0.9);
    });
});

describe('what stops a mixture from being saved', () => {
    it('needs at least one component', () => {
        expect(problemsOf([])).toEqual(['empty']);
    });

    it('needs every value to be a number', () => {
        expect(problemsOf([{ ...newComponent('n'), stdev: 'wide' }])).toEqual(['number']);
    });

    it('refuses a negative deviation or weight, and weights that add up to nothing', () => {
        expect(problemsOf([{ ...newComponent('n'), stdev: '-0.1' }])).toEqual(['negative']);
        expect(problemsOf([{ ...newComponent('n'), weight: '0' }])).toEqual(['weights']);
    });

    it('lets a weight sum other than one through, because the draw normalises it', () => {
        expect(
            problemsOf([
                { ...newComponent('a'), weight: '0.5' },
                { ...newComponent('b'), weight: '0.4' },
            ]),
        ).toEqual([]);
    });
});

describe('the density the mixture implies', () => {
    it('integrates to one, with the weights normalised', () => {
        const { xs, mixture } = densityOf(
            [
                { mean: 0, stdev: 1, weight: 2 },
                { mean: 1, stdev: 0.5, weight: 2 },
            ],
            2001,
        );
        const step = (xs[1] ?? 0) - (xs[0] ?? 0);
        const area = mixture.reduce((sum, value) => sum + value * step, 0);
        expect(area).toBeCloseTo(1, 2);
    });

    it('is the sum of its weighted parts', () => {
        const { mixture, parts } = densityOf([
            { mean: 0, stdev: 1, weight: 0.8 },
            { mean: 0.5, stdev: 2, weight: 0.2 },
        ]);
        expect(parts).toHaveLength(2);
        expect(mixture[70]).toBeCloseTo((parts[0]?.[70] ?? 0) + (parts[1]?.[70] ?? 0));
    });

    it('draws nothing when no component has a deviation', () => {
        expect(densityOf([{ mean: 0, stdev: 0, weight: 1 }]).xs).toEqual([]);
    });
});

describe('the save plan', () => {
    it('writes nothing when the draft is the saved mixture', () => {
        const before = [saved('a', 0, 0.8), saved('b', 1, 0.2)];
        expect(savePlan(FEED, before, before)).toEqual({ changes: [], removals: [] });
    });

    it('writes a changed component against the version read, and a new one as new', () => {
        const before = [saved('a', 0, 0.8, 4)];
        const draft = [{ ...saved('a', 0, 0.8, 4), weight: '1' }, newComponent('n')];
        const plan = savePlan(FEED, before, draft);
        expect(plan.removals).toEqual([]);
        expect(plan.changes).toEqual([
            {
                write: {
                    id: 'a',
                    fx_spot_config_id: FEED,
                    component_index: 0,
                    description: 'c0',
                    mean: 0,
                    stdev: 0.01,
                    weight: 1,
                },
                version: 4,
            },
            {
                write: {
                    id: 'n',
                    fx_spot_config_id: FEED,
                    component_index: 1,
                    description: '',
                    mean: 0,
                    stdev: 0.01,
                    weight: 1,
                },
                version: null,
            },
        ]);
    });

    it('removes a component the draft dropped, and reindexes the ones after it', () => {
        const before = [saved('a', 0, 0.5, 2), saved('b', 1, 0.5, 5)];
        const plan = savePlan(FEED, before, [before[1] as DraftComponent]);
        expect(plan.removals).toEqual([{ key: 'a', version: 2 }]);
        expect(plan.changes.map((change) => change.write['component_index'])).toEqual([0]);
        expect(plan.changes[0]?.version).toBe(5);
    });
});

describe('what the screen marks', () => {
    it('gives each component its share of the draws, as the draw normalises the weights', () => {
        const shares = sharesOf([saved('a', 0, 0.97), saved('b', 1, 0.5)]);
        expect(shares[0]).toBeCloseTo(0.97 / 1.47);
        expect(shares[1]).toBeCloseTo(0.5 / 1.47);
        expect(sharesOf([saved('a', 0, 0)])).toEqual([undefined]);
        expect(sharesOf([{ ...saved('a', 0, 1), weight: 'x' }])).toEqual([undefined]);
    });

    it('marks a new row, a moved row and the fields a row changed', () => {
        const before = [saved('a', 0, 0.8), saved('b', 1, 0.2)];
        const draft = [
            { ...saved('b', 1, 0.2), weight: '0.25' },
            saved('a', 0, 0.8),
            newComponent('n'),
        ];
        const changes = changesOf(before, draft);
        expect(changes[0]).toEqual({ added: false, moved: true, fields: new Set(['weight']) });
        expect(changes[1]).toEqual({ added: false, moved: true, fields: new Set() });
        expect(changes[2]?.added).toBe(true);
    });

    it('reads 0.10 typed for a saved 0.1 as no change', () => {
        const before = [saved('a', 0, 0.1)];
        expect(changesOf(before, [{ ...saved('a', 0, 0.1), weight: '0.10' }])[0]?.fields.size).toBe(
            0,
        );
    });

    it('names the saved components the draft deleted', () => {
        const before = [saved('a', 0, 0.8), saved('b', 1, 0.2)];
        expect(removedFrom(before, [before[1] as DraftComponent]).map((c) => c.id)).toEqual(['a']);
    });
});
