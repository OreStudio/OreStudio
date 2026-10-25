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
 */

import { describe, expect, it } from 'vitest';
import type { PricingDocument, PricingVocabulary } from '../api/pricingEngines.js';
import {
    EMPTY_DRAFT,
    blocked,
    check,
    cloneDraft,
    difference,
    documentationOf,
    draftOf,
    enginesFor,
    isKnownPair,
    methodInfo,
    missingUsual,
    modelInfo,
    modelsFor,
    nameInUse,
    pairChange,
    pairNote,
    pairsFor,
    settingKey,
    settingsFor,
    startsWith,
    templateInfo,
    usualPair,
    withModelAndEngine,
    withParameter,
    withPair,
    withProductTypes,
    withValue,
    withoutParameter,
} from './model.js';

const vocabulary: PricingVocabulary = {
    types: [
        { code: 'BermudanSwaption', description: '', instrumentTypeCode: 'SWPTN' },
        { code: 'Swap', description: '', instrumentTypeCode: 'IRS' },
    ],
    pairs: [
        {
            typeCode: 'BermudanSwaption',
            model: 'LGM',
            engine: 'Grid',
            usual: true,
            help: '',
            description: '',
        },
        {
            typeCode: 'BermudanSwaption',
            model: 'LGM',
            engine: 'MC',
            usual: false,
            help: '',
            description: '',
        },
        {
            typeCode: 'Swap',
            model: 'DiscountedCashflows',
            engine: 'DiscountingSwapEngine',
            usual: true,
            help: '',
            description: '',
        },
        {
            typeCode: 'Swap',
            model: 'CrossAssetModel',
            engine: 'AMC',
            usual: false,
            help: '',
            description: '',
        },
    ],
    settings: [
        {
            typeCode: 'BermudanSwaption',
            model: 'LGM',
            engine: 'Grid',
            scope: 'model',
            name: 'Calibration',
            kind: 'select',
            usual: 'Bootstrap',
            options: ['Bootstrap', 'BestFit'],
            required: true,
            help: '',
            description: '',
        },
        {
            typeCode: 'BermudanSwaption',
            model: 'LGM',
            engine: 'Grid',
            scope: 'engine',
            name: 'sx',
            kind: 'number',
            usual: '3.0',
            options: [],
            required: true,
            help: '',
            description: '',
        },
        {
            typeCode: 'BermudanSwaption',
            model: 'LGM',
            engine: 'Grid',
            scope: 'engine',
            name: 'SensitivityTemplate',
            kind: 'reference',
            usual: 'IR_FD',
            options: [],
            required: false,
            help: '',
            description: '',
        },
    ],
    models: [
        {
            code: 'LGM',
            description: 'Linear Gauss Markov model',
            detail: 'A one-factor Gaussian rates model.',
            source: 'ore_guide',
        },
    ],
    methods: [
        {
            code: 'Grid',
            description: 'Convolution grid engine',
            detail: 'Rolls the value back on a grid.',
            source: 'ore_guide',
        },
    ],
    templates: [
        {
            code: 'IR_FD',
            assetClass: 'IR',
            method: 'Finite difference',
            description: 'Interest rate products priced on a grid.',
        },
    ],
    tenors: [
        { code: '3M', displayName: '3 months' },
        { code: '6M', displayName: '6 months' },
    ],
};

const document: PricingDocument = {
    id: 'c1',
    partyId: 'p1',
    partyName: 'ACME Corporation UK',
    partyCode: 'UK',
    version: 3,
    name: 'EOD pricing',
    description: '',
    variant: 'eod',
    configurationId: 'h1',
    modifiedBy: 'quant',
    recordedAt: '2026-10-19 10:00:00.000000Z',
    changeReason: 'common.correction',
    changeCommentary: '',
    products: [
        {
            id: 'p1',
            version: 1,
            typeCode: 'BermudanSwaption',
            model: 'LGM',
            engine: 'Grid',
            position: 0,
        },
    ],
    parameters: [
        {
            id: 'a1',
            version: 1,
            productId: 'p1',
            scope: 'model',
            name: 'Calibration',
            value: 'Bootstrap',
            position: 0,
        },
        {
            id: 'a2',
            version: 1,
            productId: 'p1',
            scope: 'engine',
            name: 'sx',
            value: '3.0',
            position: 1,
        },
    ],
};

describe('the vocabulary', () => {
    it('puts the usual pair first and finds it', () => {
        expect(pairsFor(vocabulary, 'BermudanSwaption').map((pair) => pair.engine)).toEqual([
            'Grid',
            'MC',
        ]);
        expect(usualPair(vocabulary, 'BermudanSwaption')?.engine).toBe('Grid');
        expect(usualPair(vocabulary, 'Nothing')).toBeUndefined();
    });

    it('knows a pair by type, model and engine together', () => {
        expect(isKnownPair(vocabulary, 'BermudanSwaption', 'LGM', 'Grid')).toBe(true);
        expect(isKnownPair(vocabulary, 'Swap', 'LGM', 'Grid')).toBe(false);
    });

    it('offers the models and engines that price the type before the others', () => {
        expect(modelsFor(vocabulary, 'Swap')).toEqual(['DiscountedCashflows', 'CrossAssetModel']);
        expect(enginesFor(vocabulary, 'BermudanSwaption', 'LGM')).toEqual(['Grid', 'MC']);
    });
});

describe('check', () => {
    it('passes a document that is complete and known', () => {
        const findings = check(draftOf(document), vocabulary);
        expect(blocked(findings)).toBe(false);
        expect(findings.filter((finding) => finding.level === 'error')).toEqual([]);
    });

    it('refuses a document without a name or a product', () => {
        const findings = check(
            { ...draftOf(document), name: ' ', products: [], parameters: [] },
            vocabulary,
        );
        expect(findings.map((finding) => finding.code)).toEqual(['nameMissing', 'noProducts']);
        expect(blocked(findings)).toBe(true);
    });

    it('refuses an empty model and warns of a pair it does not know', () => {
        const draft = draftOf(document);
        const empty = check(
            { ...draft, products: [{ ...draft.products[0]!, model: '' }] },
            vocabulary,
        );
        expect(empty.map((finding) => finding.code)).toContain('modelMissing');
        const unknown = check(
            { ...draft, products: [{ ...draft.products[0]!, engine: 'Odd' }] },
            vocabulary,
        );
        expect(unknown.find((finding) => finding.code === 'pairUnknown')?.level).toBe('warning');
        expect(blocked(unknown)).toBe(false);
    });

    it('refuses a setting that appears twice or has no value', () => {
        const draft = draftOf(document);
        const parameters = [...draft.parameters, { ...draft.parameters[0]!, id: 'a3', value: '' }];
        const codes = check({ ...draft, parameters }, vocabulary).map((finding) => finding.code);
        expect(codes).toContain('parameterRepeated');
        expect(codes).toContain('parameterMissingValue');
    });

    it('says which usual setting a product lacks, as information', () => {
        const draft = draftOf(document);
        const findings = check({ ...draft, parameters: [draft.parameters[0]!] }, vocabulary);
        expect(findings).toContainEqual(
            expect.objectContaining({ level: 'info', code: 'settingUsual', detail: 'sx' }),
        );
    });

    it('warns when a type appears more than once', () => {
        const draft = draftOf(document);
        const twice = { ...draft.products[0]!, id: 'p2', position: 1 };
        const findings = check({ ...draft, products: [...draft.products, twice] }, vocabulary);
        expect(findings.find((finding) => finding.code === 'typeRepeated')).toMatchObject({
            level: 'warning',
            typeCode: 'BermudanSwaption',
            detail: '2',
        });
    });
});

describe('changing a pair', () => {
    it('names the settings the new pair does not take and the ones it usually needs', () => {
        const draft = draftOf(document);
        const change = pairChange(vocabulary, draft, draft.products[0]!, 'LGM', 'MC');
        expect(change.dropped.map((parameter) => parameter.name)).toEqual(['Calibration', 'sx']);
        expect(change.added).toEqual([]);
    });

    it('drops and adds only what was ticked', () => {
        const draft = draftOf(document);
        const dropped = withPair(vocabulary, draft, 'p1', 'LGM', 'MC', {
            drop: [settingKey('engine', 'sx')],
            add: [],
        });
        expect(dropped.products[0]).toMatchObject({ model: 'LGM', engine: 'MC' });
        expect(dropped.parameters.map((parameter) => parameter.name)).toEqual(['Calibration']);
        const kept = withPair(vocabulary, draft, 'p1', 'LGM', 'MC', { drop: [], add: [] });
        expect(kept.parameters).toHaveLength(2);
    });

    it('adds a usual setting the product lacks when it is ticked', () => {
        const draft = draftOf(document);
        const lacking = { ...draft, parameters: [draft.parameters[0]!] };
        const added = withPair(vocabulary, lacking, 'p1', 'LGM', 'Grid', {
            drop: [],
            add: [settingKey('engine', 'sx')],
        });
        expect(added.parameters.map((parameter) => [parameter.name, parameter.value])).toEqual([
            ['Calibration', 'Bootstrap'],
            ['sx', '3.0'],
        ]);
        expect(added.parameters[1]?.version).toBe(0);
    });
});

describe('difference', () => {
    it('is empty for the same draft', () => {
        expect(difference(draftOf(document), draftOf(document))).toEqual([]);
    });

    it('states a changed pair, a changed value, an added product and a removed setting', () => {
        const before = draftOf(document);
        const after = withPair(vocabulary, before, 'p1', 'LGM', 'MC', {
            drop: [settingKey('engine', 'sx')],
            add: [],
        });
        const edited = {
            ...after,
            parameters: after.parameters.map((parameter) =>
                parameter.name === 'Calibration' ? { ...parameter, value: 'BestFit' } : parameter,
            ),
            products: [
                ...after.products,
                {
                    id: 'p9',
                    version: 0,
                    typeCode: 'Swap',
                    model: 'DiscountedCashflows',
                    engine: 'DiscountingSwapEngine',
                    position: 1,
                },
            ],
        };
        const lines = difference(before, edited).map(
            (change) => `${change.kind} ${change.what} ${change.name}`,
        );
        expect(lines).toEqual([
            'changed product BermudanSwaption',
            'added product Swap',
            'changed parameter Calibration',
            'removed parameter sx',
        ]);
    });
});

describe('authoring', () => {
    it('copies a configuration under a new name with new identifiers and version zero', () => {
        const copy = cloneDraft(draftOf(document), 'Copy of EOD pricing');
        expect(copy.name).toBe('Copy of EOD pricing');
        expect(copy.products[0]?.id).not.toBe('p1');
        expect(copy.products[0]?.version).toBe(0);
        expect(
            copy.parameters.every((parameter) => parameter.productId === copy.products[0]?.id),
        ).toBe(true);
        expect(
            copy.parameters.every(
                (parameter) => parameter.version === 0 && !['a1', 'a2'].includes(parameter.id),
            ),
        ).toBe(true);
        expect(difference(draftOf(document), copy).length).toBeGreaterThan(0);
    });

    it('refuses a name another configuration holds, ignoring case and the configuration itself', () => {
        const held = [{ id: 'c1', name: 'EOD pricing' }];
        expect(nameInUse(held, 'eod PRICING')).toBe(true);
        expect(nameInUse(held, 'eod pricing', 'c1')).toBe(false);
        expect(nameInUse(held, '  ')).toBe(false);
        expect(nameInUse(held, 'Intraday')).toBe(false);
    });

    it('starts a new product type on the usual pair and its usual settings', () => {
        const draft = withProductTypes(vocabulary, EMPTY_DRAFT, ['BermudanSwaption']);
        expect(draft.products).toHaveLength(1);
        expect(draft.products[0]).toMatchObject({ model: 'LGM', engine: 'Grid', version: 0 });
        expect(draft.parameters.map((parameter) => parameter.name)).toEqual(['Calibration', 'sx']);
    });

    it('keeps a type that stays ticked with its edits, and drops one that is unticked with its settings', () => {
        const first = withProductTypes(vocabulary, EMPTY_DRAFT, ['BermudanSwaption', 'Swap']);
        const bermudan = first.products.find((product) => product.typeCode === 'BermudanSwaption')!;
        const edited = withValue(first, first.parameters[0]!.id, 'BestFit');
        const second = withProductTypes(vocabulary, edited, ['BermudanSwaption']);
        expect(second.products.map((product) => product.id)).toEqual([bermudan.id]);
        expect(second.parameters.find((parameter) => parameter.name === 'Calibration')?.value).toBe(
            'BestFit',
        );
        const third = withProductTypes(vocabulary, second, []);
        expect(third.products).toEqual([]);
        expect(third.parameters).toEqual([]);
    });

    it('sets a model and engine, adds a setting, changes a value and removes a setting', () => {
        const draft = withProductTypes(vocabulary, EMPTY_DRAFT, ['Swap']);
        const id = draft.products[0]!.id;
        const set = withModelAndEngine(draft, id, 'CrossAssetModel', 'AMC');
        expect(set.products[0]).toMatchObject({ model: 'CrossAssetModel', engine: 'AMC' });
        const added = withParameter(set, id, 'engine', 'Scheme', 'Douglas');
        expect(parametersOfNames(added)).toEqual(['Scheme']);
        const changed = withValue(added, added.parameters[0]!.id, 'Other');
        expect(changed.parameters[0]?.value).toBe('Other');
        expect(withoutParameter(changed, changed.parameters[0]!.id).parameters).toEqual([]);
    });

    it('names the usual settings a product lacks', () => {
        const draft = withProductTypes(vocabulary, EMPTY_DRAFT, ['BermudanSwaption']);
        const without = withoutParameter(draft, draft.parameters[1]!.id);
        expect(
            missingUsual(vocabulary, without, without.products[0]!).map((setting) => setting.name),
        ).toEqual(['sx']);
        expect(missingUsual(vocabulary, draft, draft.products[0]!)).toEqual([]);
    });
});

describe('the words the vocabulary holds', () => {
    it('finds what a model, an engine and a sensitivity template are', () => {
        expect(modelInfo(vocabulary, 'LGM')?.detail).toBe('A one-factor Gaussian rates model.');
        expect(methodInfo(vocabulary, 'Grid')?.description).toBe('Convolution grid engine');
        expect(templateInfo(vocabulary, 'IR_FD')?.method).toBe('Finite difference');
        expect(modelInfo(vocabulary, 'Unknown')).toBeUndefined();
    });

    it('finds how ORE builds the engine of a pair, or says nothing', () => {
        const noted: PricingVocabulary = {
            ...vocabulary,
            pairs: [
                {
                    ...vocabulary.pairs[0]!,
                    help: 'Builds a grid engine.',
                    description: 'Builds a grid engine.',
                },
            ],
        };
        expect(pairNote(noted, 'BermudanSwaption', 'LGM', 'Grid')).toBe('Builds a grid engine.');
        expect(pairNote(noted, 'BermudanSwaption', 'LGM', 'MC')).toBe('');
    });

    it('starts a product with the settings ORE needs and leaves the optional ones out', () => {
        const names = vocabulary.settings.filter(startsWith).map((setting) => setting.name);
        expect(names).toEqual(['Calibration', 'sx']);
        const draft = withProductTypes(vocabulary, EMPTY_DRAFT, ['BermudanSwaption']);
        expect(parametersOfNames(draft)).toEqual(['Calibration', 'sx']);
    });

    it('lists the settings ORE needs first, then the optional ones', () => {
        const names = settingsFor(vocabulary, 'BermudanSwaption', 'LGM', 'Grid').map(
            (setting) => setting.name,
        );
        expect(names).toEqual(['Calibration', 'sx', 'SensitivityTemplate']);
    });

    it('finds a setting under any pair that names it', () => {
        const named: PricingVocabulary = {
            ...vocabulary,
            settings: [
                ...vocabulary.settings,
                {
                    typeCode: 'Swap',
                    model: 'CrossAssetModel',
                    engine: 'AMC',
                    scope: 'engine',
                    name: 'Training.Seed',
                    kind: 'number',
                    usual: '42',
                    options: [],
                    required: false,
                    documented: true,
                    label: 'Training seed',
                    help: 'The seed.',
                    description: 'The seed of the random engine.',
                    defaultText: '42',
                    format: 'A number.',
                    introducedIn: '1.8.7.0',
                    removedIn: '',
                    xpath: "/PricingEngines/Product/EngineParameters/Parameter[@name='Training.Seed']",
                },
            ],
        };
        expect(
            documentationOf(named, 'Swap', 'CrossAssetModel', 'AMC', 'engine', 'Training.Seed')
                ?.description,
        ).toBe('The seed of the random engine.');
        expect(
            documentationOf(named, 'BermudanSwaption', 'LGM', 'Grid', 'engine', 'Training.Seed')
                ?.label,
        ).toBe('Training seed');
        expect(
            documentationOf(named, 'Swap', 'CrossAssetModel', 'AMC', 'engine', 'Unknown'),
        ).toBeUndefined();
    });

    it('reports only a missing setting that ORE needs', () => {
        const draft = withProductTypes(vocabulary, EMPTY_DRAFT, ['BermudanSwaption']);
        const without = withoutParameter(draft, draft.parameters[1]!.id);
        const named = (d: typeof draft) =>
            check({ ...d, name: 'x' }, vocabulary)
                .filter((finding) => finding.code === 'settingUsual')
                .map((finding) => finding.detail);
        expect(named(without)).toEqual(['sx']);
        expect(named(draft)).toEqual([]);
    });
});

function parametersOfNames(draft: ReturnType<typeof draftOf>): string[] {
    return draft.parameters.map((parameter) => parameter.name);
}
