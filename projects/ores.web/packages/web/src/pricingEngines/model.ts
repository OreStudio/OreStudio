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

import type {
    PricingDocument,
    PricingParameter,
    PricingProduct,
    PricingVocabulary,
} from '../api/pricingEngines.js';

/**
 * What the pricing engine screens know about a configuration, apart from the
 * screens: the pairs that price a product type, the settings a pair takes, the
 * checks a document must pass and the difference between two versions.
 *
 * Every function is pure. The vocabulary arrives as an argument, so the same
 * checks serve the author journey, the change dialog and the import.
 */

/** The part of a document a person edits, with every id the document holds. */
export interface Draft {
    readonly name: string;
    readonly description: string;
    readonly variant: string;
    readonly products: readonly PricingProduct[];
    readonly parameters: readonly PricingParameter[];
}

/** A setting a pair is known to take. */
export type KnownSetting = PricingVocabulary['settings'][number];

/** A pair that prices a product type. */
export type KnownPair = PricingVocabulary['pairs'][number];

/** A model or an engine with the words that say what it is. */
export type Described = PricingVocabulary['models'][number];

/** A sensitivity template key. */
export type KnownTemplate = PricingVocabulary['templates'][number];

/** A vocabulary that knows nothing, for a screen that has not read one yet. */
export const EMPTY_VOCABULARY: PricingVocabulary = {
    globals: [],
    types: [],
    pairs: [],
    settings: [],
    models: [],
    methods: [],
    templates: [],
    tenors: [],
};

/** The scope of a setting that belongs to the whole configuration and to no product. */
export const GLOBAL_SCOPE = 'global';

export function draftOf(document: PricingDocument): Draft {
    return {
        name: document.name,
        description: document.description,
        variant: document.variant,
        products: document.products,
        parameters: document.parameters,
    };
}

/** The pairs the vocabulary knows to price a product type, the usual one first. */
export function pairsFor(vocabulary: PricingVocabulary, typeCode: string): readonly KnownPair[] {
    return vocabulary.pairs
        .filter((pair) => pair.typeCode === typeCode)
        .sort((a, b) => Number(b.usual) - Number(a.usual));
}

/** The pair ORE's examples use most for a product type, when there is one. */
export function usualPair(vocabulary: PricingVocabulary, typeCode: string): KnownPair | undefined {
    return pairsFor(vocabulary, typeCode).find((pair) => pair.usual);
}

/** Whether the vocabulary knows this model and engine to price the product type. */
export function isKnownPair(
    vocabulary: PricingVocabulary,
    typeCode: string,
    model: string,
    engine: string,
): boolean {
    return vocabulary.pairs.some(
        (pair) => pair.typeCode === typeCode && pair.model === model && pair.engine === engine,
    );
}

/** The models offered for a product type: the ones that price it, then every other model. */
/** The models the pairs table says price a product type, and no others. */
export function modelsFor(vocabulary: PricingVocabulary, typeCode: string): readonly string[] {
    return [...new Set(pairsFor(vocabulary, typeCode).map((pair) => pair.model))];
}

/** The engines the pairs table says price a product type with this model, and no others. */
export function enginesFor(
    vocabulary: PricingVocabulary,
    typeCode: string,
    model: string,
): readonly string[] {
    return [
        ...new Set(
            pairsFor(vocabulary, typeCode)
                .filter((pair) => pair.model === model)
                .map((pair) => pair.engine),
        ),
    ];
}

/** What the vocabulary says about a model, when it says anything. */
export function modelInfo(vocabulary: PricingVocabulary, code: string): Described | undefined {
    return vocabulary.models.find((model) => model.code === code);
}

/** What the vocabulary says about an engine, when it says anything. */
export function methodInfo(vocabulary: PricingVocabulary, code: string): Described | undefined {
    return vocabulary.methods.find((method) => method.code === code);
}

/** What the vocabulary says about a sensitivity template, when it says anything. */
export function templateInfo(
    vocabulary: PricingVocabulary,
    code: string,
): KnownTemplate | undefined {
    return vocabulary.templates.find((template) => template.code === code);
}

/** How ORE builds the engine of a pair for a product type, when the vocabulary says. */
export function pairNote(
    vocabulary: PricingVocabulary,
    typeCode: string,
    model: string,
    engine: string,
): string {
    return (
        vocabulary.pairs.find(
            (pair) => pair.typeCode === typeCode && pair.model === model && pair.engine === engine,
        )?.help ?? ''
    );
}

/** Whether a new product starts with the setting: ORE needs it and the vocabulary knows a value. */
export function startsWith(setting: KnownSetting): boolean {
    return setting.required && setting.usual !== '';
}

/** The settings a pair is known to take for a product type. */
export function settingsFor(
    vocabulary: PricingVocabulary,
    typeCode: string,
    model: string,
    engine: string,
): readonly KnownSetting[] {
    return vocabulary.settings
        .filter(
            (setting) =>
                setting.typeCode === typeCode &&
                setting.model === model &&
                setting.engine === engine,
        )
        .sort((a, b) => Number(b.required) - Number(a.required));
}

const keyOf = (scope: string, name: string): string => `${scope}:${name}`;

/** The parameters of one product, in their order. */
export function parametersOf(draft: Draft, productId: string): readonly PricingParameter[] {
    return draft.parameters
        .filter((parameter) => parameter.productId === productId)
        .sort((a, b) => a.position - b.position);
}

/** What changing a product's pair does to its settings, before anything is changed. */
export interface PairChange {
    /** Settings the old pair takes and the new pair does not, which the person may drop. */
    readonly dropped: readonly PricingParameter[];
    /** Settings the new pair usually needs and the product does not hold, which the person may add. */
    readonly added: readonly KnownSetting[];
}

export function pairChange(
    vocabulary: PricingVocabulary,
    draft: Draft,
    product: PricingProduct,
    model: string,
    engine: string,
): PairChange {
    const next = settingsFor(vocabulary, product.typeCode, model, engine);
    const nextKeys = new Set(next.map((setting) => keyOf(setting.scope, setting.name)));
    const previous = settingsFor(vocabulary, product.typeCode, product.model, product.engine);
    const previousKeys = new Set(previous.map((setting) => keyOf(setting.scope, setting.name)));
    const held = parametersOf(draft, product.id);
    const heldKeys = new Set(held.map((parameter) => keyOf(parameter.scope, parameter.name)));
    return {
        dropped: held.filter((parameter) => {
            const key = keyOf(parameter.scope, parameter.name);
            return previousKeys.has(key) && !nextKeys.has(key);
        }),
        added: next.filter((setting) => !heldKeys.has(keyOf(setting.scope, setting.name))),
    };
}

/** One finding of a check: what is wrong, and how much it matters. */
export interface Finding {
    readonly level: 'error' | 'warning' | 'info';
    /** The finding's kind, which a screen turns into its own words. */
    readonly code:
        | 'nameMissing'
        | 'noProducts'
        | 'modelMissing'
        | 'engineMissing'
        | 'pairUnknown'
        | 'typeRepeated'
        | 'parameterRepeated'
        | 'parameterMissingValue'
        | 'settingUsual';
    /** The product the finding is about, when it is about one. */
    readonly productId?: string;
    readonly typeCode?: string;
    readonly detail?: string;
}

/** Everything wrong with a draft that a person can see before a save, worst first. */
export function check(draft: Draft, vocabulary: PricingVocabulary): readonly Finding[] {
    const found: Finding[] = [];
    if (draft.name.trim() === '') {
        found.push({ level: 'error', code: 'nameMissing' });
    }
    if (draft.products.length === 0) {
        found.push({ level: 'error', code: 'noProducts' });
    }
    const seen = new Map<string, number>();
    for (const product of draft.products) {
        const where = { productId: product.id, typeCode: product.typeCode };
        seen.set(product.typeCode, (seen.get(product.typeCode) ?? 0) + 1);
        if (product.model.trim() === '') {
            found.push({ level: 'error', code: 'modelMissing', ...where });
        }
        if (product.engine.trim() === '') {
            found.push({ level: 'error', code: 'engineMissing', ...where });
        }
        if (
            product.model.trim() !== '' &&
            product.engine.trim() !== '' &&
            !isKnownPair(vocabulary, product.typeCode, product.model, product.engine)
        ) {
            found.push({
                level: 'warning',
                code: 'pairUnknown',
                ...where,
                detail: `${product.model} / ${product.engine}`,
            });
        }
        const names = new Set<string>();
        for (const parameter of parametersOf(draft, product.id)) {
            const key = keyOf(parameter.scope, parameter.name);
            if (names.has(key)) {
                found.push({
                    level: 'error',
                    code: 'parameterRepeated',
                    ...where,
                    detail: parameter.name,
                });
            }
            names.add(key);
            if (parameter.value.trim() === '') {
                found.push({
                    level: 'error',
                    code: 'parameterMissingValue',
                    ...where,
                    detail: parameter.name,
                });
            }
        }
        for (const setting of settingsFor(
            vocabulary,
            product.typeCode,
            product.model,
            product.engine,
        )) {
            if (startsWith(setting) && !names.has(keyOf(setting.scope, setting.name))) {
                found.push({
                    level: 'info',
                    code: 'settingUsual',
                    ...where,
                    detail: setting.name,
                });
            }
        }
    }
    for (const [typeCode, count] of seen) {
        if (count > 1) {
            found.push({ level: 'warning', code: 'typeRepeated', typeCode, detail: String(count) });
        }
    }
    const rank = { error: 0, warning: 1, info: 2 } as const;
    return found.sort((a, b) => rank[a.level] - rank[b.level]);
}

/** Whether a draft has a finding that stops a save. */
export function blocked(findings: readonly Finding[]): boolean {
    return findings.some((finding) => finding.level === 'error');
}

/** One line of the difference between two versions. */
export interface Change {
    readonly kind: 'added' | 'removed' | 'changed';
    readonly what: 'configuration' | 'product' | 'parameter';
    /** The product type, for a product or a parameter. */
    readonly typeCode: string;
    readonly name: string;
    readonly before: string;
    readonly after: string;
}

/** What differs between the version a person read and the draft they hold. */
export function difference(before: Draft, after: Draft): readonly Change[] {
    const changes: Change[] = [];
    const header = (field: 'name' | 'description' | 'variant'): void => {
        if (before[field] !== after[field]) {
            changes.push({
                kind: 'changed',
                what: 'configuration',
                typeCode: '',
                name: field,
                before: before[field],
                after: after[field],
            });
        }
    };
    header('name');
    header('description');
    header('variant');

    const typeOf = (draft: Draft, productId: string | null): string =>
        draft.products.find((product) => product.id === productId)?.typeCode ?? '';
    const was = new Map(before.products.map((product) => [product.id, product]));
    const is = new Map(after.products.map((product) => [product.id, product]));
    for (const product of after.products) {
        const old = was.get(product.id);
        if (old === undefined) {
            changes.push({
                kind: 'added',
                what: 'product',
                typeCode: product.typeCode,
                name: product.typeCode,
                before: '',
                after: `${product.model} / ${product.engine}`,
            });
        } else if (old.model !== product.model || old.engine !== product.engine) {
            changes.push({
                kind: 'changed',
                what: 'product',
                typeCode: product.typeCode,
                name: product.typeCode,
                before: `${old.model} / ${old.engine}`,
                after: `${product.model} / ${product.engine}`,
            });
        }
    }
    for (const product of before.products) {
        if (!is.has(product.id)) {
            changes.push({
                kind: 'removed',
                what: 'product',
                typeCode: product.typeCode,
                name: product.typeCode,
                before: `${product.model} / ${product.engine}`,
                after: '',
            });
        }
    }

    const heldBefore = new Map(before.parameters.map((parameter) => [parameter.id, parameter]));
    const heldAfter = new Map(after.parameters.map((parameter) => [parameter.id, parameter]));
    for (const parameter of after.parameters) {
        const old = heldBefore.get(parameter.id);
        const typeCode = typeOf(after, parameter.productId);
        if (old === undefined) {
            changes.push({
                kind: 'added',
                what: 'parameter',
                typeCode,
                name: parameter.name,
                before: '',
                after: parameter.value,
            });
        } else if (old.value !== parameter.value) {
            changes.push({
                kind: 'changed',
                what: 'parameter',
                typeCode,
                name: parameter.name,
                before: old.value,
                after: parameter.value,
            });
        }
    }
    for (const parameter of before.parameters) {
        if (!heldAfter.has(parameter.id)) {
            changes.push({
                kind: 'removed',
                what: 'parameter',
                typeCode: typeOf(before, parameter.productId),
                name: parameter.name,
                before: parameter.value,
                after: '',
            });
        }
    }
    return changes;
}

/** A new identifier for a row the person adds. */
export function newId(): string {
    return crypto.randomUUID();
}

/** A product the person adds, started on the usual pair when the vocabulary has one. */
export function newProduct(
    vocabulary: PricingVocabulary,
    typeCode: string,
    position: number,
): PricingProduct {
    const usual = usualPair(vocabulary, typeCode);
    return {
        id: newId(),
        version: 0,
        typeCode,
        model: usual?.model ?? '',
        engine: usual?.engine ?? '',
        position,
    };
}

/** A parameter the person adds to a product, from a setting the pair is known to take. */
export function newParameter(
    productId: string | null,
    scope: string,
    name: string,
    value: string,
    position: number,
): PricingParameter {
    return { id: newId(), version: 0, productId, scope, name, value, position };
}

/** The products of a draft with one product's pair changed, and the settings the person ticked dropped or added. */
export function withPair(
    vocabulary: PricingVocabulary,
    draft: Draft,
    productId: string,
    model: string,
    engine: string,
    options: { readonly drop: readonly string[]; readonly add: readonly string[] },
): Draft {
    const product = draft.products.find((candidate) => candidate.id === productId);
    if (product === undefined) {
        return draft;
    }
    const change = pairChange(vocabulary, draft, product, model, engine);
    const dropIds = new Set(
        change.dropped
            .filter((parameter) => options.drop.includes(keyOf(parameter.scope, parameter.name)))
            .map((parameter) => parameter.id),
    );
    const kept = draft.parameters.filter((parameter) => !dropIds.has(parameter.id));
    let position = Math.max(-1, ...kept.map((parameter) => parameter.position)) + 1;
    const added = change.added
        .filter((setting) => options.add.includes(keyOf(setting.scope, setting.name)))
        .map((setting) =>
            newParameter(productId, setting.scope, setting.name, setting.usual, position++),
        );
    return {
        ...draft,
        products: draft.products.map((candidate) =>
            candidate.id === productId ? { ...candidate, model, engine } : candidate,
        ),
        parameters: [...kept, ...added],
    };
}

/** The key a setting is ticked by in a pair change. */
export function settingKey(scope: string, name: string): string {
    return keyOf(scope, name);
}

/** An empty draft, for a configuration that starts from nothing. */
export const EMPTY_DRAFT: Draft = {
    name: '',
    description: '',
    variant: '',
    products: [],
    parameters: [],
};

/**
 * A copy of a configuration as a new one: every row has a new identifier and
 * version zero, so a save writes new rows and none of the source's.
 */
export function cloneDraft(source: Draft, name: string): Draft {
    const productIds = new Map(source.products.map((product) => [product.id, newId()]));
    return {
        name,
        description: source.description,
        variant: source.variant,
        products: source.products.map((product) => ({
            ...product,
            id: productIds.get(product.id) ?? newId(),
            version: 0,
        })),
        parameters: source.parameters.map((parameter) => ({
            ...parameter,
            id: newId(),
            version: 0,
            productId:
                parameter.productId === null ? null : (productIds.get(parameter.productId) ?? null),
        })),
    };
}

/** Whether another configuration already holds this name, which the store refuses. */
export function nameInUse(
    held: readonly { readonly id: string; readonly name: string }[],
    name: string,
    exceptId?: string,
): boolean {
    const wanted = name.trim().toLowerCase();
    return (
        wanted !== '' &&
        held.some((row) => row.id !== exceptId && row.name.trim().toLowerCase() === wanted)
    );
}

/**
 * The draft with exactly these product types: a type already held keeps its
 * product and its settings, a new type is started on the usual pair with the
 * settings that pair usually needs, and a type left out loses both.
 */
export function withProductTypes(
    vocabulary: PricingVocabulary,
    draft: Draft,
    typeCodes: readonly string[],
): Draft {
    const wanted = new Set(typeCodes);
    const kept = draft.products.filter((product) => wanted.has(product.typeCode));
    const held = new Set(kept.map((product) => product.typeCode));
    let position = Math.max(-1, ...kept.map((product) => product.position)) + 1;
    const added = typeCodes
        .filter((code) => !held.has(code))
        .map((code) => newProduct(vocabulary, code, position++));
    const keptIds = new Set(kept.map((product) => product.id));
    let next = Math.max(-1, ...draft.parameters.map((parameter) => parameter.position)) + 1;
    const settings = added.flatMap((product) =>
        settingsFor(vocabulary, product.typeCode, product.model, product.engine)
            .filter(startsWith)
            .map((setting) =>
                newParameter(product.id, setting.scope, setting.name, setting.usual, next++),
            ),
    );
    return {
        ...draft,
        products: [...kept, ...added],
        parameters: [
            ...draft.parameters.filter(
                (parameter) => parameter.productId === null || keptIds.has(parameter.productId),
            ),
            ...settings,
        ],
    };
}

/** The draft with one product's model and engine set, and its settings left as they are. */
export function withModelAndEngine(
    draft: Draft,
    productId: string,
    model: string,
    engine: string,
): Draft {
    return {
        ...draft,
        products: draft.products.map((product) =>
            product.id === productId ? { ...product, model, engine } : product,
        ),
    };
}

/** The draft with a setting added to a product, after the settings it already holds. */
export function withParameter(
    draft: Draft,
    productId: string,
    scope: string,
    name: string,
    value: string,
): Draft {
    const position = Math.max(-1, ...draft.parameters.map((parameter) => parameter.position)) + 1;
    return {
        ...draft,
        parameters: [...draft.parameters, newParameter(productId, scope, name, value, position)],
    };
}

/** The draft with one setting's value changed. */
export function withValue(draft: Draft, parameterId: string, value: string): Draft {
    return {
        ...draft,
        parameters: draft.parameters.map((parameter) =>
            parameter.id === parameterId ? { ...parameter, value } : parameter,
        ),
    };
}

/** The draft without one setting. */
export function withoutParameter(draft: Draft, parameterId: string): Draft {
    return {
        ...draft,
        parameters: draft.parameters.filter((parameter) => parameter.id !== parameterId),
    };
}

/** The settings the pair of a product usually takes, that the product does not hold yet. */
export function missingUsual(
    vocabulary: PricingVocabulary,
    draft: Draft,
    product: PricingProduct,
): readonly KnownSetting[] {
    const held = new Set(
        parametersOf(draft, product.id).map((parameter) => keyOf(parameter.scope, parameter.name)),
    );
    return settingsFor(vocabulary, product.typeCode, product.model, product.engine).filter(
        (setting) => startsWith(setting) && !held.has(keyOf(setting.scope, setting.name)),
    );
}
