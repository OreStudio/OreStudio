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

import { z } from 'zod';
import { request } from './transport.js';

/**
 * The pricing engine configuration screens' reads and writes.
 *
 * A configuration is one document: a header, a row for each product and a row for
 * each parameter. The BFF answers and takes it whole, so a screen never handles
 * the three tables apart. A save carries the version the screen read, and a
 * document that changed since is refused as a conflict.
 */

const JSON_HEADERS = { 'Content-Type': 'application/json' } as const;

const productSchema = z.object({
    id: z.string(),
    version: z.int().nonnegative(),
    typeCode: z.string(),
    model: z.string(),
    engine: z.string(),
    position: z.int().nonnegative(),
});

const parameterSchema = z.object({
    id: z.string(),
    version: z.int().nonnegative(),
    productId: z.string().nullable(),
    scope: z.string(),
    name: z.string(),
    value: z.string(),
    position: z.int().nonnegative(),
});

const documentSchema = z.object({
    id: z.string(),
    partyId: z.string(),
    partyName: z.string(),
    partyCode: z.string(),
    version: z.int().nonnegative(),
    name: z.string(),
    description: z.string(),
    variant: z.string(),
    configurationId: z.string(),
    modifiedBy: z.string(),
    recordedAt: z.string(),
    changeReason: z.string(),
    changeCommentary: z.string(),
    /** The reports that bind the configuration, or null when the read of them failed. */
    reports: z.array(z.object({ id: z.string(), name: z.string() })).nullable(),
    products: z.array(productSchema),
    parameters: z.array(parameterSchema),
});

const summarySchema = z.object({
    id: z.string(),
    partyId: z.string(),
    partyName: z.string(),
    partyCode: z.string(),
    version: z.int().nonnegative(),
    name: z.string(),
    description: z.string(),
    variant: z.string(),
    configurationId: z.string(),
    products: z.int().nonnegative(),
    parameters: z.int().nonnegative(),
    modifiedBy: z.string(),
    recordedAt: z.string(),
});

/** A model or an engine with the words that say what it is. */
const described = z.object({
    code: z.string(),
    description: z.string(),
    detail: z.string(),
    source: z.string(),
});

/** The values a setting accepts, as the value set it names states them. */
const valueSetFields = {
    valueSet: z.string().default(''),
    sourceKind: z.string().default(''),
    choices: z.array(z.object({ value: z.string(), label: z.string() })).default([]),
    minimum: z.number().nullable().default(null),
    maximum: z.number().nullable().default(null),
    pattern: z.string().default(''),
};

const vocabularySchema = z.object({
    types: z.array(
        z.object({
            code: z.string(),
            description: z.string(),
            instrumentTypeCode: z.string(),
            assetClass: z.string(),
            documentation: z.string().default(''),
        }),
    ),
    pairs: z.array(
        z.object({
            typeCode: z.string(),
            model: z.string(),
            engine: z.string(),
            usual: z.boolean(),
            label: z.string().default(''),
            help: z.string().default(''),
            description: z.string().default(''),
            defaultText: z.string().default(''),
            xpath: z.string().default(''),
        }),
    ),
    settings: z.array(
        z.object({
            typeCode: z.string(),
            model: z.string(),
            engine: z.string(),
            scope: z.string(),
            name: z.string(),
            kind: z.string(),
            usual: z.string(),
            options: z.array(z.string()),
            ...valueSetFields,
            required: z.boolean(),
            documented: z.boolean().default(false),
            label: z.string().default(''),
            help: z.string().default(''),
            description: z.string().default(''),
            defaultText: z.string().default(''),
            format: z.string().default(''),
            introducedIn: z.string().default(''),
            removedIn: z.string().default(''),
            xpath: z.string().default(''),
        }),
    ),
    models: z.array(described),
    globals: z
        .array(
            z.object({
                name: z.string(),
                domain: z.string().default('string'),
                ...valueSetFields,
                label: z.string().default(''),
                help: z.string().default(''),
                description: z.string().default(''),
                defaultText: z.string().default(''),
                format: z.string().default(''),
                introducedIn: z.string().default(''),
                removedIn: z.string().default(''),
                xpath: z.string().default(''),
            }),
        )
        .default([]),
    methods: z.array(described),
    templates: z.array(
        z.object({
            code: z.string(),
            assetClass: z.string(),
            method: z.string(),
            description: z.string(),
        }),
    ),
    tenors: z.array(z.object({ code: z.string(), displayName: z.string() })),
});

/** One product of a configuration: a pricing engine type with the model and engine that price it. */
export type PricingProduct = z.infer<typeof productSchema>;

/** One setting of a product, or of the configuration when it names no product. */
export type PricingParameter = z.infer<typeof parameterSchema>;

/** A configuration with its products and parameters. */
export type PricingDocument = z.infer<typeof documentSchema>;

/** The row a list shows for a configuration. */
export type PricingSummary = z.infer<typeof summarySchema>;

/** The product types, the pairs that price them and the settings a pair takes. */
export type PricingVocabulary = z.infer<typeof vocabularySchema>;

/** One field of one version of an entity of the document. */
const historyField = z.object({ name: z.string(), value: z.string() });

/** One field that differs from the version before. */
const historyChange = z.object({
    field: z.string(),
    before: z.string(),
    after: z.string(),
});

/** One version of one entity of the document, as the history tab draws it. */
export const documentVersionSchema = z.object({
    entityType: z.string(),
    entityId: z.string(),
    entityName: z.string(),
    version: z.int().nonnegative(),
    modifiedBy: z.string(),
    recordedAt: z.string(),
    fields: z.array(historyField),
    changes: z.array(historyChange),
});

/** Every version of every entity of the document. */
export type DocumentVersion = z.infer<typeof documentVersionSchema>;

/** Why a write is made. */
export interface PricingIntent {
    readonly reason_code: string;
    readonly commentary: string;
}

/** What a write carries: the document the screen holds, and why. */
export interface PricingWrite {
    readonly name: string;
    readonly description: string;
    readonly variant: string;
    readonly products: readonly Pick<
        PricingProduct,
        'id' | 'version' | 'typeCode' | 'model' | 'engine' | 'position'
    >[];
    readonly parameters: readonly Pick<
        PricingParameter,
        'id' | 'version' | 'productId' | 'scope' | 'name' | 'value' | 'position'
    >[];
    readonly intent: PricingIntent;
}

const ROOT = '/api/reporting/pricing-engines';

export const pricingEngines = {
    /** Every configuration the session's party holds. */
    async list(): Promise<readonly PricingSummary[]> {
        return z
            .object({ rows: z.array(summarySchema) })
            .parse(await request(ROOT, { method: 'GET' })).rows;
    },

    /** The pick lists the combo boxes draw from. */
    async vocabulary(): Promise<PricingVocabulary> {
        return vocabularySchema.parse(await request(`${ROOT}/vocabulary`, { method: 'GET' }));
    },

    /** Every version of the configuration, its products and its parameters. */
    async history(id: string): Promise<readonly DocumentVersion[]> {
        return z
            .object({ versions: z.array(documentVersionSchema) })
            .parse(await request(`${ROOT}/${encodeURIComponent(id)}/history`, { method: 'GET' }))
            .versions;
    },

    /** One configuration with its products and parameters. */
    async read(id: string): Promise<PricingDocument> {
        return documentSchema.parse(
            await request(`${ROOT}/${encodeURIComponent(id)}`, { method: 'GET' }),
        );
    },

    /** Creates a configuration and the reporting header a report binds it through. */
    async create(write: PricingWrite): Promise<PricingDocument> {
        return documentSchema.parse(
            await request(ROOT, {
                method: 'POST',
                headers: JSON_HEADERS,
                body: JSON.stringify(write),
            }),
        );
    },

    /** Saves the whole document against the version the screen read. */
    async save(id: string, version: number, write: PricingWrite): Promise<PricingDocument> {
        return documentSchema.parse(
            await request(`${ROOT}/${encodeURIComponent(id)}`, {
                method: 'PUT',
                headers: JSON_HEADERS,
                body: JSON.stringify({ ...write, version }),
            }),
        );
    },

    /** Removes the configuration and its products and parameters. */
    async remove(id: string): Promise<void> {
        await request(`${ROOT}/${encodeURIComponent(id)}`, { method: 'DELETE' });
    },
};
