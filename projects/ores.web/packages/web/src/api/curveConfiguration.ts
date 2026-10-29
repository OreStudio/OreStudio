/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

import { z } from 'zod';
import { request } from './transport.js';

const JSON_HEADERS = { 'Content-Type': 'application/json' } as const;

/**
 * The curve configuration a report binds, as the curve screen reads it.
 *
 * The server names every section, entry and leaf: the sections and their entry
 * elements come from the curve section data, and the words of each leaf come
 * from the ORE parameter contract. The screen holds no table of either, so this
 * client types what the server says rather than repeating it.
 */

/** One value a leaf accepts, as a control offers it. */
const choiceSchema = z.object({ value: z.string(), label: z.string() });

/** One parameter of one entry: its value, its words, and the values it accepts. */
const parameterSchema = z.object({
    entity: z.string(),
    column: z.string(),
    ore: z.string(),
    xpath: z.string(),
    value: z.string().nullable(),
    documented: z.boolean(),
    required: z.boolean(),
    label: z.string(),
    help: z.string(),
    description: z.string(),
    defaultText: z.string(),
    format: z.string(),
    introducedIn: z.string(),
    removedIn: z.string(),
    valueSet: z.string(),
    sourceKind: z.string(),
    catalogue: z.string().default(''),
    choices: z.array(choiceSchema),
    minimum: z.number().nullable(),
    maximum: z.number().nullable(),
    pattern: z.string(),
});

export type CurveParameter = z.infer<typeof parameterSchema>;

/** One child element an entry holds, and whether the entry states it. */
const blockSchema = z.object({
    element: z.string(),
    present: z.boolean().nullable(),
});

export type CurveBlock = z.infer<typeof blockSchema>;

/** One row of a child collection, with the leaves the contract names for it. */
const collectionRowSchema = z.object({
    id: z.string(),
    kind: z.string(),
    leaves: z.array(parameterSchema),
});

export type CurveCollectionRow = z.infer<typeof collectionRowSchema>;

/** A child collection an entry holds rows of, such as its segments. */
const collectionSchema = z.object({
    element: z.string(),
    entity: z.string(),
    rows: z.array(collectionRowSchema),
});

export type CurveCollection = z.infer<typeof collectionSchema>;

/** One curve entry: the definition every family shares, and its own parameters. */
const entrySchema = z.object({
    id: z.string(),
    curveId: z.string(),
    description: z.string(),
    version: z.number(),
    position: z.number(),
    parameters: z.array(parameterSchema),
    blocks: z.array(blockSchema),
    collections: z.array(collectionSchema),
});

export type CurveEntry = z.infer<typeof entrySchema>;

/** One section of the document, its entries, and whether its columns are known. */
const sectionSchema = z.object({
    code: z.string(),
    entryElement: z.string(),
    description: z.string(),
    mapped: z.boolean(),
    entries: z.array(entrySchema),
});

export type CurveSection = z.infer<typeof sectionSchema>;

/** The curve configuration a reporting configuration is filled by. */
const configurationSchema = z.object({
    configurationId: z.string(),
    id: z.string(),
    name: z.string(),
    description: z.string(),
    version: z.number(),
    partyId: z.string(),
    modifiedBy: z.string(),
    recordedAt: z.string(),
    documented: z.number(),
    total: z.number(),
    sections: z.array(sectionSchema),
});

export type CurveConfiguration = z.infer<typeof configurationSchema>;

/** One curve configuration as the list draws it. */
const summarySchema = z.object({
    configurationId: z.string(),
    id: z.string(),
    name: z.string(),
    description: z.string(),
    version: z.number(),
    sections: z.number(),
    curves: z.number(),
});

export type CurveConfigurationSummary = z.infer<typeof summarySchema>;

/** One leaf a save changes: the row, the column and the value it takes. */
export interface CurveChange {
    readonly entity: string;
    /** The curve definition the entry is: every family row joins back to it. */
    readonly entryId: string;
    readonly column: string;
    /** The value to hold, or null to unset the leaf and let ORE apply its default. */
    readonly value: string | null;
}

/** What a save of a curve configuration states: the changes and the version it read. */
export interface CurveAmendment {
    readonly version: number;
    readonly reasonCode: string;
    readonly commentary: string;
    readonly changes: readonly CurveChange[];
}

export const curveConfiguration = {
    /** The curve configurations the session's parties hold. */
    async list(): Promise<readonly CurveConfigurationSummary[]> {
        const answer = z
            .object({ rows: z.array(summarySchema), total: z.number() })
            .parse(await request('/api/refdata/curve-configurations', { method: 'GET' }));
        return answer.rows;
    },

    /**
     * The curve configuration the reporting configuration `id` is filled by.
     *
     * A configuration no document fills is a 404, so the page states the missing
     * case rather than drawing an empty screen.
     */
    async read(id: string, party?: string): Promise<CurveConfiguration> {
        const query = party === undefined ? '' : `?party=${encodeURIComponent(party)}`;
        return configurationSchema.parse(
            await request(`/api/refdata/curve-configurations/${encodeURIComponent(id)}${query}`, {
                method: 'GET',
            }),
        );
    },

    /**
     * Changes leaves of the curve configuration `id`.
     *
     * The server reads the stored document, changes the leaves named here and
     * writes back only the families it touched, so the rows this screen does not
     * draw keep their versions. A version that moved on is a 409, which the page
     * states as a conflict and offers to read again.
     */
    async amend(id: string, amendment: CurveAmendment, party?: string): Promise<void> {
        const query = party === undefined ? '' : `?party=${encodeURIComponent(party)}`;
        await request(`/api/refdata/curve-configurations/${encodeURIComponent(id)}${query}`, {
            method: 'PUT',
            headers: JSON_HEADERS,
            body: JSON.stringify(amendment),
        });
    },
};
