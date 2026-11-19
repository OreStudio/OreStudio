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

import { z } from 'zod';
import { request } from './transport.js';

/**
 * What the oresmd editor reads to mark a URI's fields: each field a family
 * requires or forbids, and the rules that name a URI's family. The shapes
 * themselves are read and written as records.
 */

export const REQUIREMENTS = ['required', 'forbidden'] as const;

export type Requirement = (typeof REQUIREMENTS)[number];

/** A family: its asset class, its series subclass and, for a fixing, its index family. */
export interface FamilyKey {
    readonly asset_class: string;
    readonly series_subclass: string;
    readonly index_family: string;
}

/** One field a family requires or forbids. */
export interface ShapeField extends FamilyKey {
    readonly field: string;
    readonly requirement: Requirement;
    readonly description: string;
}

const shapeFieldSchema: z.ZodType<ShapeField> = z.object({
    asset_class: z.string(),
    series_subclass: z.string(),
    index_family: z.string(),
    field: z.string(),
    requirement: z.enum(REQUIREMENTS),
    description: z.string(),
});

/**
 * A rule that gives a series its subclass. A datum rule is keyed by the
 * instrument and quote of the URI, and an empty quote applies to every quote of
 * its instrument. The one index rule gives every fixing its subclass.
 */
export interface ClassificationRule {
    readonly instrument: string;
    readonly quote: string;
    readonly key_kind: string;
    readonly series_subclass_code: string;
}

const ruleSchema: z.ZodType<ClassificationRule> = z.object({
    instrument: z.string(),
    quote: z.string(),
    key_kind: z.string(),
    series_subclass_code: z.string(),
});

/** The family as one text: class, subclass and, for a fixing, index family. */
export function familyOf(key: FamilyKey): string {
    return [key.asset_class, key.series_subclass, key.index_family]
        .filter((part) => part !== '')
        .join(' / ');
}

export const identityShapes = {
    /** Every field a family requires or forbids, the shared catalogue's and the tenant's own. */
    async fields(): Promise<readonly ShapeField[]> {
        return z
            .object({ fields: z.array(shapeFieldSchema) })
            .parse(await request('/api/marketdata/series-identity-shape-fields', { method: 'GET' }))
            .fields;
    },

    /** The rules that give a series its subclass, by the instrument and quote of its URI. */
    async classificationRules(): Promise<readonly ClassificationRule[]> {
        return z
            .object({ rules: z.array(ruleSchema) })
            .parse(await request('/api/marketdata/series-classification-rules', { method: 'GET' }))
            .rules;
    },
};
