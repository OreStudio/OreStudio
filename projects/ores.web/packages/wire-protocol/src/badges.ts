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

import type { ListBadgeDefinitionsRequest as GeneratedListBadgeDefinitionsRequest } from './generated/dq/protocol/badge_definition_protocol.js';
import { z } from 'zod';
import type { AuthenticatedCaller } from './account-operations.js';
import type { BadgePresentation } from './domain.js';
import { subjects as definitionSubjects } from './generated/dq/protocol/badge_definition_protocol.js';
import { orderSchema } from './operations.js';

/**
 * The badge catalogue, as a screen reads it.
 *
 * A badge is the platform's own vocabulary for a coloured pill. The catalogue
 * holds what each one looks like, and a row that wants to be painted names the
 * badge it wants: the value carries the reference, so nothing maps a value to a
 * badge and nothing can drift from the value.
 *
 * The catalogue carries no label for a value. A badge's own name is its entry
 * in the catalogue, for whoever browses the catalogue; the words inside a pill
 * are the words of the row that chose the badge.
 */

/** Subjects for the badge reads, kept beside the operations that use them. */
export const BADGE_SUBJECTS = {
    definitions: definitionSubjects.list_badge_definitions_request,
} as const;

/** The catalogue row as the registry writes it. */
const wireDefinitionSchema = z.object({
    code: z.string(),
    name: z.string(),
    description: z.string().default(''),
    background_colour: z.string().default(''),
    text_colour: z.string().default(''),
    severity_code: z.string().default(''),
    css_class: z.string().default(''),
    display_order: z.int().nonnegative().default(0),
});

/** `list_badge_definitions_request`, sent on its subject. */
export const listBadgeDefinitionsRequestSchema = z.object({
    offset: z.int().nonnegative().default(0),
    limit: z.int().positive().max(1000).default(200),
    order: orderSchema.default({ field: '', descending: false }),
    filter: z.null().default(null),
}) satisfies z.ZodType<GeneratedListBadgeDefinitionsRequest>;

/** The catalogue, keyed by the badge's own code. */
export type BadgeCatalogue = Readonly<Record<string, BadgePresentation>>;

/** Every badge the deployment holds, keyed by its code. */
export async function readBadgeCatalogue(caller: AuthenticatedCaller): Promise<BadgeCatalogue> {
    const definitions = await caller.callAuthenticated(
        BADGE_SUBJECTS.definitions,
        listBadgeDefinitionsRequestSchema.parse({}),
        z.object({ definitions: z.array(wireDefinitionSchema).default([]) }),
    );

    const catalogue: Record<string, BadgePresentation> = {};
    for (const row of definitions.definitions) {
        catalogue[row.code] = {
            code: row.code,
            label: row.name,
            description: row.description,
            backgroundColour: row.background_colour,
            textColour: row.text_colour,
            severity: row.severity_code,
        };
    }
    return catalogue;
}
