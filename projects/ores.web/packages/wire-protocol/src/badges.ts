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
import type { AuthenticatedCaller } from './account-operations.js';
import type { BadgePresentation } from './domain.js';
import { subjects as definitionSubjects } from './generated/dq/protocol/badge_definition_protocol.js';
import { subjects as mappingSubjects } from './generated/dq/protocol/badge_mapping_protocol.js';
import { orderSchema } from './operations.js';

/**
 * How a value from a code domain is painted.
 *
 * A badge is the platform's own vocabulary for a coloured pill, and it is
 * reference data rather than a screen's decision: `ores.dq` holds the badge
 * catalogue, and the mapping junction says which domain value gets which
 * badge. A screen that picks its own colours for a status is a screen that
 * disagrees with every other screen showing the same status, and it is the
 * reason this read exists.
 *
 * Two reads answer one screen: the mapping for a domain, which is small and
 * indexed by the domain, and the catalogue itself, which is a few dozen rows
 * shared by every domain. They are joined here rather than in the browser,
 * because a screen asking "how do I paint `suspended`" should not have to hold
 * two tables to find out.
 */

/** Subjects for the badge reads, kept beside the operations that use them. */
export const BADGE_SUBJECTS = {
    definitions: definitionSubjects.list_badge_definitions_request,
    mappingsByDomain: mappingSubjects.list_by_code_domain_code_badge_mappings_request,
} as const;

/** The mapping row as the registry writes it. */
const wireMappingSchema = z.object({
    code_domain_code: z.string(),
    entity_code: z.string(),
    badge_code: z.string(),
});

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

/**
 * `list_by_code_domain_code_badge_mappings_request`, sent on its subject.
 *
 * Every field is stated rather than left out, including the filter, because the
 * server decodes the request into a struct that names them all.
 */
export const listBadgeMappingsRequestSchema = z.object({
    code_domain_code: z.string().min(1),
    scope: z.enum(['direct', 'subtree']).default('direct'),
    offset: z.int().nonnegative().default(0),
    limit: z.int().positive().max(1000).default(100),
    order: orderSchema.default({ field: '', descending: false }),
    filter: z.null().default(null),
});

/** `list_badge_definitions_request`, sent on its subject. */
export const listBadgeDefinitionsRequestSchema = z.object({
    offset: z.int().nonnegative().default(0),
    limit: z.int().positive().max(1000).default(200),
    order: orderSchema.default({ field: '', descending: false }),
});

/**
 * Every value of a domain that has a badge, keyed by the value.
 *
 * A value with no mapping is absent rather than defaulted, so a screen can tell
 * "this value has no badge" from "this value has a badge that paints it like
 * everything else".
 */
export type BadgeCatalogue = Readonly<Record<string, BadgePresentation>>;

/**
 * The badges a code domain maps its values to.
 *
 * The mappings are read for the domain named, and the definitions are read
 * whole: the catalogue is shared by every domain, so asking for the definitions
 * a domain happens to use would be a second query per screen for rows the
 * deployment already holds. A mapping whose badge has gone from the catalogue
 * is dropped, because a pill with no colours is worse than the plain value.
 */
export async function readBadgesForDomain(
    caller: AuthenticatedCaller,
    domain: string,
): Promise<BadgeCatalogue> {
    const mappings = await caller.callAuthenticated(
        BADGE_SUBJECTS.mappingsByDomain,
        listBadgeMappingsRequestSchema.parse({ code_domain_code: domain }),
        z.object({ badge_mappings: z.array(wireMappingSchema).default([]) }),
    );
    const definitions = await caller.callAuthenticated(
        BADGE_SUBJECTS.definitions,
        listBadgeDefinitionsRequestSchema.parse({}),
        z.object({ definitions: z.array(wireDefinitionSchema).default([]) }),
    );

    const byCode = new Map(definitions.definitions.map((row) => [row.code, row]));
    const catalogue: Record<string, BadgePresentation> = {};
    for (const mapping of mappings.badge_mappings) {
        const badge = byCode.get(mapping.badge_code);
        if (badge !== undefined) {
            catalogue[mapping.entity_code] = {
                code: badge.code,
                label: badge.name,
                description: badge.description,
                backgroundColour: badge.background_colour,
                textColour: badge.text_colour,
                severity: badge.severity_code,
            };
        }
    }
    return catalogue;
}
