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

import type { ListTenantTypesRequest as GeneratedListTenantTypesRequest } from './generated/iam/protocol/tenant_type_protocol.js';
import { z } from 'zod';
import type { AuthenticatedCaller } from './account-operations.js';
import { readBadgeCatalogue } from './badges.js';
import type { TenantType } from './domain.js';
import { subjects as tenantTypeSubjects } from './generated/iam/protocol/tenant_type_protocol.js';
import { orderSchema } from './operations.js';

/**
 * The tenant types, and the badge each one is painted with.
 *
 * As with the statuses, the type row carries its badge's code, so the read is a
 * lookup and not a mapping, and the type's own name is the label.
 */

/** Subjects for the tenant type reads, kept beside the operations that use them. */
export const TENANT_TYPE_SUBJECTS = {
    list: tenantTypeSubjects.list_tenant_types_request,
} as const;

/** The type row as the registry writes it. */
const wireTenantTypeSchema = z.object({
    type: z.string(),
    name: z.string(),
    description: z.string().default(''),
    display_order: z.int().nonnegative().default(0),
    badge_code: z.string().nullable().default(''),
});

/** `list_tenant_types_request`, sent on its subject. */
export const listTenantTypesRequestSchema = z.object({
    offset: z.int().nonnegative().default(0),
    limit: z.int().positive().max(1000).default(100),
    order: orderSchema.default({ field: '', descending: false }),
    filter: z.null().default(null),
    as_of: z.string().nullable().default(null),
}) satisfies z.ZodType<GeneratedListTenantTypesRequest>;

/** Every tenant type, in the order the rows declare. */
export async function readTenantTypes(caller: AuthenticatedCaller): Promise<readonly TenantType[]> {
    const listed = await caller.callAuthenticated(
        TENANT_TYPE_SUBJECTS.list,
        listTenantTypesRequestSchema.parse({}),
        z.object({ types: z.array(wireTenantTypeSchema).default([]) }),
    );
    const catalogue = await readBadgeCatalogue(caller);

    return [...listed.types]
        .sort((left, right) => left.display_order - right.display_order)
        .map((row) => ({
            code: row.type,
            name: row.name,
            description: row.description,
            badge: catalogue[row.badge_code ?? ''] ?? null,
        }));
}
