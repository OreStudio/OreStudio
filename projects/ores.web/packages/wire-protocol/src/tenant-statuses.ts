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
import { readBadgeCatalogue } from './badges.js';
import type { TenantStatus } from './domain.js';
import { subjects as tenantStatusSubjects } from './generated/iam/protocol/tenant_status_protocol.js';
import { orderSchema } from './operations.js';

/**
 * The tenant lifecycle statuses, and the badge each one is painted with.
 *
 * The status row carries the badge's code, so the two reads are a lookup and
 * not a mapping: there is no table saying which badge a status gets, because
 * the status already names it. That is what makes a value unable to drift from
 * its badge, and it is why a status's own name is the label while the badge
 * supplies only the colours.
 *
 * A status whose badge has left the catalogue keeps its words and loses its
 * colours, which is worse to look at and better than not being readable.
 */

/** Subjects for the tenant status reads, kept beside the operations that use them. */
export const TENANT_STATUS_SUBJECTS = {
    list: tenantStatusSubjects.list_tenant_statuses_request,
} as const;

/** The status row as the registry writes it. */
const wireTenantStatusSchema = z.object({
    status: z.string(),
    name: z.string(),
    description: z.string().default(''),
    display_order: z.int().nonnegative().default(0),
    badge_code: z.string().nullable().default(''),
});

/** `list_tenant_statuses_request`, sent on its subject. */
export const listTenantStatusesRequestSchema = z.object({
    offset: z.int().nonnegative().default(0),
    limit: z.int().positive().max(1000).default(100),
    order: orderSchema.default({ field: '', descending: false }),
});

/** Every tenant status, in the order the rows declare. */
export async function readTenantStatuses(
    caller: AuthenticatedCaller,
): Promise<readonly TenantStatus[]> {
    const listed = await caller.callAuthenticated(
        TENANT_STATUS_SUBJECTS.list,
        listTenantStatusesRequestSchema.parse({}),
        z.object({ statuses: z.array(wireTenantStatusSchema).default([]) }),
    );
    const catalogue = await readBadgeCatalogue(caller);

    return listed.statuses.map((row) => ({
        code: row.status,
        name: row.name,
        description: row.description,
        badge: catalogue[row.badge_code ?? ''] ?? null,
    }));
}
