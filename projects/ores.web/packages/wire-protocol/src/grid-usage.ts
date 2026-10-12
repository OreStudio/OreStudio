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

import type { AuthenticatedCaller } from './account-operations.js';
import { OperationFailedError } from './errors.js';
import {
    SUBJECTS,
    gridUsageReplySchema,
    gridUsageRequestSchema,
    type GridUsageReply,
} from './operations.js';

/**
 * The work of the caller's tenant counted by party.
 *
 * The caller decides the tenant: the session's own, or the tenant a system
 * administrator has entered. The read names no tenant of its own, so it is the
 * same call for a tenant's administrator and for an operator reading one tenant
 * from inside it. A refusal that the read states in its body is an error rather
 * than an empty usage, because an empty usage would read as a tenant that runs
 * nothing.
 */
export async function readGridUsage(caller: AuthenticatedCaller): Promise<GridUsageReply> {
    const reply = await caller.callAuthenticated(
        SUBJECTS.gridUsage,
        gridUsageRequestSchema.parse({}),
        gridUsageReplySchema,
    );
    if (!reply.success) {
        throw new OperationFailedError(SUBJECTS.gridUsage, reply.message);
    }
    return reply;
}
