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

import type { AuthenticatedCaller } from './account-operations.js';
import type { PartyPage, TenantParty } from './domain.js';
import { OperationFailedError } from './errors.js';
import { SUBJECTS, listPartiesReplySchema } from './operations.js';

/** What one page of parties asks for. */
export interface PartyPageQuery {
    readonly offset: number;
    readonly limit: number;
}

/**
 * One page of the parties of the session's own tenant, and how many it holds.
 *
 * The tenant is the session's, so row-level security scopes the read from the
 * token and the request names no tenant. The server pages in key order. A
 * parent is named when it is on the same page; nothing reads a party by its
 * id, so naming one on another page would mean reading every page.
 */
export async function readPartiesPage(
    caller: AuthenticatedCaller,
    query: PartyPageQuery,
): Promise<PartyPage> {
    const reply = await caller.callAuthenticated(
        SUBJECTS.listParties,
        { offset: query.offset, limit: query.limit, order: { field: '', descending: false } },
        listPartiesReplySchema,
    );
    if (reply.result.outcome !== 'ok') {
        throw new OperationFailedError(SUBJECTS.listParties, reply.result.message);
    }
    const names = new Map<string, string>(
        reply.parties.map((party) => [party.id, party.full_name]),
    );
    const parties = reply.parties.map((party): TenantParty => ({
        id: party.id,
        code: party.short_code,
        name: party.full_name,
        category: party.party_category,
        type: party.party_type,
        status: party.status,
        parentId: party.parent_party_id,
        parentName:
            party.parent_party_id === null ? null : (names.get(party.parent_party_id) ?? null),
    }));
    return { parties, totalCount: reply.total };
}
