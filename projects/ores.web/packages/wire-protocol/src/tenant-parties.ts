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
import { uuidSchema, type TenantParty } from './domain.js';
import type { Party } from './generated/refdata/domain/party.js';
import {
    subjects as tenantPartySubjects,
    type ListTenantPartiesRequest,
} from './generated/refdata/protocol/tenant_party_protocol.js';

/** The most parties one read asks for; the server caps a page at the same. */
export const TENANT_PARTY_READ_LIMIT = 1000;

/** The fields of a party the tenant's screen reads. */
const wirePartySchema = z.object({
    id: uuidSchema,
    short_code: z.string(),
    full_name: z.string(),
    party_category: z.string(),
    party_type: z.string().default(''),
    status: z.string(),
    parent_party_id: z.string().nullable().default(null),
});

/*
 * The schema reads fields the generated party declares. A rename on the C++
 * party that this schema does not follow fails the typecheck here.
 */
const wirePartyFieldsExist: [Exclude<keyof z.input<typeof wirePartySchema>, keyof Party>] extends [
    never,
]
    ? true
    : false = true;
void wirePartyFieldsExist;

const wireTenantPartiesSchema = z.object({
    success: z.boolean().default(false),
    message: z.string().default(''),
    parties: z.array(wirePartySchema).default([]),
    total: z.int().nonnegative().default(0),
});

/** A tenant's parties, and how many it holds in all. */
export interface TenantParties {
    readonly parties: readonly TenantParty[];
    readonly total: number;
}

/**
 * The current parties of one named tenant.
 *
 * Every other party read answers the caller's own tenant. This one names the
 * tenant, and the server serves it only to a caller in the system tenant. The
 * system party is listed first, then the others by name. Each parent is
 * resolved to its name from the same answer.
 */
export async function readTenantParties(
    caller: AuthenticatedCaller,
    tenantId: string,
): Promise<TenantParties> {
    const request: ListTenantPartiesRequest = {
        tenant_id: tenantId,
        offset: 0,
        limit: TENANT_PARTY_READ_LIMIT,
    };
    const answer = await caller.callAuthenticated(
        tenantPartySubjects.list_tenant_parties_request,
        request,
        wireTenantPartiesSchema,
    );
    if (!answer.success) {
        throw new Error(answer.message === '' ? 'The parties were not read.' : answer.message);
    }
    const names = new Map<string, string>(
        answer.parties.map((party) => [party.id, party.full_name]),
    );
    const parties = answer.parties.map((party): TenantParty => ({
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
    parties.sort(
        (a, b) =>
            Number(b.category === 'System') - Number(a.category === 'System') ||
            a.name.localeCompare(b.name),
    );
    return { parties, total: answer.total };
}
