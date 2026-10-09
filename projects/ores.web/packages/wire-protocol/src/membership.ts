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
import { OperationFailedError } from './errors.js';
import { subjects as accountSubjects } from './generated/iam/protocol/account_operations_protocol.js';
import { decidedResultSchema } from './operations.js';

/**
 * The membership read path: the parties the caller's own account works in.
 *
 * The association read answers with a party identifier and nothing on the
 * browser's path turns one into a name, so the server names them here. The
 * request names no account, because the session names it, so the read cannot
 * answer about anybody else and it needs no permission.
 */

/** One party the caller works in, as the member's own screen reads it. */
export const myPartySchema = z.object({
    partyId: z.string(),
    /** The party's registered name. Empty when the server cannot name it. */
    name: z.string(),
    shortCode: z.string(),
    partyCategory: z.string(),
    businessCenterCode: z.string(),
});

export type MyParty = z.infer<typeof myPartySchema>;

export const myPartiesSchema = z.object({
    /** The party quick sign-in uses, or empty when the account stores none. */
    defaultPartyId: z.string(),
    parties: z.array(myPartySchema),
});

export type MyParties = z.infer<typeof myPartiesSchema>;

/**
 * The wire shape, snake_case as the protocol states it. A party the server
 * cannot name arrives with empty strings, which the screen states as a gap.
 */
const myPartiesReplySchema = z.object({
    result: decidedResultSchema,
    default_party_id: z.string(),
    parties: z.array(
        z.object({
            party_id: z.string(),
            name: z.string(),
            short_code: z.string(),
            party_category: z.string(),
            business_center_code: z.string(),
        }),
    ),
});

export async function readMyParties(caller: AuthenticatedCaller): Promise<MyParties> {
    const reply = await caller.callAuthenticated(
        accountSubjects.get_my_parties_request,
        {},
        myPartiesReplySchema,
    );
    if (reply.result.outcome !== 'ok') {
        throw new OperationFailedError(
            accountSubjects.get_my_parties_request,
            reply.result.message,
        );
    }
    return myPartiesSchema.parse({
        defaultPartyId: reply.default_party_id,
        parties: reply.parties.map((party) => ({
            partyId: party.party_id,
            name: party.name,
            shortCode: party.short_code,
            partyCategory: party.party_category,
            businessCenterCode: party.business_center_code,
        })),
    });
}

const setMyDefaultPartyReplySchema = z.object({
    success: z.boolean(),
    message: z.string().default(''),
});

/**
 * The body of the default-party write, as the BFF takes it from the browser.
 *
 * An empty =partyId= clears the stored default, which is the only way the
 * operation can say "no default".
 */
export const defaultPartyRequestSchema = z.object({
    partyId: z.string(),
});

/**
 * Sets or clears the party quick sign-in uses.
 *
 * An empty party clears the stored default, which is the only way this
 * operation can say "no default". The server checks the party is one the
 * account works in and refuses the write otherwise, so the screen cannot store
 * a party the member cannot act for.
 */
export async function setMyDefaultParty(
    caller: AuthenticatedCaller,
    partyId: string,
): Promise<void> {
    const reply = await caller.callAuthenticated(
        accountSubjects.set_my_default_party_request,
        { party_id: partyId },
        setMyDefaultPartyReplySchema,
    );
    if (!reply.success) {
        throw new OperationFailedError(
            accountSubjects.set_my_default_party_request,
            reply.message,
        );
    }
}
