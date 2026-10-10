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

/**
 * Everything the party details journey reaches the server with.
 *
 * A step's body is a function of the answers these calls give, so a test that
 * hands the page a server of its own walks the whole journey with no transport
 * in the way. Each set a party carries is read on its own call, so a slow panel
 * does not hold the others.
 */

import { useMemo } from 'react';
import { partyDetails } from '../api/partyDetails.js';
import type {
    PartyComposite,
    PartyCompositeRequest,
    PartyIntent,
    PartyPage,
    PartyPickLists,
    PartySets,
    PartyWriteOutcome,
} from '../api/partyDetails.js';
import type { BusinessUnit } from '@ores/wire-protocol/generated/refdata/domain/business_unit';

export type {
    PartyComposite,
    PartyCompositeRequest,
    PartyIntent,
    PartyPage,
    PartyPickLists,
    PartySets,
    PartyWriteOutcome,
};

export type Junction = 'countries' | 'currencies' | 'counterparties';

export interface PartyDetailsServer {
    /** One page of the tenant's parties, searched on the server. */
    readonly listParties: (query: {
        readonly offset: number;
        readonly limit: number;
        readonly search: string;
    }) => Promise<PartyPage>;
    /** The identifiers, contacts, memberships, counterparty links and units of one party. */
    readonly setsOf: (partyId: string) => Promise<PartySets>;
    /** Every list a step's picker draws from. */
    readonly pickLists: () => Promise<PartyPickLists>;
    /** The party with its identifiers and contacts as they stood in one version's window. */
    readonly compositeAsOf: (partyId: string, version: number) => Promise<PartyComposite>;
    /** Writes the party and the identifiers and contacts the review staged, as one act. */
    readonly writeComposite: (body: PartyCompositeRequest) => Promise<PartyWriteOutcome>;
    /** Retires an identifier whose value changed. */
    readonly retireIdentifier: (
        partyId: string,
        idValue: string,
        version: number,
        intent: PartyIntent,
    ) => Promise<PartyWriteOutcome>;
    /** Links a country, a currency or a counterparty. */
    readonly linkMembership: (
        partyId: string,
        junction: Junction,
        far: string,
        intent: PartyIntent,
    ) => Promise<PartyWriteOutcome>;
    /** Closes a membership or unlinks a counterparty. */
    readonly closeMembership: (
        partyId: string,
        junction: Junction,
        far: string,
        intent: PartyIntent,
    ) => Promise<PartyWriteOutcome>;
    /** Reclassifies a business unit against the version the person read. */
    readonly reclassifyUnit: (
        unit: BusinessUnit,
        unitTypeId: string | null,
        intent: PartyIntent,
    ) => Promise<PartyWriteOutcome>;
}

/** The deployment's own server, as the party details journey reaches it. */
export function usePartyDetailsServer(): PartyDetailsServer {
    return useMemo<PartyDetailsServer>(
        () => ({
            listParties: partyDetails.page,
            setsOf: partyDetails.sets,
            pickLists: partyDetails.pickLists,
            compositeAsOf: partyDetails.compositeAsOf,
            writeComposite: partyDetails.writeComposite,
            retireIdentifier: partyDetails.retireIdentifier,
            linkMembership: partyDetails.linkMembership,
            closeMembership: partyDetails.closeMembership,
            reclassifyUnit: partyDetails.reclassifyUnit,
        }),
        [],
    );
}
