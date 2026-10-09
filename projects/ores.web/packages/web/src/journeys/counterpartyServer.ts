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
 * Everything the counterparty journey reaches the server with.
 *
 * The party journey names its calls here for the same reason: a step's body is
 * a function of the answers these calls give, and a test that hands the page a
 * server of its own walks the whole journey with no transport and no broker in
 * the way.
 *
 * The landing list reads one page of counterparties and then one child read per
 * child type for that page, which is the shape the journey document settled on:
 * the identifiers and the contacts are read by `counterparty_id_one_of` rather
 * than carried on the counterparty itself. The confirm is the one write, and it
 * is one act over the whole graph.
 */

import { useMemo } from 'react';
import { counterparties } from '../api/counterparties.js';
import type {
    CounterpartyPickLists,
    CounterpartyPage,
    CounterpartyPageQuery,
    CounterpartyWriteOutcome,
} from '../api/counterparties.js';
import type { CounterpartyChildren, CounterpartyIntent } from './counterpartyState.js';
import type { PartyCounterparty } from '@ores/wire-protocol/generated/refdata/domain/party_counterparty';
import type { PutCounterpartyCompositeRequest } from '@ores/wire-protocol/generated/refdata/protocol/counterparty_protocol';

export type {
    CounterpartyChildren,
    CounterpartyPage,
    CounterpartyPageQuery,
    CounterpartyPickLists,
    CounterpartyWriteOutcome,
};

export interface CounterpartyServer {
    /** One page of the tenant's counterparties, searched and filtered on the server. */
    readonly listCounterparties: (query: CounterpartyPageQuery) => Promise<CounterpartyPage>;
    /**
     * The identifiers, contacts and centres of a page of counterparties.
     *
     * One call per child type whatever the page holds, which is what stops a
     * landing list of twenty rows costing sixty calls.
     */
    readonly childrenOf: (
        counterpartyIds: readonly string[],
    ) => Promise<readonly CounterpartyChildren[]>;
    /** The parties that may trade with a counterparty. */
    readonly visibilityOf: (counterpartyId: string) => Promise<readonly PartyCounterparty[]>;
    /** Every list a step's picker draws from. */
    readonly pickLists: () => Promise<CounterpartyPickLists>;
    /** Writes the counterparty and every child the review staged, as one act. */
    readonly writeComposite: (
        request: PutCounterpartyCompositeRequest,
    ) => Promise<CounterpartyWriteOutcome>;
    /**
     * Records the business centres the counterparty deals through.
     *
     * The composite write does not carry them, because the centre is a junction
     * row rather than a column of the counterparty; the confirm sends this
     * after it, against the id the composite reply named.
     */
    readonly writeBusinessCentres: (
        counterpartyId: string,
        codes: readonly string[],
        intent: CounterpartyIntent,
    ) => Promise<CounterpartyWriteOutcome>;
    /**
     * Records which parties may trade with the counterparty.
     *
     * The visibility junction is its own subject rather than a field of the
     * composite, so the confirm sends it after the composite names the id.
     */
    readonly writeVisibility: (
        partyId: string,
        counterpartyId: string,
        intent: CounterpartyIntent,
    ) => Promise<CounterpartyWriteOutcome>;
}

/** The deployment's own server, as the counterparty journey reaches it. */
export function useCounterpartyServer(): CounterpartyServer {
    return useMemo<CounterpartyServer>(
        () => ({
            listCounterparties: counterparties.page,
            childrenOf: counterparties.children,
            visibilityOf: counterparties.visibility,
            pickLists: counterparties.pickLists,
            writeComposite: counterparties.writeComposite,
            writeBusinessCentres: counterparties.writeBusinessCentres,
            writeVisibility: counterparties.writeVisibility,
        }),
        [],
    );
}
