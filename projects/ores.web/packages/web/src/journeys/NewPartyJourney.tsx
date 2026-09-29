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
 * The new party journey: adding a party to the tenant the person works in.
 *
 * The person is a signed-in tenant administrator, so the tenant is not a
 * question and neither is the data: a party's reference data is the
 * deployment's own, and the server holds the starting point that publishes it.
 * What the person answers is which legal entity this party is, what the tenant
 * will call it, and then whether to work as it.
 *
 * Nothing is read before the first step, because nothing on it comes from the
 * server: the search is asked only once somebody types into it, and the
 * deployment's party stage is stated by the run rather than by a screen.
 */

import { useState, type ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { JourneyPage } from './JourneyPage.js';
import { partyHeader, newPartySteps } from './newPartySteps.js';
import { useNewParty } from './partyState.js';
import type { JourneyServer } from './server.js';

export interface NewPartyJourneyProps {
    readonly server: JourneyServer;
    /** The journey is over, so the screen behind it takes the browser back. */
    readonly onFinished: () => void;
}

/**
 * Working as the party the journey just made.
 *
 * It is a function rather than a closure so the exit can be walked without a
 * browser: what it does is re-scope the session to the party, which is one
 * write and one answer. A journey that has not recorded a party yet has nothing
 * to scope to, and the guard says so once rather than at every call site.
 */
export async function workInNewParty(
    server: Pick<JourneyServer, 'switchParty'>,
    partyId: string | undefined,
): Promise<void> {
    if (partyId === undefined) {
        return;
    }
    await server.switchParty(partyId);
}

export function NewPartyJourney({ server, onFinished }: NewPartyJourneyProps): ReactNode {
    const { t } = useTranslation();
    /*
     * Where the rail stands, and nothing until somebody moves: the journey
     * opens on the legal entity, which is the first thing it asks.
     */
    const [at, setAt] = useState<number>();
    const party = useNewParty();

    const steps = newPartySteps({
        t,
        server,
        state: party,
        onWorkInIt: async () => {
            await workInNewParty(server, party.partyId);
            onFinished();
        },
        onAnother: () => {
            party.restart();
            setAt(0);
        },
        onDone: onFinished,
    });

    return <JourneyPage steps={steps} at={at ?? 0} onMove={setAt} header={partyHeader(t, party)} />;
}
