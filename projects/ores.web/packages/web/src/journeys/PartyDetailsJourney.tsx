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
 * The party details journey.
 *
 * A tenant administrator corrects a legal entity of their own tenant: its
 * record, its identifiers, its contacts, the countries and currencies it works
 * in, and the counterparties and business units it is made of. The screen
 * behind the journey is the party list, and the journey returns to it when it
 * is done.
 *
 * The pick lists and the reasons are read once here rather than by each step,
 * because they do not change while the person walks the rail.
 */

import { useEffect, useState, type ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { JourneyPage } from './JourneyPage.js';
import { partyHeader, partySteps } from './partyDetailsSteps.js';
import { useParty } from './partyDetailsState.js';
import type { PartyDetailsServer, PartyPickLists } from './partyDetailsServer.js';

export interface PartyDetailsJourneyProps {
    readonly server: PartyDetailsServer;
    /** The journey is over, so the screen behind it takes the browser back. */
    readonly onFinished: () => void;
}

type Reasons = Awaited<ReturnType<PartyDetailsServer['amendReasons']>>;

export function PartyDetailsJourney({ server, onFinished }: PartyDetailsJourneyProps): ReactNode {
    const { t } = useTranslation();
    const party = useParty();
    const [at, setAt] = useState(0);
    const [pickLists, setPickLists] = useState<PartyPickLists>();
    const [pickFailure, setPickFailure] = useState<string>();
    const [reasons, setReasons] = useState<Reasons>([]);

    useEffect(() => {
        let cancelled = false;
        const load = async (): Promise<void> => {
            try {
                const lists = await server.pickLists();
                if (!cancelled) {
                    setPickLists(lists);
                    setPickFailure(undefined);
                }
            } catch (error) {
                if (!cancelled) {
                    setPickFailure(error instanceof Error ? error.message : String(error));
                }
            }
            try {
                const read = await server.amendReasons();
                if (!cancelled) {
                    setReasons(read);
                }
            } catch {
                // The review then offers the default reason rather than refusing the step.
            }
        };
        void load();
        return () => {
            cancelled = true;
        };
    }, [server]);

    const steps = partySteps({
        t,
        server,
        state: party,
        pickLists,
        pickFailure,
        reasons,
        onMove: setAt,
        onFinished,
    });

    return <JourneyPage steps={steps} at={at} onMove={setAt} header={partyHeader(t, party)} />;
}
