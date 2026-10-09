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
 * The counterparty onboarding journey.
 *
 * A tenant administrator, or a person holding the Operations role, brings a
 * counterparty on board: the identity, the names it answers to, the people it
 * is reached through, the agreements it trades under with their collateral
 * terms, one review, one write and one outcome. The screen behind the journey
 * is the counterparty list, and the journey returns to it when it is done.
 *
 * The two reads the whole screen depends on, the pick lists and the
 * counterparties a new one may name as its parent, are taken once here rather
 * than by each step that needs them, because they do not change while the
 * person is walking the rail.
 */

import { useEffect, useState, type ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { JourneyPage } from './JourneyPage.js';
import { counterpartyStepIndex, counterpartySteps } from './counterpartySteps.js';
import { useCounterparty, type NewCounterparty } from './counterpartyState.js';
import type { CounterpartyPickLists, CounterpartyServer } from './counterpartyServer.js';
import type { Counterparty } from '@ores/wire-protocol/generated/refdata/domain/counterparty';
import type { Translator } from '../i18n/translate.js';

export interface CounterpartyJourneyProps {
    readonly server: CounterpartyServer;
    /** The tenant's own party, which an agreement's fixed tenant side names. */
    readonly partyId: string;
    /** The journey is over, so the screen behind it takes the browser back. */
    readonly onFinished: () => void;
}

/**
 * What stands above every step once the counterparty has a name.
 *
 * A person who looked away should not have to walk back up the rail to remember
 * which counterparty the run is about, so the name and the short code stand
 * above whatever the step below is asking.
 */
export function counterpartyHeader(
    t: Translator['t'],
    state: NewCounterparty,
): ReactNode | undefined {
    if (state.fullName === '') {
        return undefined;
    }
    return (
        <div className="flex items-baseline justify-between gap-3 text-sm">
            <span className="truncate font-medium">{state.fullName}</span>
            <span className="truncate font-mono text-xs text-ink-faint">
                {state.shortCode === '' ? t('journey.counterparty.noShortCode') : state.shortCode}
            </span>
        </div>
    );
}

export function CounterpartyJourney({
    server,
    partyId,
    onFinished,
}: CounterpartyJourneyProps): ReactNode {
    const { t } = useTranslation();
    const counterparty = useCounterparty();
    const [at, setAt] = useState(0);
    const [pickLists, setPickLists] = useState<CounterpartyPickLists>();
    const [pickFailure, setPickFailure] = useState<string>();
    const [parents, setParents] = useState<readonly Counterparty[]>([]);

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
                const page = await server.listCounterparties({
                    offset: 0,
                    limit: 200,
                    search: '',
                    status: 'all',
                });
                if (!cancelled) {
                    setParents(page.rows);
                }
            } catch {
                // The parent picker offers no parent rather than refusing the step.
            }
        };
        void load();
        return () => {
            cancelled = true;
        };
    }, [server]);

    const steps = counterpartySteps({
        t,
        server,
        state: counterparty,
        partyId,
        pickLists,
        pickFailure,
        parents,
        onMove: setAt,
        onFinished,
        onAnother: () => setAt(counterpartyStepIndex('list')),
    });

    return (
        <JourneyPage
            steps={steps}
            at={at}
            onMove={setAt}
            header={counterpartyHeader(t, counterparty)}
        />
    );
}
