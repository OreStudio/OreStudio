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
 * The instrument convention journey.
 *
 * A person who maintains the tenant's reference data authors the terms ORE uses
 * for one instrument: the family, the convention, its terms, one review and one
 * write. The pick lists and the reasons do not change while the person walks the
 * rail, so they are read once here rather than by each step.
 */

import { useEffect, useState, type ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { JourneyPage } from './JourneyPage.js';
import { conventionHeader, conventionSteps } from './conventionsSteps.js';
import { useConvention } from './conventionsState.js';
import type { ConventionPickLists, ConventionsServer } from './conventionsServer.js';

export interface ConventionJourneyProps {
    readonly server: ConventionsServer;
    /** The tenant's own party, which a written convention names. */
    readonly partyId: string;
    /** The journey is over, so the screen behind it takes the browser back. */
    readonly onFinished: () => void;
}

type Reasons = Awaited<ReturnType<ConventionsServer['amendReasons']>>;

export function ConventionJourney({
    server,
    partyId,
    onFinished,
}: ConventionJourneyProps): ReactNode {
    const { t } = useTranslation();
    const convention = useConvention();
    const [at, setAt] = useState(0);
    const [pickLists, setPickLists] = useState<ConventionPickLists>();
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

    const steps = conventionSteps({
        t,
        server,
        state: convention,
        partyId,
        pickLists,
        pickFailure,
        reasons,
        onMove: setAt,
        onFinished,
    });

    return (
        <JourneyPage
            steps={steps}
            at={at}
            onMove={setAt}
            header={conventionHeader(t, convention)}
        />
    );
}
