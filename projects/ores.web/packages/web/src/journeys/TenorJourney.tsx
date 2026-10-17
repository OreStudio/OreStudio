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
 * The tenor resolution journey.
 *
 * A person who maintains the tenant's reference data defines how a tenor
 * resolves: the tenor, the convention that resolves it, the tenors in that
 * convention's set, one review and one confirm. The catalogue, the pick lists
 * and the reasons do not change while the person walks the rail, so they are
 * read once here rather than by each step.
 */

import { useEffect, useState, type ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { JourneyPage } from './JourneyPage.js';
import { tenorHeader, tenorSteps } from './tenorsSteps.js';
import { useTenor } from './tenorsState.js';
import { useLabelCatalogue } from '../refdata/shared.js';
import type { TenorCatalogue, TenorPickLists, TenorsServer } from './tenorsServer.js';

export interface TenorJourneyProps {
    readonly server: TenorsServer;
    /** The journey is over, so the screen behind it takes the browser back. */
    readonly onFinished: () => void;
}

type Reasons = Awaited<ReturnType<TenorsServer['amendReasons']>>;

export function TenorJourney({ server, onFinished }: TenorJourneyProps): ReactNode {
    const { t } = useTranslation();
    const tenor = useTenor();
    /*
     * The badge catalogue, read once here rather than by each step: a tenor's
     * kind, its unit and a convention's anchor, algorithm and roll rule are all
     * codes, and every one of them is drawn through CodedValue so the journey
     * shows them the way the rest of the application does.
     */
    const labels = useLabelCatalogue();
    const [at, setAt] = useState(0);
    const [catalogue, setCatalogue] = useState<TenorCatalogue>();
    const [catalogueFailure, setCatalogueFailure] = useState<string>();
    const [pickLists, setPickLists] = useState<TenorPickLists>();
    const [pickFailure, setPickFailure] = useState<string>();
    const [reasons, setReasons] = useState<Reasons>([]);

    useEffect(() => {
        let cancelled = false;
        const load = async (): Promise<void> => {
            try {
                const read = await server.catalogue();
                if (!cancelled) {
                    setCatalogue(read);
                    setCatalogueFailure(undefined);
                }
            } catch (error) {
                if (!cancelled) {
                    setCatalogueFailure(error instanceof Error ? error.message : String(error));
                }
            }
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

    const steps = tenorSteps({
        labels,
        t,
        server,
        state: tenor,
        catalogue,
        catalogueFailure,
        pickLists,
        pickFailure,
        reasons,
        onMove: setAt,
        onFinished,
    });

    return <JourneyPage steps={steps} at={at} onMove={setAt} header={tenorHeader(t, tenor)} />;
}
