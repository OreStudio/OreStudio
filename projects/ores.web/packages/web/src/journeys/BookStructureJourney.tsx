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
 * The book structure journey.
 *
 * A tenant administrator shapes the book structure: which portfolio each book
 * sits in, what the book is for, how it is classified, and who may see it. The
 * screen behind the journey is the book tree, and the journey returns to it when
 * it is done.
 *
 * The tree is read here and again after a write, so the tree and the outcome
 * agree. The pick lists and the reasons do not change while the person walks the
 * rail, so they are read once.
 */

import { useCallback, useEffect, useState, type ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { JourneyPage } from './JourneyPage.js';
import { bookHeader, bookSteps } from './booksSteps.js';
import { useBookStructure } from './booksState.js';
import type { BookPickLists, BookTree, BooksServer } from './booksServer.js';

export interface BookStructureJourneyProps {
    readonly server: BooksServer;
    /** The tenant's own party, which a written book and portfolio name. */
    readonly partyId: string;
    /** The journey is over, so the screen behind it takes the browser back. */
    readonly onFinished: () => void;
}

type Reasons = Awaited<ReturnType<BooksServer['amendReasons']>>;

export function BookStructureJourney({
    server,
    partyId,
    onFinished,
}: BookStructureJourneyProps): ReactNode {
    const { t } = useTranslation();
    const structure = useBookStructure();
    const [at, setAt] = useState(0);
    const [tree, setTree] = useState<BookTree>();
    const [pickLists, setPickLists] = useState<BookPickLists>();
    const [pickFailure, setPickFailure] = useState<string>();
    const [reasons, setReasons] = useState<Reasons>([]);

    const readTree = useCallback(async (): Promise<void> => {
        try {
            setTree(await server.tree());
            setPickFailure(undefined);
        } catch (error) {
            setPickFailure(error instanceof Error ? error.message : String(error));
        }
    }, [server]);

    useEffect(() => {
        let cancelled = false;
        const load = async (): Promise<void> => {
            await readTree();
            try {
                const lists = await server.pickLists();
                if (!cancelled) {
                    setPickLists(lists);
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
    }, [server, readTree]);

    const steps = bookSteps({
        t,
        server,
        state: structure,
        partyId,
        tree,
        pickLists,
        pickFailure,
        reasons,
        onMove: setAt,
        onWritten: () => void readTree(),
        onFinished,
    });

    return <JourneyPage steps={steps} at={at} onMove={setAt} header={bookHeader(t, structure)} />;
}
