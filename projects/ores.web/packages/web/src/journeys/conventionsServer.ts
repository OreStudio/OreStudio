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
 * Everything the convention journey reaches the server with.
 *
 * A step's body is a function of the answers these calls give, so a test that
 * hands the page a server of its own walks the whole journey with no transport
 * in the way.
 */

import { useMemo } from 'react';
import { api } from '../api/client.js';
import { conventions } from '../api/conventions.js';
import type {
    ConventionFamilyCard,
    ConventionIntent,
    ConventionPage,
    ConventionPickLists,
    ConventionRow,
    ConventionWriteOutcome,
} from '../api/conventions.js';

export type {
    ConventionFamilyCard,
    ConventionIntent,
    ConventionPage,
    ConventionPickLists,
    ConventionRow,
    ConventionWriteOutcome,
};

export interface ConventionsServer {
    /** Every family with the entity that serves it and how many conventions it holds. */
    readonly families: () => Promise<readonly ConventionFamilyCard[]>;
    /** The conventions of one family. */
    readonly rowsOf: (family: string) => Promise<ConventionPage>;
    /** Every list a term picker draws from. */
    readonly pickLists: () => Promise<ConventionPickLists>;
    /** The reasons a record may be amended for. */
    readonly amendReasons: () => Promise<
        readonly {
            readonly code: string;
            readonly description: string;
            readonly requiresCommentary: boolean;
        }[]
    >;
    /** Writes a convention: as new when no version is given. */
    readonly write: (
        family: string,
        write: Readonly<Record<string, unknown>>,
        version: number | null,
        intent: ConventionIntent,
    ) => Promise<ConventionWriteOutcome>;
}

/** The deployment's own server, as the convention journey reaches it. */
export function useConventionsServer(): ConventionsServer {
    return useMemo<ConventionsServer>(
        () => ({
            families: conventions.families,
            rowsOf: conventions.page,
            pickLists: conventions.pickLists,
            amendReasons: api.amendReasons,
            write: conventions.write,
        }),
        [],
    );
}
