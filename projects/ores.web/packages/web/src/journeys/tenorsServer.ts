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
 * Everything the tenor journey reaches the server with.
 *
 * A step's body is a function of the answers these calls give, so a test that
 * hands the page a server of its own walks the whole journey with no transport
 * in the way.
 */

import { useMemo } from 'react';
import { api } from '../api/client.js';
import { tenors } from '../api/tenors.js';
import type {
    TenorCatalogue,
    TenorIntent,
    TenorPickLists,
    TenorResolutionDraft,
    TenorResolution,
    TenorWriteOutcome,
} from '../api/tenors.js';
import type { Tenor } from '@ores/wire-protocol/generated/refdata/domain/tenor';
import type { TenorConvention } from '@ores/wire-protocol/generated/refdata/domain/tenor_convention';
import type { TenorConventionResolution } from '@ores/wire-protocol/generated/refdata/domain/tenor_convention_resolution';

export type {
    TenorCatalogue,
    TenorIntent,
    TenorPickLists,
    TenorResolutionDraft,
    TenorResolution,
    TenorWriteOutcome,
};

export interface TenorsServer {
    /** Every tenor, every convention and every schedule axis. */
    readonly catalogue: () => Promise<TenorCatalogue>;
    /** Every list a picker draws from. */
    readonly pickLists: () => Promise<TenorPickLists>;
    /** The tenors one convention resolves. */
    readonly resolutions: (convention: string) => Promise<readonly TenorConventionResolution[]>;
    /** The date a drafted rule resolves to, whether or not the rule is accepted. */
    readonly previewResolution: (draft: TenorResolutionDraft) => Promise<TenorResolution>;
    /** The reasons a record may be amended for. */
    readonly amendReasons: () => Promise<
        readonly {
            readonly code: string;
            readonly description: string;
            readonly requiresCommentary: boolean;
        }[]
    >;
    readonly writeTenor: (
        write: Tenor,
        version: number | null,
        intent: TenorIntent,
    ) => Promise<TenorWriteOutcome<Tenor>>;
    readonly writeConvention: (
        write: TenorConvention,
        version: number | null,
        intent: TenorIntent,
    ) => Promise<TenorWriteOutcome<TenorConvention>>;
    readonly writeMembership: (
        convention: string,
        tenor: string,
        anchorOverride: string | null,
        version: number | null,
        intent: TenorIntent,
    ) => Promise<TenorWriteOutcome<null>>;
    readonly removeMembership: (
        convention: string,
        tenor: string,
        version: number,
        intent: TenorIntent,
    ) => Promise<TenorWriteOutcome<null>>;
}

/** The deployment's own server, as the tenor journey reaches it. */
export function useTenorsServer(): TenorsServer {
    return useMemo<TenorsServer>(
        () => ({
            catalogue: tenors.catalogue,
            pickLists: tenors.pickLists,
            resolutions: tenors.resolutions,
            previewResolution: tenors.previewResolution,
            amendReasons: api.amendReasons,
            writeTenor: tenors.writeTenor,
            writeConvention: tenors.writeConvention,
            writeMembership: tenors.writeMembership,
            removeMembership: tenors.removeMembership,
        }),
        [],
    );
}
