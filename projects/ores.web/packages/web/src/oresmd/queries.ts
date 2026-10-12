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

import { keepPreviousData, queryOptions } from '@tanstack/react-query';
import type { OresmdComposeRequest } from '@ores/contracts';
import { oresmd } from '../api/oresmd.js';
import { keyOf } from './editor.js';

/**
 * The reads of the oresmd editor. Each is quiet: the editor states a failure
 * itself, where the person is typing, so the app's banner would only repeat it.
 */
const QUIET = { quiet: true } as const;

/** The schema is the codec's table and changes with a release, so it is kept for the session. */
const SCHEMA_STALE_MS = 5 * 60 * 1000;

export const oresmdQueries = {
    schema: () =>
        queryOptions({
            queryKey: ['oresmd-schema'],
            queryFn: () => oresmd.schema(),
            staleTime: SCHEMA_STALE_MS,
            meta: QUIET,
        }),

    parse: (uri: string) =>
        queryOptions({
            queryKey: ['oresmd-parse', uri],
            queryFn: () => oresmd.parse(uri),
            meta: QUIET,
        }),

    /** The URI the parts write. The previous answer stays while the next arrives, so the preview does not flash. */
    compose: (parts: OresmdComposeRequest | null) =>
        queryOptions({
            queryKey: ['oresmd-compose', keyOf(parts)],
            queryFn: () => oresmd.compose(parts as OresmdComposeRequest),
            enabled: parts !== null,
            placeholderData: keepPreviousData,
            meta: QUIET,
        }),
};
