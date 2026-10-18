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

import { useQuery } from '@tanstack/react-query';
import { useMemo } from 'react';
import { useActingPartyId } from '../session/SessionProvider.js';
import { seriesTargetOf } from './identity.js';
import { chunksOf, oresmdQueries } from './queries.js';

/**
 * The page each URL opens, for a screen that draws many URLs at once.
 *
 * A record asks the codec about its one URL. A list of records asks about the URIs
 * of its rows in one read, and reads the party's series list, which the list needs
 * anyway, so a grid costs neither a call a row nor a call of its own. This is the
 * same rule a record uses, so a URL behaves one way on both.
 *
 * A URL has no page until the codec and the party's series list answer, and it has
 * none at all when it names no series. The caller draws plain text for both.
 */
export function useSeriesTargets(uris: readonly string[]): (uri: string) => string | undefined {
    const partyId = useActingPartyId();
    const distinct = useMemo(() => chunksOf(uris).flat(), [uris]);
    const parsed = useQuery({
        ...oresmdQueries.parseMany(distinct),
        enabled: distinct.length > 0,
    });
    const schema = useQuery({ ...oresmdQueries.schema(), enabled: distinct.length > 0 });
    const list = useQuery({
        ...oresmdQueries.seriesList(partyId ?? ''),
        enabled: distinct.length > 0 && partyId !== undefined,
    });
    const targets = useMemo(() => {
        const at = new Map<string, string | undefined>();
        if (schema.data === undefined || list.data === undefined) return at;
        distinct.forEach((uri, index) => {
            const parts = parsed.data?.[index];
            if (parts !== undefined)
                at.set(uri, seriesTargetOf(uri, schema.data, parts, list.data));
        });
        return at;
    }, [distinct, parsed.data, schema.data, list.data]);
    return (uri: string) => targets.get(uri);
}
