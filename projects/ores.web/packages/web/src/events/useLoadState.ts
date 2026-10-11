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

import { useIsFetching, useQueryClient, type QueryKey } from '@tanstack/react-query';
import type { LoadState } from './useEntityChanges.js';

/** Whether a query's key starts with the given parts. */
function startsWith(key: QueryKey, prefix: QueryKey): boolean {
    return prefix.every((part, index) => key[index] === part);
}

/**
 * When the data of several queries last arrived, when the oldest of it did, and
 * whether any is loading.
 *
 * A page made of many reads is stale when any of what it shows was loaded before
 * a change, so the newest arrival of the group is the page's load. The queries
 * are named by the start of their keys.
 */
export function useLoadState(prefixes: readonly QueryKey[]): LoadState {
    const queries = useQueryClient();
    const matches = (key: QueryKey): boolean => prefixes.some((prefix) => startsWith(key, prefix));
    const isFetching = useIsFetching({ predicate: (query) => matches(query.queryKey) }) > 0;
    // Only the reads a screen is showing count. A page of a list that was read
    // earlier and is no longer on screen stays in the cache, and its age says
    // nothing about the data in front of the reader.
    const arrivals = queries
        .getQueryCache()
        .findAll({ type: 'active', predicate: (query) => matches(query.queryKey) })
        .map((query) => query.state.dataUpdatedAt)
        .filter((arrived) => arrived > 0);
    return {
        dataUpdatedAt: Math.max(0, ...arrivals),
        oldestUpdatedAt: arrivals.length === 0 ? 0 : Math.min(...arrivals),
        isFetching,
    };
}
