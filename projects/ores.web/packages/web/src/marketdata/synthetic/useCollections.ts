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
import { api, type FeedFolder } from '../../api/client.js';

/** The key of every read of the folders, so one refresh renews them all. */
export const FOLDERS_KEY = ['feed-folders'] as const;

/** How often the feed screens ask again, since a feed can start or stop from anywhere. */
export const EVERY_MS = 5_000;

/**
 * The collections the caller sees: the children of the root folders, each with
 * the feeds under it and how many run. The roots are one folder for each party.
 */
export function useCollections(): {
    readonly collections: readonly FeedFolder[];
    readonly loaded: boolean;
    readonly error: string;
} {
    const roots = useQuery({
        queryKey: [...FOLDERS_KEY, 'roots'],
        queryFn: () => api.feedFolders(''),
        refetchInterval: EVERY_MS,
    });
    const rootIds = (roots.data ?? []).map((root) => root.id);
    const collections = useQuery({
        queryKey: [...FOLDERS_KEY, 'collections', rootIds],
        queryFn: async () => (await Promise.all(rootIds.map((id) => api.feedFolders(id)))).flat(),
        enabled: roots.data !== undefined,
        refetchInterval: EVERY_MS,
    });
    return {
        collections: collections.data ?? [],
        loaded: collections.data !== undefined,
        error: roots.error?.message ?? collections.error?.message ?? '',
    };
}
