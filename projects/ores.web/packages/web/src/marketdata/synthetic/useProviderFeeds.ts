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
import { api } from '../../api/client.js';
import { EVERY_MS } from './useCollections.js';

/** The key of every read of how many feeds a provider holds. */
export const HELD_KEY = ['provider-feeds-held'] as const;

/** The asset classes a feed can have, by the code the feed carries. */
export const ASSET_CLASSES = ['fx', 'interest_rates'] as const;

/** How many feeds a provider holds, by asset class. */
export interface FeedCounts {
    readonly fx: number;
    readonly ir: number;
}

/** The feeds of a provider in all. */
export function feedsHeld(counts: FeedCounts | undefined): number {
    return counts === undefined ? 0 : counts.fx + counts.ir;
}

/**
 * How many feeds each provider holds, counted from its feeds: the service counts
 * the feeds one asset class matches, so no folder's count stands beside them. A
 * provider the service answers nothing for holds none.
 */
export function useProviderFeedCounts(
    codes: readonly string[],
): ReadonlyMap<string, FeedCounts> | undefined {
    const counts = useQuery({
        queryKey: [...HELD_KEY, codes],
        queryFn: async () => {
            const held = await Promise.all(
                codes.map(async (code): Promise<readonly [string, FeedCounts]> => {
                    const of = async (assetClass: string): Promise<number> =>
                        (
                            await api.folderFeeds(
                                { provider: code, assetClass },
                                {
                                    offset: 0,
                                    limit: 1,
                                },
                            )
                        ).total;
                    return [
                        code,
                        { fx: await of(ASSET_CLASSES[0]), ir: await of(ASSET_CLASSES[1]) },
                    ];
                }),
            );
            return new Map(held);
        },
        enabled: codes.length > 0,
        refetchInterval: EVERY_MS,
    });
    return counts.data;
}
