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

import type { FeedFilter, FeedSelection } from '../../api/client.js';

/**
 * The feeds a person picked in the feed table. Either they picked rows one by
 * one, or they asked for every feed the filter matches and left some out. The
 * second never holds the ids of the feeds it means, so it works for a set far
 * larger than a page.
 */
export type Selection =
    | { readonly mode: 'ids'; readonly ids: ReadonlySet<string> }
    | { readonly mode: 'all'; readonly excluded: ReadonlySet<string> };

export const NOTHING: Selection = { mode: 'ids', ids: new Set() };

/** Whether a feed is picked. */
export function isPicked(selection: Selection, id: string): boolean {
    return selection.mode === 'all' ? !selection.excluded.has(id) : selection.ids.has(id);
}

/** How many feeds are picked, given how many the filter matches. */
export function pickedCount(selection: Selection, matching: number): number {
    return selection.mode === 'all'
        ? Math.max(0, matching - selection.excluded.size)
        : selection.ids.size;
}

/** Picks or drops some feeds. */
export function setPicked(selection: Selection, ids: readonly string[], on: boolean): Selection {
    if (selection.mode === 'all') {
        const excluded = new Set(selection.excluded);
        for (const id of ids) {
            if (on) {
                excluded.delete(id);
            } else {
                excluded.add(id);
            }
        }
        return { mode: 'all', excluded };
    }
    const picked = new Set(selection.ids);
    for (const id of ids) {
        if (on) {
            picked.add(id);
        } else {
            picked.delete(id);
        }
    }
    return { mode: 'ids', ids: picked };
}

/** Every feed the filter matches. */
export const everything: Selection = { mode: 'all', excluded: new Set() };

/** Whether every one of these feeds is picked. */
export function allPicked(selection: Selection, ids: readonly string[]): boolean {
    return ids.length > 0 && ids.every((id) => isPicked(selection, id));
}

/**
 * The request that starts or stops a selection. A picked set names its feeds and
 * the rest of the filter is not sent, because the ids are the whole answer. An
 * "all matching" set sends the filter and the feeds left out.
 */
export function selectionRequest(selection: Selection, filter: FeedFilter): FeedSelection {
    return selection.mode === 'ids'
        ? { feedIds: [...selection.ids] }
        : { ...filter, excludedIds: [...selection.excluded] };
}

/** The most feeds one request may name; the server refuses more. */
export const MOST_NAMED = 2000;

/** Whether the selection can be sent: a long list of ids must be "all matching" instead. */
export function sendable(selection: Selection): boolean {
    return selection.mode === 'ids'
        ? selection.ids.size <= MOST_NAMED
        : selection.excluded.size <= MOST_NAMED;
}
