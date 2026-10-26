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

import type { FolderFeed } from '../../api/client.js';
import { PATHS } from '../../shell/paths.js';

/** The address of a feed's folder in the trail of the feed's page. */
export const folderKey = (id: string): string => `folder:${id}`;

/** The plain facts of a folder row, as the folders list gives them. */
export interface FolderRow {
    readonly id: string;
    readonly parent_id: string;
    readonly name: string;
    readonly kind: string;
}

/**
 * The folders from a feed's folder up to the collection, outermost first,
 * without the root, which the providers take the place of.
 */
export function folderChain(rows: readonly FolderRow[], id: string): readonly FolderRow[] {
    const byId = new Map(rows.map((row) => [row.id, row]));
    const chain: FolderRow[] = [];
    // A folder has one parent and the database refuses a cycle; the bound is for a bad read.
    for (
        let at = byId.get(id);
        at !== undefined && chain.length < 16;
        at = byId.get(at.parent_id)
    ) {
        if (at.kind !== 'root') {
            chain.unshift(at);
        }
    }
    return chain;
}

/** The address of a feed's own page. */
export function feedPageOf(feed: Pick<FolderFeed, 'kind' | 'id'>): string {
    return `${feed.kind === 'ir_curve' ? PATHS.irFeed : PATHS.fxFeed}/${encodeURIComponent(feed.id)}`;
}
