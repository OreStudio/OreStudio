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

import type { FeedFolder } from '../../api/client.js';

/** What runs under a folder: running, stopped or some of its feeds. */
export type RunState = 'running' | 'stopped' | 'partial' | 'empty';

/**
 * The feeds under a node, the sum of the folders it stands for, and how many of
 * them run. Only a provider's source states what runs, so `running` is what
 * `servedTotals` makes of these counts and is otherwise zero.
 */
export interface Totals {
    readonly fx: number;
    readonly ir: number;
    readonly running: number;
}

export const NO_FEEDS: Totals = { fx: 0, ir: 0, running: 0 };

/** The feeds a node holds in all. */
export function totalOf(totals: Totals): number {
    return totals.fx + totals.ir;
}

/** Running when all run, stopped when none runs, and partial between; empty with no feeds. */
export function stateOf(totals: Totals): RunState {
    const all = totalOf(totals);
    if (all === 0) {
        return 'empty';
    }
    if (totals.running === 0) {
        return 'stopped';
    }
    return totals.running >= all ? 'running' : 'partial';
}

/** The totals of the given folders. */
export function totalsOf(folders: readonly FeedFolder[]): Totals {
    return folders.reduce<Totals>(
        (out, folder) => ({
            fx: out.fx + folder.fx_feeds,
            ir: out.ir + folder.ir_feeds,
            running: 0,
        }),
        NO_FEEDS,
    );
}
