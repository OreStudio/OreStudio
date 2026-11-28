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
 * What the tables screen reads out of the largest-relations answer.
 *
 * The answer starts from the relations, so it holds tables the catalogue never
 * names. The screen's job is to state three things about each: how large it is,
 * what share of the database that is, and whether anything names it. The pie is
 * the head of the same list, with everything it does not name folded into one
 * slice so the shares still describe the whole.
 */

import type { LargestRelation } from '@ores/wire-protocol/browser';
import type { PieSlice } from '../ui/PieChart.js';

/** The slices the pie offers, so a person reads a handful of shapes and not eighty. */
export const TOP_SLICES = [5, 10, 20] as const;

export type TopSliceCount = (typeof TOP_SLICES)[number];

/**
 * The pie's slices: the largest N named relations, and one slice for everything
 * else the database holds.
 *
 * The remainder is the database's size less the head, and never a sum of the
 * rows: the rows are the largest relations and not every relation, so a total
 * built from them would state a whole the read never measured. A remainder of
 * nothing or less adds no slice.
 */
export function pieSlices(
    rows: readonly LargestRelation[],
    databaseBytes: number,
    top: TopSliceCount,
    restLabel: string,
): readonly PieSlice[] {
    const head = rows.slice(0, top);
    const slices: PieSlice[] = head.map((row) => ({
        key: row.relation_name,
        label: row.relation_name,
        value: row.size_bytes,
    }));
    const named = head.reduce((sum, row) => sum + row.size_bytes, 0);
    const rest = databaseBytes - named;
    if (rest > 0) {
        slices.push({ key: 'rest', label: restLabel, value: rest });
    }
    return slices;
}

/** One relation's share of the database, from 0 to 1. */
export function relationShare(sizeBytes: number, databaseBytes: number): number {
    if (!Number.isFinite(sizeBytes) || !Number.isFinite(databaseBytes) || databaseBytes <= 0) {
        return 0;
    }
    return Math.max(sizeBytes, 0) / databaseBytes;
}

/**
 * Which rows the screen shows: the largest few, or every row the read answered.
 *
 * A person reads the head first and then asks for the rest, so the screen shows
 * the head by default and never lists hundreds of rows unasked.
 */
export const TABLE_ROWS = [10, 25, 100] as const;

export type TableRowCount = (typeof TABLE_ROWS)[number];
