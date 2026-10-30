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

import { z } from 'zod';
import { request } from './transport.js';

/**
 * The rail's grouping: the party's books, or the counterparties its rows face.
 *
 * The two are different topologies over the same rows, not two lists: a person
 * chooses which one they are reading by, and the rows are the same trades and
 * structures either way.
 */
export type BlotterView = 'portfolio' | 'counterparty';

/** One node of the rail. Its id names its kind, and the party is the empty id. */
export interface BlotterNode {
    readonly id: string;
    readonly label: string;
    readonly kind: 'party' | 'portfolio' | 'book' | 'counterparty';
    readonly parent: string;
    /** The rows placed under it: a structure counts once, where it is placed. */
    readonly count: number;
}

/** One row: a trade no structure holds, or a whole structure. */
export interface BlotterRow {
    readonly row_kind: string;
    readonly id: string;
    /** The structure's own reference. A trade row carries none. */
    readonly reference: string;
    /** The structure that holds this trade, when one does. */
    readonly structure: string;
    readonly structure_reference: string;
    readonly instrument_type: string;
    readonly book_id: string;
    readonly book_name: string;
    readonly counterparty_id: string;
    readonly counterparty_name: string;
    readonly status: string;
    readonly trade_date: string;
    /** Whether the structure's legs sit in more than one book. */
    readonly spans_books: boolean;
}

export interface BlotterPage {
    readonly tree: readonly BlotterNode[];
    readonly rows: readonly BlotterRow[];
    readonly total: number;
}

/** What the screen asks the read for. */
export interface BlotterQuery {
    readonly view: BlotterView;
    /** The node the list is narrowed to, or empty for the party. */
    readonly node: string;
    readonly flat: boolean;
    readonly search: string;
    readonly offset: number;
    readonly limit: number;
    readonly sort: string;
    readonly descending: boolean;
}

const nodeSchema = z.looseObject({
    id: z.string(),
    label: z.string(),
    kind: z.enum(['party', 'portfolio', 'book', 'counterparty']),
    parent: z.string().default(''),
    count: z.coerce.number().int().default(0),
});

const rowSchema = z.looseObject({
    row_kind: z.string(),
    id: z.string(),
    reference: z.string().default(''),
    structure: z.string().default(''),
    structure_reference: z.string().default(''),
    instrument_type: z.string().default(''),
    book_id: z.string().default(''),
    book_name: z.string().default(''),
    counterparty_id: z.string().default(''),
    counterparty_name: z.string().default(''),
    status: z.string().default(''),
    trade_date: z.string().default(''),
    spans_books: z.boolean().default(false),
});

const pageSchema = z.looseObject({
    tree: z.array(nodeSchema).default([]),
    rows: z.array(rowSchema).default([]),
    total: z.coerce.number().int().default(0),
});

export const blotter = {
    /**
     * One page of the blotter, with the rail beside it.
     *
     * The rail is read with the page rather than before it: its figures are the
     * rows placed on each node, and a count a screen draws is part of the answer
     * to the page it shows.
     */
    async page(query: BlotterQuery): Promise<BlotterPage> {
        const params = new URLSearchParams({
            view: query.view,
            node: query.node,
            flat: query.flat ? '1' : '0',
            search: query.search,
            offset: String(query.offset),
            limit: String(query.limit),
            // A list that has not been sorted reads newest first, which is the
            // order a blotter is read in.
            sort: query.sort === '' ? 'trade_date' : query.sort,
            descending: (query.sort === '' ? true : query.descending) ? 'true' : 'false',
        });
        return pageSchema.parse(
            await request(`/api/blotter?${params.toString()}`, { method: 'GET' }),
        ) as BlotterPage;
    },
};
