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
 * The blotter's trade list.
 *
 * The BFF reads the trade summary read model and enriches each page, so this
 * module states only the HTTP path the screen calls and the shape it parses.
 * A field name is the C++ member name, because that is the key the server
 * serialises.
 */

/** The fields the server can order the read model by. */
export const TRADE_SORTS = ['trade_date', 'instrument_type', 'status', 'row_kind'] as const;

/** One row of the list: a trade or a structure, and what the page adds to it. */
export interface TradeRow {
    readonly id: string;
    readonly row_kind: string;
    readonly party_id: string;
    readonly counterparty_id: string | null;
    readonly book_id: string | null;
    readonly instrument_type: string;
    readonly status: string | null;
    readonly authorisation_status: string;
    readonly trade_date: string | null;
    readonly trade_count: number;
    readonly search_text: string;
    readonly identifier: string;
    readonly counterparty_name: string;
    readonly book_name: string;
    readonly version: number | null;
    readonly external_version: number | null;
}

/** The filters one page is read with, in the words the screen uses. */
export interface TradePageQuery {
    readonly offset: number;
    readonly limit: number;
    readonly search: string;
    readonly sort: string;
    readonly descending: boolean;
    readonly status: string;
    readonly type: string;
}

/** One page of trades and the total the filter matches. */
export interface TradePage {
    readonly rows: readonly TradeRow[];
    readonly total: number;
}

const tradeRowSchema: z.ZodType<TradeRow> = z.object({
    id: z.string(),
    row_kind: z.string(),
    party_id: z.string(),
    counterparty_id: z.string().nullable(),
    book_id: z.string().nullable(),
    instrument_type: z.string(),
    status: z.string().nullable(),
    authorisation_status: z.string(),
    trade_date: z.string().nullable(),
    trade_count: z.coerce.number().int(),
    search_text: z.string().default(''),
    identifier: z.string().default(''),
    counterparty_name: z.string().default(''),
    book_name: z.string().default(''),
    version: z.coerce.number().int().nullable().default(null),
    external_version: z.coerce.number().int().nullable().default(null),
});

export const trades = {
    /** One page of trades, searched, filtered and ordered on the server. */
    async page(query: TradePageQuery): Promise<TradePage> {
        const params = new URLSearchParams({
            offset: String(query.offset),
            limit: String(query.limit),
            search: query.search,
            sort: query.sort === '' ? 'trade_date' : query.sort,
            descending: String(query.descending),
            status: query.status,
            type: query.type,
        });
        return z
            .object({ rows: z.array(tradeRowSchema), total: z.int().nonnegative() })
            .parse(await request(`/api/trades?${params.toString()}`, { method: 'GET' }));
    },
};
