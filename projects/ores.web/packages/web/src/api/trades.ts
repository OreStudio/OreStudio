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

/** One identifier a trade carries. */
export interface TradeIdentifier {
    readonly id: string;
    readonly trade_id: string;
    readonly id_type: string;
    readonly id_value: string;
    readonly issuing_party_id: string;
}

/** One party role the trade was written with. */
export interface TradePartyRole {
    readonly trade_id: string;
    readonly party_id: string;
    readonly role_type: string;
}

/** The trade's booking terms. */
export interface TradeBooking {
    readonly trade_id: string;
    readonly trade_date: string | null;
    readonly execution_timestamp: string | null;
}

/** One link, seen from the trade the page is on. */
export interface TradeLink {
    readonly from_trade_id: string;
    readonly to_trade_id: string;
    readonly link_type: string;
}

/** One version of the trade, newest first. */
export interface TradeVersion {
    readonly version: number;
}

/** One trade, as its page reads it. */
export interface TradePageRecord {
    readonly trade: TradeRow;
    /** The terms, when the trade's type has a table; null when that table has no row. */
    readonly instrument: Readonly<Record<string, unknown>> | null;
    /** Whether the trade's type is routed to a table at all. */
    readonly isInstrumentRouted: boolean;
    readonly identifiers: readonly TradeIdentifier[];
    readonly roles: readonly TradePartyRole[];
    readonly booking: TradeBooking | null;
    readonly links: { readonly from: readonly TradeLink[]; readonly to: readonly TradeLink[] };
    readonly history: readonly TradeVersion[];
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

const tradePageRecordSchema = z.object({
    trade: tradeRowSchema,
    instrument: z.looseObject({}).nullable().default(null),
    isInstrumentRouted: z.boolean().default(false),
    identifiers: z.array(
        z.looseObject({
            id: z.string().default(''),
            trade_id: z.string().default(''),
            id_type: z.string().default(''),
            id_value: z.string().default(''),
            issuing_party_id: z.string().default(''),
        }),
    ),
    roles: z.array(
        z.looseObject({
            trade_id: z.string().default(''),
            party_id: z.string().default(''),
            role_type: z.string().default(''),
        }),
    ),
    booking: z
        .looseObject({
            trade_date: z.string().nullable().default(null),
            execution_timestamp: z.string().nullable().default(null),
        })
        .nullable()
        .default(null),
    links: z.object({
        from: z.array(
            z.looseObject({
                from_trade_id: z.string(),
                to_trade_id: z.string(),
                link_type: z.string(),
            }),
        ),
        to: z.array(
            z.looseObject({
                from_trade_id: z.string(),
                to_trade_id: z.string(),
                link_type: z.string(),
            }),
        ),
    }),
    history: z.array(z.looseObject({ version: z.coerce.number().int() })),
});

export const trades = {
    /** One trade with everything its page shows: identifiers, roles, booking, links and history. */
    async one(id: string): Promise<TradePageRecord> {
        const parsed = tradePageRecordSchema.parse(
            await request(`/api/trades/${encodeURIComponent(id)}`, { method: 'GET' }),
        );
        return parsed as TradePageRecord;
    },

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
