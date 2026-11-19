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
 */

import { classificationRowSchema } from '@ores/wire-protocol/browser';
import { z } from 'zod';
import { baseFor } from './servers.js';
import { request } from './transport.js';

/**
 * The quotes view's reads, which the C++ server answers: the feeds the
 * session may watch, the board's quotes of those feeds, and one provider's series
 * over a span. The server asks the market data service which feeds the session may
 * watch on every read, so the browser never states its own entitlement.
 */

/**
 * The quotes routes are the C++ server's alone. Behind the BFF the build lists the screen
 * in =VITE_ORE_CPP_SCREENS= and the proxy sends =/cpp= on; served by =ores.http= itself,
 * the screen asks with no prefix.
 */
const CPP = baseFor('quotes');

export const quoteFeedSchema = z.object({
    provider: z.string(),
    feed: z.string(),
    /** The provider's rank, the lower the more trusted. */
    precedence: z.number().int(),
    source: z.string(),
    last_at: z.string().nullable(),
});
export type QuoteFeed = z.infer<typeof quoteFeedSchema>;

export const quoteSchema = z.object({
    provider: z.string(),
    feed: z.string(),
    source: z.string(),
    uri: z.string(),
    value: z.string(),
    previous: z.string(),
    at: z.string(),
    asset_class: z.string(),
    instrument_type: z.string(),
    series_uri: z.string(),
    series: z.string(),
    point: z.string(),
});
export type Quote = z.infer<typeof quoteSchema>;

export const quoteBoardSchema = z.object({
    sequence: z.number().int().nonnegative(),
    quotes: z.array(quoteSchema),
});
export type QuoteBoard = z.infer<typeof quoteBoardSchema>;

export const quoteHistorySchema = z.object({
    ticks: z.array(z.object({ at: z.string(), uri: z.string(), value: z.number() })),
});
export type QuoteHistory = z.infer<typeof quoteHistorySchema>;

export const quotes = {
    /** The feeds the session may watch for display, with each one's source and last tick. */
    async feeds(): Promise<readonly QuoteFeed[]> {
        return z
            .object({ feeds: z.array(quoteFeedSchema) })
            .parse(await request(`${CPP}/api/marketdata/quotes/feeds`, { method: 'GET' })).feeds;
    },

    /** The quotes that arrived after @p after, or the whole board for zero. */
    async board(after: number): Promise<QuoteBoard> {
        const query = after > 0 ? `?after=${after}` : '';
        return quoteBoardSchema.parse(
            await request(`${CPP}/api/marketdata/quotes${query}`, { method: 'GET' }),
        );
    },

    /** One point of a provider's series from @p from to now, for display. */
    async history(
        provider: string,
        series: string,
        point: string,
        from: string,
    ): Promise<QuoteHistory> {
        const query = new URLSearchParams({ provider, series, point, from });
        return quoteHistorySchema.parse(
            await request(`${CPP}/api/marketdata/quotes/history?${query.toString()}`, {
                method: 'GET',
            }),
        );
    },

    /** The asset class codes table, the class filter's choices, in its display order. */
    async assetClasses(): Promise<readonly { readonly code: string; readonly name: string }[]> {
        return z
            .object({ rows: z.array(classificationRowSchema) })
            .parse(await request(`${CPP}/api/classifications/asset-class-code`, { method: 'GET' }))
            .rows.map((row) => ({ code: row.code, name: row.name }));
    },
};
