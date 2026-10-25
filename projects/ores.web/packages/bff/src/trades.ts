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

import type { FastifyInstance, FastifyRequest } from 'fastify';
import { z } from 'zod';
import { subjects as bookSubjects } from '@ores/wire-protocol/generated/refdata/protocol/book_protocol';
import { subjects as counterpartySubjects } from '@ores/wire-protocol/generated/refdata/protocol/counterparty_protocol';
import { subjects as identifierSubjects } from '@ores/wire-protocol/generated/trading/protocol/trade_identifier_protocol';
import { subjects as summarySubjects } from '@ores/wire-protocol/generated/trading/protocol/trade_summary_protocol';
import { subjects as tradeSubjects } from '@ores/wire-protocol/generated/trading/protocol/trade_protocol';
import { input, readAll, refusal, resultSchema, row, text } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/** The most rows one page may hold, which is what the record list offers, including Load all. */
const MAX_PAGE = 1000;

/** The fields the trade summary read model can order by. */
const SORTABLE = ['trade_date', 'instrument_type', 'status', 'row_kind'] as const;

/**
 * The trade list's query. Every filter the read model answers is named here in
 * the words the screen uses, and an empty value means "no filter", which the
 * wire states as null rather than as the empty string.
 */
const tradeListQuerySchema = z.object({
    offset: z.coerce.number().int().min(0).default(0),
    limit: z.coerce.number().int().min(1).max(MAX_PAGE).default(25),
    search: z.string().trim().max(256).default(''),
    sort: z.enum(SORTABLE).default('trade_date'),
    descending: z
        .enum(['true', 'false'])
        .default('true')
        .transform((value) => value === 'true'),
    status: z.string().trim().max(100).default(''),
    type: z.string().trim().max(200).default(''),
    party: z.uuid().or(z.literal('')).default(''),
    counterparty: z.uuid().or(z.literal('')).default(''),
    book: z.uuid().or(z.literal('')).default(''),
    from: z.string().trim().max(10).default(''),
    to: z.string().trim().max(10).default(''),
});

/** A value the screen left empty is no filter at all. */
const orNull = (value: string): string | null => (value === '' ? null : value);

/**
 * The identifier a person recognizes the trade by.
 *
 * The read model's own key is a uuid, which no desk quotes, so the page is
 * enriched with the trade's identifiers. A trade may carry several and no type
 * is marked primary, so the order is stated here: the internal reference, then
 * the regulatory ones, then the ORE document's own id.
 */
const IDENTIFIER_ORDER = ['Internal', 'UTI', 'USI', 'ORE'] as const;

function rankOf(idType: string): number {
    const index = IDENTIFIER_ORDER.findIndex((known) => known === idType);
    return index === -1 ? IDENTIFIER_ORDER.length : index;
}

/** Every member of the read model's filter set, absent. The wire states an absent member as null. */
const NO_FILTER = {
    row_kind: null,
    party_id: null,
    counterparty_id: null,
    book_id: null,
    instrument_type: null,
    status: null,
    authorisation_status: null,
    trade_date: null,
    id_one_of: null,
    row_kind_one_of: null,
    party_id_one_of: null,
    counterparty_id_one_of: null,
    book_id_one_of: null,
    instrument_type_one_of: null,
    status_one_of: null,
    authorisation_status_one_of: null,
    trade_date_one_of: null,
    trade_date_from: null,
    trade_date_to: null,
    search: null,
} as const;

/**
 * The filter the page is read with: the set, with the screen's choices in the
 * members it names.
 *
 * Every member is stated, absent ones as null. The service decodes the filter
 * strictly and refuses a request that leaves a member out, so a filter built
 * from only the screen's choices would work with no filter and fail with one.
 */
function filterOf(query: z.infer<typeof tradeListQuerySchema>) {
    return {
        ...NO_FILTER,
        party_id: orNull(query.party),
        counterparty_id: orNull(query.counterparty),
        book_id: orNull(query.book),
        instrument_type: orNull(query.type),
        status: orNull(query.status),
        trade_date_from: orNull(query.from),
        trade_date_to: orNull(query.to),
        search: orNull(query.search),
    };
}

/**
 * The trade list the blotter calls: one page of the trade summary read model,
 * enriched with the names, references and versions a person reads.
 *
 * The read model is the only source of the page, its filters and its total, so
 * the screen pages, searches and orders on the server rather than in the
 * browser. Each enrichment read is bounded by the page: fifteen rows name
 * fifteen trades, so their identifiers, books and counterparties are read by
 * key, never by walking a whole list.
 */
export function registerTradeRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    server.get('/api/trades', async (request) => {
        const session = requireSession(request);
        const query = input(tradeListQuerySchema, request.query);
        const reply = await session.client.callAuthenticated(
            summarySubjects.list_trade_summaries_request,
            {
                offset: query.offset,
                limit: query.limit,
                order: { field: query.sort, descending: query.descending },
                filter: filterOf(query),
            },
            z.looseObject({
                result: resultSchema,
                trade_summaries: z.array(row).default([]),
                total: z.coerce.number().int().nonnegative().default(0),
            }),
        );
        if (reply.result.outcome !== 'ok') {
            throw refusal(reply.result);
        }
        return {
            rows: await enrich(session, reply.trade_summaries),
            total: reply.total,
        };
    });
}

/** One page of summary rows with the names, references and versions of the trades it names. */
async function enrich(
    session: LiveSession,
    summaries: readonly Record<string, unknown>[],
): Promise<readonly Record<string, unknown>[]> {
    const ids = summaries
        .filter((entry) => text(entry['row_kind']) === 'trade')
        .map((entry) => text(entry['id']));
    const bookIds = distinct(summaries.map((entry) => text(entry['book_id'])));
    const counterpartyIds = distinct(summaries.map((entry) => text(entry['counterparty_id'])));

    if (ids.length === 0 && bookIds.length === 0 && counterpartyIds.length === 0) {
        return summaries;
    }
    const identifiers = await readAll(
        session,
        identifierSubjects.list_trade_identifiers_request,
        'identifiers',
        {
            filter: { trade_id: null, trade_id_one_of: ids },
            as_of: null,
        },
    );
    const books = await readAll(session, bookSubjects.list_books_request, 'books', {
        filter: {
            parent_portfolio_id: null,
            id_one_of: bookIds,
            parent_portfolio_id_one_of: null,
        },
        as_of: null,
    });
    const counterparties = await readAll(
        session,
        counterpartySubjects.list_counterparties_request,
        'counterparties',
        { filter: { id_one_of: counterpartyIds }, as_of: null },
    );
    const trades =
        ids.length === 0
            ? []
            : await readAll(session, tradeSubjects.list_trades_request, 'trades', {
                  filter: {
                      counterparty_id: null,
                      trade_type: null,
                      counterparty_scope: null,
                      id_one_of: ids,
                      counterparty_id_one_of: null,
                      trade_type_one_of: null,
                      counterparty_scope_one_of: null,
                      search: null,
                  },
                  as_of: null,
              });

    const identifiersOf = bestIdentifier(identifiers);
    const bookOf = nameById(books, 'name');
    const counterpartyOf = nameById(counterparties, 'full_name');
    const tradeOf = new Map(trades.map((entry) => [text(entry['id']), entry]));

    return summaries.map((summary) => {
        const id = text(summary['id']);
        const trade = tradeOf.get(id);
        return {
            ...summary,
            identifier: identifiersOf.get(id) ?? '',
            counterparty_name: counterpartyOf.get(text(summary['counterparty_id'])) ?? '',
            book_name: bookOf.get(text(summary['book_id'])) ?? '',
            version: trade === undefined ? null : (trade['version'] ?? null),
            external_version: trade === undefined ? null : (trade['external_version'] ?? null),
        };
    });
}

/** The ids a page names, without the empty ones and without repeats. */
function distinct(values: readonly string[]): string[] {
    return [...new Set(values.filter((value) => value !== ''))];
}

/** The identifier of each trade, chosen by the order the screen states. */
function bestIdentifier(
    identifiers: readonly Record<string, unknown>[],
): ReadonlyMap<string, string> {
    const best = new Map<string, { rank: number; value: string }>();
    for (const entry of identifiers) {
        const tradeId = text(entry['trade_id']);
        const value = text(entry['id_value']);
        if (tradeId === '' || value === '') {
            continue;
        }
        const rank = rankOf(text(entry['id_type']));
        const held = best.get(tradeId);
        if (held === undefined || rank < held.rank) {
            best.set(tradeId, { rank, value });
        }
    }
    return new Map([...best].map(([tradeId, chosen]) => [tradeId, chosen.value]));
}

/** One field of every row, keyed by id. */
function nameById(
    rows: readonly Record<string, unknown>[],
    field: string,
): ReadonlyMap<string, string> {
    return new Map(rows.map((entry) => [text(entry['id']), text(entry[field])]));
}
