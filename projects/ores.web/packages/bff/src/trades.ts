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
import { subjects as bondIssueSubjects } from '@ores/wire-protocol/generated/trading/protocol/bond_issue_protocol';
import { subjects as bondSubjects } from '@ores/wire-protocol/generated/trading/protocol/bond_instrument_protocol';
import { subjects as bookingSubjects } from '@ores/wire-protocol/generated/trading/protocol/trade_booking_protocol';
import { subjects as identifierSubjects } from '@ores/wire-protocol/generated/trading/protocol/trade_identifier_protocol';
import { subjects as structureIdentifierSubjects } from '@ores/wire-protocol/generated/trading/protocol/structure_identifier_protocol';
import { subjects as structureMemberSubjects } from '@ores/wire-protocol/generated/trading/protocol/structure_member_protocol';
import { subjects as linkSubjects } from '@ores/wire-protocol/generated/trading/protocol/trade_link_protocol';
import { subjects as partyRoleSubjects } from '@ores/wire-protocol/generated/trading/protocol/trade_party_role_protocol';
import { subjects as summarySubjects } from '@ores/wire-protocol/generated/trading/protocol/trade_summary_protocol';
import { subjects as tradeSubjects } from '@ores/wire-protocol/generated/trading/protocol/trade_protocol';
import { invalidRequest, notFound } from './errors.js';
import { input, readAll, refusal, resultSchema, row, text } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/**
 * The answer to a read the page can do without, or nothing.
 *
 * The structure a trade belongs to is supplementary to the trade: a caller who may
 * read trades and not structures still gets the trade, and the page says nothing
 * about a structure rather than failing. Only this read is treated so; a read the
 * page is about still refuses loudly.
 */
async function optionalRead(
    session: LiveSession,
    subject: string,
    rows: string,
    request: Readonly<Record<string, unknown>>,
): Promise<readonly Record<string, unknown>[]> {
    try {
        return await readAll(session, subject, rows, request);
    } catch {
        return [];
    }
}

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
    structure_id: null,
    is_leg: null,
    id_one_of: null,
    row_kind_one_of: null,
    party_id_one_of: null,
    counterparty_id_one_of: null,
    book_id_one_of: null,
    instrument_type_one_of: null,
    status_one_of: null,
    authorisation_status_one_of: null,
    trade_date_one_of: null,
    structure_id_one_of: null,
    is_leg_one_of: null,
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
    registerTradePageRoute(server, requireSession);

    server.get('/api/trades/:id/versions/:version', async (request) => {
        const session = requireSession(request);
        const parsed = tradeVersionParamsSchema.safeParse(request.params);
        if (!parsed.success) {
            throw invalidRequest('The address does not name a trade id and a version.');
        }
        const reply = await session.client.callAuthenticated(
            tradeSubjects.get_trade_version_request,
            { key: { trade: { id: parsed.data.id }, version: parsed.data.version } },
            z.looseObject({ result: resultSchema, version: row.nullable().optional() }),
        );
        // A version the trade never had is an answer to the question, not a
        // refusal of it: the screen says the version is not there.
        if (reply.result.outcome === 'missing') {
            return { trade: null };
        }
        if (reply.result.outcome !== 'ok') {
            throw refusal(reply.result);
        }
        return { trade: reply.version ?? null };
    });

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

/**
 * The instrument tables a trade's terms can live in, by the type the read model
 * states.
 *
 * A trade's terms sit in the table for its product type, so the type decides
 * which list answers. Only the tables that exist today are named here: a type
 * that is missing has no terms stored yet, which the page states rather than
 * inventing.
 */
const INSTRUMENT_TABLES: Readonly<
    Record<string, { readonly subject: string; readonly rows: string }>
> = {
    Bond: { subject: bondSubjects.list_bond_instruments_request, rows: 'bond_instruments' },
};

/**
 * One trade's instrument terms.
 *
 * `routed` says the type has a table, which is a fact about the type. `refused`
 * says a read was attempted and the service declined it, so the page can draw
 * the rest of the trade instead of failing whole. `terms` is null when the table
 * holds no row for the trade.
 */
async function readInstrument(
    session: LiveSession,
    instrumentType: string,
    tradeId: string,
): Promise<{
    readonly routed: boolean;
    readonly refused: boolean;
    readonly terms: Record<string, unknown> | null;
}> {
    // `in` walks the prototype chain, so a type named `constructor` would read
    // as routed.
    if (!Object.hasOwn(INSTRUMENT_TABLES, instrumentType)) {
        return { routed: false, refused: false, terms: null };
    }
    const table = INSTRUMENT_TABLES[instrumentType] as {
        readonly subject: string;
        readonly rows: string;
    };
    let rows: readonly Record<string, unknown>[];
    try {
        rows = await readAll(session, table.subject, table.rows, {
            filter: { trade_id_one_of: [tradeId] },
        });
    } catch {
        return { routed: true, refused: true, terms: null };
    }
    const terms = rows[0];
    if (terms === undefined) {
        return { routed: true, refused: false, terms: null };
    }
    // The terms name their issue by id, which a person cannot read, so the issue
    // is read here and travels with the terms.
    const issueId = text(terms['issue_id']);
    let issue: Record<string, unknown> | null = null;
    if (issueId !== '') {
        try {
            const issues = await readAll(
                session,
                bondIssueSubjects.list_bond_issues_request,
                'issues',
                { filter: { issue_id_one_of: [issueId] } },
            );
            issue = issues[0] ?? null;
        } catch {
            issue = null;
        }
    }
    return { routed: true, refused: false, terms: { ...terms, issue } };
}

/**
 * One trade, as its own page reads it.
 *
 * Every read is narrowed to the trade: the summary by id, its identifiers, the
 * roles it was written with, its booking, its links at both ends, and its
 * version history by key. The page renders what this returns, so a tab never
 * reaches for a list of its own.
 */
function registerTradePageRoute(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    server.get('/api/trades/:id', async (request) => {
        const session = requireSession(request);
        const { id } = input(z.object({ id: z.uuid() }), request.params);

        const summaries = await readAll(
            session,
            summarySubjects.list_trade_summaries_request,
            'trade_summaries',
            { filter: { id_one_of: [id] } },
        );
        const summary = summaries[0];
        if (summary === undefined) {
            throw notFound('No trade has that id.');
        }

        const [enriched] = await enrich(session, [summary]);
        const [identifiers, roles, bookings, fromLinks, toLinks] = await Promise.all([
            readAll(session, identifierSubjects.list_trade_identifiers_request, 'identifiers', {
                filter: { trade_id_one_of: [id] },
            }),
            readAll(session, partyRoleSubjects.list_trade_party_roles_request, 'roles', {
                filter: { trade_id_one_of: [id] },
            }),
            readAll(session, bookingSubjects.list_trade_bookings_request, 'trade_bookings', {
                filter: { trade_id_one_of: [id] },
            }),
            readAll(session, linkSubjects.list_trade_links_request, 'links', {
                filter: { from_trade_id_one_of: [id] },
            }),
            readAll(session, linkSubjects.list_trade_links_request, 'links', {
                filter: { to_trade_id_one_of: [id] },
            }),
        ]);

        /*
         * Each link names its other end by id, and an id is not a name a person
         * reads. The identifiers of those trades are read in one go and travel
         * with the link, so the page shows what the desk calls them.
         */
        const otherIds = [
            ...new Set(
                [...fromLinks, ...toLinks].map((row) =>
                    text(row['from_trade_id']) === id
                        ? text(row['to_trade_id'])
                        : text(row['from_trade_id']),
                ),
            ),
        ].filter((other) => other !== '');
        const labels = new Map<string, string>();
        if (otherIds.length > 0) {
            const found = await readAll(
                session,
                identifierSubjects.list_trade_identifiers_request,
                'identifiers',
                { filter: { trade_id_one_of: otherIds } },
            );
            for (const row of found) {
                const tradeId = text(row['trade_id']);
                const value = text(row['id_value']);
                if (tradeId !== '' && value !== '' && !labels.has(tradeId)) {
                    labels.set(tradeId, value);
                }
            }
        }
        const labelled = (
            rows: readonly Record<string, unknown>[],
            other: 'from_trade_id' | 'to_trade_id',
        ): readonly Record<string, unknown>[] =>
            rows.map((row) => ({ ...row, other_label: labels.get(text(row[other])) ?? '' }));

        /*
         * The structure this trade is a leg of, if any. A trade is a leg of one
         * structure at a time, which the store states by keying members on the
         * trade, so one read answers it.
         */
        const memberships = await optionalRead(
            session,
            structureMemberSubjects.list_structure_members_request,
            'members',
            { filter: { trade_id_one_of: [id] }, as_of: null },
        );
        const memberStructures = distinct(memberships.map((entry) => text(entry['structure_id'])));
        const memberIdentifiers =
            memberStructures.length === 0
                ? []
                : await optionalRead(
                      session,
                      structureIdentifierSubjects.list_structure_identifiers_request,
                      'identifiers',
                      {
                          filter: {
                              structure_id: null,
                              structure_id_one_of: memberStructures,
                          },
                          as_of: null,
                      },
                  );
        const referenceOfStructure = new Map(
            memberIdentifiers.map(
                (entry) => [text(entry['structure_id']), text(entry['id_value'])] as const,
            ),
        );
        const structureOfTrade = memberships.map((entry) => {
            const structureId = text(entry['structure_id']);
            return {
                structure_id: structureId,
                reference: referenceOfStructure.get(structureId) ?? '',
                role: text(entry['role']),
                sequence_number: Number(entry['sequence_number'] ?? 0),
                version: Number(entry['version'] ?? 0),
            };
        });

        const versions = await readAll(
            session,
            tradeSubjects.list_trade_versions_request,
            'versions',
            { key: { id }, order: { field: 'version', descending: true } },
        );

        return {
            trade: enriched ?? summary,
            identifiers,
            roles,
            booking: bookings[0] ?? null,
            ...(await readInstrument(session, text(summary['instrument_type']), id)),
            links: {
                from: labelled(fromLinks, 'to_trade_id'),
                to: labelled(toLinks, 'from_trade_id'),
            },
            structures: structureOfTrade,
            history: versions,
        };
    });
}

/**
 * One trade as it stood at one version.
 *
 * A run records the version of a trade it read, and the error screen links to
 * it. The trade page answers for the current row, so the version is read from
 * the trade itself rather than filtered out of a list.
 */
const tradeVersionParamsSchema = z.object({
    id: z.uuid(),
    version: z.coerce.number().int().min(1),
});

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

    /*
     * A structure row is a row in its own right, and the identifier that names
     * it is its own: a trade's identifiers say nothing about it, so the rows are
     * read separately and the two meet on the row.
     */
    const structureIds = distinct(
        summaries
            .filter((entry) => text(entry['row_kind']) === 'structure')
            .map((entry) => text(entry['id'])),
    );

    if (
        ids.length === 0 &&
        structureIds.length === 0 &&
        bookIds.length === 0 &&
        counterpartyIds.length === 0
    ) {
        return summaries;
    }
    const structureIdentifiers =
        structureIds.length === 0
            ? []
            : await optionalRead(
                  session,
                  structureIdentifierSubjects.list_structure_identifiers_request,
                  'identifiers',
                  {
                      filter: { structure_id: null, structure_id_one_of: structureIds },
                      as_of: null,
                  },
              );
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
    const structureIdentifierOf = new Map(
        structureIdentifiers
            .map((entry) => [text(entry['structure_id']), text(entry['id_value'])] as const)
            .filter(([structureId, value]) => structureId !== '' && value !== ''),
    );
    const bookOf = nameById(books, 'name');
    const counterpartyOf = nameById(counterparties, 'full_name');
    const tradeOf = new Map(trades.map((entry) => [text(entry['id']), entry]));

    return summaries.map((summary) => {
        const id = text(summary['id']);
        const trade = tradeOf.get(id);
        const isStructure = text(summary['row_kind']) === 'structure';
        return {
            ...summary,
            identifier: isStructure
                ? (structureIdentifierOf.get(id) ?? '')
                : (identifiersOf.get(id) ?? ''),
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
