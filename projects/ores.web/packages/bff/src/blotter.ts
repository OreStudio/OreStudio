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
import { subjects as portfolioSubjects } from '@ores/wire-protocol/generated/refdata/protocol/portfolio_protocol';
import { subjects as memberSubjects } from '@ores/wire-protocol/generated/trading/protocol/structure_member_protocol';
import { subjects as tradeIdentifierSubjects } from '@ores/wire-protocol/generated/trading/protocol/trade_identifier_protocol';
import { subjects as structureIdentifierSubjects } from '@ores/wire-protocol/generated/trading/protocol/structure_identifier_protocol';
import { subjects as structureSubjects } from '@ores/wire-protocol/generated/trading/protocol/structure_protocol';
import { subjects as summarySubjects } from '@ores/wire-protocol/generated/trading/protocol/trade_summary_protocol';
import { input, readAll, refusal, resultSchema, text } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/*
 * The blotter: the rows a person reads, and the rail they are read by.
 *
 * A row is a trade or a whole structure. The summary view states both kinds, and
 * a structure's legs are trades too, so it states them as well; the view also
 * states, for each trade, the structure that holds it. The top list is therefore
 * the rows the view marks as not legs, and a structure stands in place of the legs
 * it holds. The flat list is every trade, legs included, each leg naming its
 * structure.
 *
 * A structure has no book of its own. It is placed where its legs are: on the book
 * they share, or on the lowest portfolio that holds them all, which is the spot a
 * person looks for a structure that spans books. The rail counts what sits where:
 * a book's figure is the rows placed on it, a portfolio's is every row below it.
 */

/** The identifiers a trade is quoted by, best first. */
const IDENTIFIER_ORDER = ['Internal', 'UTI', 'USI', 'ORE'] as const;

/** Where an identifier type ranks: the order above, then the rest. */
function rankOf(idType: string): number {
    const index = IDENTIFIER_ORDER.findIndex((known) => known === idType);
    return index === -1 ? IDENTIFIER_ORDER.length : index;
}

const MAX_PAGE = 200;

/** How many rows one read asks for, and the stop against a runaway read. */
const READ_PAGE = 200;
const READ_CAP = 20_000;

const blotterQuerySchema = z.object({
    /** The grouping the rail is drawn by. */
    view: z.enum(['portfolio', 'counterparty']).default('portfolio'),
    /**
     * The node the list is narrowed to: `book:<id>`, `portfolio:<id>`,
     * `counterparty:<id>`, or empty for the party itself.
     */
    node: z.string().trim().max(100).default(''),
    flat: z
        .enum(['0', '1'])
        .default('0')
        .transform((value) => value === '1'),
    search: z.string().trim().max(256).default(''),
    offset: z.coerce.number().int().min(0).default(0),
    limit: z.coerce.number().int().min(1).max(MAX_PAGE).default(25),
    sort: z.enum(['trade_date', 'instrument_type', 'status', 'row_kind']).default('trade_date'),
    descending: z
        .enum(['true', 'false'])
        .default('true')
        .transform((value) => value === 'true'),
});

/** Every member of the summary filter, absent. The wire states an absent member as null. */
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
    trade_date_from: null,
    trade_date_to: null,
    structure_id_one_of: null,
    is_leg_one_of: null,
    search: null,
} as const;

type Summary = Readonly<Record<string, unknown>>;

/** One page of the summaries, with the total the filter matches. */
async function summaries(
    session: LiveSession,
    filter: Readonly<Record<string, unknown>>,
    order: { readonly field: string; readonly descending: boolean },
    offset: number,
    limit: number,
): Promise<{ readonly rows: readonly Summary[]; readonly total: number }> {
    /* The filter is built above the call so every member it declares is stated in one place. */
    const filterBody = { ...NO_FILTER, ...filter };
    const reply = await session.client.callAuthenticated(
        summarySubjects.list_trade_summaries_request,
        { offset, limit, order, filter: filterBody, as_of: null },
        z.looseObject({
            result: resultSchema,
            trade_summaries: z.array(z.record(z.string(), z.unknown())).default([]),
            total: z.coerce.number().int().nonnegative().default(0),
        }),
    );
    if (reply.result.outcome !== 'ok') {
        throw refusal(reply.result);
    }
    return { rows: reply.trade_summaries, total: reply.total };
}

/**
 * Every row a filter matches, in the order asked for.
 *
 * The rail's figures come from these rows, so a page cannot answer them: a count
 * needs the whole set, and the view has no grouped read yet. The list itself is
 * sliced after the rail is counted.
 */
async function allSummaries(
    session: LiveSession,
    filter: Readonly<Record<string, unknown>>,
    order: { readonly field: string; readonly descending: boolean },
): Promise<readonly Summary[]> {
    const rows: Summary[] = [];
    for (let offset = 0; offset < READ_CAP; offset += READ_PAGE) {
        const page = await summaries(session, filter, order, offset, READ_PAGE);
        rows.push(...page.rows);
        if (page.rows.length < READ_PAGE || rows.length >= page.total) {
            return rows;
        }
    }
    return rows;
}

/** A node of the rail. Its id says what kind of node it is; the party is the empty id. */
interface TreeNode {
    readonly id: string;
    readonly label: string;
    readonly kind: 'party' | 'portfolio' | 'book' | 'counterparty';
    readonly parent: string;
    readonly count: number;
}

const bookNodeId = (id: string): string => `book:${id}`;
const portfolioNodeId = (id: string): string => `portfolio:${id}`;
const counterpartyNodeId = (id: string): string => `counterparty:${id}`;

/** Where a portfolio sits: its parent, or the empty string when it is a root. */
type PortfolioParent = ReadonlyMap<string, string>;

/** A portfolio and every portfolio under it. */
function subtreeOf(id: string, parentOf: PortfolioParent): readonly string[] {
    const under = [id];
    for (let index = 0; index < under.length; index += 1) {
        const current = under[index] ?? '';
        for (const [candidate, parent] of parentOf) {
            if (parent === current && !under.includes(candidate)) {
                under.push(candidate);
            }
        }
    }
    return under;
}

/** The portfolios above a portfolio, nearest first, ending at a root. */
function ancestorsOf(id: string, parentOf: PortfolioParent): readonly string[] {
    const above: string[] = [];
    let parent = parentOf.get(id) ?? '';
    while (parent !== '' && !above.includes(parent)) {
        above.push(parent);
        parent = parentOf.get(parent) ?? '';
    }
    return above;
}

/**
 * The lowest portfolio that holds every one of these portfolios.
 *
 * A portfolio holds itself, so a set of one answers itself. A set that shares no
 * portfolio above them answers the party, which the empty string states.
 */
function lowestCommonAncestor(ids: readonly string[], parentOf: PortfolioParent): string {
    const first = ids[0];
    if (first === undefined) {
        return '';
    }
    for (const candidate of [first, ...ancestorsOf(first, parentOf)]) {
        if (ids.every((id) => id === candidate || ancestorsOf(id, parentOf).includes(candidate))) {
            return candidate;
        }
    }
    return '';
}

/** The rail nodes a placement sits under: itself, the portfolios above it, and the party. */
function ancestorsOfPlacement(
    placement: string,
    portfolioOfBook: ReadonlyMap<string, string>,
    parentOf: PortfolioParent,
): readonly string[] {
    const chain = [placement];
    let portfolio = '';
    if (placement.startsWith('book:')) {
        portfolio = portfolioOfBook.get(placement.slice('book:'.length)) ?? '';
    } else if (placement.startsWith('portfolio:')) {
        portfolio = placement.slice('portfolio:'.length);
    }
    if (portfolio !== '') {
        chain.push(portfolioNodeId(portfolio));
        for (const above of ancestorsOf(portfolio, parentOf)) {
            chain.push(portfolioNodeId(above));
        }
    }
    chain.push('');
    return chain;
}

/** A row of the list, as the screen reads it. */
interface BlotterRow {
    readonly row_kind: string;
    readonly id: string;
    readonly reference: string;
    readonly structure: string;
    readonly structure_reference: string;
    readonly instrument_type: string;
    readonly book_id: string;
    readonly book_name: string;
    readonly counterparty_id: string;
    readonly counterparty_name: string;
    readonly status: string;
    readonly trade_date: string;
    readonly spans_books: boolean;
}

export function registerBlotterRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    /**
     * One page of the blotter, with the rail beside it.
     *
     * The rail is the grouping the view states: the party's portfolios and their
     * books, or the counterparties its rows face. Each node carries the number of
     * rows placed under it.
     */
    server.get('/api/blotter', async (request) => {
        const session = requireSession(request);
        const query = input(blotterQuerySchema, request.query);
        const order = { field: query.sort, descending: query.descending };

        const [books, portfolios, structures, members, counterparties] = await Promise.all([
            readAll(session, bookSubjects.list_books_request, 'books', {
                filter: {
                    parent_portfolio_id: null,
                    id_one_of: null,
                    parent_portfolio_id_one_of: null,
                },
                as_of: null,
            }),
            readAll(session, portfolioSubjects.list_portfolios_request, 'portfolios', {
                filter: { sandbox_id: null, id_one_of: null, sandbox_id_one_of: null },
                as_of: null,
            }),
            readAll(session, structureSubjects.list_structures_request, 'structures', {
                filter: { id_one_of: null },
                as_of: null,
            }),
            readAll(session, memberSubjects.list_structure_members_request, 'members', {
                filter: { trade_id_one_of: null },
                as_of: null,
            }),
            readAll(session, counterpartySubjects.list_counterparties_request, 'counterparties', {
                filter: null,
                as_of: null,
            }),
        ]);

        /* Which structure holds which trade. */
        const holderOf = new Map<string, string>();
        for (const member of members) {
            const trade = text(member['trade_id']);
            if (trade !== '') {
                holderOf.set(trade, text(member['structure_id']));
            }
        }
        const legsOf = new Map<string, string[]>();
        for (const [trade, structure] of holderOf) {
            legsOf.set(structure, [...(legsOf.get(structure) ?? []), trade]);
        }

        /*
         * The books a structure's legs sit in, read in one filtered read so a
         * structure of any size is placed by all of its legs, not by a page.
         */
        const legIds = [...holderOf.keys()];
        const legRows =
            legIds.length === 0
                ? []
                : await allSummaries(session, { id_one_of: legIds, row_kind: 'trade' }, order);
        /*
         * A structure row states no date and no state of its own: those are its
         * legs'. The date is the earliest leg's, and the state is the legs' when
         * they agree, because a page that showed one leg's state would speak for
         * the others.
         */
        const legDateOf = new Map<string, string>();
        const legStatesOf = new Map<string, Set<string>>();
        for (const row of legRows) {
            const owner = holderOf.get(text(row['id'])) ?? '';
            if (owner === '') {
                continue;
            }
            const date = text(row['trade_date']);
            if (
                date !== '' &&
                (legDateOf.get(owner) ?? '') !== '' &&
                date < (legDateOf.get(owner) ?? '')
            ) {
                legDateOf.set(owner, date);
            } else if (date !== '' && !legDateOf.has(owner)) {
                legDateOf.set(owner, date);
            }
            const state = text(row['status']);
            if (state !== '') {
                legStatesOf.set(owner, (legStatesOf.get(owner) ?? new Set<string>()).add(state));
            }
        }
        const bookOfTrade = new Map(
            legRows.map((row) => [text(row['id']), text(row['book_id'])] as const),
        );
        const booksOfStructure = new Map<string, readonly string[]>();
        for (const row of structures) {
            const id = text(row['id']);
            booksOfStructure.set(id, [
                ...new Set(
                    (legsOf.get(id) ?? [])
                        .map((trade) => bookOfTrade.get(trade) ?? '')
                        .filter((book) => book !== ''),
                ),
            ]);
        }

        /* A structure is found by its own reference, and a leg by the structure's. */
        const referenceOf = new Map<string, string>(
            structures.map((row) => [text(row['id']), ''] as const),
        );
        if (structures.length > 0) {
            const identifiers = await readAll(
                session,
                structureIdentifierSubjects.list_structure_identifiers_request,
                'identifiers',
                {
                    filter: {
                        structure_id: null,
                        structure_id_one_of: structures.map((row) => text(row['id'])),
                    },
                    as_of: null,
                },
            );
            for (const row of identifiers) {
                const owner = text(row['structure_id']);
                const value = text(row['id_value']);
                if (owner !== '' && value !== '' && (referenceOf.get(owner) ?? '') === '') {
                    referenceOf.set(owner, value);
                }
            }
        }

        /* The tree of portfolios and books, built once and read by every placement. */
        const portfolioOf = new Map(portfolios.map((row) => [text(row['id']), row] as const));
        const parentOf: PortfolioParent = new Map(
            portfolios.map((row) => [text(row['id']), text(row['parent_portfolio_id'])] as const),
        );
        const portfolioOfBook = new Map(
            books.map((row) => [text(row['id']), text(row['parent_portfolio_id'])] as const),
        );

        /*
         * Where a row is placed: a trade on its book, a structure on the book its
         * legs share or on the lowest portfolio that holds them. A row with no book
         * sits at the party.
         */
        const placementOf = (row: Summary): string => {
            if (text(row['row_kind']) !== 'structure') {
                const book = text(row['book_id']);
                return book === '' ? '' : bookNodeId(book);
            }
            const structureBooks = booksOfStructure.get(text(row['id'])) ?? [];
            if (structureBooks.length === 0) {
                return '';
            }
            if (structureBooks.length === 1) {
                return bookNodeId(structureBooks[0] ?? '');
            }
            const holdings = structureBooks.map((id) => portfolioOfBook.get(id) ?? '');
            const holding = lowestCommonAncestor(
                holdings.filter((id) => id !== ''),
                parentOf,
            );
            return holding === '' ? '' : portfolioNodeId(holding);
        };

        /* The rows the top list holds: the trades no structure holds, and the structures. */
        const topRows = await allSummaries(session, { is_leg: false }, order);
        const byNode = new Map<string, number>();
        for (const row of topRows) {
            const node = placementOf(row);
            byNode.set(node, (byNode.get(node) ?? 0) + 1);
        }

        const labelOfBook = new Map(
            books.map((row) => [text(row['id']), text(row['name']) || text(row['id'])] as const),
        );
        const labelOfCounterparty = new Map(
            counterparties.map(
                (row) =>
                    [
                        text(row['id']),
                        text(row['full_name']) || text(row['short_code']) || text(row['id']),
                    ] as const,
            ),
        );

        const portfolioRail = (): readonly TreeNode[] => {
            const booksUnder = new Map<string, Summary[]>();
            for (const book of books) {
                const portfolio = text(book['parent_portfolio_id']);
                if (portfolio !== '' && portfolioOf.has(portfolio)) {
                    booksUnder.set(portfolio, [...(booksUnder.get(portfolio) ?? []), book]);
                }
            }
            const shown = new Set(booksUnder.keys());
            for (const start of [...shown]) {
                let parent = text(portfolioOf.get(start)?.['parent_portfolio_id']);
                while (parent !== '' && portfolioOf.has(parent) && !shown.has(parent)) {
                    shown.add(parent);
                    parent = text(portfolioOf.get(parent)?.['parent_portfolio_id']);
                }
            }
            const childrenOf = new Map<string, string[]>();
            for (const id of shown) {
                const parent = text(portfolioOf.get(id)?.['parent_portfolio_id']);
                const key = parent !== '' && shown.has(parent) ? parent : '';
                childrenOf.set(key, [...(childrenOf.get(key) ?? []), id]);
            }
            const countUnder = (node: string): number => {
                if (node === '') {
                    return topRows.length;
                }
                if (node.startsWith('book:')) {
                    return byNode.get(node) ?? 0;
                }
                const portfolio = node.slice('portfolio:'.length);
                const below = subtreeOf(portfolio, parentOf);
                const nested = below
                    .slice(1)
                    .reduce((sum, id) => sum + (byNode.get(portfolioNodeId(id)) ?? 0), 0);
                const booksHere = books
                    .filter((book) => below.includes(text(book['parent_portfolio_id'])))
                    .reduce(
                        (sum, book) => sum + (byNode.get(bookNodeId(text(book['id']))) ?? 0),
                        0,
                    );
                return (byNode.get(node) ?? 0) + nested + booksHere;
            };
            const nodes: TreeNode[] = [
                {
                    id: '',
                    label: session.party?.name ?? '',
                    kind: 'party',
                    parent: '',
                    count: topRows.length,
                },
            ];
            const walk = (parent: string): void => {
                for (const id of childrenOf.get(parent) ?? []) {
                    nodes.push({
                        id: portfolioNodeId(id),
                        label: text(portfolioOf.get(id)?.['name']) || id,
                        kind: 'portfolio',
                        parent: parent === '' ? '' : portfolioNodeId(parent),
                        count: countUnder(portfolioNodeId(id)),
                    });
                    for (const book of booksUnder.get(id) ?? []) {
                        nodes.push({
                            id: bookNodeId(text(book['id'])),
                            label: text(book['name']) || text(book['id']),
                            kind: 'book',
                            parent: portfolioNodeId(id),
                            count: countUnder(bookNodeId(text(book['id']))),
                        });
                    }
                    walk(id);
                }
            };
            walk('');
            return nodes;
        };

        const counterpartyRail = (): readonly TreeNode[] => {
            const counts = new Map<string, number>();
            for (const row of topRows) {
                const id = text(row['counterparty_id']);
                if (id !== '') {
                    counts.set(id, (counts.get(id) ?? 0) + 1);
                }
            }
            return [
                {
                    id: '',
                    label: session.party?.name ?? '',
                    kind: 'party',
                    parent: '',
                    count: topRows.length,
                },
                ...[...counts.entries()]
                    .sort(([left], [right]) =>
                        (labelOfCounterparty.get(left) ?? left).localeCompare(
                            labelOfCounterparty.get(right) ?? right,
                        ),
                    )
                    .map(([id, count]) => ({
                        id: counterpartyNodeId(id),
                        label: labelOfCounterparty.get(id) ?? id,
                        kind: 'counterparty' as const,
                        parent: '',
                        count,
                    })),
            ];
        };

        const drawn = query.view === 'counterparty' ? counterpartyRail() : portfolioRail();
        const tree = drawn.filter((node) => node.id === '' || node.count > 0);

        /* The rows the list is read from: every trade when flat, the top rows otherwise. */
        const listed =
            query.flat || query.search !== ''
                ? await allSummaries(
                      session,
                      {
                          ...(query.flat ? { row_kind: 'trade' } : { is_leg: false }),
                          ...(query.search === '' ? {} : { search: query.search }),
                      },
                      order,
                  )
                : topRows;

        /* Narrow to the node, and read the page from what is left. */
        const within = (row: Summary): boolean => {
            const node = query.node;
            if (node === '') {
                return true;
            }
            if (node.startsWith('counterparty:')) {
                return text(row['counterparty_id']) === node.slice('counterparty:'.length);
            }
            const placement = placementOf(row);
            return ancestorsOfPlacement(placement, portfolioOfBook, parentOf).includes(node);
        };
        const rows = listed.filter(within);

        /*
         * The page is ordered here, not by the service, because a structure has
         * no date of its own: the view orders structures by a null date, which
         * puts them first however old their legs are. Sorting on the facts the
         * rows state — including the date a structure takes from its legs — keeps
         * the order a person sees and the order they paged in the same one.
         */
        const fieldOf = (row: Summary): string => {
            const kind = text(row['row_kind']);
            if (query.sort === 'trade_date') {
                return kind === 'structure'
                    ? (legDateOf.get(text(row['id'])) ?? '')
                    : text(row['trade_date']);
            }
            if (query.sort === 'status') {
                return kind === 'structure'
                    ? (legStatesOf.get(text(row['id']))?.size ?? 0) === 1
                        ? ([...(legStatesOf.get(text(row['id'])) ?? [])][0] ?? '')
                        : ''
                    : text(row['status']);
            }
            return text(row[query.sort]);
        };
        const ordered = [...rows].sort((left, right) => {
            const a = fieldOf(left);
            const b = fieldOf(right);
            if (a === b) {
                return text(left['id']).localeCompare(text(right['id']));
            }
            return query.descending ? b.localeCompare(a) : a.localeCompare(b);
        });

        /*
         * A trade is found by its identifiers, not by the surrogate key the
         * summary carries. The page's trades are named in one filtered read, and
         * the internal reference wins, as it does on the trade list.
         */
        const pageTrades = rows
            .filter((row) => text(row['row_kind']) === 'trade')
            .map((row) => text(row['id']))
            .filter((id) => id !== '');
        const tradeReferenceOf = new Map<string, string>();
        if (pageTrades.length > 0) {
            const identifiers = await readAll(
                session,
                tradeIdentifierSubjects.list_trade_identifiers_request,
                'identifiers',
                {
                    filter: { trade_id: null, trade_id_one_of: pageTrades },
                    as_of: null,
                },
            );
            const ranked = new Map<string, { type: string; value: string }>();
            for (const identifier of identifiers) {
                const trade = text(identifier['trade_id']);
                const type = text(identifier['id_type']);
                const value = text(identifier['id_value']);
                if (trade === '' || value === '') {
                    continue;
                }
                const held = ranked.get(trade);
                if (held === undefined || rankOf(type) < rankOf(held.type)) {
                    ranked.set(trade, { type, value });
                }
            }
            for (const [trade, found] of ranked) {
                tradeReferenceOf.set(trade, found.value);
            }
        }
        const page: BlotterRow[] = rows
            .slice(query.offset, query.offset + query.limit)
            .map((row) => {
                const id = text(row['id']);
                const kind = text(row['row_kind']);
                const structure = holderOf.get(id) ?? '';
                return {
                    row_kind: kind,
                    id,
                    reference:
                        kind === 'structure'
                            ? (referenceOf.get(id) ?? '')
                            : (tradeReferenceOf.get(id) ?? ''),
                    structure,
                    structure_reference: structure === '' ? '' : (referenceOf.get(structure) ?? ''),
                    instrument_type: text(row['instrument_type']),
                    book_id: text(row['book_id']),
                    book_name:
                        kind === 'structure'
                            ? (booksOfStructure.get(id) ?? [])
                                  .map((book) => labelOfBook.get(book) ?? '')
                                  .filter((name) => name !== '')
                                  .join(' + ')
                            : (labelOfBook.get(text(row['book_id'])) ?? ''),
                    counterparty_id: text(row['counterparty_id']),
                    counterparty_name: labelOfCounterparty.get(text(row['counterparty_id'])) ?? '',
                    status:
                        kind === 'structure'
                            ? (legStatesOf.get(id)?.size ?? 0) === 1
                                ? ([...(legStatesOf.get(id) ?? [])][0] ?? '')
                                : ''
                            : text(row['status']),
                    trade_date:
                        kind === 'structure' ? (legDateOf.get(id) ?? '') : text(row['trade_date']),
                    spans_books: (booksOfStructure.get(id) ?? []).length > 1,
                };
            });

        return {
            tree,
            rows: page,
            total: rows.length,
            flat: query.flat,
            view: query.view,
            node: query.node,
        };
    });
}
