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
import { subjects as portfolioSubjects } from '@ores/wire-protocol/generated/refdata/protocol/portfolio_protocol';
import { subjects as memberSubjects } from '@ores/wire-protocol/generated/trading/protocol/structure_member_protocol';
import { subjects as structureIdentifierSubjects } from '@ores/wire-protocol/generated/trading/protocol/structure_identifier_protocol';
import { subjects as structureSubjects } from '@ores/wire-protocol/generated/trading/protocol/structure_protocol';
import { subjects as summarySubjects } from '@ores/wire-protocol/generated/trading/protocol/trade_summary_protocol';
import { readAll, resultSchema, text } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/*
 * The blotter: the party's books as a tree, and one page of the rows a person
 * reads.
 *
 * The rows are the trade summary view's, which states two kinds already — a
 * trade and a structure. A structure's legs are trades too, so the view states
 * them as well. The top list leaves out the trades a structure holds and shows
 * the structure in their place; the flat list does the opposite and shows the
 * legs, each carrying the structure it belongs to.
 *
 * The wire has no "not one of" filter and no structure column on the summary, so
 * the memberships are read and applied here, and a page is narrowed after it is
 * read. That is a first cut, and the read design the task owes says so: the
 * summary wants a structure column a filter can name, and the tree's counts want
 * one grouped read rather than one per node.
 */

const MAX_PAGE = 200;

const blotterQuerySchema = z.object({
    view: z.enum(['portfolio', 'counterparty', 'instrument']).default('portfolio'),
    /** The node the list is narrowed to: `book:<id>` or `portfolio:<id>`, empty for the party. */
    node: z.string().trim().max(100).default(''),
    flat: z.enum(['0', '1']).default('0').transform((value) => value === '1'),
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

type Summary = Readonly<Record<string, unknown>>;

/** One page of the summaries, with the total the filter matches. */
async function summaries(
    session: LiveSession,
    filter: Readonly<Record<string, unknown>>,
    order: { readonly field: string; readonly descending: boolean },
    offset: number,
    limit: number,
): Promise<{ readonly rows: readonly Summary[]; readonly total: number }> {
    /*
     * The filter is built before the call so the static check of the wire
     * requests can tell a variable from a literal, as it does for the other
     * reads: every member of the filter is stated here, absent or not.
     */
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
        throw new Error(
            reply.result.message === '' ? 'The rows could not be read.' : reply.result.message,
        );
    }
    return { rows: reply.trade_summaries as readonly Summary[], total: reply.total };
}

/** A node of the tree the list is narrowed to. Its id says what kind of node it is. */
interface TreeNode {
    readonly id: string;
    readonly label: string;
    readonly kind: 'party' | 'portfolio' | 'book';
    readonly parent: string;
    readonly count: number;
}

const bookNodeId = (id: string): string => `book:${id}`;
const portfolioNodeId = (id: string): string => `portfolio:${id}`;

/** The books under a portfolio or a book, and the ones under it at any depth. */
function bookIdsUnder(node: string, books: readonly Summary[]): readonly string[] {
    if (node === '') {
        return [];
    }
    if (node.startsWith('book:')) {
        return [node.slice('book:'.length)];
    }
    return books.filter((book) => text(book['parent_portfolio_id']) === node.slice('portfolio:'.length)).map((book) => text(book['id']));
}

export function registerBlotterRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    /**
     * One page of the blotter, with the tree beside it.
     *
     * The tree's counts are the number of rows under each node, read from the
     * total a page of one row answers: the count is part of the answer to the
     * page, as every list's count is.
     */
    server.get('/api/blotter', async (request) => {
        const session = requireSession(request);
        const query = blotterQuerySchema.parse(request.query);
        const order = { field: query.sort, descending: query.descending };

        const [books, portfolios, structures, members] = await Promise.all([
            readAll(session, bookSubjects.list_books_request, 'books', {
                filter: { parent_portfolio_id: null, id_one_of: null, parent_portfolio_id_one_of: null },
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
        ]);

        /* Which structure holds which trade, and so which trades are legs. */
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

        /* The books a structure's legs sit in, so the list can say it spans them. */
        const legIds = [...holderOf.keys()];
        const legRows =
            legIds.length === 0
                ? []
                : (
                      await summaries(
                          session,
                          { id_one_of: legIds, row_kind: 'trade' },
                          { field: 'trade_date', descending: true },
                          0,
                          MAX_PAGE,
                      )
                  ).rows;
        const bookOfTrade = new Map(legRows.map((row) => [text(row['id']), text(row['book_id'])] as const));
        const booksOfStructure = new Map<string, readonly string[]>();
        for (const row of structures) {
            const id = text(row['id']);
            booksOfStructure.set(
                id,
                [
                    ...new Set(
                        (legsOf.get(id) ?? [])
                            .map((trade) => bookOfTrade.get(trade) ?? '')
                            .filter((book) => book !== ''),
                    ),
                ],
            );
        }

        /* A structure is found by its own reference, and a leg by the structure's. */
        const referenceOf = new Map<string, string>();
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
                if (owner !== '' && value !== '' && !referenceOf.has(owner)) {
                    referenceOf.set(owner, value);
                }
            }
        }

        /* The tree: the party, its portfolios and their books, each with its count. */
        const portfolioOf = new Map(portfolios.map((row) => [text(row['id']), row] as const));
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
        const bare: Omit<TreeNode, 'count'>[] = [
            { id: '', label: session.party?.name ?? '', kind: 'party', parent: '' },
        ];
        const walk = (parent: string): void => {
            for (const id of childrenOf.get(parent) ?? []) {
                bare.push({
                    id: portfolioNodeId(id),
                    label: text(portfolioOf.get(id)?.['name']) || id,
                    kind: 'portfolio',
                    parent: parent === '' ? '' : portfolioNodeId(parent),
                });
                for (const book of booksUnder.get(id) ?? []) {
                    bare.push({
                        id: bookNodeId(text(book['id'])),
                        label: text(book['name']) || text(book['id']),
                        kind: 'book',
                        parent: portfolioNodeId(id),
                    });
                }
                walk(id);
            }
        };
        walk('');

        const tree: TreeNode[] = [];
        for (const node of bare) {
            const filter =
                node.kind === 'book'
                    ? { book_id: node.id.slice('book:'.length) }
                    : node.kind === 'portfolio'
                      ? { book_id_one_of: bookIdsUnder(node.id, books) }
                      : {};
            tree.push({ ...node, count: (await summaries(session, filter, order, 0, 1)).total });
        }

        /* The page, narrowed by the node and the search, then by what the list holds. */
        const nodeFilter =
            query.node === '' ? {} : query.node.startsWith('book:')
                ? { book_id: query.node.slice('book:'.length) }
                : { book_id_one_of: bookIdsUnder(query.node, books) };
        const page = await summaries(
            session,
            { ...nodeFilter, ...(query.search === '' ? {} : { search: query.search }) },
            order,
            0,
            MAX_PAGE,
        );
        const kept = page.rows.filter((row) => {
            const kind = text(row['row_kind']);
            return query.flat ? kind === 'trade' : kind === 'structure' || !holderOf.has(text(row['id']));
        });
        const rows = kept.slice(query.offset, query.offset + query.limit).map((row) => {
            const id = text(row['id']);
            const kind = text(row['row_kind']);
            const structure = holderOf.get(id) ?? '';
            return {
                row_kind: kind,
                id,
                reference: kind === 'structure' ? (referenceOf.get(id) ?? '') : '',
                structure,
                structure_reference: structure === '' ? '' : (referenceOf.get(structure) ?? ''),
                instrument_type: text(row['instrument_type']),
                book_id: text(row['book_id']),
                counterparty_id: text(row['counterparty_id']),
                status: text(row['status']),
                trade_date: text(row['trade_date']),
                legs: kind === 'structure' ? (legsOf.get(id) ?? []).length : 0,
                spans_books: (booksOfStructure.get(id) ?? []).length > 1,
            };
        });

        return { tree, rows, total: kept.length, flat: query.flat, view: query.view, node: query.node };
    });
}
