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
import { recordResource } from '@ores/wire-protocol';
import { subjects as accountSubjects } from '@ores/wire-protocol/generated/iam/protocol/account_protocol';
import { subjects as bookPurposeSubjects } from '@ores/wire-protocol/generated/refdata/protocol/book_purpose_type_protocol';
import { subjects as bookStatusSubjects } from '@ores/wire-protocol/generated/refdata/protocol/book_status_protocol';
import { subjects as bookSubjects } from '@ores/wire-protocol/generated/refdata/protocol/book_protocol';
import { subjects as businessUnitSubjects } from '@ores/wire-protocol/generated/refdata/protocol/business_unit_protocol';
import { subjects as ledgerFeedSubjects } from '@ores/wire-protocol/generated/refdata/protocol/ledger_feed_type_protocol';
import { subjects as portfolioRightSubjects } from '@ores/wire-protocol/generated/refdata/protocol/portfolio_right_protocol';
import { subjects as portfolioSubjects } from '@ores/wire-protocol/generated/refdata/protocol/portfolio_protocol';
import { subjects as purposeSubjects } from '@ores/wire-protocol/generated/refdata/protocol/purpose_type_protocol';
import { subjects as regulatoryTypeSubjects } from '@ores/wire-protocol/generated/refdata/protocol/regulatory_book_type_protocol';
import { invalidRequest } from './errors.js';
import {
    idSchema,
    input,
    intentSchema,
    paramId,
    readAll,
    resultSchema,
    row,
} from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/**
 * A write of a book or a portfolio: the row, and the version the person read.
 *
 * No version claims the row is new. A version claims the row is still the one
 * read, so a write made meanwhile is refused instead of lost.
 */
const writeBodySchema = z.object({
    intent: intentSchema,
    version: z.int().nonnegative().nullable(),
    write: z.looseObject({
        id: idSchema,
        party_id: idSchema,
        name: z.string().trim().min(1).max(200),
    }),
});

const PICK_LISTS = [
    {
        key: 'bookStatuses',
        subject: bookStatusSubjects.list_book_statuses_request,
        rows: 'statuses',
    },
    {
        key: 'regulatoryBookTypes',
        subject: regulatoryTypeSubjects.list_regulatory_book_types_request,
        rows: 'types',
    },
    {
        key: 'bookPurposeTypes',
        subject: bookPurposeSubjects.list_book_purpose_types_request,
        rows: 'types',
    },
    {
        key: 'ledgerFeedTypes',
        subject: ledgerFeedSubjects.list_ledger_feed_types_request,
        rows: 'types',
    },
    { key: 'purposeTypes', subject: purposeSubjects.list_purpose_types_request, rows: 'types' },
] as const;

async function putRow(
    session: LiveSession,
    subject: string,
    body: z.infer<typeof writeBodySchema>,
): Promise<{ readonly result: z.infer<typeof resultSchema>; readonly row: unknown }> {
    const reply = await session.client.callAuthenticated(
        subject,
        {
            change: {
                write: body.write,
                precondition:
                    body.version === null
                        ? { kind: 'must_not_exist', version: null }
                        : { kind: 'must_match_version', version: body.version },
            },
            intent: body.intent,
        },
        z.looseObject({ result: resultSchema }),
    );
    return { result: reply.result, row: reply['book'] ?? reply['portfolio'] ?? null };
}

/**
 * The book structure routes: the tree, the pick lists, the rights at a node,
 * and the book and portfolio writes.
 *
 * The rights are read only. The server addresses a right by its code alone, so
 * a grant of a code another account already holds would collide and a removal
 * would reach whichever account's row it found first. Granting and removing
 * wait for the server to address a right by account, portfolio and code.
 */
export function registerBookRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    server.get('/api/books/tree', async (request) => {
        const session = requireSession(request);
        return {
            portfolios: await readAll(
                session,
                portfolioSubjects.list_portfolios_request,
                'portfolios',
                {
                    filter: null,
                },
            ),
            books: await readAll(session, bookSubjects.list_books_request, 'books', {
                filter: null,
            }),
        };
    });

    server.get('/api/books/pick-lists', async (request) => {
        const session = requireSession(request);
        const lists: Record<string, readonly Record<string, unknown>[]> = {};
        for (const list of PICK_LISTS) {
            lists[list.key] = await readAll(session, list.subject, list.rows, { filter: null });
        }
        for (const [key, resource] of [
            ['currencies', recordResource('currencies')],
            ['businessCentres', recordResource('business-centres')],
        ] as const) {
            if (resource === undefined) {
                throw invalidRequest(`The ${key} list is not registered.`);
            }
            lists[key] = await readAll(session, resource.subjects.list, resource.rows, {
                filter: null,
                ...(resource.asOf ? { as_of: null } : {}),
            });
        }
        lists['businessUnits'] = await readAll(
            session,
            businessUnitSubjects.list_business_units_request,
            'business_units',
            { filter: null },
        );
        return lists;
    });

    server.get('/api/books/portfolios/:id/rights', async (request) => {
        const session = requireSession(request);
        const id = paramId(request);
        return {
            rights: await readAll(
                session,
                portfolioRightSubjects.list_by_portfolio_id_portfolio_rights_request,
                'portfolio_rights',
                { portfolio_id: id, scope: 'direct', filter: null },
            ),
            accounts: await readAll(session, accountSubjects.list_accounts_request, 'accounts', {
                filter: null,
                as_of: null,
            }),
        };
    });

    server.put('/api/books/books', async (request) => {
        const session = requireSession(request);
        const body = input(writeBodySchema, request.body);
        const { result, row: written } = await putRow(session, bookSubjects.put_book_request, body);
        return { result, book: written };
    });

    server.put('/api/books/portfolios', async (request) => {
        const session = requireSession(request);
        const body = input(writeBodySchema, request.body);
        const { result, row: written } = await putRow(
            session,
            portfolioSubjects.put_portfolio_request,
            body,
        );
        return { result, portfolio: written };
    });
}
