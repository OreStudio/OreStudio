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
 * Everything the book structure journey reaches the server with.
 *
 * A step's body is a function of the answers these calls give, so a test that
 * hands the page a server of its own walks the whole journey with no transport
 * in the way.
 */

import { useMemo } from 'react';
import { api } from '../api/client.js';
import { books } from '../api/books.js';
import type {
    BookIntent,
    BookPickLists,
    BookTree,
    BookWriteOutcome,
    PortfolioRights,
} from '../api/books.js';
import type { Book } from '@ores/wire-protocol/generated/refdata/domain/book';
import type { Portfolio } from '@ores/wire-protocol/generated/refdata/domain/portfolio';
import type { BookLineInput } from '@ores/wire-protocol/browser';

export type { BookIntent, BookPickLists, BookTree, BookWriteOutcome, PortfolioRights };

/** What the policy says about a book change, before anything is written. */
export interface BookProposal {
    /** The parts the policy names for the change; empty when the book is written directly. */
    readonly partCodes: readonly string[];
    /** The refusal the real write gave, or empty when it would stand. */
    readonly refusal: string;
    /** The columns the change touches. */
    readonly columns: readonly string[];
    /** The parts that decide the change alone. */
    readonly linePartCodes: readonly string[];
}

/** What raising a request for a book change came to. */
export interface BookRaised {
    /** Set when the request was raised. */
    readonly requestId: string | null;
    readonly partCodes: readonly string[];
    readonly refusalCode: string;
    readonly refusalMessage: string;
}

export interface BooksServer {
    /** Every portfolio and every book of the tenant. */
    readonly tree: () => Promise<BookTree>;
    /** Every list a step's picker draws from. */
    readonly pickLists: () => Promise<BookPickLists>;
    /** The rights at one portfolio node, with the accounts that hold them. */
    readonly rightsAt: (portfolioId: string) => Promise<PortfolioRights>;
    /** The reasons a record may be amended for. */
    readonly amendReasons: () => Promise<
        readonly {
            readonly code: string;
            readonly description: string;
            readonly requiresCommentary: boolean;
        }[]
    >;
    /** Writes a portfolio: as new when no version is given. */
    readonly writePortfolio: (
        write: Portfolio,
        version: number | null,
        intent: BookIntent,
    ) => Promise<BookWriteOutcome<Portfolio>>;
    /** Writes a book: as new when no version is given. */
    readonly writeBook: (
        write: Book,
        version: number | null,
        intent: BookIntent,
    ) => Promise<BookWriteOutcome<Book>>;
    /**
     * Asks the policy what a book change needs, and runs the real write without
     * keeping it, so the person learns the parts and every refusal before they
     * confirm.
     */
    readonly previewBook: (write: Book, version: number | null) => Promise<BookProposal>;
    /** Raises one request that holds the book change, for the parts the policy named. */
    readonly raiseBook: (
        write: Book,
        version: number | null,
        reason: string,
    ) => Promise<BookRaised>;
}

/** The book as the proposal operations read it. */
export function bookLine(write: Book, version: number | null): BookLineInput {
    return {
        operation: 'put',
        entityId: write.id,
        baseVersion: version ?? 0,
        partyId: write.party_id,
        name: write.name,
        description: write.description,
        parentPortfolioId: write.parent_portfolio_id,
        ownerUnitId: write.owner_unit_id,
        functionalCurrency: write.functional_currency,
        glAccountRef: write.gl_account_ref,
        costCenter: write.cost_center,
        bookStatus: write.book_status,
        regulatoryBookType: write.regulatory_book_type,
        bookPurposeType: write.book_purpose_type,
        ledgerFeedType: write.ledger_feed_type,
        isSweepable: write.is_sweepable,
        ratesCentreCode: write.rates_centre_code,
        sandboxId: write.sandbox_id,
    };
}

/** The deployment's own server, as the book structure journey reaches it. */
export function useBooksServer(): BooksServer {
    return useMemo<BooksServer>(
        () => ({
            tree: books.tree,
            pickLists: books.pickLists,
            rightsAt: books.rights,
            amendReasons: api.amendReasons,
            writePortfolio: books.writePortfolio,
            writeBook: books.writeBook,
            previewBook: async (write, version) => {
                const preview = await api.previewBookChanges([bookLine(write, version)]);
                const line = preview.lines[0];
                return {
                    partCodes: preview.partCodes,
                    refusal: line?.refusal ?? '',
                    columns: line?.columns ?? [],
                    linePartCodes: line?.partCodes ?? [],
                };
            },
            raiseBook: async (write, version, reason) => {
                const raised = await api.raiseBookChanges({
                    reason,
                    lines: [bookLine(write, version)],
                });
                return {
                    requestId: raised.requestId,
                    partCodes: raised.partCodes,
                    refusalCode: raised.refusalCode,
                    refusalMessage:
                        raised.refusalMessage !== ''
                            ? raised.refusalMessage
                            : (raised.lines.find((line) => line.refusal !== '')?.refusal ?? ''),
                };
            },
        }),
        [],
    );
}
