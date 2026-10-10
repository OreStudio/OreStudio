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

export type { BookIntent, BookPickLists, BookTree, BookWriteOutcome, PortfolioRights };

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
        }),
        [],
    );
}
