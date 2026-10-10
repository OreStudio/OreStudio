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
import type { Account } from '@ores/wire-protocol/generated/iam/domain/account';
import type { Book } from '@ores/wire-protocol/generated/refdata/domain/book';
import type { BookPurposeType } from '@ores/wire-protocol/generated/refdata/domain/book_purpose_type';
import type { BookStatus } from '@ores/wire-protocol/generated/refdata/domain/book_status';
import type { BusinessCentre } from '@ores/wire-protocol/generated/refdata/domain/business_centre';
import type { BusinessUnit } from '@ores/wire-protocol/generated/refdata/domain/business_unit';
import type { Currency } from '@ores/wire-protocol/generated/refdata/domain/currency';
import type { LedgerFeedType } from '@ores/wire-protocol/generated/refdata/domain/ledger_feed_type';
import type { Portfolio } from '@ores/wire-protocol/generated/refdata/domain/portfolio';
import type { PortfolioRight } from '@ores/wire-protocol/generated/refdata/domain/portfolio_right';
import type { PurposeType } from '@ores/wire-protocol/generated/refdata/domain/purpose_type';
import type { RegulatoryBookType } from '@ores/wire-protocol/generated/refdata/domain/regulatory_book_type';
import { request } from './transport.js';

/**
 * The book structure screen's reads and writes.
 *
 * Rows are the refdata service's, passed through the BFF under its own field
 * names, so each is typed from the generated domain row and parsed only as an
 * object with a version.
 */

const JSON_HEADERS = { 'Content-Type': 'application/json' } as const;

/** Why a write is made. */
export interface BookIntent {
    readonly reason_code: string;
    readonly commentary: string;
}

/** The result a write carries. */
export interface BookResult {
    readonly outcome:
        'ok' | 'invalid' | 'denied' | 'missing' | 'conflict' | 'unavailable' | 'failed';
    readonly code: string;
    readonly message: string;
    readonly fields: readonly {
        readonly field: string;
        readonly code: string;
        readonly message: string;
    }[];
}

/** The outcome of a write as the screen renders it. */
export interface BookWriteOutcome<Row> {
    readonly success: boolean;
    readonly code: string;
    readonly message: string;
    readonly fields: BookResult['fields'];
    readonly row: Row | undefined;
}

/** The portfolios and the books under them. */
export interface BookTree {
    readonly portfolios: readonly Portfolio[];
    readonly books: readonly Book[];
}

/** The reference data the screen's pickers draw from. */
export interface BookPickLists {
    readonly bookStatuses: readonly BookStatus[];
    readonly regulatoryBookTypes: readonly RegulatoryBookType[];
    readonly bookPurposeTypes: readonly BookPurposeType[];
    readonly ledgerFeedTypes: readonly LedgerFeedType[];
    readonly purposeTypes: readonly PurposeType[];
    readonly currencies: readonly Currency[];
    readonly businessCentres: readonly BusinessCentre[];
    readonly businessUnits: readonly BusinessUnit[];
}

/** The rights at one portfolio node, and the accounts that hold them. */
export interface PortfolioRights {
    readonly rights: readonly PortfolioRight[];
    readonly accounts: readonly Account[];
}

const resultSchema: z.ZodType<BookResult> = z.object({
    outcome: z.enum(['ok', 'invalid', 'denied', 'missing', 'conflict', 'unavailable', 'failed']),
    code: z.string(),
    message: z.string(),
    fields: z.array(z.object({ field: z.string(), code: z.string(), message: z.string() })),
});

function rowsOf<Row>(): z.ZodType<readonly Row[]> {
    return z.array(z.looseObject({})) as unknown as z.ZodType<readonly Row[]>;
}

function rowOf<Row>(): z.ZodType<Row | null> {
    return z.looseObject({ version: z.int() }).nullable() as unknown as z.ZodType<Row | null>;
}

function outcome<Row>(result: BookResult, row: Row | null): BookWriteOutcome<Row> {
    return {
        success: result.outcome === 'ok',
        code: result.code,
        message: result.message,
        fields: result.fields,
        row: result.outcome === 'ok' && row !== null ? row : undefined,
    };
}

async function put<Row>(
    path: string,
    field: 'book' | 'portfolio',
    write: Row,
    version: number | null,
    intent: BookIntent,
): Promise<BookWriteOutcome<Row>> {
    const answer = z.looseObject({ result: resultSchema }).parse(
        await request(path, {
            method: 'PUT',
            headers: JSON_HEADERS,
            body: JSON.stringify({ intent, version, write }),
        }),
    );
    return outcome(answer.result, rowOf<Row>().parse(answer[field] ?? null));
}

export const books = {
    /** Every portfolio and every book of the tenant. */
    async tree(): Promise<BookTree> {
        return z
            .object({ portfolios: rowsOf<Portfolio>(), books: rowsOf<Book>() })
            .parse(await request('/api/books/tree', { method: 'GET' }));
    },

    /** Every list a picker draws from. */
    async pickLists(): Promise<BookPickLists> {
        return z
            .object({
                bookStatuses: rowsOf<BookStatus>(),
                regulatoryBookTypes: rowsOf<RegulatoryBookType>(),
                bookPurposeTypes: rowsOf<BookPurposeType>(),
                ledgerFeedTypes: rowsOf<LedgerFeedType>(),
                purposeTypes: rowsOf<PurposeType>(),
                currencies: rowsOf<Currency>(),
                businessCentres: rowsOf<BusinessCentre>(),
                businessUnits: rowsOf<BusinessUnit>(),
            })
            .parse(await request('/api/books/pick-lists', { method: 'GET' }));
    },

    /** The rights at one portfolio node. */
    async rights(portfolioId: string): Promise<PortfolioRights> {
        return z.object({ rights: rowsOf<PortfolioRight>(), accounts: rowsOf<Account>() }).parse(
            await request(`/api/books/portfolios/${encodeURIComponent(portfolioId)}/rights`, {
                method: 'GET',
            }),
        );
    },

    /** Writes a book: as new when no version is given, else against the version read. */
    writeBook(write: Book, version: number | null, intent: BookIntent) {
        return put('/api/books/books', 'book', write, version, intent);
    },

    /** Writes a portfolio: as new when no version is given, else against the version read. */
    writePortfolio(write: Portfolio, version: number | null, intent: BookIntent) {
        return put('/api/books/portfolios', 'portfolio', write, version, intent);
    },
};
