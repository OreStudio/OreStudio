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

import { describe, expect, it, vi } from 'vitest';
import { createTranslator } from '../i18n/translate.js';
import { enFlat } from '../i18n/locales/en.js';
import { BOOK_STEP_IDS, bookStepIndex, bookSteps } from './booksSteps.js';
import {
    blankDraft,
    changesOf,
    reduceBook,
    type BookAction,
    type BookDraft,
    type BookStructure,
} from './booksState.js';
import type { BookWriteOutcome, BooksServer } from './booksServer.js';
import type { Book } from '@ores/wire-protocol/generated/refdata/domain/book';
import type { Portfolio } from '@ores/wire-protocol/generated/refdata/domain/portfolio';

const t = createTranslator('en', enFlat, enFlat).t;

const audit = {
    tenant_id: 't',
    modified_by: 'm',
    performed_by: 'p',
    change_reason_code: 'r',
    change_commentary: '',
    recorded_at: '2026-01-01T00:00:00Z',
};

const book: Book = {
    ...audit,
    version: 4,
    id: 'book-1',
    party_id: 'party-1',
    name: 'RATES-1',
    description: '',
    parent_portfolio_id: 'rates',
    owner_unit_id: null,
    functional_currency: 'GBP',
    gl_account_ref: '',
    cost_center: '',
    book_status: 'Active',
    regulatory_book_type: 'Trading',
    book_purpose_type: 'Hedging',
    ledger_feed_type: 'Daily',
    is_sweepable: false,
    rates_centre_code: 'GBLO',
    sandbox_id: null,
};

function success<Row>(row: Row | undefined): BookWriteOutcome<Row> {
    return { success: true, code: '', message: '', fields: [], row };
}

function refused<Row>(message: string): BookWriteOutcome<Row> {
    return {
        success: false,
        code: 'status_transition_not_allowed',
        message,
        fields: [{ field: 'book_status', code: 'transition', message }],
        row: undefined,
    };
}

function fakeServer(log: string[], overrides: Partial<BooksServer> = {}): BooksServer {
    return {
        tree: vi.fn(async () => ({ portfolios: [], books: [] })),
        pickLists: vi.fn(async () => {
            throw new Error('not read here');
        }),
        rightsAt: vi.fn(async () => ({ rights: [], accounts: [] })),
        amendReasons: vi.fn(async () => []),
        writePortfolio: vi.fn(async (write: Portfolio) => {
            log.push('portfolio');
            return success(write);
        }),
        writeBook: vi.fn(async (write: Book) => {
            log.push('book');
            return success({ ...write, version: write.version + 1 });
        }),
        ...overrides,
    };
}

function asStructure(draft: BookDraft): BookStructure {
    const never = vi.fn();
    return {
        ...draft,
        changes: changesOf(draft),
        selectPortfolio: never,
        startPortfolio: never,
        cancelPortfolio: never,
        portfolioWritten: vi.fn(),
        setPortfolioField: never,
        setPortfolioVirtual: never,
        startBook: never,
        openBook: never,
        setField: never,
        setSweepable: never,
        revertFields: never,
        setReason: never,
        setCommentary: never,
        recordWritten: vi.fn(),
        recordRefusal: vi.fn(),
        clearRefusal: vi.fn(),
    };
}

function opened(...actions: readonly BookAction[]): BookDraft {
    return actions.reduce(reduceBook, reduceBook(blankDraft(), { kind: 'open-book', book }));
}

function stepsFor(state: BookStructure, server: BooksServer, onWritten = vi.fn()) {
    return bookSteps({
        t,
        server,
        state,
        partyId: 'party-1',
        tree: undefined,
        pickLists: undefined,
        pickFailure: undefined,
        reasons: [],
        onMove: vi.fn(),
        onWritten,
        onFinished: vi.fn(),
    });
}

function reviewOf(steps: ReturnType<typeof stepsFor>) {
    const review = steps[bookStepIndex('review')];
    if (review === undefined) {
        throw new Error('no review step');
    }
    return review;
}

describe('the book structure steps', () => {
    it('lists the steps in the order the rail draws them', () => {
        const steps = stepsFor(asStructure(blankDraft()), fakeServer([]));
        expect(steps.map((step) => step.id)).toEqual([...BOOK_STEP_IDS]);
    });

    it('holds the confirm until something has changed', () => {
        expect(reviewOf(stepsFor(asStructure(opened()), fakeServer([]))).next?.enabled).toBe(false);
        const changed = opened({ kind: 'set-field', field: 'description', value: 'Rates desk' });
        expect(reviewOf(stepsFor(asStructure(changed), fakeServer([]))).next?.enabled).toBe(true);
    });

    it('writes the portfolio before the book that sits in it, then reads the tree again', async () => {
        const log: string[] = [];
        const draft = [
            { kind: 'start-portfolio', parentPortfolioId: '' },
            { kind: 'set-portfolio-field', field: 'name', value: 'Credit' },
            { kind: 'set-portfolio-field', field: 'purposeType', value: 'Trading' },
            { kind: 'set-portfolio-field', field: 'aggregationCcy', value: 'GBP' },
            { kind: 'start-book' },
            { kind: 'set-field', field: 'name', value: 'CREDIT-1' },
            { kind: 'set-field', field: 'functionalCurrency', value: 'GBP' },
            { kind: 'set-field', field: 'bookStatus', value: 'Active' },
            { kind: 'set-field', field: 'regulatoryBookType', value: 'Trading' },
            { kind: 'set-field', field: 'bookPurposeType', value: 'Hedging' },
            { kind: 'set-field', field: 'ledgerFeedType', value: 'Daily' },
        ].reduce(reduceBook, blankDraft());
        const state = asStructure(draft);
        const onWritten = vi.fn();
        await reviewOf(stepsFor(state, fakeServer(log), onWritten)).next?.run?.();
        expect(log).toEqual(['portfolio', 'book']);
        expect(state.recordWritten).toHaveBeenCalled();
        expect(onWritten).toHaveBeenCalled();
        expect(state.portfolioWritten).toHaveBeenCalledWith(expect.any(String));
    });

    it('stops at a refused portfolio and writes no book', async () => {
        const log: string[] = [];
        const draft = [
            { kind: 'start-portfolio', parentPortfolioId: '' },
            { kind: 'set-portfolio-field', field: 'name', value: 'Credit' },
            { kind: 'set-portfolio-field', field: 'purposeType', value: 'Trading' },
            { kind: 'set-portfolio-field', field: 'aggregationCcy', value: 'GBP' },
            { kind: 'start-book' },
            { kind: 'set-field', field: 'name', value: 'CREDIT-1' },
            { kind: 'set-field', field: 'functionalCurrency', value: 'GBP' },
            { kind: 'set-field', field: 'bookStatus', value: 'Active' },
            { kind: 'set-field', field: 'regulatoryBookType', value: 'Trading' },
            { kind: 'set-field', field: 'bookPurposeType', value: 'Hedging' },
            { kind: 'set-field', field: 'ledgerFeedType', value: 'Daily' },
        ].reduce(reduceBook, blankDraft());
        const state = asStructure(draft);
        const server = fakeServer(log, {
            writePortfolio: vi.fn(async () => refused<Portfolio>('Taken.')),
        });
        await expect(reviewOf(stepsFor(state, server)).next?.run?.()).rejects.toThrow(/Taken/);
        expect(log).toEqual([]);
        expect(state.recordRefusal).toHaveBeenCalledWith(
            expect.objectContaining({ step: 'portfolio', written: [] }),
        );
    });

    it('names the book step and what was already written when the book is refused', async () => {
        const state = asStructure(
            opened({ kind: 'set-field', field: 'bookStatus', value: 'Closed' }),
        );
        const server = fakeServer([], {
            writeBook: vi.fn(async () => refused<Book>('Closed is not allowed from here.')),
        });
        await expect(reviewOf(stepsFor(state, server)).next?.run?.()).rejects.toThrow(
            /Nothing was written/,
        );
        expect(state.recordRefusal).toHaveBeenCalledWith(
            expect.objectContaining({
                step: 'book',
                code: 'status_transition_not_allowed',
                subject: 'refdata.v1.books.put',
            }),
        );
        expect(state.recordWritten).not.toHaveBeenCalled();
    });

    it('writes a changed book against the version it read', async () => {
        const write = vi.fn(async (b: Book) => success({ ...b, version: 5 }));
        const state = asStructure(
            opened({ kind: 'set-field', field: 'description', value: 'Rates desk' }),
        );
        await reviewOf(stepsFor(state, fakeServer([], { writeBook: write }))).next?.run?.();
        expect(write).toHaveBeenCalledWith(
            expect.objectContaining({ name: 'RATES-1', description: 'Rates desk' }),
            4,
            expect.objectContaining({ reason_code: 'common.non_material_update' }),
        );
    });
});
