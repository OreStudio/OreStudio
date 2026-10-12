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
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import { TranslationProvider } from '../i18n/Provider.js';
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
import type { BookProposal, BookRaised, BookWriteOutcome, BooksServer } from './booksServer.js';
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

const ungated: BookProposal = { partCodes: [], refusal: '', columns: [], linePartCodes: [] };

const gated: BookProposal = {
    partCodes: ['finance', 'operations'],
    refusal: '',
    columns: ['cost_center'],
    linePartCodes: ['finance', 'operations'],
};

function raisedAs(requestId: string): BookRaised {
    return { requestId, partCodes: ['finance', 'operations'], refusalCode: '', refusalMessage: '' };
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
        // The policy names no part unless a case says it does, so a book is
        // written directly, as it was before the policy gated it.
        previewBook: vi.fn(async () => ungated),
        raiseBook: vi.fn(async () => {
            throw new Error('no request is raised here');
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
        recordRaised: vi.fn(),
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

    it('raises a request instead of writing when the policy names a part', async () => {
        const write = vi.fn();
        const raise = vi.fn(async () => raisedAs('request-1'));
        const state = asStructure(opened({ kind: 'set-field', field: 'costCenter', value: 'CC9' }));
        const server = fakeServer([], {
            previewBook: vi.fn(async () => gated),
            raiseBook: raise,
            writeBook: write,
        });
        const onWritten = vi.fn();

        await reviewOf(stepsFor(state, server, onWritten)).next?.run?.();

        expect(write).not.toHaveBeenCalled();
        expect(raise).toHaveBeenCalledWith(
            expect.objectContaining({ name: 'RATES-1', cost_center: 'CC9' }),
            4,
            expect.any(String),
        );
        expect(state.recordRaised).toHaveBeenCalledWith({
            requestId: 'request-1',
            partCodes: ['finance', 'operations'],
        });
        expect(state.recordWritten).not.toHaveBeenCalled();
        expect(onWritten).not.toHaveBeenCalled();
    });

    it('gives the request the commentary the person wrote as its reason', async () => {
        const raise = vi.fn(async () => raisedAs('request-1'));
        const state = asStructure(
            opened(
                { kind: 'set-field', field: 'costCenter', value: 'CC9' },
                { kind: 'set-commentary', commentary: 'Moved to the rates desk' },
            ),
        );
        const server = fakeServer([], { previewBook: vi.fn(async () => gated), raiseBook: raise });

        await reviewOf(stepsFor(state, server)).next?.run?.();

        expect(raise).toHaveBeenCalledWith(expect.anything(), 4, 'Moved to the rates desk');
    });

    it('writes a portfolio first and then raises the request for the book that sits in it', async () => {
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
        const raise = vi.fn(async () => raisedAs('request-2'));
        const server = fakeServer(log, {
            previewBook: vi.fn(async () => gated),
            raiseBook: raise,
        });

        await reviewOf(stepsFor(state, server)).next?.run?.();

        expect(log).toEqual(['portfolio']);
        expect(raise).toHaveBeenCalled();
        expect(state.recordRaised).toHaveBeenCalledWith(
            expect.objectContaining({ requestId: 'request-2' }),
        );
    });

    it('stops at a change the real write would refuse, and raises nothing', async () => {
        const raise = vi.fn();
        const state = asStructure(
            opened({ kind: 'set-field', field: 'bookStatus', value: 'Closed' }),
        );
        const server = fakeServer([], {
            previewBook: vi.fn(async () => ({ ...gated, refusal: 'Closed is not allowed.' })),
            raiseBook: raise,
        });

        await expect(reviewOf(stepsFor(state, server)).next?.run?.()).rejects.toThrow(
            /Closed is not allowed/,
        );
        expect(raise).not.toHaveBeenCalled();
        expect(state.recordRefusal).toHaveBeenCalledWith(
            expect.objectContaining({
                step: 'book',
                code: 'line_refused',
                subject: 'refdata.v1.ops.preview_book_changes',
            }),
        );
    });

    it('records a raise the server refused, with its reason', async () => {
        const state = asStructure(opened({ kind: 'set-field', field: 'costCenter', value: 'CC9' }));
        const server = fakeServer([], {
            previewBook: vi.fn(async () => gated),
            raiseBook: vi.fn(async () => ({
                requestId: null,
                partCodes: [],
                refusalCode: 'needs_no_request',
                refusalMessage: 'Write it directly.',
            })),
        });

        await expect(reviewOf(stepsFor(state, server)).next?.run?.()).rejects.toThrow(
            /Write it directly/,
        );
        expect(state.recordRaised).not.toHaveBeenCalled();
    });

    it('writes directly when the policy names no part, even if the preview refuses the change', async () => {
        const write = vi.fn(async () => refused<Book>('Closed is not allowed from here.'));
        const raise = vi.fn();
        const state = asStructure(
            opened({ kind: 'set-field', field: 'bookStatus', value: 'Closed' }),
        );
        const server = fakeServer([], {
            previewBook: vi.fn(async () => ({ ...ungated, refusal: 'Closed is not allowed.' })),
            writeBook: write,
            raiseBook: raise,
        });

        await expect(reviewOf(stepsFor(state, server)).next?.run?.()).rejects.toThrow();

        // The write's own refusal, with its code and fields, is the one recorded.
        expect(write).toHaveBeenCalled();
        expect(raise).not.toHaveBeenCalled();
        expect(state.recordRefusal).toHaveBeenCalledWith(
            expect.objectContaining({ code: 'status_transition_not_allowed' }),
        );
    });

    it('finishes the walk once a request is raised, as it does once a book is written', () => {
        const waiting = { ...opened(), raised: { requestId: 'request-1', partCodes: ['finance'] } };
        const steps = stepsFor(asStructure(waiting), fakeServer([]));

        expect(reviewOf(steps).final).toBe(true);
    });

    it('says the request is waiting, for whom, and where to open it', () => {
        const waiting = {
            ...opened({ kind: 'set-field', field: 'costCenter', value: 'CC9' }),
            raised: { requestId: 'request-1', partCodes: ['finance', 'operations'] },
        };
        const steps = stepsFor(asStructure(waiting), fakeServer([]));
        const outcome = steps[bookStepIndex('outcome')];

        const html = renderToStaticMarkup(
            <TranslationProvider>
                <MemoryRouter>{outcome?.body}</MemoryRouter>
            </TranslationProvider>,
        );

        expect(html).toContain('is waiting for Finance, Operations');
        expect(html).toContain('href="/requests/request-1"');
        expect(html).not.toContain('is now at version');
    });
});
