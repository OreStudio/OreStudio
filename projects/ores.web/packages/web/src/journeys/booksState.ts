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
 * The book structure journey's draft, and what the confirm makes of it.
 *
 * A book is created or corrected under one portfolio, and the portfolio may be
 * created in the same walk. The draft records the book as the person left it,
 * and the review is a function of it against the book as read, so nothing is
 * sent for a field that did not change. A book reads its aggregation currency,
 * its sandbox and its rights from the portfolio and copies none of them, so
 * the draft holds none of those.
 */

import { useReducer } from 'react';
import type { Book } from '@ores/wire-protocol/generated/refdata/domain/book';
import type { Portfolio } from '@ores/wire-protocol/generated/refdata/domain/portfolio';
import type { BookIntent } from './booksServer.js';

/** The reason a correction is filed under until the person states another. */
export const BOOK_REASON = 'common.non_material_update';

/** The reason a new record is filed under, which the reasons list for corrections does not offer. */
export const NEW_BOOK_REASON = 'system.new_record';

export const BOOK_ENTITY_TYPE = 'ores.refdata.book';

/** The book fields the person edits. The name is the natural key and does not change. */
export interface BookFields {
    readonly name: string;
    readonly description: string;
    readonly functionalCurrency: string;
    readonly glAccountRef: string;
    readonly costCenter: string;
    readonly ownerUnitId: string;
    readonly bookStatus: string;
    readonly regulatoryBookType: string;
    readonly bookPurposeType: string;
    readonly ledgerFeedType: string;
    readonly isSweepable: boolean;
    readonly ratesCentreCode: string;
}

export type BookField = Exclude<keyof BookFields, 'isSweepable'>;

/** A portfolio the person is creating in this walk. */
export interface PortfolioDraft {
    readonly id: string;
    readonly name: string;
    readonly description: string;
    readonly parentPortfolioId: string;
    readonly ownerUnitId: string;
    readonly purposeType: string;
    readonly aggregationCcy: string;
    readonly isVirtual: boolean;
}

export type PortfolioField = Exclude<keyof PortfolioDraft, 'id' | 'isVirtual'>;

/** What the server refused, kept so the review can name it and the person can walk back. */
export interface BookRefusal {
    readonly step: string;
    readonly subject: string;
    readonly code: string;
    readonly message: string;
    readonly fields: readonly {
        readonly field: string;
        readonly code: string;
        readonly message: string;
    }[];
    /** The calls that were already written when this one was refused. */
    readonly written: readonly string[];
}

export interface BookDraft {
    /** The portfolio the book sits in, existing or new. */
    readonly portfolioId: string;
    readonly newPortfolio: PortfolioDraft | undefined;
    /** The book as read, or undefined for a new book. */
    readonly opened: Book | undefined;
    /** Whether a book is being shaped at all. */
    readonly shaping: boolean;
    readonly bookId: string;
    readonly fields: BookFields;
    readonly reasonCode: string;
    readonly commentary: string;
    readonly refusal: BookRefusal | undefined;
    readonly written: Book | undefined;
}

export function blankFields(): BookFields {
    return {
        name: '',
        description: '',
        functionalCurrency: '',
        glAccountRef: '',
        costCenter: '',
        ownerUnitId: '',
        bookStatus: '',
        regulatoryBookType: '',
        bookPurposeType: '',
        ledgerFeedType: '',
        isSweepable: false,
        ratesCentreCode: '',
    };
}

export function blankDraft(): BookDraft {
    return {
        portfolioId: '',
        newPortfolio: undefined,
        opened: undefined,
        shaping: false,
        bookId: '',
        fields: blankFields(),
        reasonCode: BOOK_REASON,
        commentary: '',
        refusal: undefined,
        written: undefined,
    };
}

function fieldsOf(book: Book): BookFields {
    return {
        name: book.name,
        description: book.description,
        functionalCurrency: book.functional_currency,
        glAccountRef: book.gl_account_ref,
        costCenter: book.cost_center,
        ownerUnitId: book.owner_unit_id ?? '',
        bookStatus: book.book_status,
        regulatoryBookType: book.regulatory_book_type,
        bookPurposeType: book.book_purpose_type,
        ledgerFeedType: book.ledger_feed_type,
        isSweepable: book.is_sweepable,
        ratesCentreCode: book.rates_centre_code,
    };
}

export type BookAction =
    | { readonly kind: 'select-portfolio'; readonly portfolioId: string }
    | { readonly kind: 'start-portfolio'; readonly parentPortfolioId: string }
    | { readonly kind: 'cancel-portfolio' }
    | { readonly kind: 'portfolio-written'; readonly portfolioId: string }
    | {
          readonly kind: 'set-portfolio-field';
          readonly field: PortfolioField;
          readonly value: string;
      }
    | { readonly kind: 'set-portfolio-virtual'; readonly value: boolean }
    | { readonly kind: 'start-book' }
    | { readonly kind: 'open-book'; readonly book: Book }
    | { readonly kind: 'set-field'; readonly field: BookField; readonly value: string }
    | { readonly kind: 'set-sweepable'; readonly value: boolean }
    | { readonly kind: 'revert-fields'; readonly book: Book }
    | { readonly kind: 'set-reason'; readonly reasonCode: string }
    | { readonly kind: 'set-commentary'; readonly commentary: string }
    | { readonly kind: 'record-written'; readonly book: Book | undefined }
    | { readonly kind: 'record-refusal'; readonly refusal: BookRefusal }
    | { readonly kind: 'clear-refusal' };

export function reduceBook(draft: BookDraft, action: BookAction): BookDraft {
    switch (action.kind) {
        case 'select-portfolio':
            return { ...draft, portfolioId: action.portfolioId, newPortfolio: undefined };
        case 'start-portfolio':
            return {
                ...draft,
                portfolioId: '',
                newPortfolio: {
                    id: crypto.randomUUID(),
                    name: '',
                    description: '',
                    parentPortfolioId: action.parentPortfolioId,
                    ownerUnitId: '',
                    purposeType: '',
                    aggregationCcy: '',
                    isVirtual: false,
                },
            };
        case 'portfolio-written':
            // The portfolio now exists, so a retry must not write it as new again.
            return { ...draft, portfolioId: action.portfolioId, newPortfolio: undefined };
        case 'cancel-portfolio':
            return { ...draft, newPortfolio: undefined };
        case 'set-portfolio-field':
            return draft.newPortfolio === undefined
                ? draft
                : {
                      ...draft,
                      newPortfolio: { ...draft.newPortfolio, [action.field]: action.value },
                  };
        case 'set-portfolio-virtual':
            return draft.newPortfolio === undefined
                ? draft
                : { ...draft, newPortfolio: { ...draft.newPortfolio, isVirtual: action.value } };
        case 'start-book':
            return {
                ...draft,
                shaping: true,
                opened: undefined,
                reasonCode: NEW_BOOK_REASON,
                bookId: crypto.randomUUID(),
                fields: blankFields(),
                written: undefined,
                refusal: undefined,
            };
        case 'open-book':
            return {
                ...draft,
                shaping: true,
                opened: action.book,
                bookId: action.book.id,
                portfolioId: action.book.parent_portfolio_id,
                newPortfolio: undefined,
                fields: fieldsOf(action.book),
                written: undefined,
                refusal: undefined,
            };
        case 'set-field':
            return { ...draft, fields: { ...draft.fields, [action.field]: action.value } };
        case 'set-sweepable':
            return { ...draft, fields: { ...draft.fields, isSweepable: action.value } };
        case 'revert-fields':
            // The name is the natural key, so an older version never renames the book.
            return {
                ...draft,
                fields: { ...fieldsOf(action.book), name: draft.fields.name },
                written: undefined,
            };
        case 'set-reason':
            return { ...draft, reasonCode: action.reasonCode };
        case 'set-commentary':
            return { ...draft, commentary: action.commentary };
        case 'record-written':
            return { ...draft, written: action.book, refusal: undefined };
        case 'record-refusal':
            return { ...draft, refusal: action.refusal };
        case 'clear-refusal':
            return { ...draft, refusal: undefined };
    }
}

/** One line of the review: what changes, before, after, and the operation that writes it. */
export interface Change {
    readonly id: string;
    readonly what: string;
    readonly before: string;
    readonly after: string;
    readonly operation: string;
}

const BOOK_LABELS: readonly (readonly [keyof BookFields, string])[] = [
    ['name', 'Name'],
    ['description', 'Description'],
    ['functionalCurrency', 'Functional currency'],
    ['glAccountRef', 'GL account ref'],
    ['costCenter', 'Cost center'],
    ['ownerUnitId', 'Owner unit'],
    ['bookStatus', 'Status'],
    ['regulatoryBookType', 'Regulatory book type'],
    ['bookPurposeType', 'Book purpose'],
    ['ledgerFeedType', 'Ledger feed'],
    ['isSweepable', 'Sweepable'],
    ['ratesCentreCode', 'Rates centre'],
];

function show(value: string | boolean): string {
    if (typeof value === 'boolean') {
        return value ? 'yes' : 'no';
    }
    return value === '' ? '-' : value;
}

/** Every difference from the book as read, and the portfolio the walk creates. */
export function changesOf(draft: BookDraft): readonly Change[] {
    if (!draft.shaping) {
        return [];
    }
    const changes: Change[] = [];
    if (draft.newPortfolio !== undefined) {
        changes.push({
            id: 'portfolio:new',
            what: 'New portfolio',
            before: '-',
            after: draft.newPortfolio.name.trim(),
            operation: 'refdata.v1.portfolios.put',
        });
    }
    const target = draft.newPortfolio?.id ?? draft.portfolioId;
    if (
        draft.opened !== undefined &&
        target !== '' &&
        target !== draft.opened.parent_portfolio_id
    ) {
        changes.push({
            id: 'book:portfolio',
            what: 'Portfolio',
            before: draft.opened.parent_portfolio_id,
            after: target,
            operation: 'refdata.v1.books.put',
        });
    }
    const before = draft.opened === undefined ? blankFields() : fieldsOf(draft.opened);
    for (const [field, label] of BOOK_LABELS) {
        if (draft.fields[field] !== before[field]) {
            changes.push({
                id: `book:${field}`,
                what: label,
                before: show(before[field]),
                after: show(draft.fields[field]),
                operation: 'refdata.v1.books.put',
            });
        }
    }
    return changes;
}

/**
 * The owner units a book may name: the owner unit of its portfolio and of every
 * ancestor of it. The server refuses any other, and the picker marks them.
 */
export function ancestryUnits(
    portfolioId: string,
    portfolios: readonly Portfolio[],
    pending?: PortfolioDraft,
): ReadonlySet<string> {
    const units = new Set<string>();
    const seen = new Set<string>();
    let next: string | null = portfolioId;
    if (pending !== undefined) {
        if (pending.ownerUnitId !== '') {
            units.add(pending.ownerUnitId);
        }
        next = pending.parentPortfolioId === '' ? null : pending.parentPortfolioId;
    }
    while (next !== null && !seen.has(next)) {
        seen.add(next);
        const portfolio = portfolios.find((candidate) => candidate.id === next);
        if (portfolio === undefined) {
            break;
        }
        if (portfolio.owner_unit_id !== null) {
            units.add(portfolio.owner_unit_id);
        }
        next = portfolio.parent_portfolio_id;
    }
    return units;
}

/** The audit fields a write carries empty: the service sets tenancy, provenance and the validity window. */
function stamp(): Pick<
    Book,
    | 'version'
    | 'tenant_id'
    | 'modified_by'
    | 'performed_by'
    | 'change_reason_code'
    | 'change_commentary'
    | 'recorded_at'
> {
    return {
        version: 0,
        tenant_id: '',
        modified_by: '',
        performed_by: '',
        change_reason_code: '',
        change_commentary: '',
        recorded_at: '',
    };
}

/** What the confirm sends, in the order it sends it. */
export interface WritePlan {
    readonly intent: BookIntent;
    /** A portfolio the walk creates, written before the book that sits in it. */
    readonly portfolio: Portfolio | undefined;
    readonly book: Book;
    /** The version the book was read at, or null for a new book. */
    readonly version: number | null;
}

/** The plan the confirm runs, or undefined before a book is being shaped. */
export function writePlan(draft: BookDraft, partyId: string): WritePlan | undefined {
    if (!draft.shaping) {
        return undefined;
    }
    const portfolioId = draft.newPortfolio?.id ?? draft.portfolioId;
    const f = draft.fields;
    const book: Book = {
        ...stamp(),
        ...(draft.opened ?? {}),
        id: draft.bookId,
        party_id: partyId,
        name: draft.opened?.name ?? f.name.trim(),
        description: f.description,
        parent_portfolio_id: portfolioId,
        owner_unit_id: f.ownerUnitId === '' ? null : f.ownerUnitId,
        functional_currency: f.functionalCurrency,
        gl_account_ref: f.glAccountRef,
        cost_center: f.costCenter,
        book_status: f.bookStatus,
        regulatory_book_type: f.regulatoryBookType,
        book_purpose_type: f.bookPurposeType,
        ledger_feed_type: f.ledgerFeedType,
        is_sweepable: f.isSweepable,
        rates_centre_code: f.ratesCentreCode,
        sandbox_id: draft.opened?.sandbox_id ?? null,
    };
    const pending = draft.newPortfolio;
    const portfolio: Portfolio | undefined =
        pending === undefined
            ? undefined
            : {
                  ...stamp(),
                  id: pending.id,
                  party_id: partyId,
                  name: pending.name.trim(),
                  description: pending.description,
                  parent_portfolio_id:
                      pending.parentPortfolioId === '' ? null : pending.parentPortfolioId,
                  owner_unit_id: pending.ownerUnitId === '' ? null : pending.ownerUnitId,
                  purpose_type: pending.purposeType,
                  aggregation_ccy: pending.aggregationCcy,
                  is_virtual: pending.isVirtual,
                  sandbox_id: null,
                  status: 'Active',
              };
    return {
        intent: { reason_code: draft.reasonCode, commentary: draft.commentary },
        portfolio,
        book,
        version: draft.opened === undefined ? null : draft.opened.version,
    };
}

/** Whether the book can be written: it is named, placed and classified. */
export function bookProblems(draft: BookDraft): readonly string[] {
    const problems: string[] = [];
    if (!draft.shaping) {
        return problems;
    }
    if (draft.fields.name.trim() === '') {
        problems.push('Name the book.');
    }
    if (draft.portfolioId === '' && draft.newPortfolio === undefined) {
        problems.push('Choose the portfolio the book sits in.');
    }
    if (draft.newPortfolio !== undefined) {
        const pending = draft.newPortfolio;
        if (pending.name.trim() === '') {
            problems.push('Name the new portfolio.');
        }
        if (pending.aggregationCcy === '' || pending.purposeType === '') {
            problems.push('Give the new portfolio a purpose and an aggregation currency.');
        }
    }
    if (draft.fields.functionalCurrency === '') {
        problems.push('Choose the functional currency.');
    }
    if (
        draft.fields.bookStatus === '' ||
        draft.fields.regulatoryBookType === '' ||
        draft.fields.bookPurposeType === '' ||
        draft.fields.ledgerFeedType === ''
    ) {
        problems.push('Set the status and the three classification axes.');
    }
    return problems;
}

export interface BookStructure extends BookDraft {
    readonly changes: readonly Change[];
    readonly selectPortfolio: (portfolioId: string) => void;
    readonly startPortfolio: (parentPortfolioId: string) => void;
    readonly cancelPortfolio: () => void;
    readonly portfolioWritten: (portfolioId: string) => void;
    readonly setPortfolioField: (field: PortfolioField, value: string) => void;
    readonly setPortfolioVirtual: (value: boolean) => void;
    readonly startBook: () => void;
    readonly openBook: (book: Book) => void;
    readonly setField: (field: BookField, value: string) => void;
    readonly setSweepable: (value: boolean) => void;
    readonly revertFields: (book: Book) => void;
    readonly setReason: (reasonCode: string) => void;
    readonly setCommentary: (commentary: string) => void;
    readonly recordWritten: (book: Book | undefined) => void;
    readonly recordRefusal: (refusal: BookRefusal) => void;
    readonly clearRefusal: () => void;
}

export function useBookStructure(): BookStructure {
    const [draft, dispatch] = useReducer(reduceBook, undefined, blankDraft);
    return {
        ...draft,
        changes: changesOf(draft),
        selectPortfolio: (portfolioId) => dispatch({ kind: 'select-portfolio', portfolioId }),
        startPortfolio: (parentPortfolioId) =>
            dispatch({ kind: 'start-portfolio', parentPortfolioId }),
        cancelPortfolio: () => dispatch({ kind: 'cancel-portfolio' }),
        portfolioWritten: (portfolioId) => dispatch({ kind: 'portfolio-written', portfolioId }),
        setPortfolioField: (field, value) =>
            dispatch({ kind: 'set-portfolio-field', field, value }),
        setPortfolioVirtual: (value) => dispatch({ kind: 'set-portfolio-virtual', value }),
        startBook: () => dispatch({ kind: 'start-book' }),
        openBook: (book) => dispatch({ kind: 'open-book', book }),
        setField: (field, value) => dispatch({ kind: 'set-field', field, value }),
        setSweepable: (value) => dispatch({ kind: 'set-sweepable', value }),
        revertFields: (book) => dispatch({ kind: 'revert-fields', book }),
        setReason: (reasonCode) => dispatch({ kind: 'set-reason', reasonCode }),
        setCommentary: (commentary) => dispatch({ kind: 'set-commentary', commentary }),
        recordWritten: (book) => dispatch({ kind: 'record-written', book }),
        recordRefusal: (refusal) => dispatch({ kind: 'record-refusal', refusal }),
        clearRefusal: () => dispatch({ kind: 'clear-refusal' }),
    };
}
